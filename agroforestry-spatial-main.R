
library(raster)
library(tidyverse)

bigdata_repo <- "GIS data repository"
projdata_repo <- "DEFRA Clean Growth Project/agroforestry-refinement"

#####################################
# process spatial data
#####################################

# UK shapefile
Shp_UK <- shapefile(find_onedrive(dir = bigdata_repo, path = "DA shapefile/GBR_adm_shp/GBR_adm1.shp"))

# pasture area
Ras_pastarea <- raster(find_onedrive(dir = bigdata_repo, path = "Created rasters/UK-pasture-area-10km-CLC-based-WGS84-2.tif"))

# pasture yield
Ras_pastyield <- raster(find_onedrive(dir = bigdata_repo, path = "Created rasters/UK-pasture-yield-RB209-10km.tif"))

# workable lowland pasture fraction
Ras_past_workable <- raster(find_onedrive(dir = bigdata_repo, path = "Created rasters/UK-pasture-area-10km-CLC-based-WGS84-lowland-workable.tif"))

# fractions not on peat
Ras_past_minfrac <- raster(find_onedrive(dir = bigdata_repo, path = "Created rasters/UK-pasture-fraction-not-under-peat-10km-CLC-based-WGS84.tif"))
Ras_crop_minfrac <- raster(find_onedrive(dir = bigdata_repo, path = "Created rasters/UK-crop-fraction-not-under-peat-10km-CLC-based-WGS84.tif"))

# soil characteristics
Brk_soil <- stack(
  raster(find_onedrive(dir = bigdata_repo, path = "SoilGrids 5km/Sand content/Fixed/SNDPPT_M_sl4_5km_ll.tif")), # sand %
  raster(find_onedrive(dir = bigdata_repo, path = "SoilGrids 5km/Silt content/Fixed/SLTPPT_M_sl4_5km_ll.tif")), # silt %
  raster(find_onedrive(dir = bigdata_repo, path = "SoilGrids 5km/Clay content/Fixed/CLYPPT_M_sl4_5km_ll.tif")) # clay %
)

# crop areas and yields
# read in crop area raster data and stack
uk_crops <- c("barley", "cereals_other", "oil_crops_other", "potato", "pulses_other", "rapeseed", "vegetable", "wheat")

readdir <- find_onedrive(dir = bigdata_repo, path = "MapSPAM data/Physical area")
file.names <- paste0("phys_area_", uk_crops, ".tif")

Brk_croparea <- raster::stack()
for(i in 1:length(file.names)){
  readpath <- paste(readdir, file.names[i], sep="/") # aggregate strings to create filepath
  x <- raster(readpath) # read raster
  Brk_croparea <- addLayer(Brk_croparea, x)
  rm(x)
  print(file.names[i])
}

# read in crop yield raster data and stack
readdir <- find_onedrive(dir = bigdata_repo, path = "MapSPAM data/Yield")
file.names <- paste0("yield_", uk_crops, ".tif")

Brk_cropyield <- raster::stack()
for(i in 1:length(file.names)){
  readpath <- paste(readdir, file.names[i], sep="/") # aggregate strings to create filepath
  x <- raster(readpath) # read raster
  Brk_cropyield <- addLayer(Brk_cropyield, x)
  rm(x)
  print(file.names[i])
}
rm(readdir, file.names, readpath, i, uk_crops)

# crop and mask global datasets
Brk_croparea <- Brk_croparea %>% crop(Shp_UK) %>% mask(Shp_UK)
Brk_cropyield <- Brk_cropyield %>% crop(Shp_UK) %>% mask(Shp_UK)
Brk_soil <- Brk_soil %>% crop(Shp_UK) %>% mask(Shp_UK)

# crop and mask UK stuff — necessary to sort minor extent issues
Ras_past_minfrac <- Ras_past_minfrac %>% crop(Shp_UK) %>% mask(Shp_UK)
Ras_crop_minfrac <- Ras_crop_minfrac %>% crop(Shp_UK) %>% mask(Shp_UK)
Ras_pastarea <- Ras_pastarea %>% crop(Shp_UK) %>% mask(Shp_UK)
Ras_pastyield <- Ras_pastyield %>% crop(Shp_UK) %>% mask(Shp_UK)
Ras_past_workable <- Ras_past_workable %>% crop(Shp_UK) %>% mask(Shp_UK)

# stacks for crops and pasture
Brk_crop <- stack(Brk_soil, Ras_crop_minfrac, Brk_croparea, Brk_cropyield)
Brk_pasture <- stack(Brk_soil, Ras_past_minfrac, Ras_pastarea, Ras_pastyield, Ras_past_workable)

# remove all preliminary/original variables
rm(Ras_past_minfrac, Ras_crop_minfrac, Ras_pastarea, Ras_pastyield, Ras_past_workable, Brk_croparea, Brk_cropyield, Brk_soil)

#####################################
# build crop base data frame
#####################################

# convert to data frame
Dat_crop <- Brk_crop %>%
  as.data.frame(xy = T) %>%
  as_tibble() %>%
  rename(sand = SNDPPT_M_sl4_5km_ll,
         silt = SLTPPT_M_sl4_5km_ll,
         clay = CLYPPT_M_sl4_5km_ll,
         min_frac = UK.crop.fraction.not.under.peat.10km.CLC.based.WGS84) %>%
  drop_na(sand) %>%
  gather(-(x:min_frac), key = "key", value = "value")

Dat_crop <- Dat_crop %>%
  mutate(metric = key %>% str_extract("^phys_area|^yield"),
         crop = key %>% str_replace_all("phys_area_|yield_", "")) %>%
  select(-key) %>%
  spread(key = metric, value = value)

# final na drop and rename
Dat_crop <- Dat_crop %>%
  drop_na(phys_area, yield) %>%
  rename(area_ha = phys_area,
         yield_tha = yield) %>%
  mutate(min_frac = ifelse(is.na(min_frac), mean(min_frac, na.rm = T), min_frac))

# adjust crop area to mineral fraction only
Dat_crop <- Dat_crop %>%
  mutate(area_ha = area_ha * min_frac) %>%
  select(-min_frac)

#####################################
# original crop sale values
#####################################

# sale values for different crops from FMH 17/18, all in 2017 GBP
# for grass, estimate is mean production cost from FMH 17/18
# linseed uses OSR values, potatoes assumes dual purpose and price is weighted according to relative yields
# vegetables takes data for potatoes — very similar to most veg prices
Dat_saleval <- tibble(crop = Dat_crop %>% pull(crop) %>% unique(),
                      maincrop_saleval = c(145, 155, 325, 113, 200, 325, 113, 165),
                      bycrop_saleval = c(55, 50, 0, 0, 0, 0, 0, 50), # secondary crop e.g. straw
                      bycrop_ratio = c(0.55, 0.60, 0, 0, 0, 0, 0, 0.53)) # ratio of secondary crop to main crop yield

# join sale values to main data
Dat_crop <- Dat_crop %>%
  left_join(Dat_saleval, by = "crop") %>%
  mutate(croprev_gbp = yield_tha * maincrop_saleval + yield_tha * bycrop_ratio * bycrop_saleval) %>%
  select(-maincrop_saleval, -bycrop_ratio, -bycrop_saleval)


