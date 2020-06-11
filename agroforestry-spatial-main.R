
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
  raster(find_onedrive(dir = bigdata_repo, path = "SoilGrids 5km/Clay content/Fixed/CLYPPT_M_sl4_5km_ll.tif")), # clay %
  raster(find_onedrive(dir = bigdata_repo, path = "SoilGrids 5km/Soil pH/Fixed/PHIHOX_M_sl4_5km_ll.tif"))
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

# create monthly precipitation dataset
Brk_clim <- raster::stack()
for(i in 1:12){
  path <- paste0(find_onedrive(dir = bigdata_repo,
                               path = "WorldClim data/Precipitation (mm)/wc2.0_5m_prec_"),
                 formatC(i, width=2, flag="0"),
                 ".tif")
  x <- raster(path)
  Brk_clim <- addLayer(Brk_clim, x)
}

for(i in 1:12){
  path <- paste0(find_onedrive(dir = bigdata_repo,
                               path = "WorldClim data/Average temperature (oC)/wc2.0_5m_tavg_"),
                 formatC(i, width=2, flag="0"),
                 ".tif")
  x <- raster(path)
  Brk_clim <- addLayer(Brk_clim, x)
}
rm(x, i, path)

# global horizontal irradiation
Ras_solar <- raster(find_onedrive(dir = bigdata_repo, path = "Solar GIS data/United-Kingdom_GISdata_LTAy_AvgDailyTotals_GlobalSolarAtlas-v2_GEOTIFF/GHI.tif"))

# crop and mask global datasets
Brk_croparea <- Brk_croparea %>% crop(Shp_UK) %>% mask(Shp_UK)
Brk_cropyield <- Brk_cropyield %>% crop(Shp_UK) %>% mask(Shp_UK)
Brk_soil <- Brk_soil %>% crop(Shp_UK) %>% mask(Shp_UK)
Brk_clim <- Brk_clim %>% crop(Shp_UK) %>% mask(Shp_UK)

# crop and mask UK stuff — necessary to sort minor extent issues
Ras_past_minfrac <- Ras_past_minfrac %>% crop(Shp_UK) %>% mask(Shp_UK)
Ras_crop_minfrac <- Ras_crop_minfrac %>% crop(Shp_UK) %>% mask(Shp_UK)
Ras_pastarea <- Ras_pastarea %>% crop(Shp_UK) %>% mask(Shp_UK)
Ras_pastyield <- Ras_pastyield %>% crop(Shp_UK) %>% mask(Shp_UK)
Ras_past_workable <- Ras_past_workable %>% crop(Shp_UK) %>% mask(Shp_UK)

# resample, crop and mask GHI raster
Ras_solar <- Ras_solar %>%
  resample(Brk_soil[[1]])

Ras_solar <- Ras_solar %>% crop(Shp_UK) %>% mask(Shp_UK)

# stacks for crops and pasture
#Brk_crop <- stack(Brk_soil, Ras_crop_minfrac, Ras_pastyield, Ras_past_workable, Brk_croparea, Brk_cropyield) # includes pasture so as to be able to include lowland pasture here
#Brk_pasture <- stack(Brk_soil, Ras_past_minfrac, Ras_pastarea, Ras_pastyield, Ras_past_workable)

Brk_main <- stack(Brk_soil, Brk_clim, Ras_solar, Ras_crop_minfrac, Ras_past_minfrac, Ras_pastarea, Ras_pastyield, Ras_past_workable, Brk_croparea, Brk_cropyield)

# remove all preliminary/original variables
rm(Ras_past_minfrac, Ras_crop_minfrac, Ras_pastarea, Ras_pastyield, Ras_past_workable, Brk_croparea, Brk_cropyield, Brk_soil, Brk_clim)

#####################################
# build crop base data frame
#####################################

# convert to data frame
Dat_crop <- Brk_main %>%
  as.data.frame(xy = T) %>%
  as_tibble()

Dat_prec <- Dat_crop %>%
  select(wc2.0_5m_prec_01:wc2.0_5m_prec_12)

# calculate MAP + MAT and drop monthly vars
Dat_crop <- Dat_crop %>%
  mutate(map = rowSums(Dat_crop %>% select(wc2.0_5m_prec_01:wc2.0_5m_prec_12)),
         mat = rowMeans(Dat_crop %>% select(wc2.0_5m_tavg_01:wc2.0_5m_tavg_12))) %>%
  select(-(wc2.0_5m_prec_01:wc2.0_5m_tavg_12))

# rename core vars and gather
Dat_crop <- Dat_crop %>% 
  rename(sand = SNDPPT_M_sl4_5km_ll,
         silt = SLTPPT_M_sl4_5km_ll,
         clay = CLYPPT_M_sl4_5km_ll,
         pH = PHIHOX_M_sl4_5km_ll,
         ghi = GHI,
         min_frac_crop = UK.crop.fraction.not.under.peat.10km.CLC.based.WGS84, # use this for pasture too since we're focusing on lowland/tilled pasture
         min_frac_grass = UK.pasture.fraction.not.under.peat.10km.CLC.based.WGS84,
         area_grassland = UK.pasture.area.10km.CLC.based.WGS84.2,
         yield_pasture = UK.pasture.yield.RB209.10km,
         phys_area_pasture = UK.pasture.area.10km.CLC.based.WGS84.lowland.workable) %>%
  drop_na(sand) %>%
  mutate(phys_area_upland = (area_grassland - phys_area_pasture)* 10^6 / 10^4, # upland pasture in ha
         phys_area_pasture = phys_area_pasture * 10^6 / 10^4, # convert from km2 to ha before gathering
         pH = pH / 10) %>% # it's x10 in original raster
  select(-area_grassland) %>%
  gather(-c(x:min_frac_grass, mat, map), key = "key", value = "value")

# spread metrics
Dat_crop <- Dat_crop %>%
  mutate(metric = key %>% str_extract("^phys_area|^yield"),
         crop = key %>% str_replace_all("phys_area_|yield_", "")) %>%
  select(-key) %>%
  spread(key = metric, value = value)

# final na drop and rename
Dat_crop <- Dat_crop %>%
  drop_na(phys_area) %>%
  filter(phys_area != 0) %>% # necessary only for pasture -- some entries have +ve yield w/ 0 area -- artefect of calculation method for this layer
  rename(area_ha = phys_area,
         yield_tha = yield) %>%
  mutate(min_frac_crop = ifelse(is.na(min_frac_crop), mean(min_frac_crop, na.rm = T), min_frac_crop),
         min_frac_grass = ifelse(is.na(min_frac_grass), mean(min_frac_grass, na.rm = T), min_frac_grass))

# adjust crop area to mineral fraction only
Dat_crop <- Dat_crop %>%
  mutate(area_ha = ifelse(crop == "pasture" | crop == "upland",
                          area_ha * min_frac_grass,
                          area_ha * min_frac_crop)) %>%
  select(-min_frac_crop, -min_frac_grass)

# split out upland
Dat_upland <- Dat_crop %>%
  filter(crop == "upland") %>%
  select(-yield_tha)

Dat_crop <- Dat_crop %>%
  filter(crop != "upland")

#####################################
# original crop sale values
#####################################

# sale values for different crops from FMH 17/18, all in 2017 GBP
# for grass, estimate is mean production cost from FMH 17/18
# linseed uses OSR values, potatoes assumes dual purpose and price is weighted according to relative yields
# vegetables takes data for potatoes — very similar to most veg prices
Dat_saleval <- tibble(crop = Dat_crop %>% pull(crop) %>% unique(),
                      maincrop_saleval = c(22.5, 145, 155, 325, 113, 200, 325, 113, 165),
                      bycrop_saleval = c(0, 55, 50, 0, 0, 0, 0, 0, 50), # secondary crop e.g. straw
                      bycrop_ratio = c(0, 0.55, 0.60, 0, 0, 0, 0, 0, 0.53)) # ratio of secondary crop to main crop yield

add_saleval <- function(df){
  df %>%
    left_join(Dat_saleval, by = "crop") %>%
    mutate(croprev_gbp = (yield_tha * maincrop_saleval + yield_tha * bycrop_ratio * bycrop_saleval) * area_ha) %>%
    select(-maincrop_saleval, -bycrop_ratio, -bycrop_saleval)
}

# join sale values to main data
Dat_crop <- Dat_crop %>%
  add_saleval()

#####################################
# calculate tree NPP according to Del Grosso et al. 2008
#####################################

# above ground net primary productivity equation, pg 2120
add_tree_anpp <- function(df){
  df %>%
    mutate(fmap = 0.1665 * map^1.185 / exp(0.000414 * map),
           fmat = 3139 / (1 + exp(2.2 - 0.0307 * mat)),
           tree_anpp = ifelse(fmap < fmat, fmap, fmat)) %>%
    select(-map, -mat, -fmap, -fmat)
}

Dat_crop <- Dat_crop %>%
  add_tree_anpp()

Dat_upland <- Dat_upland %>%
  add_tree_anpp()

#####################################
# write out crop base data files
#####################################

write_rds(Dat_crop, "simulation-base-data/crop-base-data.rds")
write_rds(Dat_upland, "simulation-base-data/upland-base-data.rds")

# END
