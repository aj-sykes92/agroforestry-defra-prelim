
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
Ras_pastyield <- raster(find_onedrive(dir = bigdata_repo, path = "Created rasters/pasture-yield-estimates/pasture-yield-tha-rb209-w-mod-nrate-57-wgs84-5km.tif"))

# upland yield (no fertiliser)
Ras_uplandyield <- raster(find_onedrive(dir = bigdata_repo, path = "Created rasters/pasture-yield-estimates/pasture-yield-tha-rb209-w-mod-nrate-0-wgs84-5km.tif"))

# workable lowland pasture fraction
Ras_past_workable <- raster(find_onedrive(dir = bigdata_repo, path = "Created rasters/UK-pasture-area-10km-CLC-based-WGS84-lowland-workable.tif"))

# fractions not on peat
Ras_past_minfrac <- raster(find_onedrive(dir = bigdata_repo, path = "Created rasters/UK-pasture-fraction-not-under-peat-10km-CLC-based-WGS84.tif"))
Ras_crop_minfrac <- raster(find_onedrive(dir = bigdata_repo, path = "Created rasters/UK-crop-fraction-not-under-peat-10km-CLC-based-WGS84.tif"))

# SOC fractions
Ras_past_soc <- raster(find_onedrive(dir = bigdata_repo, path = "Created rasters/UK-pasture-SOC-tonnes-ha-10km-CLC-SG250-WGS84.tif"))
Ras_crop_soc <- raster(find_onedrive(dir = bigdata_repo, path = "Created rasters/UK-crop-SOC-tonnes-ha-10km-CLC-SG250-WGS84.tif"))

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

Ras_past_soc <- Ras_past_soc %>% crop(Shp_UK) %>% mask(Shp_UK)
Ras_crop_soc <- Ras_crop_soc %>% crop(Shp_UK) %>% mask(Shp_UK)

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

Brk_main <- stack(Brk_soil, Brk_clim, Ras_solar, Ras_crop_minfrac, Ras_past_minfrac, Ras_crop_soc, Ras_past_soc, Ras_pastarea, Ras_pastyield, Ras_uplandyield, Ras_past_workable, Brk_croparea, Brk_cropyield)

# remove all preliminary/original variables
rm(Ras_past_minfrac, Ras_crop_minfrac, Ras_pastarea, Ras_pastyield, Ras_uplandyield, Ras_past_workable, Brk_croparea, Brk_cropyield, Brk_soil, Brk_clim)

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
         soc_crop = UK.crop.SOC.tonnes.ha.10km.CLC.SG250.WGS84,
         soc_grass = UK.pasture.SOC.tonnes.ha.10km.CLC.SG250.WGS84,
         area_grassland = UK.pasture.area.10km.CLC.based.WGS84.2,
         yield_pasture = pasture.yield.tha.rb209.w.mod.nrate.57.wgs84.5km,
         yield_upland = pasture.yield.tha.rb209.w.mod.nrate.0.wgs84.5km,
         phys_area_pasture = UK.pasture.area.10km.CLC.based.WGS84.lowland.workable) %>%
  drop_na(sand) %>%
  mutate(phys_area_upland = (area_grassland - phys_area_pasture)* 10^6 / 10^4, # upland pasture in ha
         phys_area_pasture = phys_area_pasture * 10^6 / 10^4, # convert from km2 to ha before gathering
         pH = pH / 10) %>% # it's x10 in original raster
  select(-area_grassland) %>%
  gather(-c(x:soc_grass, mat, map), key = "key", value = "value")

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
# drop any newly created zero areas
Dat_crop <- Dat_crop %>%
  mutate(area_ha = ifelse(crop == "pasture" | crop == "upland",
                          area_ha * min_frac_grass,
                          area_ha * min_frac_crop),
         soc = ifelse(crop == "pasture" | crop == "upland",
                       soc_grass,
                       soc_crop)) %>%
  select(-min_frac_crop, -min_frac_grass, -soc_grass, -soc_crop) %>%
  filter(area_ha != 0)

#####################################
# impute pasture yield nas for Northern Ireland (missing from model) and GHIs and SOCs where missing
#####################################
Dat_crop <- Dat_crop %>%
  group_by(crop) %>%
  mutate(yield_tha = ifelse(is.na(yield_tha),
                            mean(yield_tha, na.rm = T),
                            yield_tha),
         ghi = ifelse(is.na(ghi),
                      mean(ghi, na.rm = T),
                      ghi),
         soc = ifelse(is.na(soc),
                      mean(soc, na.rm = T),
                      soc)) %>%
           ungroup()

#####################################
# original crop sale values
#####################################

# sale values for different crops from FMH 19/20, all in 2019 GBP
# for grass, estimate is converted from livestock enterprise gms -- sheep on upland, beef on lowland
# cereals, other = oats
# oil crops, other = OSR
# potatoes assumes dual purpose and price is weighted according to relative yields
# note potato costs very close to revenue — GM set to 0
# vegetables = brassicas
# pulses, other = field beans

add_saleval <- function(df) {
  
  #############################################################################
  # add values for arable crops
  #############################################################################
  
  # sale values for arable crops
  Dat_saleval <- tibble(crop = df %>% pull(crop) %>% unique(),
                        maincrop_saleval = c(NA, NA, 135, 150, 330, 5090/45, 205, 330, 420, 150), # main crop sale value, gbp / tonne fw
                        bycrop_saleval = c(NA, NA, 55, 50, 0, 0, 0, 0, 0, 50), # secondary crop e.g. straw sale value, gbp / tonne fw
                        bycrop_ratio = c(NA, NA, 0.55, 0.60, 0, 0, 0, 0, 0, 0.53), # ratio of secondary crop to main crop yield
                        varcosts_gbpha = c(NA, NA, 433, 366, 393, NA, 273, 393, 1917, 488)) # variable costs per hectare, gbp
  
  # for all arable crops
  df <- df %>%
    left_join(Dat_saleval, by = "crop") %>%
    mutate(croprev_gbp = (yield_tha * maincrop_saleval + yield_tha * bycrop_ratio * bycrop_saleval) * area_ha,
           varcosts_gbp = ifelse(crop == "potato", croprev_gbp, varcosts_gbpha * area_ha)) # potato assume breakeven based on FMH data
  
  # tidy up
  df <- df %>%
    select(-maincrop_saleval, -bycrop_ratio, -bycrop_saleval, -varcosts_gbpha)

  #############################################################################
  # special case for pasture data -- NA so far -- assuming used to graze cattle
  #############################################################################
  
  # grazing stats
  mean_grazyield <- df %>%
    filter(crop == "pasture") %>%
    pull(yield_tha) %>%
    mean(na.rm = T)
  
  cows_ha <- 379 / 152 # fmh pg 159, lowground suckler system, gm per ha / gm per cow
  rev_cow <- 731 # revenue per cow
  cost_cow <- 579 # cost per cow
  
  # add in grazing calculations
  df <- df %>%
    mutate(croprev_gbp = ifelse(crop == "pasture",
                                rev_cow * cows_ha * yield_tha / mean_grazyield * area_ha,
                                croprev_gbp),
           varcosts_gbp = ifelse(crop == "pasture",
                                 cost_cow * cows_ha * yield_tha / mean_grazyield * area_ha,
                                 varcosts_gbp))
  
  #############################################################################
  # special case for upland data -- NA so far -- assuming used to graze cattle
  #############################################################################
  
  # livestock units per tonne grassland DM, pg 76 fmh 2019/20
  lu_tonne_dm <- 0.46 / 3
  
  # lu for 100 sheep + repl + rams + lambs, 90% lambing, hill extensive (pg 184 + 116, fmh 2019/20)
  lu_sheep <- 100 * sum(c(0.08 * 1, # ewes
                          0.08 * 0.26, # repl hoggs
                          0.04 * 0.09, # lambs,
                          0.08 * 0.03)) # rams
  
  rev_sheep <- 2746
  cost_sheep <- 2175
  
  # add to df
  df <- df %>%
    mutate(croprev_gbp = ifelse(crop == "upland",
                                rev_sheep * (lu_tonne_dm * yield_tha) / lu_sheep * area_ha, # revenue * fraction of 100 sheep enterprise per ha * area
                                croprev_gbp),
           varcosts_gbp = ifelse(crop == "upland",
                                 cost_sheep * (lu_tonne_dm * yield_tha) / lu_sheep * area_ha, # cost * fraction of 100 sheep enterprise per ha * area
                                 varcosts_gbp)) 
  
  # return
  return(df)
}

# join sale values to main data
Dat_crop <- Dat_crop %>%
  add_saleval()

# add gm
Dat_crop <- Dat_crop %>%
  mutate(gm_gbp = croprev_gbp - varcosts_gbp)

# check gms calculated with yields from spatial data
# big spread in veg especially, but not out of bounds of FMH estimates
Dat_crop %>%
  filter(crop != "vegetable") %>%
  mutate(gm_ha = gm_gbp / area_ha) %>%
  ggplot(aes(x = gm_ha)) +
  geom_histogram() +
  facet_wrap(~ crop)

#####################################
# calculate tree NPP according to Del Grosso et al. 2008
#####################################

# above ground net primary productivity equation, pg 2120
add_tree_anpp <- function(df){
  df %>%
    mutate(fmap = 0.1665 * map^1.185 / exp(0.000414 * map),
           fmat = 3139 / (1 + exp(2.2 - 0.0307 * mat)),
           tree_anpp = ifelse(fmap < fmat, fmap, fmat)) %>%
    select(-fmap, -fmat)
}

# total ground net primary productivity equation, pg 2120
add_tree_tnpp <- function(df){
  df %>%
    mutate(fmap = 0.551 * map^1.055 / exp(0.000306 * map),
           fmat = 2540 / (1 + exp(1.584 - 0.0622 * mat)),
           tree_tnpp = ifelse(fmap < fmat, fmap, fmat)) %>%
    select(-fmap, -fmat)
}

Dat_crop <- Dat_crop %>%
  add_tree_anpp() %>%
  add_tree_tnpp() %>%
  select(-map, -mat)

#####################################
# write out crop base data files
#####################################
write_rds(Dat_crop, "simulation-base-data/crop-base-data.rds")

# END

