library(raster)
library(tidyverse)
library(lubridate)
library(rasterVis)

bigdata_repo <- "GIS data repository"

# note pulling data from different project data repository here
projdata_repo <- "AgRE Calc PLC/Development work/Soil C methodology/Model update/Data preprocessing/"

# UK shapefile
Shp_UK <- shapefile(find_onedrive(dir = bigdata_repo, path = "DA shapefile/GBR_adm_shp/GBR_adm1.shp"))

#######################################
# read in raw data file from AC-defra-grass-growth-gis-modifiers
#######################################
Dat_GGC <- read_rds(find_onedrive(dir = projdata_repo,
                                  path = "Defra RB209 grass model modifiers/RB209 grass growth GIS full data with modifiers.rds"))

glimpse(Dat_GGC)

#######################################
# manipulate data from Armstrong et al. (1997) to estimate altitude effect
#######################################
Dat_alt <- read_csv(find_onedrive(dir = projdata_repo,
                                  path = "Defra RB209 grass model modifiers/armstrong-1997-grass-alt-model.csv"))

Dat_alt <- tibble(day = dmy("01-01-2019") + 0:364,
                  month = month(day)) %>%
  full_join(Dat_alt %>%
              mutate(alt_mean = (alt_min + alt_max) / 2,
                     month = paste0("01-", month, "-2019") %>%
                       dmy() %>%
                       month()),
            by = "month")

Dat_alt_summ <- Dat_alt %>%
  group_by(alt_mean) %>%
  summarise(adj_fac = mean(adj_fac))

alt_adj_model <- lm(adj_fac ~ alt_mean, data = Dat_alt_summ)

# function to predict loss of productivity with increasing altitude
alt_adj <- function(altitude){
  x <- predict(alt_adj_model, newdata = tibble(alt_mean = altitude))
  x <- ifelse(x > 1, 1, x)
  x <- ifelse(x < 0, 0, x)
  return(x)
}

#######################################
# read in data for Woodlands Field model predict grass yield response to pH
#######################################

Dat_pH <- read_csv(find_onedrive(dir = projdata_repo, path = "Defra RB209 grass model modifiers/woodlands-ph-rotation-model-parameters.csv"))
Dat_pH <- Dat_pH %>% filter(Crop == "Hay")

# calculate relative yields at given pH
# based on analyses by Kairsty Topp of Woodlands Field trial data
pH_adj <- function(pH){
  x <- Dat_pH$a_est + (Dat_pH$b_est / (1 + Dat_pH$d_est * pH))
  x <- ifelse(x > 1, 1, x)
  x <- ifelse(x < 0, 0, x)
  return(x)
}

#######################################
# calculate adjustment factors based on defined models + summarise
#######################################

# apply models
Dat_GGC <- Dat_GGC %>%
  mutate(pH = pH / 10, # for some reason it's x10 in original raster
         pH_adj = pH_adj(pH),
         Alt_adj = alt_adj(Altitude_m))

# check results
set.seed(2605)
Dat_GGC %>% sample_n(10^4, replace = F) %>% pull(pH_adj) %>% qplot()
Dat_GGC %>% sample_n(10^4, replace = F) %>% pull(Alt_adj) %>% qplot()

#######################################
# calculate actual grass growth based on Defra RB209 model
#######################################

# read in RB209 digitisation
Dat_rb209 <- find_onedrive(dir = projdata_repo, path = "Defra RB209 grass model/Defra RB209 final models.csv") %>% read_csv()

Dat_rb209 <- Dat_rb209 %>%
  rename(`1` = Vpoor, `2` = Poor, `3` = Av, `4` = Good, `5` = Vgood) %>%
  gather(-N_rate, key = "GGC", value = Yield_tha) %>%
  mutate(GGC = as.integer(GGC))

# join yield for no fertiliser by GGC
Dat_GGC <- Dat_GGC %>%
  left_join(Dat_rb209 %>%
              filter(N_rate == 1) %>%
              select(-N_rate) %>%
              rename(Yield_tha_0n = Yield_tha),
            by = "GGC") %>%
  mutate(Yield_tha_0n = Yield_tha_0n * pH_adj * Alt_adj)

# join yield for BSFP average (57 kg N / ha) by GGC
Dat_GGC <- Dat_GGC %>%
  left_join(Dat_rb209 %>%
              filter(N_rate == 57) %>%
              select(-N_rate) %>%
              rename(Yield_tha_57n = Yield_tha),
            by = "GGC") %>%
  mutate(Yield_tha_57n = Yield_tha_57n * pH_adj * Alt_adj)

# transform to raster stack
Brk_grazyield <- stack(
  rasterFromXYZ(Dat_GGC %>% select(x, y, Yield_tha_0n), crs = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"),
  rasterFromXYZ(Dat_GGC %>% select(x, y, Yield_tha_57n), crs = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
)

# write out hi res rasters
writeRaster(Brk_grazyield[[1]], find_onedrive(dir = bigdata_repo, path = "Created rasters/pasture-yield-estimates/pasture-yield-tha-rb209-w-mod-nrate-0-wgs84-250m.tif"), overwrite = T)
writeRaster(Brk_grazyield[[2]], find_onedrive(dir = bigdata_repo, path = "Created rasters/pasture-yield-estimates/pasture-yield-tha-rb209-w-mod-nrate-57-wgs84-250m.tif"), overwrite = T)

# check
levelplot(Brk_grazyield[[1]]) +
  layer(sp.polygons(Shp_UK, lwd=0.1))

levelplot(Brk_grazyield[[2]]) +
  layer(sp.polygons(Shp_UK, lwd=0.1))

# resample stack to template
Ras_sand <- raster(find_onedrive(dir = bigdata_repo, path = "SoilGrids 5km/Sand content/Fixed/SNDPPT_M_sl4_5km_ll.tif")) %>%
  crop(Shp_UK) %>%
  mask(Shp_UK)

Brk_grazyield <- Brk_grazyield %>% resample(Ras_sand)

# crop to Shp_UK to avoid padding effect
Brk_grazyield <- Brk_grazyield %>%
  crop(Shp_UK) %>%
  mask(Shp_UK)

# write out lo res rasters
writeRaster(Brk_grazyield[[1]], find_onedrive(dir = bigdata_repo, path = "Created rasters/pasture-yield-estimates/pasture-yield-tha-rb209-w-mod-nrate-0-wgs84-5km.tif"), overwrite = T)
writeRaster(Brk_grazyield[[2]], find_onedrive(dir = bigdata_repo, path = "Created rasters/pasture-yield-estimates/pasture-yield-tha-rb209-w-mod-nrate-57-wgs84-5km.tif"), overwrite = T)
