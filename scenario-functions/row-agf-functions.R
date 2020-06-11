
library(tidyverse)

#####################################
# C per tree + vol per tree estimation
#####################################

Dat_crop <- read_rds("simulation-base-data/crop-base-data.rds")

# dummy data, adjusted to 1 ha area per row to sensecheck aggregate calcs
set.seed(2605)
Dat_dummy <- Dat_crop %>%
  sample_n(25, replace = F) %>%
  mutate(croprev_gbp = croprev_gbp / area_ha,
         area_ha = area_ha / area_ha)
         

add_tree_data <- function(df, felling_age){
  
  # read in dataset from preprocessing
  row_agf_data <- read_rds("row-agroforestry-data-processing/row-agroforestry-data-clean.rds")
  
  # add in tree spp to df based on soil type
  df <- df %>%
    mutate(spp = ifelse(clay > 25, "OK", "SAB")) # plant oak on soils with > 25% clay
  
  # nest df
  Dat_nest <- row_agf_data %>%
    group_by(spp, yield_class) %>%
    nest()
  
  # add loess smoothers for timeseries and interpolate
  Dat_nest <- Dat_nest %>%
    mutate(CO2_loess = map(data, ~loess(CO2_per_tree ~ period_end, data = .x)),
           vol_loess = map(data, ~loess(vol_tree ~ period_end, data = .x))) %>%
    mutate(CO2_tree = map_dbl(CO2_loess, ~predict(.x, newdata = felling_age)),
           vol_tree = map_dbl(vol_loess, ~predict(.x, newdata = felling_age)))
  
  # add loess #2 for relative yield
  Dat_nest <- Dat_nest %>%
    ungroup() %>%
    group_by(spp) %>%
    mutate(yield_rel = (yield_class - min(yield_class)) / (max(yield_class) - min(yield_class))) %>% # subtract min, divide by range
    nest() %>%
    mutate(CO2_loess = map(data, ~suppressWarnings(loess(CO2_tree ~ yield_rel, data = .x, span = 1.1))), # loess makes a fuss due to small sample, but it's used for interpolation only
           vol_loess = map(data, ~suppressWarnings(loess(vol_tree ~ yield_rel, data = .x, span = 1.1)))) %>%
    select(spp, CO2_loess, vol_loess)
  
  # rel yield from df based on ag npp
  df <- df %>%
    mutate(tree_rel_yield = (tree_anpp -min(tree_anpp)) / (max(tree_anpp) - min(tree_anpp))) # equivalant operation to yield class std.
  
  # add models + make predictions
  df <- df %>%
    left_join(Dat_nest, by = "spp") %>%
    mutate(CO2_tree = map2_dbl(CO2_loess, tree_rel_yield, ~predict(.x, newdata = .y)),
           vol_tree = map2_dbl(vol_loess, tree_rel_yield, ~predict(.x, newdata = .y))) %>%
    select(-CO2_loess, -vol_loess, -tree_rel_yield)
  
  return(df)
  
}

# add_tree_data(Dat_crop, felling_age = 50) # works

Dat_agf_row <- Dat_dummy %>% add_tree_data(felling_age = 50)

#####################################
# agroforestry system scaling
#####################################
scale_system <- function(Dat_crop, row_spacing){
  
  spacings <- read_rds("row-agroforestry-data-processing/row-agroforestry-data-clean.rds") %>%
    select(spp, agf_spacing_min, agf_spacing_max) %>%
    distinct() %>%
    mutate(inrow_spacing_m = (agf_spacing_min + agf_spacing_max) / 2) %>%
    select(spp, inrow_spacing_m)
    
  Dat_crop <- Dat_crop %>%
    left_join(spacings, by = "spp") %>%
    mutate(planted_length_m = 100 / row_spacing * 100 * area_ha,
           planted_area_ha = planted_length_m * 2 * 10^-4, # tree area in ha. Burgess et al. (2003) estimate 2m row width for trees
           ntrees = planted_length_m / inrow_spacing_m * area_ha)
  
  return(Dat_crop)
  
}

Dat_agf_row <- Dat_agf_row %>%
  scale_system(row_spacing = 10)

#####################################
# crop yield impacts
#####################################



         
?loess
  
