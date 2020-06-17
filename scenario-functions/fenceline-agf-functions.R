library(tidyverse)

fl_env <- new.env()

# function set is built on row agf functions -- with minor differences
source("scenario-functions/row-agf-functions.R")

#####################################
# agroforestry system scaling
#####################################
fl_env$scale_system <- function(df){
  
  # field boundary data
  boundaries <- read_csv("field-boundary-estimation/field-boundaries-per-ha.csv", col_types = "cccnnn") %>%
    filter(feature == "Fence") %>%
    left_join(tibble(country = c("England", "Wales", "Scotland"),
                     da_num = 1:3),
              by = "country") %>%
    select(da_num, ag_type, fl_length = length_be)
  
  # northern ireland imputation
  ni <- boundaries %>%
    group_by(ag_type) %>%
    summarise(fl_length = mean(fl_length)) %>%
    mutate(da_num = 4)
  
  boundaries <- bind_rows(list(boundaries, ni)) %>% arrange(da_num)
  
  fl_length <- read_rds("simulation-base-data/crop-base-data.rds") %>%
    distinct(crop, da_num) %>%
    mutate(ag_type = ifelse(crop == "pasture" | crop == "upland",
                            "Grass",
                            "Arable")) %>%
    left_join(boundaries, by = c("ag_type", "da_num")) %>%
    select(-ag_type)
  
  spacings <- read_rds("row-agroforestry-data-processing/row-agroforestry-data-clean.rds") %>%
    select(spp, agf_spacing_min, agf_spacing_max) %>%
    distinct() %>%
    mutate(inrow_spacing_m = (agf_spacing_min + agf_spacing_max) / 2) %>%
    select(spp, inrow_spacing_m)
  
  df <- df %>%
    left_join(spacings, by = "spp") %>%
    left_join(fl_length, by = c("crop", "da_num")) %>%
    mutate(planted_length_m = fl_length * area_ha,
           planted_area_ha = planted_length_m * 0 * 10^-4, # modified from row_agf func -- assume no space taken
           ntrees = planted_length_m / inrow_spacing_m,
           area_impact = 1 - (planted_area_ha / area_ha)) %>%
    select(-fl_length)
  
  return(df)
  
}

#####################################
# crop yield and margin impacts
#####################################
# impact is zero -- added + modified for parity with row agf calcs
fl_env$add_crop_impacts <- function(df) {
  
  df <- df %>%
    mutate(frac_yi = 1,
           yield_tha_agf = yield_tha * frac_yi,
           gm_gbp_agf = (croprev_gbp * frac_yi - varcosts_gbp) * area_impact)
  
  return(df)
}

#####################################
# wrapper function
#####################################
build_fl_agf <- function(felling_age, row_spacing, discount_rate,
                          applies_to = c("barley",
                                         "cereals_other",
                                         "oil_crops_other",
                                         "pasture",
                                         "pulses_other",
                                         "rapeseed",
                                         "wheat")
) {
  
  source("scenario-functions/univ-functions.R")
  
  read_rds("simulation-base-data/crop-base-data.rds") %>%
    filter_crops(applies_to) %>%
    row_env$add_tree_data(felling_age) %>%
    fl_env$scale_system() %>%
    fl_env$add_crop_impacts() %>%
    row_env$add_agf_margins(felling_age, discount_rate) %>%
    row_env$add_abatement(felling_age) %>%
    calc_mac() %>%
    return()
  
}