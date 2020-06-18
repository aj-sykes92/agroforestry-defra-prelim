library(tidyverse)

hdg_env <- new.env()

#####################################
# add field boundary data
#####################################
hdg_env$add_boundary_data <- function(df){
  
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
  
  # join to df
  df <- df %>%
    left_join(fl_length, by = c("crop", "da_num")) %>%
    mutate(planted_length_m = fl_length * area_ha,
           planted_area_ha = planted_length_m * 1.5 * 10^-4,
           yield_tha_agf = yield_tha,
           area_impact = 1) %>%
    select(-fl_length)
  
  return(df)
  
}

#####################################
# calculate total costs
#####################################
hdg_env$add_hedge_costs <- function(df, discount_rate){
  
  # planting costs (Nix pocketbook, 2018)
  # 20 year lifespan (Hedgerow Management and Wildlife, Barr et al.)
  set_cost_m <- mean(c(0.54, 1.36))
  lab_cost_m <- mean(c(2.9, 3.9))
  
  plant_cost_m <- annualise_cost(set_cost_m + lab_cost_m, lifespan = 20, dr = discount_rate)
  
  # cutting costs (Nix pocketbook, 2018)
  cut_cost_day <- mean(c(39.7, 48.0)) * 8 # assume 8 hour day
  cut_cost_m <- cut_cost_day / 6437 # average 4 miles a day = 6437 m
  
  cost_m <- plant_cost_m + cut_cost_m
  
  df <- df %>%
    mutate(hdgcost_gbp = planted_length_m * cost_m)
  
  return(df)
  
}

#####################################
# calculate abatement
#####################################
hdg_env$add_abatement <- function(df){
  
  co2_thayear <- (34.86 - 2.6) * 44/12 * 1/20 # from Amanda/CEH assumptions
  
  # scale seq potential / year by tnpp
  av_tnpp <- read_rds("simulation-base-data/crop-base-data.rds") %>%
    filter(crop != "upland") %>% # not relevant
    pull(tree_tnpp) %>%
    mean()
  
  df <- df %>%
    mutate(tnpp_ratio = tree_tnpp / av_tnpp,
           co2_tyear = planted_area_ha * co2_thayear * tnpp_ratio)
  
  return(df)
}

#####################################
# calculate total cost, abatement and MAC (spacial case here for hedges only)
#####################################
hdg_env$calc_mac <- function(df){
  df %>%
    mutate(ar_tha = co2_tyear / area_ha,
           totrev_gbp = -hdgcost_gbp,
           mac_gbp_tco2 = -totrev_gbp / co2_tyear)
}

#####################################
# wrapper function
#####################################
build_hdg_agf <- function(discount_rate,
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
    hdg_env$add_boundary_data() %>%
    hdg_env$add_hedge_costs(discount_rate) %>%
    hdg_env$add_abatement() %>%
    hdg_env$calc_mac() %>%
    return()
}



