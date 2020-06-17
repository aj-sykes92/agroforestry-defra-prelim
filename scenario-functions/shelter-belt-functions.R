
library(tidyverse)

# new environment
sb_env <- new.env()

sb_env$add_tree_data <- function(df, spp_short, felling_age){
  
  # read in tree data
  Dat_SB <- read_rds("shelter-belt-data-preprocessing/shelter-belt-data-clean.rds")
  
  # filter spp
  Dat_SB <- Dat_SB %>%
    filter(spp == spp_short)
  
  # filter felling age
  felling_age <- round(felling_age / 5, 0) * 5
  
  Dat_SB <- Dat_SB %>%
    filter(period_end == felling_age)
  
  # build tree data
  Dat_treedat <- read_rds("simulation-base-data/crop-base-data.rds") %>%
    distinct(crop) %>%
    mutate(tight = c(T, T, rep(F, 8))) %>% # tight for livestock, loose for arable
    left_join(Dat_SB, by = "tight") %>%
    select(crop, av_tnpp, co2_tha = C_cum_total, vol_m3ha, spacing_m = spacing, wood_type = type)
  
  # add vars to df
  df <- df %>%
    left_join(Dat_treedat, by = "crop") %>%
    mutate(tnpp_ratio = tree_tnpp / av_tnpp,
           treeco2_tha = co2_tha * tnpp_ratio,
           treevol_m3ha = vol_m3ha * tnpp_ratio) %>%
    select(-tnpp_ratio)
  
  return(df)
  
}

#####################################
# agroforestry system scaling
#####################################
sb_env$scale_system <- function(df, belt_spacing, width, length){
  
  # number of complete belts per ha
  nbelts_ha <- (10^4 / length) / (belt_spacing + width)
  
  # belt area
  beltarea_ha <- width * length * 10^-4
  
  # area impact
  area_impact <- 1 - nbelts_ha * beltarea_ha
  
  df <- df %>%
    mutate(area_impact = area_impact,
           planted_area_ha = area_ha * (1 - area_impact))
  
  return(df)
}

#####################################
# crop yield and margin impacts
#####################################
sb_env$add_crop_impacts <- function(df){
  
  # yield improvements from Caborn (1957)
  Dat_yieldimp <- read_rds("simulation-base-data/crop-base-data.rds") %>%
    distinct(crop) %>%
    # removed old forestry commission data -- so old as to be potentially misleading for newer crop varietals
    #mutate(frac_yi = c(mean(c(1.34, 1.241)),
    #                   mean(c(1.34, 1.241)),
    #                   mean(c(1.171, 1.117)),
    #                   mean(c(1.171, 1.117)),
    #                   mean(c(1.171, 1.117)),
    #                   1.169,
    #                   mean(c(1.171, 1.117)),
    #                   mean(c(1.171, 1.117)),
    #                   mean(c(1.232, 1.119)),
    #                   mean(c(1.171, 1.117))))
    mutate(frac_yi = 1.1) # new data from FC x SAC report
  
  df <- df %>%
    left_join(Dat_yieldimp, by = "crop") %>%
    mutate(yield_tha_agf = yield_tha * frac_yi,
           gm_gbp_agf = (croprev_gbp * frac_yi - varcosts_gbp) * area_impact)
  
  return(df)
}

#####################################
# agroforestry cost and revenue
#####################################
sb_env$add_agf_margins <- function(df, felling_age, length, width, discount_rate){
  
  # fencing costs
  belt_circ <- length * 2 + width * 2
  beltarea_ha <- length * width * 10^-4
  fencelength_ha_m <- belt_circ / beltarea_ha
  
  # fmh 19/20, £5.57 per m for deer fence
  # much labour accounted for already in tree planting etc. so assume labour cost absorbed
  # est 30 year lifespan for fence https://www.deerbusters.com/blog/poly-deer-fences-vs-steel-deer-fences/
  deerfence_gbp_m <- 5.57
  regfence_gbp_m <- 3.12
  deerfencecost_ha_gbp <- annualise_cost(fencelength_ha_m * deerfence_gbp_m, felling_age, dr = discount_rate)
  regfencecost_ha_gbp <- annualise_cost(fencelength_ha_m * regfence_gbp_m, felling_age, dr = discount_rate)
  
  # cost dataset
  Dat_cost <- read_rds("simulation-base-data/crop-base-data.rds") %>%
    distinct(crop) %>%
    
    # establishment costs from fmh 19/20 (pg 319)
    # maintenance costs from Trees For Shelter (1997) -- FC x SAC tech note
    mutate(set_cost_tree = annualise_cost(3250 / 7220, felling_age, discount_rate),
           plant_cost_tree = annualise_cost(1080 / 7220, felling_age, discount_rate),
           fence_cost_ha = ifelse(crop == "upland" | crop == "pasture", deerfencecost_ha_gbp, regfencecost_ha_gbp),
           maint_cost_ha = c(30, 20, 30, 30, 30, 30, 30, 30, 30, 30) * 1.832) %>% # inf fac https://www.in2013dollars.com/uk/inflation/1997
    
    mutate(cost_tree = set_cost_tree + plant_cost_tree,
           cost_ha = fence_cost_ha + maint_cost_ha) %>%
    select(crop, cost_tree, cost_ha)
  
  # timber (rough wood) revenue
  rev_tonne <- mean(c(7, 30)) # rough logs value, gbp / tonne, fmh 19/20
  Dat_rev <- tibble(wood_type = c("H", "S"),
                    density = c(1.25, 1.43)) %>%  # m3 / tonne, https://www.forestresearch.gov.uk/tools-and-resources/statistics/forestry-statistics/forestry-statistics-2016-introduction/sources/timber/conversion-factors/
    mutate(rev_m3 = rev_tonne / density) %>%
    select(-density)
  
  df <- df %>%
    left_join(Dat_cost, by = "crop") %>%
    left_join(Dat_rev, by = "wood_type") %>%
    mutate(ntrees = planted_area_ha * (100 / spacing_m)^2,
           timbrev_gbp = annualise_return(rev_m3 * treevol_m3ha * planted_area_ha, felling_age, discount_rate),
           timbcost_gbp = cost_tree * ntrees + cost_ha * planted_area_ha,
           timbgm_gbp = timbrev_gbp - timbcost_gbp) %>%
    select(-rev_m3)
}

#####################################
# final estimates of carbon sequestration
#####################################
sb_env$add_abatement <- function(df, felling_age){
  
  # add ipcc soc scs calculations
  df <- df %>%
    mutate(co2_start = soc * ifelse(crop == "pasture" | crop == "upland", 1, 0.7) * 44/12, # ref * 0.7 if crop, IPCC 2019, Chptr 5, Table 5.10
           co2_end = soc * 44/12, # forest = reference stocks, IPCC 2019, Chptr 4, 4.14
           soil_co2_tyear = (co2_end - co2_start) / felling_age) %>%
    select(-co2_start, -co2_end)
  
  # add tree co2 seq calculations and combine
  df <- df %>%
    mutate(tree_co2_tyear = treeco2_tha / felling_age * planted_area_ha,
           co2_tyear = tree_co2_tyear + soil_co2_tyear) %>%
    select(-tree_co2_tyear, -soil_co2_tyear)
  
  return(df)
}

#####################################
# wrapper function
#####################################
build_sb_agf <- function(spp_short,
                         felling_age,
                         discount_rate,
                         belt_spacing = 250,
                         length = 180,
                         width = 10,
                         applies_to = c("barley",
                                        "cereals_other",
                                        "oil_crops_other",
                                        "pasture",
                                        #"potato",
                                        "pulses_other",
                                        "rapeseed",
                                        "upland",
                                        #"vegetable",
                                        "wheat")
) {
  
  source("scenario-functions/univ-functions.R")
  
  read_rds("simulation-base-data/crop-base-data.rds") %>%
    filter_crops(applies_to) %>%
    sb_env$add_tree_data(spp_short, felling_age) %>%
    sb_env$scale_system(belt_spacing, width, length) %>%
    sb_env$add_crop_impacts() %>%
    sb_env$add_agf_margins(felling_age, length, width, discount_rate) %>%
    sb_env$add_abatement(felling_age) %>%
    calc_mac() %>%
    return()
  
}
