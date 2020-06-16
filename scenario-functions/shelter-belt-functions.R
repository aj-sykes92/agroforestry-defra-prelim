
library(tidyverse)

# temp <- Dat_dummy
# Dat_dummy <- temp

add_tree_data <- function(df, spp_short, felling_age, sys_type){
  
  # read in tree data
  Dat_SB <- read_rds("shelter-belt-data-preprocessing/shelter-belt-data-clean.rds")
  
  # filter spp and tree density type
  if(sys_type == "livestock") is_tight <- TRUE
  if(sys_type == "crop") is_tight <- FALSE
  
  Dat_SB <- Dat_SB %>%
    filter(spp == spp_short,
           tight == is_tight) %>%
    select(-tight)
  
  # filter felling age
  felling_age <- round(felling_age / 5, 0) * 5
  
  Dat_SB <- Dat_SB %>%
    filter(period_end == felling_age)
  
  # pull vars
  av_tnpp <- Dat_SB$av_tnpp
  co2_tha <- Dat_SB$C_cum_total
  vol_m3ha <- Dat_SB$vol_m3ha
  spacing <- Dat_SB$spacing
  type <- Dat_SB$type
  
  # tnpp ratio
  tnpp_ratio <- df$tree_tnpp / av_tnpp
  
  # add vars to df
  df <- df %>%
    mutate(treeco2_tha = co2_tha * tnpp_ratio,
           treevol_m3ha = vol_m3ha * tnpp_ratio,
           spacing_m = spacing,
           wood_type = type)
  
  return(df)
  
}

Dat_dummy <- Dat_dummy %>%
  add_tree_data(spp_short = "SAB", felling_age = 60, sys_type = "livestock")


#####################################
# agroforestry system scaling
#####################################
scale_system <- function(df, belt_spacing = 250, width = 10, length = 180){
  
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

Dat_dummy <- Dat_dummy %>%
  scale_system()

#####################################
# crop yield and margin impacts
#####################################

add_crop_impacts <- function(df){
  
  # yield improvements from Caborn (1957)
  Dat_yieldimp <- read_rds("simulation-base-data/crop-base-data.rds") %>%
    distinct(crop) %>%
    mutate(frac_yi = c(mean(c(1.34, 1.241)),
                       mean(c(1.34, 1.241)),
                       mean(c(1.171, 1.117)),
                       mean(c(1.171, 1.117)),
                       mean(c(1.171, 1.117)),
                       1.169,
                       mean(c(1.171, 1.117)),
                       mean(c(1.171, 1.117)),
                       mean(c(1.232, 1.119)),
                       mean(c(1.171, 1.117))))
  
  df <- df %>%
    left_join(Dat_yieldimp, by = "crop") %>%
    mutate(yield_tha_agf = yield_tha * frac_yi,
           gm_gbp_agf = (croprev_gbp * frac_yi - varcosts_gbp) * area_impact)
  
  return(df)
}

Dat_dummy <- Dat_dummy %>%
  add_crop_impacts()

#####################################
# agroforestry cost and revenue
#####################################
add_agf_margins <- function(df, felling_age, length, width, discount_rate){
  
  # timber cost datasets
  # Below is additional Monte Carlo for cost, using data from Burgess et al. (2003). Data assigned into vectors
  # below are reported values from Burgess' cost analysis (pp. 33-34)
  
  MC_n <- 10^4
  
  # establishment costs in gbp
  set_cost <- c(0.95, 1, 0.8, 0.6, 0.25) # purchase cost per set (young tree)
  prot_cost <- c(0.22, 0.18, 0.24, 0.16) # protection cost per set
  contmulch_cost <- c(0.1, 0.4) # continuous mulch application per m2 (tree area only)
  indimulch_cost <- c(0.4, 0.47) # invividual mulch per tree
  
  # establishment cost Monte Carlo
  Dat_cost <- tibble(est_cost_tree =
                       rpert(n = MC_n, mode = mean(set_cost), min = min(set_cost), max = max(set_cost)) +
                       rpert(n = MC_n, mode = mean(prot_cost), min = min(prot_cost), max = max(prot_cost)) +
                       runif(n = MC_n, min = min(indimulch_cost), max = max(indimulch_cost)),
                     est_cost_m2 = 
                       runif(n = MC_n, min = min(contmulch_cost), max = max(contmulch_cost))
  )
  
  # establishment labour in hours. Total pruning labour is included here since it is not an annual cost.
  set_lab <- c(1.5, 1.7, 2.1, 3) / 60 # planting labour per tree
  prot_lab <- c(0.4, 0.7) / 60 # protection labour per tree
  contmulchon_lab <- 1.7 / 60 # continuous mulch application labour per m2 (tree area)
  contmulchoff_lab <- 1.5 / 60 # continuous mulch removal labour per m2 (tree area) (year 8)
  indimulch_lab <- c(1.7, 2) / 60 # individual mulch per tree
  
  # establishment labour Monte Carlo
  Dat_cost <- Dat_cost %>%
    mutate(
      est_lab_tree =
        rpert(n = MC_n, mode = mean(set_lab), min = min(set_lab), max = max(set_lab)) +
        runif(n = MC_n, min = min(prot_lab), max = max(prot_lab)) +
        runif(n = MC_n, min = min(indimulch_lab), max = max(indimulch_lab)),
      est_lab_m2 =
        rep(contmulchon_lab, n = MC_n) +
        rep(contmulchoff_lab, n = MC_n)
    )
  
  # annual maintenance costs in ?
  herb_cost <- c(0.15, 0.5) # herbicide cost per tree
  
  Dat_cost <- Dat_cost %>%
    mutate(
      maint_cost_tree =
        runif(n = MC_n, min = min(herb_cost), max = max(herb_cost)),
      maint_cost_m2 =
        rep(0, n = MC_n)
    )
  
  # annual maintenance labour in hours
  herb_lab <- c(0.5, 0.2, 0.085, 0.06) / 60 # herbicide labour per m2
  treemaint_lab <- c(1.15, 2.3, 1.1, 4) / 60 # annual maintenance labour, per tree
  
  Dat_cost <- Dat_cost %>%
    mutate(
      maint_lab_tree =
        rpert(n = MC_n, mode = mean(treemaint_lab), min = min(treemaint_lab), max = max(treemaint_lab)),
      maint_lab_m2 =
        rpert(n = MC_n, mode = mean(herb_lab), min = min(herb_lab), max = max(herb_lab))
    )
  
  # convert and select down
  Dat_cost <- Dat_cost %>%
    mutate(lab_rate = rpert(n = MC_n, min = 12, mode = 12.8, max = 14.05), # fmh 19/20 skilled labour hourly rate
           est_cost_tree = est_cost_tree + est_lab_tree * lab_rate,
           est_cost_m2 = est_cost_m2 + est_lab_m2 * lab_rate,
           maint_cost_tree = maint_cost_tree + maint_lab_tree * lab_rate,
           maint_cost_m2 = maint_cost_m2 + maint_lab_m2 * lab_rate) %>%
    select(-est_lab_tree, -est_lab_m2, -maint_lab_tree, -maint_lab_m2, -lab_rate) %>%
    mutate(cost_tree =
             maint_cost_tree +
             annualise_cost(est_cost_tree,
                            lifespan = felling_age,
                            dr = discount_rate),
           cost_m2 =
             maint_cost_m2 +
             annualise_cost(est_cost_m2,
                            lifespan = felling_age,
                            dr = discount_rate)) %>%
    select(-maint_cost_tree, -est_cost_tree, -maint_cost_m2, -est_cost_m2)
  
  cost_tree <- Dat_cost %>% pull(cost_tree) %>% mean()
  cost_ha <- (Dat_cost %>% pull(cost_m2) %>% mean()) * 10^4
  
  # add fmh estimate for fencing
  belt_circ <- length * 2 + width * 2
  beltarea_ha <- length * width * 10^-4
  fencelength_ha_m <- belt_circ / beltarea_ha
  
  cost_ha <- cost_ha + fencelength_ha_m * 10
  
  # timber (rough wood) revenue
  rev_tonne <- mean(c(7, 30)) # rough logs value, gbp / tonne, fmh 19/20
  timbrev <- tibble(wood_type = c("H", "S"),
                    density = c(1.25, 1.43)  # m3 / tonne, https://www.forestresearch.gov.uk/tools-and-resources/statistics/forestry-statistics/forestry-statistics-2016-introduction/sources/timber/conversion-factors/
                    ) %>%
    mutate(rev_m3 = rev_tonne / density) %>%
    select(-density)
  
  df <- df %>%
    left_join(timbrev, by = "wood_type") %>%
    mutate(ntrees = planted_area_ha * (100 / spacing_m)^2,
           timbrev_gbp = annualise_return(rev_m3 * treevol_m3ha * planted_area_ha, felling_age, discount_rate),
           timbcost_gbp = cost_tree * ntrees + cost_ha * planted_area_ha,
           timbgm_gbp = timbrev_gbp - timbcost_gbp) %>%
    select(-rev_m3)
  
  return(df)
  
}

Dat_dummy <- Dat_dummy %>%
  add_agf_margins(felling_age = 60,
                  length = 180,
                  width = 10,
                  discount_rate = 0.035)

#####################################
# final estimates of carbon sequestration
#####################################

add_abatement <- function(df, felling_age){
  
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

Dat_dummy <- Dat_dummy %>%
  add_abatement(felling_age = 60)

# final mac / abatement + wrapper
