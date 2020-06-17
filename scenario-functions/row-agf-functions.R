
library(tidyverse)
library(mc2d)

# new environment
row_env <- new.env()

#####################################
# C per tree + vol per tree estimation
#####################################
row_env$add_tree_data <- function(df, felling_age){
  
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

#####################################
# agroforestry system scaling
#####################################
row_env$scale_system <- function(df, row_spacing){
  
  spacings <- read_rds("row-agroforestry-data-processing/row-agroforestry-data-clean.rds") %>%
    select(spp, agf_spacing_min, agf_spacing_max) %>%
    distinct() %>%
    mutate(inrow_spacing_m = (agf_spacing_min + agf_spacing_max) / 2) %>%
    select(spp, inrow_spacing_m)
    
  df <- df %>%
    left_join(spacings, by = "spp") %>%
    mutate(planted_length_m = 100 / row_spacing * 100 * area_ha,
           planted_area_ha = planted_length_m * 2 * 10^-4, # tree area in ha. Burgess et al. (2003) estimate 2m row width for trees
           ntrees = planted_length_m / inrow_spacing_m,
           area_impact = 1 - (planted_area_ha / area_ha))
  
  return(df)
  
}

#####################################
# crop yield and margin impacts
#####################################
row_env$add_crop_impacts <- function(df, row_spacing) {
  
  cropyield_loess <- read_rds("crop-yield-impacts/cropyield-loess-models.rds")
  
  yi_mode <- predict(cropyield_loess[[1]], newdata = row_spacing)
  yi_min <- predict(cropyield_loess[[2]], newdata = row_spacing)
  yi_max <- predict(cropyield_loess[[3]], newdata = row_spacing)
  
  ghi_mean <- mean(df$ghi)
  ghi_sd <- sd(df$ghi)
  
  p_ghi <- pnorm(df$ghi, ghi_mean, ghi_sd)
  p_yi <- qpert(p_ghi, min = yi_min, mode = yi_mode, max = yi_max)
  
  df <- df %>%
    mutate(frac_yi = p_yi,
           yield_tha_agf = yield_tha * frac_yi,
           gm_gbp_agf = (croprev_gbp * frac_yi - varcosts_gbp) * area_impact)
  
  return(df)
}

#####################################
# agroforestry cost and revenue
#####################################
row_env$add_agf_margins <- function(df, felling_age, discount_rate){
  
  # one-use helper function to estimate timber price, based on Whiteman et al. (1991), adjusted for inflation
  timber_value <- function(spp, S){
    # implement price curve equation from Whiteman et al. (1991) pg. 17
    
    # coefficients
    sum_d <- ifelse(spp == "OK", 0.40 - 0.35, 0.42 - 0.35)
    a <- 2.24
    b <- 0.47
    conv_fac_1990 <- 3.6780
    P <- conv_fac_1990 * exp(a + sum_d) * S ^ b
    
    # adjust to GBP2017 using inflation factor (1990 - 2017) http://www.in2013dollars.com/1990-GBP-in-2017?amount=1
    inf_fac <- 2.16
    
    # adjust
    P_adj <- P * inf_fac
    return(P_adj)
  }
  
  # timber cost datasets
  # Below is additional Monte Carlo for cost, using data from Burgess et al. (2003). Data assigned into vectors
  # below are reported values from Burgess' cost analysis (pp. 33-34)
  
  MC_n <- 10^4
  
  # establishment costs in gbp
  set_cost <- c(0.95, 1, 0.8, 0.6, 0.25) # purchase cost per set (young tree)
  prot_cost <- c(0.22, 0.18, 0.24, 0.16) # protection cost per set
  contmulch_cost <- c(0.1, 0.4) # continuous mulch application per m2 (tree area only)
  indimulch_cost <- c(0.4, 0.47) # invividual mulch per tree
  grass_cost <- 0.035 # grass sward establishment cost, per m2 (tree area)
  
  # establishment cost Monte Carlo
  Dat_cost <- tibble(est_cost_tree =
                       rpert(n = MC_n, mode = mean(set_cost), min = min(set_cost), max = max(set_cost)) +
                       rpert(n = MC_n, mode = mean(prot_cost), min = min(prot_cost), max = max(prot_cost)) +
                       runif(n = MC_n, min = min(indimulch_cost), max = max(indimulch_cost)),
                     est_cost_m2 = 
                       runif(n = MC_n, min = min(contmulch_cost), max = max(contmulch_cost)) +
                       rep(grass_cost, n = MC_n)
  )
  
  # establishment labour in hours. Total pruning labour is included here since it is not an annual cost.
  set_lab <- c(1.5, 1.7, 2.1, 3) / 60 # planting labour per tree
  prot_lab <- c(0.4, 0.7) / 60 # protection labour per tree
  contmulchon_lab <- 1.7 / 60 # continuous mulch application labour per m2 (tree area)
  contmulchoff_lab <- 1.5 / 60 # continuous mulch removal labour per m2 (tree area) (year 8)
  indimulch_lab <- c(1.7, 2) / 60 # individual mulch per tree
  grass_lab <- 0.5 / 60 # grass sward establishment labour, per m2
  prune_lab <- 5 * c(4, 1.5, 1, 0.5, 15, 12, 4) / 60 # labour time for pruning, per tree. Takes longer as trees age. Burgess estimate 5 prunings in lifetime.
  pruneremove_lab <- 5 * c(1, 4, 0.14) / 60 # labour time for pruned branch removal, per tree.
  
  # establishment labour Monte Carlo
  Dat_cost <- Dat_cost %>%
    mutate(
      est_lab_tree =
        rpert(n = MC_n, mode = mean(set_lab), min = min(set_lab), max = max(set_lab)) +
        runif(n = MC_n, min = min(prot_lab), max = max(prot_lab)) +
        runif(n = MC_n, min = min(indimulch_lab), max = max(indimulch_lab)) +
        rpert(n = MC_n, mode = mean(prune_lab), min = min(prune_lab), max = max(prune_lab)) +
        rpert(n = MC_n, mode = mean(pruneremove_lab), min = min(pruneremove_lab), max = max(pruneremove_lab)),
      est_lab_m2 =
        rep(contmulchon_lab, n = MC_n) +
        rep(contmulchoff_lab, n = MC_n) +
        rep(grass_lab, n = MC_n)
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
  grasscut_lab <- c(0.3, 0.4, 0.6) / 60 # cutting grass between trees, per m2
  treemaint_lab <- c(1.15, 2.3, 1.1, 4) / 60 # annual maintenance labour, per tree
  
  Dat_cost <- Dat_cost %>%
    mutate(
      maint_lab_tree =
        rpert(n = MC_n, mode = mean(treemaint_lab), min = min(treemaint_lab), max = max(treemaint_lab)),
      maint_lab_m2 =
        rpert(n = MC_n, mode = mean(herb_lab), min = min(herb_lab), max = max(herb_lab)) +
        rpert(n = MC_n, mode = mean(grasscut_lab), min = min(grasscut_lab), max = max(grasscut_lab))
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
  
  
  # add in economics values to main Monte Carlo (scaled by tree numbers/area) and adjust from GBP2003 to GBP2017 http://www.in2013dollars.com/2003-GBP-in-2017?amount=1
  # including stochastic adjustment factor for timber prices based on variation in the real-terms FPI, 1990-2017 (1990 was a real-terms all-time high for standing timber in this period, hence max is 1)
  # see data in Excel workbook [Forestry Price Indices adjustment for Whiteman model.xlsx]
  
  #inflation factor for 2003 economic values
  inf_fac <- 1.50
  
  # price index adjustment factor
  pi_fac <- 0.513
  
  
  # timber revenue
  df <- df %>%
    mutate(timbrev_gbp = timber_value(spp = spp, S = vol_tree) * ntrees * vol_tree * pi_fac %>%
             annualise_return(real_int = felling_age, dr = discount_rate),
           timbcost_gbp = cost_tree * ntrees + cost_ha * planted_area_ha,
           timbgm_gbp = timbrev_gbp - timbcost_gbp)
  
  return(df)
}
  
#####################################
# final estimates of carbon sequestration
#####################################
row_env$add_abatement <- function(df, felling_age){
  df %>%
    mutate(tree_co2_tyear = ntrees * CO2_tree / felling_age,
           # add in estimate of below ground C sequestration (from Aertsens et al., 2013; see Excel workbook [Belowground C per tree.xlsx])
           soil_co2_tyear = (ntrees * 3.97 * 10^-3 * 44/12 * 20) / felling_age, # convert from IPCC 20yr to map evenly over lifespan
           co2_tyear = tree_co2_tyear + soil_co2_tyear) %>%
    select(-tree_co2_tyear, -soil_co2_tyear)
}

#####################################
# wrapper function
#####################################
build_row_agf <- function(felling_age, row_spacing, discount_rate,
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
    row_env$scale_system(row_spacing) %>%
    row_env$add_crop_impacts(row_spacing) %>%
    row_env$add_agf_margins(felling_age, discount_rate) %>%
    row_env$add_abatement(felling_age) %>%
    calc_mac() %>%
    return()
  
}
  