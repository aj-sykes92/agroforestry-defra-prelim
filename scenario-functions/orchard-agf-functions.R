
library(tidyverse)

# new environment
orch_env <- new.env()

# tree data add, adapted from row agroforestry
orch_env$add_tree_data <- function(df, felling_age) {
  
  # read in dataset from preprocessing
  row_agf_data <- read_rds("row-agroforestry-data-processing/row-agroforestry-data-clean.rds")
  
  # add in tree spp to df based on soil type
  df <- df %>%
    mutate(spp = "SAB") # all fruit trees seem to follow SAB path
  
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

# crop impacts and orchard yield
orch_env$add_crop_impacts <- function(df, row_spacing) {
  
  cropyield_loess <- read_rds("crop-yield-impacts/cropyield-loess-models.rds")
  
  yi_mode <- predict(cropyield_loess[[1]], newdata = row_spacing)
  yi_min <- predict(cropyield_loess[[2]], newdata = row_spacing)
  yi_max <- predict(cropyield_loess[[3]], newdata = row_spacing)
  
  ghi_mean <- mean(df$ghi)
  ghi_sd <- sd(df$ghi)
  
  p_ghi <- pnorm(df$ghi, ghi_mean, ghi_sd)
  p_yi <- qpert(p_ghi, min = yi_min, mode = yi_mode, max = yi_max)
  
  # apple yield per tree
  appleyield_tonnes <- 37.5 * 10^-3 # 37.5 kg per tree, from Parke Estate costings https://ptes.org/wp-content/uploads/2016/07/Orchard-Produce.pdf
  
  df <- df %>%
    mutate(frac_yi = p_yi,
           orch_yield_tha = (orchard_prodindex * appleyield_tonnes * ntrees) / area_ha, # apple yield
           yield_tha_agf = yield_tha * frac_yi + orch_yield_tha,
           gm_gbp_agf = (croprev_gbp * frac_yi - varcosts_gbp) * area_impact)
  
  return(df)
}

# orchard margins
orch_env$add_agf_margins <- function(df, felling_age, discount_rate) {
  
  # costs and revenue from Burgess et al. (2017) Lessons learnt – Grazed orchards in England and Wales
  # https://www.agforward.eu/index.php/en/Grazed_Orchards.html?file=files/agforward/documents/LessonsLearnt/WP3_UK_grazed_orchards_lessons%20learnt.pdf.
  
  # orchard costs / tree @ 525 trees per ha
  cost_tree <- 2133 / 525
  
  # orchard revenue / tree @ 525 trees per ha
  
  rev_tree <-  2640 / 525
  
  # note not changing 'timb' element of names to avoid having to rewrite other functions
  df <- df %>%
    mutate(timbrev_gbp = rev_tree * ntrees * orchard_prodindex,
           timbcost_gbp = cost_tree * ntrees,
           timbgm_gbp = timbrev_gbp - timbcost_gbp)
  
  return(df)
}

# wrapper function
build_orch_agf <- function(row_spacing, discount_rate, felling_age = 50,
                           da = c("England", "Scotland", "Wales", "Northern Ireland"),
                           applies_to = c("barley",
                                          "cereals_other",
                                          "oil_crops_other",
                                          "pasture",
                                          "pulses_other",
                                          "rapeseed",
                                          "wheat")) {
  
  source("scenario-functions/row-agf-functions.R")
  source("scenario-functions/univ-functions.R")
  
  read_rds("simulation-base-data/crop-base-data.rds") %>%
    drop_na(orchard_prodindex) %>%
    filter_crops(applies_to) %>%
    filter_da(da = da) %>%
    orch_env$add_tree_data(felling_age) %>%
    row_env$scale_system(row_spacing) %>%
    orch_env$add_crop_impacts(row_spacing) %>%
    orch_env$add_agf_margins(felling_age, discount_rate) %>%
    row_env$add_abatement(felling_age) %>%
    calc_mac() %>%
    return()
}
