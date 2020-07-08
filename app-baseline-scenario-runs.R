# script to build and write baseline (opening) scenarios for Shiny app

library(tidyverse)
library(mc2d)

##########################
# build scenarios
##########################

# row agroforestry
source("scenario-functions/row-agf-functions.R")
Dat_row <- build_row_agf(felling_age = 60, row_spacing = 30, discount_rate = 0.035)

# shelter belt agroforestry
source("scenario-functions/shelter-belt-functions.R")
Dat_sb <- build_sb_agf(spp_short = "SAB", felling_age = 60, discount_rate = 0.035)

# fenceline agroforestry
source("scenario-functions/fenceline-agf-functions.R")
Dat_fl <- build_fl_agf(felling_age = 60, discount_rate = 0.035)

# hedges
source("scenario-functions/hedge-functions.R")
Dat_hdg <- build_hdg_agf(discount_rate = 0.035)

# aggregate
source("scenario-functions/post-and-plot-functions.R")
Dat_ag <- bind_rows(list(Intercropping = cheap_scale(Dat_row, 0.1),
                         Shelterbelts = even_scale(Dat_sb, 0.1),
                         `Fenceline tree planting` = cheap_scale(Dat_fl, 0.1),
                         `Hedge expansion` = cheap_scale(Dat_hdg, 0.1)),
                    .id = "sys_type")

##########################
# write scenarios
##########################
write_rds(Dat_row, "app-baseline-simulations/row-agf.rds")
write_rds(Dat_sb, "app-baseline-simulations/sb-agf.rds")
write_rds(Dat_fl, "app-baseline-simulations/fl-agf.rds")
write_rds(Dat_hdg, "app-baseline-simulations/hdg-agf.rds")
write_rds(Dat_ag, "app-baseline-simulations/agf-ag.rds")
