
library(tidyverse)
library(mc2d)

Dat_crop <- read_rds("simulation-base-data/crop-base-data.rds")

# dummy data, adjusted to 1 ha area per row to allow sensechecking aggregate calcs
# use as needed
set.seed(2605)
Dat_dummy <- Dat_crop %>%
  sample_n(25, replace = F) %>%
  mutate(croprev_gbp = croprev_gbp / area_ha,
         varcosts_gbp = varcosts_gbp / area_ha,
         gm_gbp = gm_gbp / area_ha,
         area_ha = area_ha / area_ha)


source("scenario-functions/row-agf-functions.R")
Dat_scen1 <- build_row_agf(60, 10, 0.035)

Dat_scen1 %>% pull(mac_gbp_tco2) %>% qplot()

##########################
# UK full MACCs
##########################

# arable
Dat_scen1 %>%
  filter(crop != "pasture") %>%
  build_macc_plot()

# pasture land
Dat_scen1 %>%
  filter(crop == "pasture") %>%
  build_macc_plot()
  