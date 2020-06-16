
library(tidyverse)
library(mc2d)

# load base data
Dat_crop <- read_rds("simulation-base-data/crop-base-data.rds")

# source universal functions
source("scenario-functions/univ-and-plot-functions.R")

# dummy data, adjusted to 1 ha area per row to allow sensechecking aggregate calcs
# use as needed
set.seed(2605)
Dat_dummy <- Dat_crop %>%
  filter_crops(applies_to = "upland") %>%
  sample_n(25, replace = F) %>%
  mutate(croprev_gbp = croprev_gbp / area_ha,
         varcosts_gbp = varcosts_gbp / area_ha,
         gm_gbp = gm_gbp / area_ha,
         area_ha = area_ha / area_ha)

# build scenarios

# row agroforestry
source("scenario-functions/row-agf-functions.R")
Dat_scen1 <- build_row_agf(felling_age = 60, row_spacing = 30, discount_rate = 0.035)

Dat_scen1 %>% pull(mac_gbp_tco2) %>% qplot()

##########################
# UK full MACCs
##########################

# arable + pasture
Dat_scen1 %>%
  build_macc_plot() +
  labs(title = "UK MAC curve for row agroforestry",
       subtitle = "30m spacing\n60 year lifespan\n3.5% DR")

ggsave("output-plots/uk-full-macc.png", width = 8, height = 5)

# arable only
Dat_scen1 %>%
  filter(crop != "pasture") %>%
  build_macc_plot()

# pasture only
Dat_scen1 %>%
  filter(crop == "pasture") %>%
  build_macc_plot()

# MAC map
Dat_scen1 %>%
  build_macc_map() +
  labs(title = "UK MAC map for row agroforestry",
       subtitle = "30m spacing\n60 year lifespan\n3.5% DR")

ggsave("output-plots/uk-macc-map.png", width = 8, height = 8)
  