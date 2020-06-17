
library(tidyverse)
library(mc2d)

# load base data
Dat_crop <- read_rds("simulation-base-data/crop-base-data.rds")

# dummy data, adjusted to 1 ha area per row to allow sensechecking aggregate calcs
# use as needed
set.seed(2605)
Dat_dummy <- Dat_crop %>%
  #filter_crops(applies_to = "upland") %>%
  sample_n(25, replace = F) %>%
  mutate(croprev_gbp = croprev_gbp / area_ha,
         varcosts_gbp = varcosts_gbp / area_ha,
         gm_gbp = gm_gbp / area_ha,
         area_ha = area_ha / area_ha)

# build scenarios

# row agroforestry
source("scenario-functions/row-agf-functions.R")
Dat_row1 <- build_row_agf(felling_age = 60, row_spacing = 30, discount_rate = 0.035)
Dat_row1 %>% pull(mac_gbp_tco2) %>% qplot()

# shelter belt agroforestry
source("scenario-functions/shelter-belt-functions.R")
read_rds("shelter-belt-data-preprocessing/shelter-belt-data-clean.rds") %>%
  distinct(spp, full_name)

Dat_sb1 <- build_sb_agf(spp_short = "SAB", felling_age = 60, discount_rate = 0.035)
Dat_sb1 %>% pull(mac_gbp_tco2) %>% qplot()

Dat_sb2 <- build_sb_agf(spp_short = "SP", felling_age = 40, discount_rate = 0.035)
Dat_sb2 %>% pull(mac_gbp_tco2) %>% qplot()

##########################
# plots
##########################
source("scenario-functions/post-and-plot-functions.R")

##########################
# UK full MACCs
##########################

# row, arable + pasture
Dat_row1 %>%
  build_macc_plot() +
  labs(title = "UK MAC curve for row agroforestry",
       subtitle = "30m spacing\n60 year lifespan\n3.5% DR")

ggsave("output-plots/uk-full-macc-row-agf-30-m-60-y-035-dr.png", width = 8, height = 5)

# row, arable only
Dat_row1 %>%
  filter(crop != "pasture") %>%
  build_macc_plot()

# row, pasture only
Dat_row1 %>%
  filter(crop == "pasture") %>%
  build_macc_plot()

# sb1, arable, pasture and upland
Dat_sb1 %>%
  build_macc_plot() +
  labs(title = "UK MAC curve for shelter belt agroforestry",
       subtitle = "Sycamore/ash/birch spp\n60 year lifespan\nBelt dimensions 180m x 10m\nBelt spacing 250m\n3.5% DR")

ggsave("output-plots/uk-full-macc-shelt-belt-sab-60-y-180-10-250-m-035-dr.png", width = 8, height = 5)

Dat_sb2 %>%
  build_macc_plot() +
  labs(title = "UK MAC curve for shelter belt agroforestry",
       subtitle = "Scots pine spp\n40 year lifespan\nBelt dimensions 180m x 10m\nBelt spacing 250m\n3.5% DR")

ggsave("output-plots/uk-full-macc-shelt-belt-sp-40-y-180-10-250-m-035-dr.png", width = 8, height = 5)

##########################
# UK MACC maps
##########################

# row, MAC map
Dat_row1 %>%
  build_macc_map() +
  labs(title = "UK MAC map for row agroforestry",
       subtitle = "30m spacing\n60 year lifespan\n3.5% DR")

ggsave("output-plots/uk-mac-map-row-agf-30-m-60-y-035-dr.png", width = 8, height = 8)

Dat_sb1 %>%
  build_macc_map() +
  labs(title = "UK MAC map for shelter belt agroforestry",
       subtitle = "SAB spp\n60 year lifespan\nBelt dimensions 180m x 10m\nBelt spacing 250m\n3.5% DR")

ggsave("output-plots/uk-mac-map-shelt-belt-sab-60-y-180-10-250-m-035-dr.png", width = 8, height = 8)

  