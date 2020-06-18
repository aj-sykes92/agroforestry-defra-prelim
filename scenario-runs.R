
library(tidyverse)
library(mc2d)

##########################
# build scenarios
##########################

# row agroforestry
##########################
source("scenario-functions/row-agf-functions.R")
Dat_row1 <- build_row_agf(felling_age = 60, row_spacing = 30, discount_rate = 0.035)
Dat_row1 %>% pull(mac_gbp_tco2) %>% summary()

Dat_row2 <- build_row_agf(felling_age = 80, row_spacing = 30, discount_rate = 0.035)
Dat_row2 %>% pull(mac_gbp_tco2) %>% summary()

Dat_row3 <- build_row_agf(felling_age = 60, row_spacing = 10, discount_rate = 0.035)
Dat_row3 %>% pull(mac_gbp_tco2) %>% summary()

##########################

# shelter belt agroforestry
##########################
source("scenario-functions/shelter-belt-functions.R")

# crib sheet for short spp names
read_rds("shelter-belt-data-preprocessing/spp-name-crib-sheet.rds")

Dat_sb1 <- build_sb_agf(spp_short = "SAB", felling_age = 60, discount_rate = 0.035)
Dat_sb1 %>% pull(mac_gbp_tco2) %>% summary()

Dat_sb2 <- build_sb_agf(spp_short = "SP", felling_age = 40, discount_rate = 0.035)
Dat_sb2 %>% pull(mac_gbp_tco2) %>% summary()

Dat_sb3 <- build_sb_agf(spp_short = "SS", felling_age = 40, discount_rate = 0.035)
Dat_sb3 %>% pull(mac_gbp_tco2) %>% summary()

##########################
# fenceline agroforestry
##########################
source("scenario-functions/fenceline-agf-functions.R")
Dat_fl1 <- build_fl_agf(felling_age = 60, discount_rate = 0.035)

##########################
# hedges
##########################
source("scenario-functions/hedge-functions.R")
Dat_hdg1 <- build_hdg_agf(discount_rate = 0.035)
##########################

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
Dat_row1 %>% filter(crop != "pasture") %>% build_macc_plot()

# row, pasture only
Dat_row1 %>% filter(crop == "pasture") %>% build_macc_plot()

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

# fenceline agroforestry
Dat_fl1 %>%
  build_macc_plot() +
  labs(title = "UK MAC curve for fenceline agroforestry",
       subtitle = "60 year lifespan\n3.5% DR")

ggsave("output-plots/uk-full-macc-fenceline-agf-60-y-035-dr.png", width = 8, height = 5)

# hedges
Dat_hdg1 %>%
  build_macc_plot()

ggsave("output-plots/uk-full-macc-hedges-035-dr.png", width = 8, height = 5)

##########################
# UK MACC maps
##########################

# row, MAC map
Dat_row1 %>%
  build_macc_map() +
  labs(title = "UK MAC map for row agroforestry",
       subtitle = "30m spacing\n60 year lifespan\n3.5% DR")

ggsave("output-plots/uk-mac-map-row-agf-30-m-60-y-035-dr.png", width = 8, height = 8)

# shelterbelts
Dat_sb1 %>%
  build_macc_map() +
  labs(title = "UK MAC map for shelter belt agroforestry",
       subtitle = "SAB spp\n60 year lifespan\nBelt dimensions 180m x 10m\nBelt spacing 250m\n3.5% DR")

ggsave("output-plots/uk-mac-map-shelt-belt-sab-60-y-180-10-250-m-035-dr.png", width = 8, height = 8)

# fenceline
Dat_fl1 %>%
  build_macc_map()

# hedges
Dat_hdg1 %>%
  build_macc_map()

##########################
# aggregate dataset / MACC
##########################
Dat_ag <- bind_rows(list(Intercropping = cheap_scale(Dat_row1, 0.1),
                         Shelterbelts = even_scale(Dat_sb1, 0.1),
                         `Fenceline tree planting` = cheap_scale(Dat_fl1, 0.1),
                         `Hedge expansion` = cheap_scale(Dat_hdg1, 0.1)),
                    .id = "sys_type")

Dat_ag %>%
  build_agmacc_plot() +
  labs(title = "UK aggregated MAC curve for agroforestry",
       fill = "Measure",
       subtitle = "10% uptake\nShelterbelt with SAB spp.\nIntercropping @ 30m\n3.5% DR")

ggsave("output-plots/uk-full-aggregated-macc.png", width = 8, height = 5)

Dat_ag %>%
  build_macc_plot() +
  labs(title = "UK aggregated MAC curve for agroforestry",
       subtitle = "10% uptake\nShelterbelt with SAB spp.\nIntercropping @ 30m\n3.5% DR")

ggsave("output-plots/uk-full-aggregated-macc-cropwise.png", width = 8, height = 5)


Dat_ag %>% build_ab_map()

##########################
# summary data/descriptives
##########################

# row system
Dat_row1 %>%
  cheap_scale(0.1) %>%
  get_descriptives("crop") %>%
  write.table("output-tables/desc-row-agf-30-m-60-y-035-dr.txt")

# shelterbelt system
Dat_sb1 %>%
  even_scale(0.1) %>%
  get_descriptives("crop") %>%
  write.table("output-tables/desc-sb-agf-180-10-250-m-60-y-035-dr.txt", row.names = F)

# fenceline system
Dat_fl1 %>%
  cheap_scale(0.1) %>%
  get_descriptives("crop") %>%
  write.table("output-tables/desc-fl-agf-60-y-035-dr.txt",
              sep = ",",
              quote = F,
              row.names = F)

# hedge systems
Dat_hdg1 %>%
  cheap_scale(0.1) %>%
  get_descriptives("crop") %>%
  write.table("output-tables/desc-hdg-035-dr.txt",
              sep = ",",
              quote = F,
              row.names = F)

# all systems
Dat_ag %>%
  get_descriptives("crop") %>%
  write.table("output-tables/desc-full-agg-cropwise.txt",
              sep = ",",
              quote = F,
              row.names = F)

Dat_ag %>%
  get_descriptives("sys_type") %>%
  write.table("output-tables/desc-full-agg-syswise.txt",
              sep = ",",
              quote = F,
              row.names = F)

