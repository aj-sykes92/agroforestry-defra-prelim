
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
  build_macc_plot() +
  labs(title = "UK MAC curve for hedges",
       subtitle = "3.5% DR")

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
  build_macc_map() +
  labs(title = "UK MAC map for hedges",
       subtitle = "3.5% DR")

##########################
# aggregate dataset / MACC
##########################
Dat_ag <- bind_rows(list(Intercropping = cheap_scale(Dat_row1, 0.1),
                         Shelterbelts = even_scale(Dat_sb1, 0.1),
                         `Fenceline tree planting` = cheap_scale(Dat_fl1, 0.1),
                         `Hedge expansion` = cheap_scale(Dat_hdg1, 0.1)),
                    .id = "sys_type")

Dat_ag %>%
  build_agmacc_plot()
  #labs(title = "UK aggregated MAC curve for agroforestry",
  #     fill = "Measure",
  #     subtitle = "10% uptake\nShelterbelt with SAB spp.\nIntercropping @ 30m\n3.5% DR")

ggsave("output-plots/uk-full-aggregated-macc.png", width = 8, height = 5)

Dat_ag %>%
  build_macc_plot()
  #labs(title = "UK aggregated MAC curve for agroforestry",
  #     subtitle = "10% uptake\nShelterbelt with SAB spp.\nIntercropping @ 30m\n3.5% DR")

ggsave("output-plots/uk-full-aggregated-macc-cropwise.png", width = 8, height = 5)


# only on CE 10% area
Dat_ag %>%
  filter(sys_type != "Shelterbelts") %>%
  group_by(x, y) %>%
  summarise(co2_ktyear = sum(co2_tyear) * 10^-3) %>%
  ggplot(aes(x = x, y = y, fill = co2_ktyear)) +
  geom_raster() +
  geom_polygon(data = raster::shapefile(find_onedrive(dir = "GIS data repository", path = "DA shapefile/GBR_adm_shp/GBR_adm1.shp")), aes(x = long, y = lat, group = group), colour = "black", fill = NA, size = 0.5) +
  coord_quickmap() +
  theme_void()

# total kt co2 per grid cell
bind_rows(list(Dat_row1, Dat_sb1, Dat_fl1, Dat_hdg1), .id = "sys_type") %>%
  group_by(x, y) %>%
  summarise(co2_ktyear = sum(co2_tyear) * 10^-3) %>%
  ggplot(aes(x = x, y = y, fill = co2_ktyear)) +
  geom_raster() +
  geom_polygon(data = raster::shapefile(find_onedrive(dir = "GIS data repository", path = "DA shapefile/GBR_adm_shp/GBR_adm1.shp")), aes(x = long, y = lat, group = group), colour = "black", fill = NA, size = 0.5) +
  coord_quickmap() +
  theme_void()

# abatement rate
bind_rows(list(Dat_row1, Dat_sb1, Dat_fl1, Dat_hdg1), .id = "sys_type") %>%
  group_by(x, y) %>%
  summarise(co2_tyear = sum(co2_tyear),
            area_ha = sum(area_ha)) %>%
  mutate(ar_tha = co2_tyear / area_ha) %>%
  ggplot(aes(x = x, y = y, fill = ar_tha)) +
  geom_raster() +
  geom_polygon(data = raster::shapefile(find_onedrive(dir = "GIS data repository", path = "DA shapefile/GBR_adm_shp/GBR_adm1.shp")), aes(x = long, y = lat, group = group), colour = "black", fill = NA, size = 0.5) +
  coord_quickmap() +
  annotate("text", x = -7, y = 51, label = "a)") +
  labs(fill = expression("AR (tCO"[2]*" ha"^{-1}*")")) +
  theme_void()

ggsave("output-plots/uk-all-measures-abatement-rate-ha-map.png", height = 5, width = 5)

# mac
bind_rows(list(Dat_row1, Dat_sb1, Dat_fl1, Dat_hdg1), .id = "sys_type") %>%
  group_by(x, y) %>%
  summarise(co2_tyear = sum(co2_tyear),
            totrev_gbp = sum(totrev_gbp)) %>%
  mutate(ar_tha = -totrev_gbp / co2_tyear) %>%
  ggplot(aes(x = x, y = y, fill = ar_tha)) +
  geom_raster() +
  geom_polygon(data = raster::shapefile(find_onedrive(dir = "GIS data repository", path = "DA shapefile/GBR_adm_shp/GBR_adm1.shp")), aes(x = long, y = lat, group = group), colour = "black", fill = NA, size = 0.5) +
  coord_quickmap() +
  annotate("text", x = -7, y = 51, label = "b)") +
  labs(fill = expression("MAC (£ tCO"[2]^{-1}*")")) +
  theme_void()

ggsave("output-plots/uk-all-measures-mac-map.png", height = 5, width = 5)

##########################
# summary data/descriptives
##########################

# row system
Dat_row1 %>%
  cheap_scale(0.1) %>%
  get_descriptives("crop") %>%
  write.table("output-tables/desc-row-agf-30-m-60-y-035-dr.txt",
              sep = "|",
              quote = F,
              row.names = F)

# cubic m of timber produced
# scaling manually to keep 1-off vars
Dat_row1 %>%
  arrange(mac_gbp_tco2) %>%
  mutate(area_cumfrac = cumsum(area_ha) / sum(area_ha)) %>%
  filter(area_cumfrac <= 0.1) %>%
  mutate(timbvol = vol_tree * ntrees) %>%
  pull(timbvol) %>%
  sum() *
  1/60

# shelterbelt system
Dat_sb1 %>%
  even_scale(0.1) %>%
  get_descriptives("crop") %>%
  write.table("output-tables/desc-sb-agf-180-10-250-m-60-y-035-dr.txt",
              sep = "|",
              quote = F,
              row.names = F)

# cubic m of biomass produced
# scaling manually to keep 1-off vars
Dat_sb1 %>%
  mutate(woodvol = treevol_m3ha * planted_area_ha * 0.1) %>%
  pull(woodvol) %>%
  sum() *
  1/60

# fenceline system
Dat_fl1 %>%
  cheap_scale(0.1) %>%
  get_descriptives("crop") %>%
  write.table("output-tables/desc-fl-agf-60-y-035-dr.txt",
              sep = "|",
              quote = F,
              row.names = F)

# cubic m of timber produced
# scaling manually to keep 1-off vars
Dat_fl1 %>%
  arrange(mac_gbp_tco2) %>%
  mutate(area_cumfrac = cumsum(area_ha) / sum(area_ha)) %>%
  filter(area_cumfrac <= 0.1) %>%
  mutate(timbvol = vol_tree * ntrees) %>%
  pull(timbvol) %>%
  sum() *
  1/60

# hedge systems
Dat_hdg1 %>%
  cheap_scale(0.1) %>%
  get_descriptives("crop") %>%
  write.table("output-tables/desc-hdg-035-dr.txt",
              sep = "|",
              quote = F,
              row.names = F)

# all systems
Dat_ag %>%
  get_descriptives("crop") %>%
  write.table("output-tables/desc-full-agg-cropwise.txt",
              sep = "|",
              quote = F,
              row.names = F)

Dat_ag %>%
  get_descriptives("sys_type") %>%
  write.table("output-tables/desc-full-agg-syswise.txt",
              sep = "|",
              quote = F,
              row.names = F)

Dat_ag %>%
  get_descriptives() %>%
  View()

##########################
# anpp output for Mark B @ Defra
##########################
read_rds("simulation-base-data/crop-base-data.rds") %>%
  ggplot() +
  geom_raster(aes(x = x, y = y, fill = tree_anpp)) +
  geom_polygon(data = raster::shapefile(find_onedrive(dir = "GIS data repository", path = "DA shapefile/GBR_adm_shp/GBR_adm1.shp")),
               aes(x = long, y = lat, group = group), colour = "black", fill = NA, size = 0.5) +
  coord_quickmap() +
  theme_void() +
  labs(fill = expression("Above-ground woody\nbiomass NPP (g C m"^{-2}*" year"^{-1}*")"))
ggsave("output-plots/tree-anpp-map.png", width = 5, height = 5)
