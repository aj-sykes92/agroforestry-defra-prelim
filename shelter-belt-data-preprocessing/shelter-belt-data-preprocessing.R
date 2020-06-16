library(tidyverse)
library(readxl)
library(mc2d)
library(rmutil)

#######################
# working with CO2-eq lookup tables from West and Matthews for yield classes, sequestration, spacing, etc.
#######################

# read in data
Dat_CL <- read_excel("fc-carbon-accounting/WCC_Carbon_Lookup_Tables_Version2.0_08March2018.xlsx")
glimpse(Dat_CL)

# rename
orig_names <- colnames(Dat_CL)
colnames(Dat_CL) <- c("spp", "spacing", "yield_class", "mgmt", "period", "C_standing", "C_debris",
                      "C_total", "C_cum_period", "C_cum_biomass", "C_cum_mgmt", "C_cum_total",
                      "C_removed")
Dat_CL_key <- tibble(original = orig_names,
                     abbrev = colnames(Dat_CL))
rm(orig_names)

# calculate numeric period variables
Dat_CL <- Dat_CL %>% 
  mutate(period_start = period %>% str_extract("\\d+(?=-)") %>% as.numeric(),
         period_end = period %>% str_extract("(?<=-)\\d+") %>% as.numeric(),
         period_cntr = (period_start + period_end) / 2)

# tibble of full names for tree species so we can know what we're dealing with
Species_full <- tibble(spp = Dat_CL %>% pull(spp) %>% unique(),
                       full_name = c("Beech", "Corsican pine", "Douglas fir", "European larch", "Grand fir",
                                     "Hybrid larch", "Japanese larch", "Leyland cypress", "Lodgepole pine",
                                     "Noble fir", "Norway spruce", "Oak", "Western red cedar", "Sycamore",
                                     "Scots pine", "Sitka spruce", "Western hemlock"),
                       type = c("H", "S", "S", "S", "S", "S", "S", "S", "S", "S", "S", "H", "S", "H", "S", "S", "S"))



# summarise to mean
Dat_SB <- Dat_CL %>%
  filter(mgmt == "NO_thin") %>%
  group_by(spp) %>%
  mutate(tight = spacing <= mean(spacing)) %>%
  select(-mgmt, -period) %>%
  group_by(spp, tight, period_start, period_cntr, period_end) %>%
  summarise_all(.funs = mean)

# join full names
Dat_SB <- Dat_SB %>% left_join(Species_full, by = "spp")

# calculate mean tnpp for adjustment via Del Grosso model
av_annual_tnpp <- Dat_SB %>%
  group_by(spp) %>%
  summarise(av_tnpp = mean(C_total) * 10^-4 * 10^6)

# mean tnpp
Dat_SB <- Dat_SB %>%
  left_join(av_annual_tnpp, by = "spp")

# predict vol ha wood biomass yield
vol_ha_model <- read_rds("shelter-belt-data-preprocessing/vol-ha-lm.rds")

preds <- predict(vol_ha_model, tibble(C_cum_biomass = Dat_SB$C_cum_biomass)) %>% as.numeric()

Dat_SB <- Dat_SB %>%
  ungroup() %>%
  mutate(vol_m3ha = preds) %>%
  filter(period_cntr >= 30,
         period_cntr <= 100)
  
# write out
write_rds(Dat_SB, "shelter-belt-data-preprocessing/shelter-belt-data-clean.rds")




