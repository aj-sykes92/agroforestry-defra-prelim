library(tidyverse)
library(readxl)
library(mc2d)
library(rmutil)

#######################
# working with CO2-eq lookup tables from West and Matthews for yield classes, sequestration, spacing, etc.
# matching to outputs from Forest Yield v1.0 for yield tables
# running Monte Carlo simulation to account for uncertainty
# matching to economic data from Burgess et al. (2003) and economic model from Whiteman et al. (1991)
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

# join full names
Dat_CL <- Dat_CL %>% left_join(Species_full, by = "spp")

# bring in tidied data from Forest Yield v1.0
Dat_YT <- read_csv("forest-yield-data/forest-yield-output-tidy.csv")

# minor wrangle to help things match
Dat_YT <- Dat_YT %>%
  mutate(mgmt = ifelse(thin_treat=="No Thinning", "NO_thin", "Thinned")) %>%
  filter(full_name != "Poplar") # maybe we can do something with this, but it'll have to come separately - C lookups don't have poplar estimation

# create master dataset from matched rows of CL and YT
Dat_main <- inner_join(Dat_CL, Dat_YT, by = c("full_name", "mgmt", "yield_class", "period_end", "spacing"))

# this relationship is probably the best one to infer poplar C seq values from
ggplot(Dat_main, aes(x = vol_ha_CUM, y = C_cum_total, colour = yield_class %>% as.ordered())) + geom_point()

# calculate C per tree and equivalent spacings adjusted for thinning
Dat_main <- Dat_main %>%
  mutate(CO2_per_tree = C_cum_total / trees_ha,
         spacing_adj = 100 / sqrt(trees_ha))

# C per tree plot to sense check
ggplot(Dat_main, aes(x = period_end, y = CO2_per_tree, colour = yield_class %>% as.ordered())) +
  facet_wrap(~full_name, nrow = 3) +
  geom_line()

# segway -- yield model needed for shelter belts
Dat_main %>%
  filter(period_cntr <= 100,
         period_cntr >= 30) %>%
  ggplot(aes(x = C_cum_biomass, y = vol_ha_CUM)) +
  geom_point(aes(colour = period_cntr, shape = spp)) +
  geom_smooth(method = "lm")

sb_lm <- lm(vol_ha_CUM ~ C_cum_biomass,
            data = Dat_main %>%
              filter(period_cntr <= 100,
                     period_cntr >= 30))
write_rds(sb_lm, "shelter-belt-data-preprocessing/vol-ha-lm.rds")

# slim down to variables we need for analysis
Dat_main <- Dat_main %>%
  select(spp, full_name, spacing, spacing_adj, yield_class, period_end, CO2_per_tree, dbh, vol_tree, mai, max_mai_age)

# estimate spacing based on equivalent maximum MAI spacing. This makes logical sense (we're not keeping the trees beyond
# their max MAI) and returns values similar to those employed by Burgess et al. (2003) for their poplar system
Dat_spacing <- Dat_main %>%
  select(spp, spacing_adj, period_end, max_mai_age) %>%
  filter(period_end == max_mai_age) %>%
  group_by(spp) %>%
  summarise(agf_spacing_min = min(spacing_adj),
            agf_spacing_max = max(spacing_adj)) %>%
  ungroup() %>%
  select(spp, agf_spacing_min, agf_spacing_max)

Dat_main <- Dat_main %>% left_join(Dat_spacing, by = "spp")
rm(Dat_spacing)

write_rds(Dat_main, "row-agroforestry-data-processing/row-agroforestry-data-clean.rds")

