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

# create dataset of agroforestry implementations (opportunity to vary here, if desired)
Agf_treatments <- tibble(agf_treatment = c("10m alleys", "20m alleys", "30m alleys", "40m alleys") %>% as.character(),
                       length_be = 100 / c(10, 20, 30, 40) * 100,
                       length_min = 100 / c(10, 20, 30, 40) * 100,
                       length_max = 100 / c(10, 20, 30, 40) * 100)

# also adding in a fenceline agroforestry treatment here
# the following is data derived from the countryside survey estimated fenceline length per ha of arable land. Reformatting
# and adding to the treatments dataset
Dat_FL <- read_csv("field-boundary-estimation/field-boundaries-per-ha.csv")
Dat_FL <- Dat_FL %>%
  filter(feature=="Fence") %>%
  select(-feature) %>%
  mutate(agf_treatment = "Fenceline")
Agf_treatments <- bind_rows(Agf_treatments, Dat_FL)
rm(Dat_FL)

# paramaters for yield class pdfs
YC_params <- Dat_main %>%
  select(spp, yield_class) %>%
  distinct() %>%
  group_by(spp) %>%
  summarise(yield_class_be = mean(yield_class),
            yield_class_min = min(yield_class),
            yield_class_max = max(yield_class))

# set up Monte Carlo simulation
set.seed(2605)
MC_n = 10^5

# max MAI is an unlikely scenario given the timescale required by slower growing species. Assuming timescales of
# 50, 60, 70 and 80 years

# based on previous iterations of this analysis we're going to focus on the SAB growth path here. There are a number of
# reasons for this, but namely a) it's vastly more effective than other broadleaves for the timescales we're looking at
# and b) birch and cherry (as recommended by RAS for agf considering their lighter shade, good apical dominance) both
# follow the SAB growth paths

# construct Monte Carlo
Dat_MC <- Dat_main %>%
  select(spp, full_name) %>%
  filter(spp=="SAB") %>% # can remove or alter this if we want to think about other species
  distinct() %>%
  sample_n(MC_n, replace = T) %>% # species sample first
  mutate(period_end = sample(c(50, 60, 70, 80), MC_n, replace = T)) %>% # randomly sample durations
  left_join(YC_params, by = "spp") %>% # add in YC paramaters
  mutate(yield_class = round(rpert(n = MC_n, mode = yield_class_be, min = yield_class_min, max = yield_class_max) / 2) * 2) %>% # sample yield class with modal bias
  select(-(yield_class_be:yield_class_max)) %>% # remove redundant YC params
  left_join(Dat_main, by = c("spp", "full_name", "period_end", "yield_class")) %>% # pull in data from Dat_main
  bind_cols(Agf_treatments %>% sample_n(MC_n, replace = T)) %>% # add in agroforestry treatments
  mutate(within_row_spacing = runif(n = MC_n, min = agf_spacing_min, max = agf_spacing_max),
         planted_length = rpert(n = MC_n, mode = length_be, min = length_min, max = length_max),
         trees_ha = planted_length / within_row_spacing) %>%
  select(-agf_spacing_min, -agf_spacing_max, -(length_be:length_max)) %>%
  mutate(CO2_ha = CO2_per_tree * trees_ha,
         CO2_ha_year = CO2_ha / period_end,
         C_ha_year = CO2_ha_year / (44/12),
         tree_area = planted_length * 2, # tree area in m2. Burgess et al. (2003) estimate 2m row width for trees
         crop_area_disruption = (100^2 - tree_area) / 100^2, # fractional crop area disruption
         crop_area_disruption = ifelse(agf_treatment=="Fenceline", 1, crop_area_disruption)) # adjust for the fact that this treatment is outside cropping area

# Below is additional Monte Carlo for cost, using data from Burgess et al. (2003). Data assigned into vectors
# below are reported values from Burgess' cost analysis (pp. 33-34)

# establishment costs in ?
set_cost <- c(0.95, 1, 0.8, 0.6, 0.25) # purchase cost per set (young tree)
prot_cost <- c(0.22, 0.18, 0.24, 0.16) # protection cost per set
contmulch_cost <- c(0.1, 0.4) # continuous mulch application per m2 (tree area only)
indimulch_cost <- c(0.4, 0.47) # invividual mulch per tree
grass_cost <- 0.035 # grass sward establishment cost, per m2 (tree area)

# establishment cost Monte Carlo
est_cost_MC <- tibble(est_cost_tree =
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
est_lab_MC <- tibble(est_lab_tree = 
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

maint_cost_MC <- tibble(maint_cost_tree =
                          runif(n = MC_n, min = min(herb_cost), max = max(herb_cost)),
                        maint_cost_m2 =
                          rep(0, n = MC_n)
)

# annual maintenance labour in hours
herb_lab <- c(0.5, 0.2, 0.085, 0.06) / 60 # herbicide labour per m2
grasscut_lab <- c(0.3, 0.4, 0.6) / 60 # cutting grass between trees, per m2
treemaint_lab <- c(1.15, 2.3, 1.1, 4) / 60 # annual maintenance labour, per tree

maint_lab_MC <- tibble(maint_lab_tree =
                         rpert(n = MC_n, mode = mean(treemaint_lab), min = min(treemaint_lab), max = max(treemaint_lab)),
                       maint_lab_m2 =
                         rpert(n = MC_n, mode = mean(herb_lab), min = min(herb_lab), max = max(herb_lab)) +
                         rpert(n = MC_n, mode = mean(grasscut_lab), min = min(grasscut_lab), max = max(grasscut_lab))
)

# function to estimate timber price, based on Whiteman et al. (1991), adjusted for inflation
timber_value <- function(S){
  # implement price curve equation from Whiteman et al. (1991) pg. 17
  a <- 2.24
  sum_d <- 0.42 - 0.35 # using Ash + true size dummies - may need to be adjusted for other spp. Assuming timber is standing, so no adjustments made.
  b <- 0.47
  conv_fac_1990 <- 3.6780
  P <- conv_fac_1990 * exp(a + sum_d) * S ^ b
  
  # adjust to GBP2017 using inflation factor (1990 - 2017) http://www.in2013dollars.com/1990-GBP-in-2017?amount=1
  inf_fac <- 2.16
  
  # adjust
  P_adj <- P * inf_fac
  return(P_adj)
}

# add in economics values to main Monte Carlo (scaled by tree numbers/area) and adjust from GBP2003 to GBP2017 http://www.in2013dollars.com/2003-GBP-in-2017?amount=1
# including stochastic adjustment factor for timber prices based on variation in the real-terms FPI, 1990-2017 (1990 was a real-terms all-time high for standing timber in this period, hence max is 1)
# see data in Excel workbook [Forestry Price Indices adjustment for Whiteman model.xlsx]

#inflation factor for 2003 economic values
inf_fac <- 1.50

Dat_MC <- Dat_MC %>%
  mutate(est_cost = (trees_ha * est_cost_MC$est_cost_tree + tree_area * est_cost_MC$est_cost_m2) * inf_fac,
         est_lab = trees_ha * est_lab_MC$est_lab_tree + tree_area * est_lab_MC$est_lab_m2,
         maint_cost = (trees_ha * maint_cost_MC$maint_cost_tree + tree_area * maint_cost_MC$maint_cost_m2) * inf_fac,
         maint_lab = trees_ha * maint_lab_MC$maint_lab_tree + tree_area * maint_lab_MC$maint_lab_m2,
         timber_value = timber_value(S = vol_tree),
         timber_value_adj = timber_value * rpert(n = MC_n, mode = 0.513, min = 0.215, max = 1), # price index adjustment factor
         timber_revenue = timber_value_adj * vol_tree * trees_ha)

# add in estimate of below ground C sequestration (from Aertsens et al., 2013; see Excel workbook [Belowground C per tree.xlsx])
Dat_MC <- Dat_MC %>%
  mutate(C_BG_ha_year = trees_ha * rpert(n = MC_n, mode = 3.97, min = 1.25, max = 6.25) * 10^-3)

# visualise simulation results
# CO2 seq
ggplot(Dat_MC %>% filter(agf_treatment!="Fenceline"),
       aes(x = yield_class %>% as.ordered(), y = CO2_ha_year, colour = period_end %>% as.ordered())) +
  geom_boxplot() +
  facet_wrap(~agf_treatment, nrow = 4) +
  scale_colour_discrete(name = "Time to\nclearfell\n(years)") +
  labs(x = "Yield Class", y = expression('Sequestration (tonnes CO'[2]*'eq year'^{-1}*')')) +
  theme_classic()

ggplot(Dat_MC %>% mutate(time_to_clearfell = paste(period_end, "years to clearfell")),
       aes(x = agf_treatment, y = CO2_ha_year, colour = yield_class %>% as.ordered())) +
  geom_boxplot() +
  facet_wrap(~time_to_clearfell %>% as.ordered, nrow = 4) +
  scale_colour_discrete(name = "Yield\nclass") +
  labs(x = "", y = expression('Sequestration (tonnes CO'[2]*'eq year'^{-1}*')')) +
  theme_classic()

# timber yield per ha
ggplot(Dat_MC %>% filter(agf_treatment!="Fenceline"),
       aes(x = yield_class %>% as.ordered(), y = vol_tree * trees_ha, colour = period_end %>% as.ordered())) +
  geom_boxplot() +
  facet_wrap(~agf_treatment, nrow = 4) +
  scale_colour_discrete(name = "Time to\nclearfell\n(years)") +
  labs(x = "Yield Class", y = expression('Timber yield (m'^{3}*' ha'^{-1}*')')) +
  theme_classic()

# timber revenue per ha
ggplot(Dat_MC %>% filter(agf_treatment!="Fenceline"),
       aes(x = yield_class %>% as.ordered(), y = timber_revenue / 1000, colour = period_end %>% as.ordered())) +
  geom_boxplot() +
  facet_wrap(~agf_treatment, nrow = 4) +
  scale_colour_discrete(name = "Time to\nclearfell\n(years)") +
  labs(x = "Yield Class", y = expression('Timber value (\'000 £ ha'^{-1}*')')) +
  theme_classic()

ggplot(Dat_MC,
       aes(x = agf_treatment, y = timber_revenue / 1000, colour = period_end %>% as.ordered())) +
  geom_boxplot(outlier.shape = NA) +
  scale_colour_discrete(name = "Time to\nclearfell\n(years)") +
  labs(x = "", y = expression('Timber value (\'000 £ ha'^{-1}*')')) +
  theme_classic()

# raw Monte Carlo simulation
write_rds(Dat_MC, "output-files/monte-carlo-sim-raw.rds")

Dat_MC <- Dat_MC %>% mutate(time_to_clearfell = paste(period_end, "years to clearfell"),
                            treatment = paste(agf_treatment, time_to_clearfell, sep = ", "))

# summarise main Monte Carlo and write out data for cost models
C_seq_summ <- Dat_MC %>%
  group_by(treatment) %>%
  summarise(mean = mean(C_ha_year),
            sd = sd(C_ha_year),
            CI_lower = quantile(C_ha_year, 0.025),
            CI_upper = quantile(C_ha_year, 0.975))
write_csv(C_seq_summ, "output-files/c-sequestration.csv")

# function to create replicate treatments for variables unaffected by system duration
treatments <- C_seq_summ %>% pull(treatment)
rep_treatment <- function(x){
  x <- x %>% rbind(x) %>% rbind(x) %>% rbind(x) %>% arrange(agf_treatment)
  x[, 1] <- treatments
  return(x)
}

Trees_ha_summ <- Dat_MC %>%
  group_by(agf_treatment) %>%
  summarise(mean = mean(trees_ha),
            sd = sd(trees_ha),
            CI_lower = quantile(trees_ha, 0.025),
            CI_upper = quantile(trees_ha, 0.975)) %>%
  rep_treatment()
write_csv(Trees_ha_summ, "output-files/trees-per-ha.csv")

Area_impact_summ <- Dat_MC %>%
  group_by(agf_treatment) %>%
  summarise(mean = mean(crop_area_disruption),
            sd = sd(crop_area_disruption),
            CI_lower = quantile(crop_area_disruption, 0.025),
            CI_upper = quantile(crop_area_disruption, 0.975)) %>%
  rep_treatment()
write_csv(Area_impact_summ, "output-files/crop-area-disruption.csv")

Est_cost_summ <- Dat_MC %>%
  group_by(agf_treatment) %>%
  summarise(mean = mean(est_cost),
            sd = sd(est_cost),
            CI_lower = quantile(est_cost, 0.025),
            CI_upper = quantile(est_cost, 0.975)) %>%
  rep_treatment()
write_csv(Est_cost_summ, "output-files/establishment-costs.csv")

Est_lab_summ <- Dat_MC %>%
  group_by(agf_treatment) %>%
  summarise(mean = mean(est_lab),
            sd = sd(est_lab),
            CI_lower = quantile(est_lab, 0.025),
            CI_upper = quantile(est_lab, 0.975)) %>%
  rep_treatment()
write_csv(Est_lab_summ, "output-files/establishment-labour.csv")

Maint_cost_summ <- Dat_MC %>%
  group_by(agf_treatment) %>%
  summarise(mean = mean(maint_cost),
            sd = sd(maint_cost),
            CI_lower = quantile(maint_cost, 0.025),
            CI_upper = quantile(maint_cost, 0.975)) %>%
  rep_treatment()
write_csv(Maint_cost_summ, "output-files/maintenance-costs.csv")

Maint_lab_summ <- Dat_MC %>%
  group_by(agf_treatment) %>%
  summarise(mean = mean(maint_lab),
            sd = sd(maint_lab),
            CI_lower = quantile(maint_lab, 0.025),
            CI_upper = quantile(maint_lab, 0.975)) %>%
  rep_treatment()
write_csv(Maint_lab_summ, "output-files/maintenance-labour.csv")

Timber_revenue_summ <- Dat_MC %>%
  group_by(treatment) %>%
  summarise(mean = mean(timber_revenue),
            sd = sd(timber_revenue),
            CI_lower = quantile(timber_revenue, 0.025),
            CI_upper = quantile(timber_revenue, 0.975))
write_csv(Timber_revenue_summ, "output-files/timber-revenue.csv")

C_BG_seq_summ <- Dat_MC %>%
  group_by(agf_treatment) %>%
  summarise(mean = mean(C_BG_ha_year),
            sd = sd(C_BG_ha_year),
            CI_lower = quantile(C_BG_ha_year, 0.025),
            CI_upper = quantile(C_BG_ha_year, 0.975)) %>%
  rep_treatment()
write_csv(C_BG_seq_summ, "output-files/c-below-ground-sequestration.csv")