
library(tidyverse)
library(mc2d)

Dat_cropyield <- readxl::read_xlsx("crop-yield-impacts/yield-impacts-combined.xlsx", sheet = "Combined", range = "X4:AA8") %>%
  rename(row_spacing = `Row width`, be = Mean, min = Min, max = Max)

# examine and determine correct loess span
Dat_cropyield %>%
  gather(-row_spacing, key = "bound", value = "rel_yield") %>%
  ggplot(aes(x = row_spacing, y = rel_yield, group = bound, colour = bound)) +
  geom_line() +
  geom_smooth(method = "loess", span = 1)

# loess models for be, min, max
cropyield_loess <- list(loess_be = loess(be ~ row_spacing, data = Dat_cropyield, span = 1),
                        loess_min = loess(min ~ row_spacing, data = Dat_cropyield, span = 1),
                        loess_max = loess(max ~ row_spacing, data = Dat_cropyield, span = 1))

# write out model objects
write_rds(cropyield_loess, "crop-yield-impacts/cropyield-loess-models.rds")

