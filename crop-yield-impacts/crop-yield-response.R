
library(tidyverse)
library(mc2d)

env_cropyield <- new.env()

env_cropyield$Dat_cropyield <- readxl::read_xlsx("crop-yield-impacts/yield-impacts-combined.xlsx", sheet = "Combined", range = "X4:AA8") %>%
  rename(row_spacing = `Row width`, be = Mean, min = Min, max = Max)

# examine and determine correct loess span
env_cropyield$Dat_cropyield %>%
  gather(-row_spacing, key = "bound", value = "rel_yield") %>%
  ggplot(aes(x = row_spacing, y = rel_yield, group = bound, colour = bound)) +
  geom_line() +
  geom_smooth(method = "loess", span = 1)

# loess models for be, min, max
env_cropyield$loess_be <- loess(be ~ row_spacing, data = env_cropyield$Dat_cropyield, span = 1)
env_cropyield$loess_min <- loess(min ~ row_spacing, data = env_cropyield$Dat_cropyield, span = 1)
env_cropyield$loess_max <- loess(max ~ row_spacing, data = env_cropyield$Dat_cropyield, span = 1)

# function
crop_yield_response <- function(row_spacing, stochastic = FALSE) {
  mode <- predict(env_cropyield$loess_be, newdata = row_spacing)
  min <- predict(env_cropyield$loess_min, newdata = row_spacing)
  max <- predict(env_cropyield$loess_max, newdata = row_spacing)
  
  if(stochastic == TRUE){
    rel_yield <- rpert(n = length(row_spacing), min = min, mode = mode, max = max, shape = 10)
  } else {
    rel_yield <- mode
  }
  return(rel_yield)
}
