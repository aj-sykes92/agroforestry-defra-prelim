
library(tidyverse)

#####################################
# filter crop function
#####################################
filter_crops <- function(df, applies_to){
  df %>%
    filter(crop %in% applies_to)
}

#####################################
# filter DA function
#####################################
filter_da <- function(df, da = c("England", "Scotland", "Wales", "Northern Ireland")){
  da <- tibble(name = c("England", "Scotland", "Wales", "Northern Ireland"),
               num = 1:4) %>%
    filter(name %in% da) %>%
    pull(num)
  
  df <- df %>%
    filter(da_num %in% da)
  
  return(df)
}

#####################################
# discounting and annualisation functions
#####################################

annualise_cost <- function(tot_cost, lifespan, dr){
  annual_cost <- tot_cost / ((1-(1/(1 + dr)^lifespan))/dr)
  return(annual_cost)
}

annualise_return <- function(tot_return, real_int, dr){
  npv <- tot_return / (1 + dr) ^ real_int
  annual_return <- npv / ((1-(1/(1 + dr)^real_int))/dr)
  return(annual_return)
}

#####################################
# calculate total cost, abatement and MAC
#####################################
calc_mac <- function(df){
  df %>%
    mutate(ar_tha = co2_tyear / area_ha,
           totrev_gbp = (gm_gbp_agf - gm_gbp) + timbgm_gbp,
           mac_gbp_tco2 = -totrev_gbp / co2_tyear)
}

