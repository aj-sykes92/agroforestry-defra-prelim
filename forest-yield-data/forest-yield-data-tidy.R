library(tidyverse)
library(readxl)

# read in outputs from forest yield model and amalgamate. Formatting is riduculous, hence ifs etc.
filenames = paste("forest-yield-data/forest-yield-outputs-raw", dir("forest-yield-data/forest-yield-outputs-raw"), sep = "/")
df_yc <- tibble(full_name = character(0),
                thin_treat = character(0),
                yield_class = numeric(0),
                spacing = numeric(0),
                period_end = numeric(0),
                top_ht = numeric(0),
                trees_ha = numeric(0),
                dbh = numeric(0),
                ba = numeric(0),
                vol_tree = numeric(0),
                vol_ha = numeric(0),
                trees_ha_THIN = numeric(0),
                dbh_THIN = numeric(0),
                ba_THIN = numeric(0),
                vol_tree_THIN = numeric(0),
                vol_ha_THIN = numeric(0),
                ba_CUM = numeric(0),
                vol_ha_CUM  = numeric(0),
                perc_mort = numeric(0),
                mai = numeric(0))

for(i in 1:length(filenames)){
  full_name <- read_excel(filenames[i], range = "A2", col_names = F) %>% pull()
  yield_class <- read_excel(filenames[i], range = "B2", col_names = F) %>% pull()
  thin_treat <- read_excel(filenames[i], range = "C2", col_names = F) %>% pull()
  main <- NA
  spacing <- NA
  
  if(thin_treat != "No Thinning"){
    spacing <- read_excel(filenames[i], range = "D2", col_names = F) %>% pull()
    main <- read_excel(filenames[i], range = cell_limits(c(6, 1), c(NA, NA)))
    colnames(main) <- c("period_end", "top_ht", "trees_ha", "dbh", "ba", "vol_tree", "vol_ha",
                        "trees_ha_THIN", "dbh_THIN", "ba_THIN", "vol_tree_THIN", "vol_ha_THIN", "ba_CUM", "vol_ha_CUM", "mai")
  }
  
  if(thin_treat == "No Thinning"){
    spacing <- read_excel(filenames[i], range = "E2", col_names = F) %>% pull()
    main <- read_excel(filenames[i], range = cell_limits(c(3, 1), c(NA, NA)))
    colnames(main) <- c("period_end", "top_ht", "trees_ha", "dbh", "ba", "vol_tree", "vol_ha", "perc_mort", "mai")
  }
  
  main <- main %>%
    mutate(thin_treat = thin_treat,
           full_name = full_name,
           yield_class = yield_class,
           spacing = spacing)
  
  df_yc <- bind_rows(df_yc, main)
}

write_csv(df_yc, "forest-yield-output-tidy.csv")
