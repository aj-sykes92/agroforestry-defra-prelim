library(tidyverse)

#####################################
# make crop names nice
#####################################
prettify <- function(crops){
  first <- str_sub(crops, 1, 1) %>%
    str_to_upper()
  
  rest <- str_sub(crops, 2, -1) %>%
    str_replace_all("_other", ", other") %>%
    str_replace_all("_", " ")
  
  pretty <- paste0(first, rest)
  
  return(pretty)
}

#####################################
# cropwise MACC plot function
#####################################
build_macc_plot <- function(df){
  
  crop_colours <- RColorBrewer::brewer.pal(9, "Pastel1") # can change up if desired
  crop_colours[10] <- "#e7e1ef" # the default grey not nice
  names(crop_colours) <- c("Oil crops, other", "Potato", "Pasture", "Vegetable", "Rapeseed",
                           "Wheat", "Barley", "Pulses, other", "Upland", "Cereals, other")
  
  scc <- 66.1
  
  df %>%
    mutate(crop = prettify(crop)) %>%
    select(mac_gbp_tco2, co2_tyear, crop) %>%
    arrange(mac_gbp_tco2) %>%
    mutate(co2_tyear = co2_tyear * 10^-6,
           xmax = cumsum(co2_tyear),
           xmin = lag(xmax, default = 0),
           ymin = ifelse(mac_gbp_tco2 < 0,mac_gbp_tco2, 0),
           ymax = ifelse(mac_gbp_tco2 > 0, mac_gbp_tco2, 0)) %>%
    ggplot() +
    geom_rect(aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = crop), colour = NA) + # main MACC
    #geom_rect(aes(xmin = below_scc, xmax = max(xmax), ymin = 0, ymax = max(ymax)), fill = "white", colour = NA, alpha = 0.027) + # grey out values @ > SCC
    geom_hline(yintercept = 0, size = 0.1, colour = "darkred", lty = 2) +
    geom_hline(yintercept = scc, size = 0.1, colour = "darkred", lty = 2) +
    #geom_vline(xintercept = below_zero, size = 0.1, colour = "grey", lty = 1) +
    #geom_vline(xintercept = below_scc, size = 0.05, colour = "grey", lty = 1) +
    scale_fill_manual(values = crop_colours) +
    labs(x = expression('Abatement potential (Mt CO'[2]*'eq year'^{-1}*')'),
         y = expression('Marginal abatement cost (GBP tonne CO'[2]*'-eq'^{-1}*')'),
         fill = "") +
    theme_classic()
}

#####################################
# mac map function
#####################################
build_macc_map <- function(df){
  Shp_UK <- raster::shapefile(find_onedrive(dir = "GIS data repository", path = "DA shapefile/GBR_adm_shp/GBR_adm1.shp"))
  
  df %>%
    group_by(x, y) %>%
    summarise(co2_tyear = sum(co2_tyear, na.rm = T),
              totrev_gbp = sum(totrev_gbp, na.rm = T)) %>%
    ungroup() %>%
    mutate(mac_gbp_tco2 = totrev_gbp / -co2_tyear) %>%
    ggplot(aes(x = x, y = y, fill = mac_gbp_tco2)) +
    geom_raster(aes(x = x, y = y, fill = mac_gbp_tco2)) +
    geom_polygon(data = Shp_UK, aes(x = long, y = lat, group = group), colour = "black", fill = NA, size = 0.5) +
    coord_quickmap() +
    theme_void()
}

#####################################
# paired mac and ar map function
#####################################
build_paired_map <- function(df){
  shp <- read_rds("app-helper-data/uk-shp.rds")
  
  df_mac <- df %>%
    ungroup() %>%
    group_by(x, y) %>%
    summarise(co2_tyear = sum(co2_tyear, na.rm = T),
              totrev_gbp = sum(totrev_gbp, na.rm = T)) %>%
    ungroup() %>%
    mutate(value = totrev_gbp / -co2_tyear)
  
  df_ar <- df %>%
    ungroup() %>%
    group_by(x, y) %>%
    summarise(co2_tyear = sum(co2_tyear, na.rm = T),
              area_ha = sum(area_ha, na.rm = T)) %>%
    ungroup() %>%
    mutate(value = co2_tyear / area_ha)
  
  data <- list(df_mac, df_ar)
  shp <- list(shp, shp)
  labs <- list(expression("MAC, GBP t CO"[2]*"-eq"^{-1}),
               expression("AR, t CO"[2]*"-eq ha"^{-1}*" year"^{-1}))
  
  mapplot <- function(df, shp, label) {
    df %>%
      ggplot() +
      geom_raster(aes(x = x, y = y, fill = value)) +
      geom_polygon(data = shp, aes(x = long, y = lat, group = group), colour = "black", fill = NA, size = 0.5) +
      labs(title = label,
           fill = "") +
      coord_quickmap() +
      theme_void()
  }
  
  plotlist <- pmap(list(data, shp, labs), mapplot)
  gridExtra::grid.arrange(grobs = plotlist, nrow = 1, ncol = 2)

}

#####################################
# scaling functions
#####################################
cheap_scale <- function(df, area_frac){
  df %>%
    arrange(mac_gbp_tco2) %>%
    mutate(area_cumfrac = cumsum(area_ha) / sum(area_ha)) %>%
    filter(area_cumfrac <= area_frac) %>%
    select(x, y, da_num, crop, area_ha, yield_tha, yield_tha_agf, area_impact, co2_tyear:mac_gbp_tco2)
}

even_scale <- function(df, area_frac){
  df %>%
    select(x, y, da_num, crop, area_ha, yield_tha, yield_tha_agf, area_impact, co2_tyear:mac_gbp_tco2) %>%
    mutate_at(vars(c(area_ha, co2_tyear, totrev_gbp)), funs(. * area_frac))
}

#####################################
# aggreagate MACC plot function
#####################################
build_agmacc_plot <- function(df){

  measure_colours <-  RColorBrewer::brewer.pal(4, "Pastel1") # can change up if desired
  names(measure_colours) <- c("Row agroforestry", "Shelterbelts", "Fenceline planting", "Hedge expansion")
  
  scc <- 66.1
  
  df %>%
    select(mac_gbp_tco2, co2_tyear, sys_type) %>%
    arrange(mac_gbp_tco2) %>%
    mutate(co2_tyear = co2_tyear * 10^-6,
           xmax = cumsum(co2_tyear),
           xmin = lag(xmax, default = 0),
           ymin = ifelse(mac_gbp_tco2 < 0,mac_gbp_tco2, 0),
           ymax = ifelse(mac_gbp_tco2 > 0, mac_gbp_tco2, 0)) %>%
    ggplot() +
    geom_rect(aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = sys_type), colour = NA) + # main MACC
    #geom_rect(aes(xmin = below_scc, xmax = max(xmax), ymin = 0, ymax = max(ymax)), fill = "white", colour = NA, alpha = 0.027) + # grey out values @ > SCC
    geom_hline(yintercept = 0, size = 0.1, colour = "darkred", lty = 2) +
    geom_hline(yintercept = scc, size = 0.1, colour = "darkred", lty = 2) +
    #geom_vline(xintercept = below_zero, size = 0.1, colour = "grey", lty = 1) +
    #geom_vline(xintercept = below_scc, size = 0.05, colour = "grey", lty = 1) +
    scale_fill_manual(values = measure_colours) +
    labs(x = expression('Abatement potential (Mt CO'[2]*'eq year'^{-1}*')'),
         y = expression('Marginal abatement cost (GBP tonne CO'[2]*'-eq'^{-1}*')'),
         fill = "") +
    theme_classic()
}

#####################################
# aggreagate abatement map function
#####################################
build_ab_map <- function(df){
  Shp_UK <- raster::shapefile(find_onedrive(dir = "GIS data repository", path = "DA shapefile/GBR_adm_shp/GBR_adm1.shp"))
  
  df %>%
    group_by(x, y) %>%
    summarise(ap_tco2 = -sum(co2_tyear, na.rm = T)) %>%
    ungroup() %>%
    ggplot() +
    geom_raster(aes(x = x, y = y, fill = ap_tco2)) +
    geom_polygon(data = Shp_UK, aes(x = long, y = lat, group = group), colour = "black", fill = NA, size = 0.5) +
    coord_quickmap() +
    theme_void()
}

#####################################
# descriptives function
#####################################
get_descriptives <- function(df, grouping_vars = NULL){
  
  # main descriptives
  df <- df %>%
    mutate(yield_t = yield_tha * area_ha,
           yield_t_agf = yield_tha_agf * area_ha * area_impact,
           yieldinc_t = yield_t_agf * area_impact - yield_t,
           areainc_ha = area_ha * area_impact - area_ha) %>%
    group_by_at(grouping_vars) %>%
    summarise_at(vars(area_ha, areainc_ha, yield_t:yieldinc_t, co2_tyear, totrev_gbp), funs(sum(.))) %>%
    mutate(area_kha = area_ha * 10^-3,
           areainc_kha = areainc_ha * 10^-3,
           areainc_frac = areainc_ha / area_ha,
           prodinc_kt = yieldinc_t * 10^-3,
           prodinc_frac = yieldinc_t / yield_t,
           totcost_milliongbp = -totrev_gbp * 10^-6,
           co2_ktyear = co2_tyear * 10^-3,
           ar_tha = co2_tyear / area_ha,
           mac_gbp_tco2 = -totrev_gbp / co2_tyear
           ) %>%
    mutate_at(vars(area_kha:mac_gbp_tco2), funs(round(., 2))) %>%
    select(grouping_vars, area_kha:mac_gbp_tco2) %>%
    arrange(mac_gbp_tco2)
  
  # adjust crop names
  if("crop" %in% colnames(df)){
    df <- df %>%
      mutate(crop = prettify(crop))
  }
  
  
  # rename
  blanks <- rep("", times = length(grouping_vars))
  colnames(df) <- c(blanks, "Total applicable area (kha)", "Area change (kha)", "Area change (fractional)",
                    "Production change (kt DM)", "Production change (fractional)",
                    "Total net cost (£m)", "AP (kt CO2 / year)", "AR (t CO2 / ha / yr)", "MAC (£ / tCO2)")
  
  return(df)
}
