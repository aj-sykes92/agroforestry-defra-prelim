library(tidyverse)

#####################################
# MACC plot function
#####################################

build_macc_plot <- function(df){
  
  crop_colours <- RColorBrewer::brewer.pal(9, "Pastel1") # can change up if desired
  crop_colours[10] <- "#e7e1ef" # the default grey not nice
  names(crop_colours) <- c("oil_crops_other", "potato", "pasture", "vegetable", "rapeseed",
                           "wheat", "barley", "pulses_other", "upland", "cereals_other")
  
  scc <- 66.1
  
  df %>%
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