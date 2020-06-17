library(tidyverse)
library(readxl)
library(mc2d)

Dat_lf <- tibble()
year <- c(1984, 1990, 1998, 2007)
for(i in 1:length(year)){
  fname = paste("field-boundary-estimation/countryside-survey-linear-features/linear-feature-lengths/CS", year[i], "_Linear_Stock.csv", sep="")
  temp <- read_csv(fname)
  Dat_lf <- Dat_lf %>% rbind(temp)
}
rm(fname, temp, i, year)

# rename and compute new variables
colnames(Dat_lf) <- c("year", "land_class", "feature_no", "feature", "length_min", "length_be", "length_max", "area")
Dat_lf <- Dat_lf %>%
  mutate(country = land_class %>% str_extract("[:alpha:]") %>%
           str_replace("\\be\\b", "England") %>%
           str_replace("\\bw\\b", "Wales") %>%
           str_replace("\\bs\\b", "Scotland"),
         ITE_no = land_class %>% str_extract("[:digit:]+") %>%
           as.numeric())


# read land classes and join
landclass_key <- read_csv("field-boundary-estimation/land-class-data/ite-class-descriptions.csv")
Dat_lf <- Dat_lf %>% left_join(landclass_key, by = "ITE_no")

# create general agriculture binary
Dat_lf <- Dat_lf %>%
  mutate(both = arable + livestock,
         both = ifelse(both > 0, 1, 0))

# summarise
arable <- Dat_lf %>%
  group_by(year, feature, country) %>%
  filter(arable==1) %>%
  summarise(length_be = sum(length_be),
            length_min = sum(length_min),
            length_max = sum(length_max),
            area = sum(area)) %>%
  ungroup()

grass <- Dat_lf %>%
  group_by(year, feature, country) %>%
  filter(livestock==1) %>%
  summarise(length_be = sum(length_be),
            length_min = sum(length_min),
            length_max = sum(length_max),
            area = sum(area)) %>%
  ungroup()

both <- Dat_lf %>%
  group_by(year, feature, country) %>%
  filter(both==1) %>%
  summarise(length_be = sum(length_be),
            length_min = sum(length_min),
            length_max = sum(length_max),
            area = sum(area)) %>%
  ungroup()

# bind into main summary
Summ_lf <- bind_rows(list("Arable" = arable, "Grass" = grass, "Both" = both), .id = "ag_type")

Summ_lf <- Summ_lf %>%
  mutate(length_be_per_ha = (length_be * 10^6) / (area * 10^2))

# visualise
ggplot(Summ_lf %>% filter(feature != "Total"),
       aes(x = year, y = length_be_per_ha, colour = feature)) +
  facet_grid(ag_type ~ country) +
  geom_line()

ggplot(Summ_lf %>% filter(feature != "Total",
                          ag_type == "Both",
                          year != 1984),
       aes(x = year, y = length_be_per_ha, colour = feature)) +
  geom_line() +
  geom_smooth(method = "lm") +
  facet_wrap(~country, nrow = 3)

# plotting shows more or less stable lengths from 1990 on

# create final summaries assuming stable lengths from 1990 on
Boundary_est <- Summ_lf %>%
  filter(ag_type != "Both",
         year != 1984, # not wanted as much changed between 1984 - 1990
         feature != "Bank/Grass Strip", # not a boundary
         feature != "Line of Trees", # not a boundary
         feature != "Total") %>% # # not needed
  group_by(feature, ag_type, country) %>%
  summarise(length_be = mean(length_be),
            length_max = mean(length_max),
            length_min = mean(length_min),
            area = mean(area)) %>% # area is all same anyway
  ungroup() %>%
  mutate_at(vars(starts_with("length")), funs(.*10^6)) %>% # convert lengths to m
  mutate(area = area * 10^2) # convert area to ha

Boundary_est_ha <- Boundary_est %>%
  mutate_at(vars(starts_with("length")), funs(./area)) %>%
  select(-area)

# write out results
write_csv(Boundary_est_ha, "field-boundary-estimation/field-boundaries-per-ha.csv")

# sense check by estimating field size
bound_length = function(field_size){
  bound_length = sqrt(field_size * 10000) * 4
  bound_length_adj = bound_length / 4 * (1 + 3 * 0.5) # assumes 3 / 4 boundaries are shared
  bound_length_adj_ha = bound_length_adj / field_size
  return(bound_length_adj_ha)
}

bound_length(1:50)
sum(Boundary_est_ha %>% filter(ag_type == "Arable", country == "England") %>% pull(length_be))
# average English arable field is ~12 ha -- seems legit
