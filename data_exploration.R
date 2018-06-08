#Data exploration on stats.csv

library(tidyverse)
raw_stats <- read_csv(file = "Data/indo_aqua_stats.csv", na = c("-"))

aqua <- raw_stats %>% 
  gather(key = 'year', value = 'value', 4:ncol(.)) %>% 
  spread(metric, value) %>% 
  mutate(area_hectares = as.numeric(gsub('\\.', '', area_hectares)),
         tons          = as.numeric(gsub('\\.', '', tons)),
         tons_per_ha   = tons / area_hectares,
         NAME_1 = factor(province))

# Area of brackish water ponds
area_map <- aqua %>% 
  dplyr::filter(year == 2015 & type == 'brackish pond') %>% 
  select(NAME_1, area_hectares) 
# Intensity of production
intensity_map <- aqua %>% 
  dplyr::filter(year == 2015 & type == 'brackish pond') %>% 
  select(NAME_1, tons_per_ha) %>% 
  mutate(tons_per_ha = ifelse(tons_per_ha > 1000, NA, tons_per_ha))
# Area of brackish water ponds
prod_map <- aqua %>% 
  dplyr::filter(year == 2015 & type == 'brackish pond') %>% 
  select(NAME_1, tons)

# Rate of intensity change
perc_change_map <- aqua %>% 
  dplyr::filter(type == 'brackish pond') %>% 
  group_by(province) %>% 
  mutate(perc_change_i  = tons_per_ha / lag(tons_per_ha),
         three_yr_avg_i   = RcppRoll::roll_meanr(perc_change_i, n = 3, na.rm = T),
         perc_change_a  = area_hectares / lag(area_hectares),
         three_yr_avg_a   = RcppRoll::roll_meanr(perc_change_a, n = 3, na.rm = T)) %>% 
  mutate(three_yr_avg_i = ifelse(three_yr_avg_i > 200, 200, three_yr_avg_i),
         three_yr_avg_a = ifelse(three_yr_avg_a > 100, 100, three_yr_avg_a)) %>% 
  dplyr::filter(year == 2015)

# Join subsets to map data
area_map <- left_join(indo, area_map)
tons_map <- left_join(indo, prod_map)
intensity_map <- left_join(indo, intensity_map)
perc_change_map <- left_join(indo, perc_change_map)


ggplot(area_map) +
  geom_sf(aes(key = NAME_1, fill = area_hectares)) +
  scale_fill_viridis(option = "D", labels = comma) +
  guides(fill = guide_colorbar(barwidth = 20,
                               barheight = 0.5,
                               title = "Hectares",
                               title.position = "top")) +
  labs(title = "Area of brackish water pond aquaculture in 2015") +
  theme_minimal() +
  theme(legend.position = 'bottom')