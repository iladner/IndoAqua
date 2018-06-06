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

##Four Axes of Exploration

#Production (tonnes)



#Production (area)

#Intensity

#Perc change in Intensity