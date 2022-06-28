rm(list = ls())

library(tidyverse)
unloadNamespace('raster')
unloadNamespace('papeR')

source('code/analysis/functions.R')
source('code/analysis/parameters.R')

allotments <- read_csv('data/temp/allotment_info.csv')

allotments %>% 
  group_by( ecoregion ) %>%
  summarise( 
    Districts = n_distinct(DISTRICT), 
    `Field Offices` = n_distinct(OFFICE), 
    Allotments = n_distinct(uname), 
    `Total Area (10^6 ha)` = sum(hectares)/10^6, 
    `Average Area (ha)` = mean(hectares), 
    `% BLM` = 100*sum(BLM)/sum(hectares), 
    `% Private` = 100*sum(Private)/sum(hectares), 
    `% Other` = 100*sum(Other)/sum(hectares), 
    `% Masked` = 100*sum(masked)/sum(hectares), 
    `Average Elevation (m)` = mean(elevation)) %>% 
  ungroup( ) %>% 
  rename( Ecoregion = ecoregion )  %>% 
  bind_rows(
    allotments %>% 
  summarise( 
    Districts = n_distinct(DISTRICT), 
    `Field Offices` = n_distinct(OFFICE), 
    Allotments = n_distinct(uname), 
    `Total Area (10^6 ha)` = sum(hectares)/10^6, 
    `Average Area (ha)` = mean(hectares), 
    `% BLM` = 100*sum(BLM)/sum(hectares), 
    `% Private` = 100*sum(Private)/sum(hectares), 
    `% Other` = 100*sum(Other)/sum(hectares), 
    `% Masked` = 100*sum(masked)/sum(hectares), 
    `Average Elevation (m)` = mean(elevation)) %>% 
    mutate( Ecoregion = 'Total')) %>% 
  kableExtra::kable(digits = c(0,0, 0, 0,2,0,0,0,1,1,0)) %>%
  kableExtra::save_kable(file = 'output/tables/Allotment_stats.html')


allotments %>% 
  ungroup()  %>% 
    summarise( mean(hectares), min(hectares), max(hectares))
