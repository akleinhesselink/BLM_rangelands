rm(list = ls()) 
library(tidyverse)
library(sf)

allotments <- read_sf('data/temp/cover_rates/allotments_with_rates.shp')
AIM_species <- read_csv('data/temp/AIM_species_by_allotment.csv')
speciesState <- read_csv('data/temp/clean_speciesState_table.csv')


allotment_species <- AIM_species %>% 
  left_join(allotments )  %>% 
  left_join( speciesState , by = c('Species' = 'SpeciesCode', 
                                   'ADMIN_ST' = 'SpeciesState'))  

common_annuals <- read_csv('output/tables/AIM_annuals_frequency_by_ecoregion.csv')
common_annuals <- common_annuals %>% 
  mutate( common = T)

common_perennials <- read_csv('output/tables/AIM_perennials_frequency_by_ecoregion.csv')
common_perennials <- common_perennials %>% 
  mutate( common = T)

common_shrubs <- read_csv('output/tables/AIM_shrub_frequency_by_ecoregion.csv')
common_shrubs <- common_shrubs %>% 
  mutate( common = T)

common_trees <- read_csv('output/tables/AIM_tree_frequency_by_ecoregion.csv')
common_trees <- common_trees %>% 
  mutate( common = T)

allotment_species %>% 
  select( ecogroup, uname, Species, avg_cover, Annual) %>%
  left_join(common_annuals %>% select( ecogroup, Species, SCIENTIFIC, freq_rank, GrowthHabitSub, common, exoticUS, invasiveUS), by = c('ecogroup', 'Species') )  %>%
  filter( common ) %>% 
  filter( !is.na(avg_cover), !is.na(Annual)) %>% 
  group_by( ecogroup, Species, SCIENTIFIC, GrowthHabitSub, freq_rank, exoticUS, invasiveUS) %>% 
  summarise( r = cor( avg_cover, Annual, use = 'complete'), n = n())  %>% 
  arrange( ecogroup, freq_rank) 

allotment_species %>% 
  select( ecogroup, uname, Species, avg_cover, Perennial) %>%
  left_join(common_perennials %>% select( ecogroup, Species, freq_rank, GrowthHabitSub, common, exoticUS, invasiveUS), by = c('ecogroup', 'Species') )  %>%
  filter( common ) %>% 
  filter( !is.na( avg_cover), !is.na(Perennial)) %>% 
  group_by( ecogroup, Species, GrowthHabitSub, freq_rank, exoticUS, invasiveUS) %>% 
  summarise( r = cor( avg_cover, Perennial, use = 'complete'), n = n())  %>% 
  arrange( ecogroup, freq_rank) 


allotment_species %>% 
  select( ecogroup, uname, Species, avg_cover, Shrub) %>% 
  left_join(common_shrubs %>% select( ecogroup, Species, freq_rank, common, exoticUS, invasiveUS), by = c('ecogroup', 'Species') )  %>%
  filter( common ) %>% 
  filter( !is.na( avg_cover), !is.na(Shrub))  %>%
  group_by( ecogroup, Species, freq_rank, exoticUS, invasiveUS) %>% 
  summarise( r = cor( avg_cover, Shrub, use = 'complete'), n = n())  %>% 
  arrange( ecogroup, freq_rank) 


allotment_species %>% 
  select( ecogroup, uname, Species, avg_cover, Tree) %>% 
  left_join(common_trees %>% select( ecogroup, Species, freq_rank, common, exoticUS, invasiveUS), by = c('ecogroup', 'Species') )  %>%
  filter( common ) %>% 
  filter( !is.na( avg_cover), !is.na(Tree))  %>%
  group_by( ecogroup, Species, freq_rank, exoticUS, invasiveUS) %>% 
  summarise( r = cor( avg_cover, Tree, use = 'complete'), n = n())  %>% 
  arrange( ecogroup, freq_rank)  %>% View 

