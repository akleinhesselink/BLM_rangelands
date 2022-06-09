rm(list = ls())
library(tidyverse)
AIM_ecoregion <- read_csv('data/temp/AIM_species_by_ecoregion.csv')
SpeciesState <- read_csv('data/temp/clean_speciesState_table.csv')

# Add states so table can be joined with SpeciesState 
AIM_ecoregion <- AIM_ecoregion %>% 
  left_join( 
    data.frame( ecogroup = c( 'AZ/NM Highlands', 
                              'E Cold Deserts', 
                              'Forested Mts', 
                              'Mediterranean California', 
                              'N Great Plains', 
                              'S Great Plains', 
                              'W Cold Deserts', 
                              'Warm Deserts') , 
                SpeciesState = c( 'AZ', 
                                  'UT', 
                                  'ID', 
                                  'CA', 
                                  'MT', 
                                  'NM', 
                                  'NV', 
                                  'AZ')  )) 


AIM_ecoregion <- AIM_ecoregion %>%   
  left_join( SpeciesState, by = c('Species' = 'SpeciesCode', 
                                  'SpeciesState' = 'SpeciesState')) %>% 
  arrange( ecogroup, GrowthHabitSub , desc(allotment_freq), desc(avg_cover))  


refinedSpecies <- AIM_ecoregion %>% 
  filter( GrowthHabitSub != 'NONVASCULAR') %>% 
  arrange( ecogroup, Duration, GrowthHabitSub, desc(allotment_freq)) %>% 
  mutate( SCIENTIFIC = ifelse( is.na(SCIENTIFIC), ScientificName, SCIENTIFIC)) 


refinedSpeciesRanks <- refinedSpecies %>% 
  group_by( ecogroup, Duration, GrowthHabitSub) %>% 
  mutate( freq_rank = order( allotment_freq, decreasing = T)) %>%
  filter( freq_rank < 6 ) 

refinedSpeciesRanks %>% 
  filter( Duration == 'ANNUAL',  GrowthHabitSub == 'GRAMINOID') %>% 
  arrange( ecogroup, freq_rank, SCIENTIFIC) %>% 
  ungroup() %>%
  select( ecogroup, SCIENTIFIC, allotment_freq, avg_cover, n_allots, num_allotments_surveyed, invasiveUS, exoticUS , Noxious) %>% 
  write_csv(file = 'output/tables/AIM_annual_grass_frequency_by_ecoregion.csv')


refinedSpeciesRanks %>% 
  filter( Duration == 'ANNUAL',  GrowthHabitSub == 'FORB') %>% 
  arrange( ecogroup, freq_rank, SCIENTIFIC) %>% 
  ungroup() %>%
  select( ecogroup, SCIENTIFIC, allotment_freq, avg_cover, n_allots, num_allotments_surveyed, invasiveUS, exoticUS , Noxious) %>% 
  write_csv(file = 'output/tables/AIM_annual_forb_frequency_by_ecoregion.csv')


refinedSpeciesRanks %>% 
  filter( Duration == 'PERENNIAL',  GrowthHabitSub == 'GRAMINOID') %>% 
  arrange( ecogroup, freq_rank, SCIENTIFIC) %>% 
  ungroup() %>% 
  select( ecogroup, SCIENTIFIC, allotment_freq, avg_cover, n_allots, num_allotments_surveyed, invasiveUS, exoticUS , Noxious) %>% 
  write_csv(file = 'output/tables/AIM_perennial_grass_frequency_by_ecoregion.csv')


refinedSpeciesRanks %>% 
  filter( Duration == 'PERENNIAL',  GrowthHabitSub == 'FORB') %>% 
  arrange( ecogroup, freq_rank, SCIENTIFIC) %>% 
  ungroup() %>% 
  select( ecogroup, SCIENTIFIC, allotment_freq, avg_cover, n_allots, num_allotments_surveyed, invasiveUS, exoticUS , Noxious) %>% 
  write_csv(file = 'output/tables/AIM_perennial_forb_frequency_by_ecoregion.csv')

refinedSpeciesRanks %>% 
  filter( Duration == 'PERENNIAL') %>% 
  filter( GrowthHabit == 'WOODY') %>% 
  filter(GrowthHabitSub == 'SHRUB') %>% 
  arrange( ecogroup, freq_rank, SCIENTIFIC) %>% 
  ungroup() %>% 
  select( ecogroup, Species, SCIENTIFIC, freq_rank, allotment_freq, avg_cover, n_allots, num_allotments_surveyed, invasiveUS, exoticUS , Noxious) %>% 
  write_csv(file = 'output/tables/AIM_shrub_frequency_by_ecoregion.csv')

refinedSpeciesRanks %>% 
  filter( Duration == 'PERENNIAL') %>% 
  filter( GrowthHabit == 'WOODY') %>% 
  filter( GrowthHabitSub == 'TREE') %>% 
  arrange( ecogroup, freq_rank, SCIENTIFIC) %>% 
  ungroup() %>% 
  select( ecogroup, Species, SCIENTIFIC, freq_rank, allotment_freq, avg_cover, n_allots, num_allotments_surveyed, invasiveUS, exoticUS , Noxious) %>% 
  View 
  

refinedSpeciesRanks %>% 
  filter( Duration == 'PERENNIAL') %>% 
  filter( GrowthHabit == 'WOODY') %>% 
  filter( GrowthHabitSub == 'TREE') %>% 
  arrange( ecogroup, freq_rank, SCIENTIFIC) %>% 
  ungroup() %>% 
  select( ecogroup, Species, SCIENTIFIC, freq_rank, allotment_freq, avg_cover, n_allots, num_allotments_surveyed, invasiveUS, exoticUS , Noxious) %>% 
  write_csv(file = 'output/tables/AIM_tree_frequency_by_ecoregion.csv')

refinedSpeciesRanks %>% 
  filter(GrowthHabitSub == 'SUBSHRUB') %>% 
  arrange( ecogroup, freq_rank, SCIENTIFIC) %>% 
  ungroup() %>% 
  select( ecogroup, SCIENTIFIC, allotment_freq, avg_cover, n_allots, num_allotments_surveyed, invasiveUS, exoticUS , Noxious) %>% 
  write_csv(file = 'output/tables/AIM_subshrub_frequency_by_ecoregion.csv')

refinedSpecies %>% 
  ungroup() %>% 
  mutate( Duration = ifelse( Species == 'ARPU9', "Perennial", Duration )) %>% 
  filter( GrowthHabit == 'NONWOODY', Duration == 'ANNUAL') %>% 
  group_by( Duration,  ecogroup ) %>% 
  mutate( freq_rank = rank( desc(allotment_freq ), ties.method = 'first')) %>% 
  select( ecogroup, Duration, Species, SCIENTIFIC, invasiveUS, exoticUS, Noxious, GrowthHabitSub, freq_rank, allotment_freq, avg_cover, num_allotments_surveyed)  %>% 
  arrange(ecogroup, Duration, desc(allotment_freq)) %>% 
  filter( freq_rank < 11) %>% 
  write_csv( file = 'output/tables/AIM_annuals_frequency_by_ecoregion.csv')

refinedSpecies %>% 
  ungroup() %>% 
  filter( GrowthHabit == 'NONWOODY', Duration == 'PERENNIAL') %>% 
  group_by( Duration,  ecogroup ) %>% 
  mutate( freq_rank = rank( desc(allotment_freq ), ties.method = 'first')) %>% 
  select( ecogroup, Duration, Species, SCIENTIFIC, invasiveUS, exoticUS, Noxious, GrowthHabitSub, freq_rank, allotment_freq, avg_cover, num_allotments_surveyed)  %>% 
  arrange(ecogroup, Duration, desc(allotment_freq)) %>% 
  filter( freq_rank < 11) %>% 
  write_csv( file = 'output/tables/AIM_perennials_frequency_by_ecoregion.csv')


usda <- read_csv('data/usda_plants_annuals.csv')
a <- read_csv('output/tables/AIM_annuals_frequency_by_ecoregion.csv')

a %>% 
  filter(freq_rank < 4 ) %>% 
  write_csv('output/tables/top3_annuals.csv')
p <- read_csv( 'output/tables/AIM_perennials_frequency_by_ecoregion.csv')
p %>%
  filter( freq_rank < 4) %>%
  mutate( ecogroup = paste0(ecogroup, " (", num_allotments_surveyed , ")")) %>% 
  mutate( allotment_freq = round( allotment_freq , 2 ), 
          avg_cover = round( avg_cover, 2)) %>% 
  mutate( GrowthHabitSub = str_replace(GrowthHabitSub, 'GRAMINOID', 'Grass'), 
          GrowthHabitSub = str_replace(GrowthHabitSub, 'FORB', 'Forb')) %>%
  select( ecogroup, freq_rank, SCIENTIFIC, exoticUS, GrowthHabitSub, allotment_freq , avg_cover , Species) %>%
  write_csv('output/tables/top3_perennials.csv')


s <- read_csv('output/tables/AIM_shrub_frequency_by_ecoregion.csv')
s %>%
  filter( freq_rank < 4) %>%
  mutate( ecogroup = paste0(ecogroup, " (", num_allotments_surveyed , ")")) %>% 
  mutate( allotment_freq = round( allotment_freq , 2 ), 
          avg_cover = round( avg_cover, 2)) %>% 
  mutate( GrowthHabitSub = "Shrub") %>%
  select( ecogroup, freq_rank, SCIENTIFIC, exoticUS, GrowthHabitSub, allotment_freq , avg_cover) %>%
  write_csv('output/tables/top3_shrubs.csv')


t <- read_csv('output/tables/AIM_tree_frequency_by_ecoregion.csv')
t %>%
  filter( freq_rank < 4) %>%
  mutate( ecogroup = paste0(ecogroup, " (", num_allotments_surveyed , ")")) %>% 
  mutate( allotment_freq = round( allotment_freq , 2 ), 
          avg_cover = round( avg_cover, 2)) %>% 
  mutate( GrowthHabitSub = "TREE") %>%
  select( ecogroup, freq_rank, SCIENTIFIC, exoticUS, GrowthHabitSub, allotment_freq , avg_cover) %>%
  write_csv('output/tables/top3_trees.csv')

t  %>% 
