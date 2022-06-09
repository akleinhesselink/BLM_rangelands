rm(list = ls())
library(tidyverse)

AIM <- read_csv('data/temp/AIM_species_by_allotment.csv')
speciesState <- read_csv('data/temp/clean_speciesState_table.csv')

AIM_ecoregion <- 
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
                                  'AZ')  ) 

ecogroup_n <- AIM %>% 
  group_by( ecogroup ) %>%
  summarise( n_allots_total = n_distinct(uname))

AIM <- AIM %>% 
  left_join(AIM_ecoregion) %>% 
  left_join(speciesState, by = c('Species' = 'SpeciesCode', 'SpeciesState' = 'SpeciesState'))


AIM_trees <- AIM %>% 
  mutate( ScientificName = str_squish( str_trim(ScientificName))) %>% 
  filter( GrowthHabit == 'WOODY') %>% 
  separate( ScientificName, into = c('genus', 'sp.')) %>% 
  mutate( GrowthHabitSub = ifelse( genus == 'Prosopis', 'TREE',  GrowthHabitSub)) %>% 
  filter( GrowthHabitSub == 'TREE') %>% 
  left_join(ecogroup_n)

genus_level <- AIM_trees %>% 
  group_by( ecogroup, genus , n_allots_total) %>%
  summarise( num_allots = n_distinct(uname), avg_cover = mean(avg_cover, na.rm = T))  %>% 
  mutate( allot_freq = num_allots/n_allots_total) %>% 
  ungroup() %>%
  arrange( ecogroup, desc(allot_freq)) %>% 
  group_by( ecogroup) %>%
  filter( row_number( ) <  4) 

species_level <- AIM_trees %>% 
  group_by( ecogroup, genus, sp., n_allots_total) %>%
  summarise( num_allots = n_distinct(uname))  %>% 
  mutate( allot_freq = num_allots/n_allots_total) %>% 
  ungroup() %>%
  arrange( ecogroup, desc(allot_freq))

species_level %>% 
  left_join( genus_level , by = c('ecogroup', 'genus', 'n_allots_total')) %>%
  arrange( ecogroup, desc(allot_freq.y)) %>% 
  filter( !is.na(allot_freq.y)) %>% 
  mutate( ScientificName = paste0(str_sub(genus, 0,1), '. ', `sp.`) ) %>% 
  mutate( ecogroup = paste0( ecogroup, ' (', n_allots_total, ')')) %>% 
  #mutate( genus = paste0( genus , ' (', num_allots.y, ')')) %>%
  mutate( Species = paste0(ScientificName,  ' (', num_allots.x, ')')) %>% 
  mutate( genus_freq = round( allot_freq.y , 2 ), 
          species_freq = round(allot_freq.x, 2)) %>% 
  group_by( ecogroup, genus, genus_freq, avg_cover ) %>% 
  summarise( Species = paste( c(Species), collapse = ', ')) %>% 
  select( ecogroup, genus, genus_freq, avg_cover, Species) %>%
  ungroup() %>% 
  distinct() %>% 
  arrange( ecogroup, desc(genus_freq)) %>% 
  mutate( avg_cover = round( avg_cover , 2 )) %>% 
  write_csv('output/tables/AIM_common_tree_genera.csv')

# AIM Annual genera and species 
AIM_annuals <- AIM %>%
  filter( GrowthHabitSub != 'NONVASCULAR') %>%
  filter( !is.na(GrowthHabit), GrowthHabit != 'WOODY') %>% 
  mutate( Duration = ifelse( Species == 'ARPU9', "Perennial", Duration )) %>% 
  filter( GrowthHabit == 'NONWOODY', Duration == 'ANNUAL') %>% 
  left_join(ecogroup_n)

usda <- read_csv('data/usda_plants_annuals.csv')

AIM_annuals <- AIM_annuals %>% 
  left_join(usda, by = c('Species' = 'Accepted Symbol')) %>% 
  select( ecogroup, n_allots_total, uname, Species, nPrimaryKeys, avg_cover, frequency, exoticUS, GrowthHabitSub, Duration.x, 
          ScientificName) %>% 
  ungroup() %>% 
  mutate( exoticUS = ifelse( is.na(exoticUS), F, exoticUS ) ) %>% 
  separate( ScientificName, c('genus', 'spp'), sep = ' ')  

AIM_genus <- AIM_annuals %>% 
  group_by( ecogroup, genus, exoticUS, n_allots_total ) %>% 
  summarise( genus_frequency = n_distinct(uname), avg_cover = mean(avg_cover, na.rm = T)) %>% 
  arrange( ecogroup, desc(genus_frequency))

AIM_species <- AIM_annuals %>% 
  group_by( ecogroup, genus, spp, exoticUS, n_allots_total) %>%
  summarise( species_frequency = n_distinct( uname) ) %>% 
  arrange( ecogroup , desc(species_frequency))

AIM_genus %>% 
  group_by( ecogroup ) %>% 
  filter( row_number() < 4 ) %>% 
  left_join( AIM_species, by = c('ecogroup', 'genus', 'exoticUS', 'n_allots_total')) %>% 
  mutate( spp = paste0( str_sub(genus, 0, 1), '. ' , spp )) %>% 
  mutate( spp = paste0( spp, " (" , species_frequency, ")")) %>% 
  mutate( ecogroup = paste0( ecogroup, " (", n_allots_total, ")" ) ) %>% 
  mutate( genus_frequency = genus_frequency/n_allots_total) %>% 
  group_by( ecogroup, genus, exoticUS, n_allots_total, genus_frequency, avg_cover ) %>% 
  summarise( spp = paste( unique(spp), collapse = ', ')) %>% 
  ungroup() %>% 
  arrange( ecogroup, desc(genus_frequency)) %>% 
  mutate( genus_frequency = round( genus_frequency, 2 ) , avg_cover = round( avg_cover, 2)) %>% 
  select( ecogroup, genus, exoticUS, genus_frequency, avg_cover, spp ) %>% 
  write_csv('output/tables/AIM_common_annual_genera.csv')
