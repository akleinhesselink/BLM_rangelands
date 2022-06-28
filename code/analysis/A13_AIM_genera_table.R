rm(list = ls())
library(tidyverse)

AIM <- read_csv('data/temp/AIM_species_by_allotment.csv') %>% 
  rename('ecoregion' = ecoregn)
speciesState <- read_csv('data/temp/clean_speciesState_table.csv')

AIM_ecoregion <- 
    data.frame( ecoregion = c( 'AZ/NM Highlands', 
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

ecoregion_n <- AIM %>% 
  group_by( ecoregion ) %>%
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
  left_join(ecoregion_n)

genus_level <- AIM_trees %>% 
  group_by( ecoregion, genus , n_allots_total) %>%
  summarise( num_allots = n_distinct(uname), avg_cover = mean(avg_cover, na.rm = T))  %>% 
  mutate( allot_freq = num_allots/n_allots_total) %>% 
  ungroup() %>%
  arrange( ecoregion, desc(allot_freq)) %>% 
  group_by( ecoregion) %>%
  filter( row_number( ) <  4) 

species_level <- AIM_trees %>% 
  group_by( ecoregion, genus, sp., n_allots_total) %>%
  summarise( num_allots = n_distinct(uname))  %>% 
  mutate( allot_freq = num_allots/n_allots_total) %>% 
  ungroup() %>%
  arrange( ecoregion, desc(allot_freq))

species_level %>% 
  left_join( genus_level , by = c('ecoregion', 'genus', 'n_allots_total')) %>%
  arrange( ecoregion, desc(allot_freq.y)) %>% 
  filter( !is.na(allot_freq.y)) %>% 
  mutate( ScientificName = paste0(str_sub(genus, 0,1), '. ', `sp.`) ) %>% 
  mutate( ecoregion = paste0( ecoregion, ' (', n_allots_total, ')')) %>% 
  #mutate( genus = paste0( genus , ' (', num_allots.y, ')')) %>%
  mutate( Species = paste0(ScientificName,  ' (', num_allots.x, ')')) %>% 
  mutate( genus_freq = round( allot_freq.y , 2 ), 
          species_freq = round(allot_freq.x, 2)) %>% 
  group_by( ecoregion, genus, genus_freq, avg_cover ) %>% 
  summarise( Species = paste( c(Species), collapse = ', ')) %>% 
  select( ecoregion, genus, genus_freq, avg_cover, Species) %>%
  ungroup() %>% 
  distinct() %>% 
  arrange( ecoregion, desc(genus_freq)) %>% 
  mutate( avg_cover = round( avg_cover , 2 )) %>% 
  write_csv('output/tables/AIM_common_tree_genera.csv')

# AIM Annual genera and species 
AIM_annuals <- AIM %>%
  filter( GrowthHabitSub != 'NONVASCULAR') %>%
  filter( !is.na(GrowthHabit), GrowthHabit != 'WOODY') %>% 
  mutate( Duration = ifelse( Species == 'ARPU9', "Perennial", Duration )) %>% 
  filter( GrowthHabit == 'NONWOODY', Duration == 'ANNUAL') %>% 
  left_join(ecoregion_n)

usda <- read_csv('data/AIM/usda_plants_annuals.csv')

AIM_annuals <- AIM_annuals %>% 
  left_join(usda, by = c('Species' = 'Accepted Symbol')) %>% 
  select( ecoregion, n_allots_total, uname, Species, nPrimaryKeys, avg_cover, frequency, exoticUS, GrowthHabitSub, Duration.x, 
          ScientificName) %>% 
  ungroup() %>% 
  mutate( exoticUS = ifelse( is.na(exoticUS), F, exoticUS ) ) %>% 
  separate( ScientificName, c('genus', 'spp'), sep = ' ')  

AIM_genus <- AIM_annuals %>% 
  group_by( ecoregion, genus, exoticUS, n_allots_total ) %>% 
  summarise( genus_frequency = n_distinct(uname), avg_cover = mean(avg_cover, na.rm = T)) %>% 
  arrange( ecoregion, desc(genus_frequency))

AIM_species <- AIM_annuals %>% 
  group_by( ecoregion, genus, spp, exoticUS, n_allots_total) %>%
  summarise( species_frequency = n_distinct( uname) ) %>% 
  arrange( ecoregion , desc(species_frequency))

AIM_genus %>% 
  group_by( ecoregion ) %>% 
  filter( row_number() < 4 ) %>% 
  left_join( AIM_species, by = c('ecoregion', 'genus', 'exoticUS', 'n_allots_total')) %>% 
  mutate( spp = paste0( str_sub(genus, 0, 1), '. ' , spp )) %>% 
  mutate( spp = paste0( spp, " (" , species_frequency, ")")) %>% 
  mutate( ecoregion = paste0( ecoregion, " (", n_allots_total, ")" ) ) %>% 
  mutate( genus_frequency = genus_frequency/n_allots_total) %>% 
  group_by( ecoregion, genus, exoticUS, n_allots_total, genus_frequency, avg_cover ) %>% 
  summarise( spp = paste( unique(spp), collapse = ', ')) %>% 
  ungroup() %>% 
  arrange( ecoregion, desc(genus_frequency)) %>% 
  mutate( genus_frequency = round( genus_frequency, 2 ) , avg_cover = round( avg_cover, 2)) %>% 
  select( ecoregion, genus, exoticUS, genus_frequency, avg_cover, spp ) %>% 
  write_csv('output/tables/AIM_common_annual_genera.csv')
