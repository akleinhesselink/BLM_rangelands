rm(list = ls())
library(tidyverse)

SpeciesState <- read_csv('data/AIM_speciesState_table.csv')


formatChars <- function(x) {
  str_remove(
    str_trim(
      str_to_upper(
        str_extract(x,'[0-9A-Za-z-]+'))),
    '-')
}

SpeciesState <- SpeciesState %>% 
  mutate_at( .vars = c('SpeciesCode', 
                       'GrowthHabit', 
                       'GrowthHabitSub', 
                       'Duration'), 
             formatChars )  %>%
  mutate( GrowthHabitSub = ifelse (GrowthHabitSub == 'SHRUBSHRUB', 'SHRUB', GrowthHabitSub)) %>% 
  mutate( GrowthHabitSub = ifelse( GrowthHabitSub == 'FORBHERB', 'FORB', GrowthHabitSub))  %>% 
  mutate( GrowthHabitSub = ifelse( GrowthHabitSub == 'GRASS', 'GRAMINOID', GrowthHabitSub))  


SpeciesState <- SpeciesState %>% 
  distinct( SpeciesCode, 
            ScientificName, 
            SpeciesState, 
            GrowthHabit, 
            GrowthHabitSub, 
            Duration, 
            Noxious)

write_csv(SpeciesState, file = 'data/temp/clean_speciesState_table.csv')
