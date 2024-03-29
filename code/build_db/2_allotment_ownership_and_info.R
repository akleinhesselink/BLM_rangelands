rm(list = ls() )
library(tidyverse)
library(sf)

if(!file.exists('data/RAP_EE_exports/ownership_area_by_allotment.csv')){ 
  print( "Ownership data missing. \n
  Complete Ownership Analysis in GEE first!")
}

allotment_info <- 
  read_csv(file = 'data/temp/allotment_info.csv') %>% 
  mutate( hectares = as.numeric(hectares))

allotment_ownership <- 
  read_csv(file = 'data/RAP_EE_exports/ownership_area_by_allotment.csv')

allotment_ownership <- allotment_ownership %>% 
  pivot_longer(`3`:`2000`, 'owner', values_to = 'hectares' ) %>% 
  mutate( hectares = replace_na(hectares, 0)) %>% 
  mutate( PADUS_class = as.numeric(owner)) %>% 
  mutate( owner = factor(PADUS_class, labels = c('State', 'Private', 'Private Conservation', 
                                   'Forest Service', 'BLM', 'other')))  %>% 
  mutate( simple_class = factor(PADUS_class)) 

levels(allotment_ownership$simple_class) <- 
  list('Other' = c(3, 11, 2000), 
          'Private' = c(6,7), 
          'BLM' = 21) 

allotment_ownership <- 
  allotment_ownership %>% 
  select( uname, simple_class, hectares ) %>% 
  group_by( uname, simple_class ) %>% 
  summarise( hectares = sum(hectares)) %>%
  spread( simple_class, hectares ) %>% 
  rowwise() %>% 
  mutate( total_unmasked = Other + Private + BLM) 

allotment_info <- 
  allotment_info %>% 
  left_join( allotment_ownership) %>% 
  mutate( total = hectares ) %>% 
  mutate( masked = total - total_unmasked )

allotment_info %>% 
  write_csv('data/temp/allotment_info.csv')

