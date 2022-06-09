rm(list = ls())
library(sf)
library(tidyverse)

blm <- st_read('data/spatial/BLM_National_Grazing_Allotments_2022/gra.gdb/')
blm_clean <- st_read('~/Desktop/clean_step_three/clean_step_three.shp')

allotment_info <- blm %>% 
  st_drop_geometry() %>% 
  select( ALLOT_NO:ST_ALLOT, created_user,last_edited_date, - GIS_ACRES) %>% 
  distinct() %>% 
  mutate( unique_allot = paste( ST_ALLOT, ALLOT_NAME, sep = '_')) %>% 
  group_by( unique_allot) %>% 
  filter( last_edited_date == max(last_edited_date) ) 

blm_clean <- blm_clean %>% 
  group_by( unq_llt ) %>% 
  summarise( geometry = st_union(geometry)) %>% 
  st_make_valid()

blm_clean <- blm_clean %>% 
  rename( unique_allot = unq_llt ) %>%
  mutate( uname = as.numeric( factor(unique_allot))) %>% 
  separate( unique_allot, c('ST_ALLOT', 'ALLOT_NAME'), sep = '_', remove = F) %>% 
  select( uname, ST_ALLOT, ALLOT_NAME, unique_allot)

join_test <- blm_clean %>% 
  left_join( allotment_info, by = "unique_allot") 

join_test %>% 
  filter( ST_ALLOT.x != ST_ALLOT.y) 
join_test %>% filter( ALLOT_NAME.x != ALLOT_NAME.y)


blm_clean <- blm_clean %>% 
  select( uname, unique_allot ) %>% 
  left_join( allotment_info, by = 'unique_allot') 

stopifnot( length( unique( blm_clean$uname )) == nrow(blm_clean) )

blm_clean %>% 
  write_rds('data/temp/allotments_cleaned.rds')

