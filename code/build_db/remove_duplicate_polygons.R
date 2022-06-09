rm(list = ls() )
library(sf)
library(tidyverse)

blm <- read_sf('~/Desktop/cleaned_step_one/cleaned_step_one.shp')

blm <- blm %>% 
  st_make_valid() %>%
  mutate( unique_allot = paste( ST_ALLOT, ALLOT_NAME, sep = '_')) 

centers <- blm %>% 
  select( fid, unique_allot ) %>% 
  st_centroid() %>% 
  mutate( unique_ctr = as.character( geometry) ) %>% 
  mutate( unique_ctr = as.numeric( factor(unique_ctr)) ) %>% 
  group_by( unique_ctr) %>% 
  mutate( duplicated = n_distinct(unique_allot)) %>%
  ungroup() 

duplicated <- centers %>% 
  filter( duplicated > 1 )

nonduplicated <- centers %>% 
  filter( duplicated == 1 )

pairs <- duplicated %>% st_drop_geometry()  %>% 
  left_join( nonduplicated %>% st_drop_geometry(), by = 'unique_allot')

pairs %>% 
  arrange( unique_ctr.x)

for( i in 1:nrow(pairs)){ 
  pairs$dist[i] <- st_distance(centers[ centers$fid == pairs$fid.x[i], ] , 
                               centers[ centers$fid == pairs$fid.y[i], ] )
}

keepers <- pairs %>% 
  group_by( unique_ctr.x ) %>% 
  arrange( unique_ctr.x, dist ) %>% 
  filter( row_number() == 1 ) %>%
  mutate( keep = T)

keepFID <- c(keepers$fid.x, nonduplicated$fid )

blm[ blm$fid %in% keepFID, ] %>% 
  select( unique_allot, ALLOT_NO, ALLOT_NAME, ADMIN_ST, ADM_OFC_CD, ADM_UNIT_C )  %>% 
  st_write('~/Desktop/clean_step_two/clean_step_two.shp')

