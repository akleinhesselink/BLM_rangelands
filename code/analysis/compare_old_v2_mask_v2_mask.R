library(tidyverse)

setwd(dir = '~/Dropbox/projects/BLM_rangeland_trends/data/RAP_EE_exports/')

cover_old <- read_csv('allotment_cover_by_year.csv') %>%
  filter( year == '1991')
cover_noMask <- read_csv('~/Downloads/drive-download-20220609T163831Z-001/allotment_cover_by_year_V2_no_Mask.csv')
cover_ReevesMask <- read_csv('~/Downloads/drive-download-20220609T163831Z-001/allotment_cover_by_year_ReevesMask.csv')
cover_NLCDMask <- read_csv('~/Downloads/drive-download-20220609T163831Z-001/allotment_cover_by_year_NLCDMask.csv')

prod_old <- read_csv('allotment_production_by_year.csv') %>% 
  filter( year == '1991')
prod_new <- read_csv('~/Downloads/drive-download-20220609T013346Z-001/allotment_production_by_year.csv')


cover_old %>% 
  left_join(cover_noMask, by = c('year', 'uname'))  %>% 
  filter( AFGC.x != AFGC.y) %>% 
  ggplot( aes( x = AFGC.x, y = AFGC.y)) + 
  geom_point() 

cover_old %>% 
  left_join(cover_ReevesMask, by = c('year', 'uname'))  %>% 
  filter( AFGC.x != AFGC.y) %>% 
  ggplot( aes( x = AFGC.x, y = AFGC.y)) + 
  geom_point() 

cover_old %>% 
  left_join(cover_NLCDMask, by = c('year', 'uname'))  %>% 
  filter( AFGC.x != AFGC.y) %>% 
  ggplot( aes( x = AFGC.x, y = AFGC.y)) + 
  geom_point() 




cover_old %>% 
  left_join(cover_noMask, by = c('year', 'uname'))  %>% 
  filter( TREE.x != TREE.y) %>% 
  ggplot( aes( x = TREE.x, y = TREE.y)) + 
  geom_point() 


cover_old %>% 
  left_join(cover_new, by = c('year', 'uname'))  %>% 
  filter( BG.x != BG.y) %>% 
  ggplot( aes( x = BG.x, y = BG.y)) + 
  geom_point() 

prod_old %>% 
  left_join( prod_new, by = c('year', 'uname')) %>% 
  select( afgAGB.x, afgAGB.y)

filter( afgAGB.x != afgAGB.y ) %>% 
  ggplot( aes( x = afgAGB.x, y = afgAGB.y)) + 
  geom_point() 
