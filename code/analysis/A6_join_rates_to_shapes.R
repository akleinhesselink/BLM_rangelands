rm(list = ls())
library(tidyverse)
library(sf)
library(lme4)
library(emmeans)

source('code/analysis/functions.R')
source('code/analysis/parameters.R')

# ------ Cover Models ---------- 
cover_trend_files <- dir(path = 'output', pattern = '.*_cover_group_trends.csv', full.names = T)
cover_trends <- lapply(cover_trend_files, read.csv)
types  <- c( str_extract( cover_trend_files, pattern = '[A-Z]+') )
types <- factor(types, labels = c('Annual', 'Bare', 'Perennial', 'Shrub', 'Tree'))
names( cover_trends ) <- types 

cover_trends <- do.call(rbind, cover_trends ) %>% 
  mutate( type = str_extract( rownames(.), "^[A-z]+")) %>% 
  mutate( measure = 'cover')


# Do the same for production trends
# ------------------------------------------------ # 
npp_trend_files <- dir(path = 'output', pattern = '.*_NPP_group_trends.csv', full.names = T)
npp_trends <- lapply(npp_trend_files, read.csv)
types  <- c( str_extract( npp_trend_files, pattern = '[A-Z]+') )
types <- factor(types, labels = c('Annual', 'Perennial'))
names( npp_trends ) <- types 

npp_trends <- do.call(rbind, npp_trends ) %>% 
  mutate( type = str_extract( rownames(.), "^[A-z]+")) %>% 
  mutate( measure = 'prod')

all_trends <- cover_trends  %>% 
  bind_rows(npp_trends) %>% 
  unite( new_type , c( type, measure) , sep  = ' ' )

all_trends <- all_trends %>% 
  select( uname, full_trend, new_type) %>% 
  pivot_wider( names_from = new_type, values_from= full_trend ) 

allotments <- read_sf('data/temp/allotments/allotments_clean.shp') %>% 
  left_join(all_trends, by = 'uname') 
  
write_sf(allotments, 'data/temp/allotments/allotments_with_trends.shp')

# join with BLM field offices 
offices <- read_sf('data/temp/BLM_field_offices_cleaned/field_offices.shp')


office_trends <- 
  cover_trends %>% 
  bind_rows(npp_trends) %>% 
  select( office_label, ecoregion,  ecoregion_trend, office_trend, type , measure ) %>% 
  mutate( office_full_trend = ecoregion_trend + office_trend ) %>% 
  unite( 'new_type', c(type, measure ), sep = ' ') %>%
  distinct() %>%
  select( ecoregion, office_label, new_type, office_full_trend)  %>% 
  pivot_wider(names_from = new_type, values_from = office_full_trend) %>% 
  mutate( office = str_extract( office_label, '[ A-Z-]+$')) %>% 
  ungroup() 


offices %>% 
  left_join( office_trends, by = c('ADMU_NA' = 'office') ) %>% 
  st_write('data/temp/BLM_field_offices_cleaned/field_offices_with_trends.shp', append = F)

read_sf('data/temp/BLM_field_offices_cleaned/field_offices_with_trends.shp')
  