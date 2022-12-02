rm(list = ls())
library(tidyverse)
unloadNamespace('raster')
unloadNamespace('papeR')

source('code/analysis/functions.R')
source('code/analysis/parameters.R')

allotments <- read_csv('data/temp/allotment_info.csv') %>% 
  rename( elevation = elevation ) %>%
  filter( ecoregion != 'Marine West Coast Forest')

# ---------------------------- # 
annual_data <- 
  read_csv('data/temp/annual_data.csv') %>% 
  rename( type = name )  %>%
  filter( year  > 1990  & year < 2021)

year <- annual_data %>% distinct(year ) %>% pull( year )
type <- annual_data %>% distinct( type ) %>% pull(type)
uname <- allotments %>% distinct( uname )  %>% pull( uname)

units <- 
  annual_data %>%
  distinct( type, unit )

annual_data <-
  expand.grid( uname = uname, year =  year, type = type) %>%
  left_join( annual_data , by = c('uname', 'year', 'type'))  %>% 
  select( - unit ) %>% 
  left_join( units )  %>% 
  ungroup() %>%
  mutate( t = year - min(year))

# ------------------------ # 
# Cover: 
annual_data %>%
  group_by( type, uname ) %>%
  filter(value > 0 ) %>%
  summarise( nyears = n_distinct(year )) %>%
  #filter( nyears < 30 ) %>%
  group_by( type, nyears) %>%
  summarise( n()) 

annual_data %>%
  group_by( type, uname ) %>%
  filter(value > 0 , !is.na(value)) %>%
  mutate( nyears = n_distinct( year )) %>%
  group_by(type, uname) %>%
  mutate( above_thresh = min(value) > 0.25 ) %>%
  group_by( type, above_thresh) %>%
  summarise( n_distinct(uname) )  

cover <- 
  annual_data %>%  
  filter( unit == 'cover') %>% 
  filter( !is.na(value)) %>% 
  group_by( type, uname) %>% 
  filter( n() == 30 ) %>% 
  filter( min(value) > 0.25 ) %>%
  ungroup() %>% 
  left_join( allotments) %>% 
  filter( !is.na(ecoregion ) ) %>% 
  group_by( ecoregion, OFFICE, unit, type) %>% 
  filter( n_distinct(uname) > 1 ) %>% 
  ungroup() %>% 
  group_by( type, unit ) %>% 
    split(f = .$type ) 

cover0.01 <- 
  annual_data %>%  
  filter( unit == 'cover') %>% 
  filter( !is.na(value)) %>% 
  mutate( value = value + 0.01 ) %>% 
  group_by( type, uname) %>% 
  filter( n() == 30 ) %>% 
  ungroup() %>% 
  left_join( allotments) %>% 
  filter( !is.na(ecoregion ) ) %>% 
  group_by( ecoregion, OFFICE, unit, type) %>% 
  filter( n_distinct(uname) > 1 ) %>% 
  ungroup() %>% 
  group_by( type, unit ) %>% 
  split(f = .$type ) 

cover <- cover %>% lapply( 
  function(x) { 
    x %>% mutate( 
      log_value = log(value), 
      value2 = scale(log(value))) %>%
      mutate( year2 = scale(year), 
              hectares2 = scale(hectares, center = F)) }) 

cover0.01<- cover0.01  %>% lapply( 
  function(x) { 
    x %>% mutate( 
      log_value = log(value), 
      value2 = scale(log(value))) %>%
      mutate( year2 = scale(year), 
              hectares2 = scale(hectares, center = F)) }) 

save(cover, file = 'data/analysis_data/cover.rda')
save(cover0.01, file = 'data/analysis_data/cover01.rda')

rm(cover, cover0.01) 

prod <- annual_data %>% 
  filter( unit == 'production') %>% 
  filter( value > 0 ) %>% 
  filter( !is.na(value)) %>% 
  group_by( type, uname) %>% 
  filter( n() == 30 ) %>% 
  filter( min(value, na.rm = T) > 0.25 ) %>%
  ungroup() %>% 
  left_join( allotments) %>% 
  filter( !is.na(ecoregion)) %>%
  group_by( ecoregion, OFFICE, unit, type) %>% 
  filter( n_distinct(uname) > 1 ) %>% 
  ungroup() %>% 
  split(f = .$type ) 

prod0.01 <- annual_data %>% 
  filter( unit == 'production') %>% 
  filter( !is.na(value)) %>% 
  mutate( value = value + 0.01) %>% 
  group_by( type, uname) %>% 
  filter( n() == 30 ) %>% 
  ungroup() %>% 
  left_join( allotments) %>% 
  filter( !is.na(ecoregion)) %>%
  group_by( ecoregion, OFFICE, unit, type) %>% 
  filter( n_distinct(uname) > 1 ) %>% 
  ungroup() %>% 
  split(f = .$type ) 

prod <- 
  prod %>% 
  lapply( function(x){ 
    x %>% 
      mutate( value2 = scale(log(value)), 
              log_value = log(value)) %>% 
      mutate( year2 = scale(year), 
              hectares2 = scale(hectares, center = F ))
  }) 

prod0.01 <- 
  prod0.01 %>% 
  lapply( function(x){ 
    x %>% 
      mutate( value2 = scale(log(value)), 
              log_value = log(value)) %>% 
      mutate( year2 = scale(year), 
              hectares2 = scale(hectares, center = F ))
  }) 

save(prod, file = 'data/analysis_data/prod.rda')

save(prod0.01, file = 'data/analysis_data/prod01.rda')


rm(prod, prod0.01, allotments)
