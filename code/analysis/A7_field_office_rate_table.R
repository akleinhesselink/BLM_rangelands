rm(list = ls())
unloadNamespace(ns = 'kableExtra')
library(tidyverse)
library(lme4)
library(emmeans)

source('code/analysis/functions.R')
source('code/analysis/parameters.R')

trend_scales <- read_csv('data/temp/trend_scales.csv')

# ------ 
cover_model_files <- dir(path = 'output', pattern = '.*_cover_trend_model.rds', full.names = T)
cover_models <- lapply(cover_model_files, read_rds)
types  <- c( str_extract( cover_model_files, pattern = '[A-Z]+') )
types <- factor(types, labels = c('Annual', 'Bare', 'Perennial', 'Shrub', 'Tree'))
names( cover_models ) <- types 

cover_att <- lapply( cover_models, function(x) attributes( x@frame$value2) )
cover_year_att <- lapply( cover_models, function(x) attributes( x@frame$year2) )

pred_grid <- lapply( types , function( x ) { 
  m <- cover_models[[x]]
  m@frame %>% 
    mutate( type = x, unit = 'Cover') %>% 
    filter( year2 == max(year2) | year2 == min(year2)) %>% 
    distinct(unit, type, year2, ecoregion, OFFICE) %>% 
    arrange(ecoregion, OFFICE, year2) %>% 
    mutate( yhat = predict( m , newdata = . , re.form = ~ (year2 | ecoregion:OFFICE))) %>% 
    group_by( unit, type, ecoregion, OFFICE ) %>% 
    summarise( rate = (yhat[which.max(year2)] - yhat[which.min(year2)])/(max(year2) - min(year2)))
})

cover_trends <- do.call(rbind, pred_grid) %>% 
  left_join(trend_scales, by = c('unit', 'type')) %>% 
  mutate( bt_trend = rate*trend_unit ) %>% 
  select(unit, type, ecoregion, OFFICE, rate, bt_trend ) %>% 
  select( -rate ) %>% 
  unite( c(type, unit), col = 'type') %>% 
  pivot_wider( id_cols = c('OFFICE', 'ecoregion'), names_from = 'type', values_from = 'bt_trend')


# Production Trends
npp_model_files <- dir(path = 'output', pattern = '.*_NPP_trend_model.rds', full.names = T)
npp_models <- lapply(npp_model_files, read_rds)
types  <- c( str_extract( npp_model_files, pattern = '[A-Z]+') )
types <- factor(types, labels = c('Annual', 'Perennial'))
names( npp_models ) <- types 

npp_att <- lapply( npp_models, function(x) attributes( x@frame$value2) )
npp_year_att <- lapply( npp_models, function(x) attributes( x@frame$year2) )


pred_grid <- lapply( types , function( x ) { 
  m <- npp_models[[x]]
  m@frame %>% 
    mutate( type = x, unit = 'AGB' ) %>% 
    filter( year2 == max(year2) | year2 == min(year2)) %>% 
    distinct(unit, type, year2, ecoregion, OFFICE) %>% 
    arrange(ecoregion, OFFICE, year2) %>% 
    mutate( yhat = predict( m , newdata = . , re.form = ~ (year2 | ecoregion:OFFICE))) %>% 
    group_by( unit, type, ecoregion, OFFICE ) %>% 
    summarise( rate = (yhat[which.max(year2)] - yhat[which.min(year2)])/(max(year2) - min(year2)))
})

npp_trends <- do.call(rbind, pred_grid) %>% 
  left_join(trend_scales, by = c('unit', 'type')) %>% 
  mutate( bt_trend = rate*trend_unit ) %>% 
  select(unit, type, ecoregion, OFFICE, rate, bt_trend ) %>% 
  select( -rate ) %>% 
  unite( c(type, unit), col = 'type') %>% 
  pivot_wider( id_cols = c('OFFICE', 'ecoregion'), names_from = 'type', values_from = 'bt_trend')

office_trends <- cover_trends %>%
  left_join(npp_trends, by = c('OFFICE', 'ecoregion'))

read_csv('data/temp/allotment_info.csv') %>%
  distinct( ecoregion, ADMIN_ST, DISTRICT, OFFICE ) %>% 
  left_join( office_trends) %>% 
  arrange( ecoregion, ADMIN_ST, DISTRICT, OFFICE ) %>%  
  write_csv('output/tables/field_office_rates.csv')




