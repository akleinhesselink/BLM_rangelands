rm(list = ls())
library(tidyverse)
library(sf)
library(lme4)
library(emmeans)

source('code/analysis/functions.R')
source('code/analysis/parameters.R')

# ------ 
cover_model_files <- dir(path = 'output', pattern = '.*_cover_trend_model.rds', full.names = T)
cover_models <- lapply(cover_model_files, read_rds)

types  <- c( str_extract( cover_model_files, pattern = '[A-Z]+') )
types <- factor(types, labels = c('Annual', 'Bare', 'Perennial', 'Shrub', 'Tree'))
names( cover_models ) <- types 

cover_att <- lapply( cover_models, function(x) attributes( x@frame$value2) )
cover_year_att <- lapply( cover_models, function(x) attributes( x@frame$year2) )

# Now back transform sd into units of log cover/log production per year
# trend_bt = trend_scaled*log_y_sd/year_sd 
trend_scales <- 
  unlist( cover_att, recursive = F ) %>% 
  data.frame() %>% 
  head( 1 ) %>%
  pivot_longer( Annual.dim:Tree.scaled.scale , 
                names_to = 'type', values_to = 'value') %>%
  separate( type, into = c('type', 'stat', 'par'))  %>% 
  filter( par == 'scale' ) %>%
  select( type, par, value ) %>% 
  mutate( unit = 'Cover', par2 = 'log_y_sd') %>% 
  bind_rows(
    unlist( cover_year_att, recursive = F ) %>% 
      data.frame() %>% 
      head(1) %>%
      pivot_longer( Annual.dim:Tree.scaled.scale , 
                    names_to = 'type', values_to = 'value') %>%
      separate( type, into = c('type', 'stat', 'par'))  %>% 
      filter( par == 'scale' ) %>% 
      select( type, par, value ) %>% 
      mutate( unit = 'Cover', par2 = 'year_sd')
  ) 

trend_scales <- 
  trend_scales %>% 
  pivot_wider( names_from = par2 , values_from = value) %>%
  mutate( trend_unit = log_y_sd/year_sd)

cover_trend_scales <- trend_scales

# Predict and get rates 
pred_grid <- lapply( types , function( x ) { 
  m <- cover_models[[x]]
  m@frame %>% 
    mutate( type = x ) %>% 
    filter( year2 == max(year2) | year2 == min(year2)) %>% 
    arrange(uname, year2) %>% 
    mutate( yhat = predict( m , newdata = . )) %>% 
    group_by( type, uname ) %>% 
    dplyr::summarise( rate = (yhat[which.max(year2)] - yhat[which.min(year2)])/(max(year2) - min(year2)))
  })


trends <- do.call(rbind, pred_grid) %>% 
  ungroup() %>%
  left_join(trend_scales, by = 'type') %>% 
  mutate( bt_trend = rate*trend_unit ) %>% 
  select(type, uname, rate, bt_trend )

# Sanity check on back transformation 
# rf <- ranef( cover_models$Annual )
# rf$uname$uname <- as.numeric( row.names( rf$uname) )
# rf$`ecoregion:OFFICE`$`ecoregion:OFFICE` <- row.names( rf$`ecoregion:OFFICE` )
# rf$`ecoregion:OFFICE` <- rf$`ecoregion:OFFICE` %>%
#   separate( `ecoregion:OFFICE`, sep = ':', into = c('ecoregion', 'OFFICE'))
# 
# ff <- emtrends(cover_models$Annual, ~ ecoregion, 'year2') %>% as.data.frame()
# 
# df <- cover_models$Annual@frame
# 
# test_df <- df %>%
#   distinct( ecoregion, OFFICE, uname ) %>%
#   left_join( rf$uname, by = 'uname') %>%
#   left_join( rf$`ecoregion:OFFICE`, by = c('ecoregion', 'OFFICE')) %>%
#   left_join(ff , by = 'ecoregion') %>%
#   select(ecoregion, uname, OFFICE, starts_with('(Inter'), starts_with('year2')) %>%
#   mutate( full_trend = year2.x  + year2.y + year2.trend) %>%
#   mutate( type = 'Annual', unit = 'Cover') %>%
#   left_join(trend_scales, by = c('type', 'unit')) %>%
#   mutate( trend_unit = trend_scales$trend_unit[trend_scales$type == 'Annual']) %>%
#   mutate( bt_trend = full_trend*trend_unit)
# 
# test_equiv <- trends %>%
#   left_join(test_df, by = c('type', 'uname')) %>%
#   select( type, uname, rate, full_trend, bt_trend.x, bt_trend.y)
# 
# plot( test_equiv$bt_trend.x , test_equiv$bt_trend.y )
# abline(0,1)

#  test against true values
# load('data/analysis_data/cover.rda')
# load('data/analysis_data/allotments.rda')
# 
# cover_att$Annual
# cover_year_att$Annual
# 
# cover_models$Annual@frame %>%
#   distinct( ecoregion, uname) %>%
#   group_by( ecoregion) %>%
#   sample_n(2 ) %>%
#   select( uname ) %>%
#   left_join( cover$AFGC ) %>%
#   ungroup() %>% 
#   mutate(yhat2 = predict( cover_models$Annual, newdata = . )) %>%
#   select( ecoregion, uname, year, year2, value, value2, yhat2 ) %>%
#   mutate( yhat = exp(yhat2*cover_att$Annual$`scaled:scale` + cover_att$Annual$`scaled:center`) ) %>%
#   left_join(trends %>% filter( type == 'Annual'))  %>% 
#   left_join( test_equiv %>% select(type, uname, bt_trend.x, bt_trend.y), by = c('uname', 'type')) %>% 
#   group_by(ecoregion,  uname) %>%
#   mutate( yhat_test = yhat[which.min(year)]*exp(bt_trend*(year-min(year))) - 1 ) %>%
#   mutate( yhat_test.x = yhat[which.min(year)]*exp(bt_trend.x*(year-min(year)))) %>%
#   mutate( yhat_test.y = yhat[which.min(year)]*exp(bt_trend.y*(year-min(year)))) %>%
#   ggplot( aes( x = year, y = value, group = uname) ) +
#   geom_line() +
#   geom_line( aes( y = yhat, group = uname), color = 'black') +
#   geom_line( aes( y = yhat_test, group = uname), color = 'blue', linetype = 2) +
#   #geom_line( aes( y = yhat_test.x, group = uname), color = 'orange', linetype = 2) +
#   geom_line( aes( y = yhat_test.y, group = uname), color = 'red', linetype = 1) +
#   facet_wrap( ~ ecoregion )

# 

allotment_shp <- sf::read_sf('data/temp/allotments/allotments_clean.shp')

allotment_rates <- allotment_shp %>% 
  left_join(
  trends %>%
    ungroup() %>%
    select( - rate ) %>%
    pivot_wider( names_from = type, values_from = bt_trend )
) 

allotment_rates %>% 
  st_write(dsn = 'data/temp/allotments/allotments_with_cover_trends.shp', 
            layer = 'allotments', 
            append = F)

# Do the same for production trends
# ------------------------------------------------ # 
npp_model_files <- dir(path = 'output', pattern = '.*_NPP_trend_model.rds', full.names = T)
npp_models <- lapply(npp_model_files, read_rds)
types  <- c( str_extract( npp_model_files, pattern = '[A-Z]+') )
types <- factor(types, labels = c('Annual', 'Perennial'))
names( npp_models ) <- types 

npp_att <- lapply( npp_models, function(x) attributes( x@frame$value2) )
npp_year_att <- lapply( npp_models, function(x) attributes( x@frame$year2) )

# Now back transform sd into units of log npp/log production per year
# trend_bt = trend_scaled*log_y_sd/year_sd 

trend_scales <- 
  unlist( npp_att, recursive = F ) %>% 
  data.frame() %>% 
  head( 1 ) %>%
  pivot_longer( Annual.dim:Perennial.scaled.scale , 
                names_to = 'type', values_to = 'value') %>% 
  separate( type, into = c('type', 'stat', 'par'))  %>% 
  filter( par == 'scale' ) %>%
  select( type, par, value ) %>% 
  mutate( unit = 'NPP', par2 = 'log_y_sd') %>% 
  bind_rows(
    unlist( npp_year_att, recursive = F ) %>% 
      data.frame() %>% 
      head(1) %>%
      pivot_longer( Annual.dim:Perennial.scaled.scale , 
                    names_to = 'type', values_to = 'value') %>%
      separate( type, into = c('type', 'stat', 'par'))  %>% 
      filter( par == 'scale' ) %>% 
      select( type, par, value ) %>% 
      mutate( unit = 'NPP', par2 = 'year_sd')
  ) 

trend_scales <- 
  trend_scales %>% 
  pivot_wider( names_from = par2 , values_from = value) %>%
  mutate( trend_unit = log_y_sd/year_sd)

cover_trend_scales %>% 
  bind_rows(trend_scales) %>% 
  write_csv('data/temp/trend_scales.csv')

# Predict and get rates 

pred_grid <- lapply( types , function( x ) { 
  m <- npp_models[[x]]
  m@frame %>% 
    mutate( type = x ) %>% 
    filter( year2 == max(year2) | year2 == min(year2)) %>% 
    arrange(uname, year2) %>% 
    mutate( yhat = predict( m , newdata = . )) %>% 
    group_by( type, uname ) %>% 
    dplyr::summarise( rate = (yhat[which.max(year2)] - yhat[which.min(year2)])/(max(year2) - min(year2)))
})


trends <- do.call(rbind, pred_grid) %>% 
  left_join(trend_scales, by = 'type') %>% 
  mutate( bt_trend = rate*trend_unit ) %>% 
  select( uname, rate, bt_trend )

allotment_shp <- sf::read_sf('data/temp/allotments/allotments_clean.shp')

allotment_rates <- allotment_shp %>% 
  left_join(
    trends %>%
      ungroup() %>%
      select( - rate ) %>%
      mutate( type = factor(type)) %>% 
      mutate( type = factor( type, labels = c('AFG', 'PFG'))) %>% 
      pivot_wider( names_from = type, values_from = bt_trend )
  ) 

allotment_rates %>% 
  st_write(dsn = 'data/temp/allotments/allotments_with_npp_trends.shp', 
           layer = 'allotments', append = F)
 

# Sanity Check on OFFICE Rates -------------- # 
rf <- ranef( npp_models$Annual )
rf$uname$uname <- as.numeric( row.names( rf$uname) )
rf$`ecoregion:OFFICE`$`ecoregion:OFFICE` <- row.names( rf$`ecoregion:OFFICE` )
rf$`ecoregion:OFFICE` <- rf$`ecoregion:OFFICE` %>%
  separate( `ecoregion:OFFICE`, sep = ':', into = c('ecoregion', 'OFFICE'))

ff <- emtrends(npp_models$Annual, ~ ecoregion, 'year2') %>% as.data.frame()

df <- npp_models$Annual@frame

test_df <- df %>%
  distinct( ecoregion, OFFICE, uname ) %>%
  left_join( rf$uname, by = 'uname') %>%
  left_join( rf$`ecoregion:OFFICE`, by = c('ecoregion', 'OFFICE')) %>%
  left_join(ff , by = 'ecoregion') %>%
  select(ecoregion, uname, OFFICE, starts_with('(Inter'), starts_with('year2')) %>%
  mutate( full_trend = year2.x  + year2.y + year2.trend) %>%
  mutate( type = 'Annual', unit = 'NPP') %>%
  left_join(trend_scales, by = c('type', 'unit')) %>%
  mutate( trend_unit = trend_scales$trend_unit[trend_scales$type == 'Annual']) %>%
  mutate( bt_trend = full_trend*trend_unit)

test_equiv <- trends %>%
  left_join(test_df, by = c('type', 'uname')) %>%
  select( type, uname, rate, full_trend, bt_trend.x, bt_trend.y)

plot( test_equiv$bt_trend.x , test_equiv$bt_trend.y )
abline(0,1)

#  test against true values ------ 
# load('data/analysis_data/npp.rda')
# load('data/analysis_data/allotments.rda')
# 
# npp_att$Annual
# npp_year_att$Annual
# 
# npp_models$Annual@frame %>%
#   distinct( ecoregion, uname) %>%
#   group_by( ecoregion) %>%
#   sample_n(2 ) %>%
#   select( uname ) %>%
#   left_join( npp$afgAGB ) %>%
#   ungroup() %>%
#   mutate(yhat2 = predict( npp_models$Annual, newdata = . )) %>%
#   select( ecoregion, uname, year, year2, value, value2, yhat2 ) %>%
#   mutate( yhat = exp(yhat2*npp_att$Annual$`scaled:scale` + npp_att$Annual$`scaled:center`) ) %>%
#   left_join(trends %>% filter( type == 'Annual')) %>%
#   left_join( test_equiv %>% select( uname, bt_trend.x, bt_trend.y), by = c('uname', 'type')) %>%
#   group_by(ecoregion,  uname) %>%
#   mutate( yhat_test = yhat[which.min(year)]*exp(bt_trend*(year-min(year))) -5 ) %>%
#   mutate( yhat_test.x = yhat[which.min(year)]*exp(bt_trend.x*(year-min(year)))) %>%
#   mutate( yhat_test.y = yhat[which.min(year)]*exp(bt_trend.y*(year-min(year)))) %>%
#   ggplot( aes( x = year, y = value, group = uname) ) +
#   geom_line() +
#   geom_line( aes( y = yhat, group = uname), color = 'black') +
#   #geom_line( aes( y = yhat_test, group = uname), color = 'blue', linetype = 2) +
#   geom_line( aes( y = yhat_test.x, group = uname), color = 'orange', linetype = 2) +
#   #geom_line( aes( y = yhat_test.y, group = uname), color = 'red', linetype = 1) +
#   facet_wrap( ~ ecoregion , scales = 'free_y') 
# 
# 
# 

