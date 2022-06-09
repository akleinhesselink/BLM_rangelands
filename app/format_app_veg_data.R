# Get data for app 
rm(list= ls())
library(tidyverse)
library(sf)

veg <- readRDS('data/temp/annual_data.rds') %>% 
  rename( "type" = name )

allotment_info  <- read_csv('data/temp/allotment_info.csv')

veg <- veg %>% 
  left_join(allotment_info, by = 'uname') %>% 
  mutate( "office_label" = admu_name, 'district_label' = parent_name) %>% 
  select( uname, year, type , value, unit, allot_name, 
          admin_st, parent_cd, parent_name, admu_name, ecogroup, hectares, district_label, office_label)


cover_rates <- read_sf('data/temp/cover_rates/allotments.shp')
prod_rates <- read_sf('data/temp/agb_rates/allotments_with_rates.shp')

stopifnot( all( cover_rates$uname == prod_rates$uname ) )
allotments <- cover_rates %>% sf::st_drop_geometry() 

last_year <- max(veg$year)
first_year <- min(veg$year)

veg <- veg %>% 
  filter( type != 'LTR') %>% 
  #filter( type != 'herbaceousAGB') %>% 
  filter( type != 'WOODY') %>%
  mutate( type_label =  factor(type )) %>% 
  mutate( type_label = factor( type_label, 
          labels = c('Annual', 'Annual', 'Bare Ground', "Herbaceous",
                     'Perennial', 'Perennial', 'Shrub', 'Tree'))) 

ecoregion_veg <- veg %>% 
  group_by( ecogroup, year, type_label, unit ) %>% 
  summarise( median = median( value, na.rm = T), 
             uq = quantile(value, 0.75, na.rm = T), 
             lq= quantile(value, 0.25, na.rm = T))

district_veg <- veg %>% 
  group_by( district_label, year, type_label, unit) %>% 
  summarise( median = median( value, na.rm = T), 
             uq = quantile(value, 0.75, na.rm  = T), 
             lq= quantile(value, 0.25, na.rm = T))

field_office_veg <- veg %>% 
  group_by( office_label, year, type_label, unit) %>% 
  summarise( median = median( value, na.rm = T), 
             uq = quantile(value, 0.75, na.rm = T), 
             lq= quantile(value, 0.25, na.rm = T))

veg <- veg %>% 
  select( uname, allot_name, office_label, district_label, admin_st, 
          ecogroup, year, type_label, unit, value, parent_cd, 
          parent_name, admu_name, hectares)

veg_rates <- cover_rates %>% 
  rename( 'Annual_Cover_Trend' = Annual, 
          'Perennial_Cover_Trend' = Perennial, 
          'Bare_Cover_Trend' = Bare, 
          'Shrub_Cover_Trend' = Shrub, 
          'Tree_Cover_Trend' = Tree) %>% 
  select( - Woody ) %>% 
  left_join(prod_rates %>% 
              st_drop_geometry() %>% 
              select( uname, AFG, HERB, PFG) %>% 
              rename( 'Annual_Production_Trend' = AFG, 
                      'Perennial_Production_Trend' = PFG, 
                      'Herbaceous_Production_Trend' = HERB) ) %>% 
  left_join( 
    veg %>% distinct( uname, allot_name, admin_st, district_label, office_label))  %>% 
  select( uname, allot_name, ALLOT_NAME, office_label, district_label, admin_st, ecogroup, Annual_Cover_Trend:Tree_Cover_Trend, Annual_Production_Trend:Perennial_Production_Trend, geometry) 

veg_rates[ is.na( veg_rates$allot_name == veg_rates$ALLOT_NAME ),  ]  # missing rates for Marine West Coast allotments 

veg_rates <- veg_rates %>% 
  filter( !is.na(allot_name))

stopifnot( all(veg_rates$allot_name == veg_rates$ALLOT_NAME) )

veg_rates %>%
  select( -ALLOT_NAME) %>% 
  st_make_valid() %>%
  write_rds( 'app/data/allotment_trends.rds')

veg %>% 
  select( uname:ecogroup, parent_cd, parent_name, admu_name, hectares ) %>% 
  distinct() %>% 
  write_csv('app/data/allotment_info.csv')

veg %>% 
  filter( year > 1990 ) %>% 
  distinct( uname, year, type_label, unit, value ) %>%
  write_csv(file = 'app/data/vegdata.csv')

ecoregion_veg %>% 
  write_csv('app/data/ecoregion_veg.csv')

district_veg %>% 
  write_csv('app/data/district_veg.csv')

field_office_veg %>% 
  write_csv('app/data/field_office_veg.csv')


read_csv('app/data/vegdata.csv') %>% head 

u <- sample( c( 1:max(veg$uname)), size = 100 )

res <- veg %>% 
  filter( uname %in% u ) %>% 
  group_by(type_label, unit, uname ) %>% 
  filter( value > 0.5 ) %>% 
  filter( year > 1990 , year < 2020 ) %>% 
  summarise( m = list( lm( log( value) ~ year ))) 

res$year_effect = unlist( lapply( res$m, function (m ) coef(m)[2] )  )

res <- res %>% 
  unite( col = 'type', c( type_label, unit)) %>% 
  select( uname, type, year_effect ) %>% 
  pivot_wider( names_from = type, values_from = year_effect ) 

veg_rates %>% 
  filter( uname %in% u ) %>% 
  left_join(res , by = c('uname') ) %>% 
  select( `Bare Ground_cover`, Bare_Cover_Trend) %>% 
  ggplot( aes( x = `Bare Ground_cover`, y = Bare_Cover_Trend)) + 
  geom_point() + 
  geom_abline( aes( intercept = 0, slope = 1 ))


m_test <- read_rds( 'output/BG_cover_trend_model.rds')

lm_models <- m_test@frame %>% 
  group_by(uname) %>% 
  summarise( m = list( lm( value2 ~ year2 )))

lm_models <- data.frame( uname = lm_models$uname,  trend = lapply(lm_models$m, function(m) coef(m)[2] ) %>% unlist() )
library(lme4)

r <- ranef(m_test)
f <- emtrends(m_test, ~ ecogroup, var = 'year2' ) %>% data.frame() 

runame <- r$uname %>%
  mutate( uname = as.numeric( row.names( .)))  

roffice <- r$`ecogroup:office_label` %>%
  mutate( office_label = row.names(.)) %>%
  separate( office_label , into = c('ecogroup', 'office_label'), sep = ':') 

scales <- read_csv('data/temp/trend_scales.csv')

out <- veg_rates %>% filter( uname %in% u) %>% 
  left_join( runame , by = 'uname')  %>% 
  left_join( roffice, by = c('ecogroup', 'office_label'))  %>% 
  left_join(f) %>%  
  left_join(lm_models, by = 'uname') %>% 
  mutate( full_trend = year2.x + year2.y + year2.trend) %>% 
  mutate( full_trend_bt = full_trend*scales$trend_unit[scales$type == 'Bare']) 


out %>% ggplot( aes( Bare_Cover_Trend, full_trend_bt)) + 
  geom_point() 

out %>% ggplot( aes( Bare_Cover_Trend, full_trend)) + 
  geom_point() 

out %>% ggplot( aes( Bare_Cover_Trend, trend)) + 
  geom_point() 
