rm(list = ls())
unloadNamespace(ns = 'kableExtra')
library(tidyverse)
library(lme4)
library(emmeans)

source('code/analysis/functions.R')
source('code/analysis/parameters.R')

# ------ 
files <- dir( 'output', '*_(cover|NPP)_group_trends.csv', full.names = T)
type <- str_extract( basename(files), pattern = '[A-Z]+')
unit <- str_extract( basename(files), pattern = 'cover|agb|NPP')
out <- list() 

for( i in 1:length(files)){ 
  out[[i]] <- 
    read_csv(files[i]) %>% 
    distinct( ecoregion, office_label, ecoregion_trend, office_trend ) %>% 
    mutate( full_office_trend = ecoregion_trend + office_trend ) %>% 
    mutate( type = type[i]) %>% 
    mutate( unit = unit[i]) %>% 
    mutate( Office = str_extract( office_label, '[ A-Z-]+$'))
}

all_field_office_rates <- do.call(rbind, out ) 

all_field_office_rates %>% 
  select( type, unit, ecoregion, Office, office_label, ecoregion_trend, office_trend, full_office_trend) %>% 
  arrange( type, unit, ecoregion, Office ) %>% 
  write_csv('output/all_field_office_rates.csv')



# Plot Mediterranean California PFG  
allotments <- read_csv('data/temp/allotment_info.csv') %>% 
  filter( ecoregion == 'Mediterranean California')

pfg_dat <- read_csv('data/temp/annual_data.csv') %>%  
  filter( unit == 'cover', name == 'PFG')  %>%
  left_join(allotments) %>% 
  filter( !is.na( ecoregion ))

pfg_dat %>% 
  ggplot( aes( x = year, y = value )) + 
  geom_line( aes( group = uname ), alpha = 0.1) + 
  stat_summary( aes( group = OFFICE ), geom = 'line', fun = 'mean') + 
  geom_smooth(se = F, method = 'lm') + 
  geom_vline(aes(xintercept = 1991)) + 
  facet_wrap( ~ OFFICE) 

pfg_dat %>% 
  filter( year > 1990 ) %>%
  filter( year < 2021) %>% 
  mutate( decade = cut( year, c('1990', '2001', '2011', '2021'), labels = c('90s', '00s', '10s'))) %>% 
  group_by( OFFICE, decade ) %>% 
  summarise( avg_cover = mean (value ), sd_cover = sd(value ) ) %>% 
  ggplot( aes( x = OFFICE , y = avg_cover  )) + 
  geom_bar(stat = 'identity', aes( fill = decade), position = position_dodge()) + 
  theme( axis.text.x = element_text( angle = -40, hjust = 0 )) + 
  ylab( 'Perennial forb and grass cover') + 
  ggtitle('Mediterranean California')

