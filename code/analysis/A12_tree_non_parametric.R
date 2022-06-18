# Tree Quantile regression 
rm(list = ls() )
library(trend)
library(tidyverse)
source('code/analysis/parameters.R')
source('code/analysis/functions.R')

tree_model <- read_rds('output/TREE_cover_trend_model.rds')
annual_data <- read_rds('data/temp/annual_data.rds')

shps <- sf::read_sf( 'data/temp/cover_rates/allotments_with_rates.shp')
allotments <- shps %>% 
  st_drop_geometry()

annual_data <- annual_data %>% 
  left_join(allotments, by = 'uname')

tree_data <- annual_data %>% 
  filter( year > 1990 ) %>% 
  filter( !is.na(value)) %>% 
  filter( ecogroup != "Marine West Coast Forest") %>% 
  rename( 'type' = name) %>% 
  filter( unit == 'cover', type == 'TREE')

tree_data <- tree_data %>%
  split( ., .$ecogroup)

out <- tree_data %>% 
  lapply(. , . %>% 
           arrange(uname, year) %>% 
           #filter( row_number( ) < 240 ) %>%
           group_by(ecogroup, uname) %>% 
           summarise( x = list( ts(value) )) %>% 
           rowwise() %>% 
           summarise( ecogroup = ecogroup,  uname = uname, res = list( mk.test(unlist(x,recursive = T)))))

do.call(rbind, out ) %>% 
  rowwise() %>% 
  mutate( significant = res$p.value < 0.05 , stat = res$statistic, est = res$estimates[1]) %>% 
  group_by( ecogroup ) %>% 
  summarise( n = n() , 
             increase = sum( significant & stat > 0), 
             decrease = sum( significant & stat < 0), 
             not_significant = sum( !significant)) %>% 
  mutate( fraction_increase = round( increase/n, 2), 
          fraction_decrease = round( decrease/n, 2), 
          fraction_ns = round( not_significant/n, 2)) %>% 
  write_csv('output/tables/non_parametric_tree_cover_incresases.csv')


