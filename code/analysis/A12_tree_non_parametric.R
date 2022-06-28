# Tree Quantile regression 
rm(list = ls() )
library(trend)
library(tidyverse)
source('code/analysis/parameters.R')
source('code/analysis/functions.R')

tree_model <- read_rds('output/TREE_cover_trend_model.rds')
annual_data <- read_csv('data/temp/annual_data.csv')
allotments <- read_csv('data/temp/allotment_info.csv')

annual_data <- annual_data %>% 
  left_join(allotments, by = 'uname')

tree_data <- annual_data %>% 
  filter( year > 1990 ) %>% 
  filter( !is.na(value)) %>% 
  filter( ecoregion != "Marine West Coast Forest") %>% 
  filter( unit == 'cover', name == 'TRE')

tree_data <- tree_data %>%
  split( ., .$ecoregion)

out <- tree_data %>% 
  lapply(. , . %>% 
           arrange(uname, year) %>% 
           #filter( row_number( ) < 240 ) %>%
           group_by(ecoregion, uname) %>% 
           summarise( x = list( ts(value) )) %>% 
           rowwise() %>% 
           summarise( ecoregion = ecoregion,  uname = uname, res = list( mk.test(unlist(x,recursive = T)))))

ecoregion_summary <- 
  do.call(rbind, out ) %>% 
  rowwise() %>% 
  left_join(allotments, by = c('ecoregion', 'uname')) %>% 
  mutate( significant = res$p.value < 0.05 , stat = res$statistic, est = res$estimates[1]) %>% 
  group_by( ecoregion ) %>% 
  summarise( n = n() , 
             increase = sum( significant & stat > 0), 
             decrease = sum( significant & stat < 0), 
             not_significant = sum( !significant), 
             hectares_of_increasing_allotments = sum( hectares[significant & stat > 0]), 
             hectares_of_decreasing_allotments = sum( hectares[significant & stat < 0]), 
             hectares_of_not_signif_allotments = sum(hectares[!significant])) %>% 
  mutate( hectares_of_decreasing_allotments = round( hectares_of_decreasing_allotments), 
          hectares_of_increasing_allotments = round( hectares_of_increasing_allotments), 
          hectares_of_not_signif_allotments = round(hectares_of_not_signif_allotments)) %>% 
  mutate( fraction_increase = round( increase/n, 2), 
          fraction_decrease = round( decrease/n, 2), 
          fraction_ns = round( not_significant/n, 2))


Total_summary <- 
  ecoregion_summary %>% 
  bind_rows(
    ecoregion_summary %>% 
      summarise_at(.vars = c("n", "increase", "decrease",
                             "not_significant", "hectares_of_increasing_allotments", 
                             "hectares_of_decreasing_allotments",
                             "hectares_of_not_signif_allotments"), .funs = sum) %>% 
      mutate( hectares_of_decreasing_allotments = round( hectares_of_decreasing_allotments), 
              hectares_of_increasing_allotments = round( hectares_of_increasing_allotments), 
              hectares_of_not_signif_allotments = round(hectares_of_not_signif_allotments)) %>%
      mutate( fraction_increase = round( increase/n, 2), 
              fraction_decrease = round( decrease/n, 2), 
              fraction_ns = round( not_significant/n, 2)) %>% 
      mutate( ecoregion = "TOTAL")
  )


Total_summary %>% 
  mutate( increase_new = paste0( 100*(fraction_increase), '% (', increase, '/', n, ')') ) %>% 
  mutate( decrease_new = paste0( 100*(fraction_decrease), '% (', decrease, '/', n, ')')) %>% 
  mutate( no_change_new = paste0( 100*(fraction_ns), '% (', not_significant, '/', n, ')')) %>% 
  select( ecoregion, increase_new, decrease_new, no_change_new, hectares_of_increasing_allotments, hectares_of_decreasing_allotments, hectares_of_not_signif_allotments) %>% 
  write_csv('output/tables/non_parametric_tree_cover_increases.csv')

