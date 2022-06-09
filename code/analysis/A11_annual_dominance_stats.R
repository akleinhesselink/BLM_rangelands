# Quantify major changes 
rm(list = ls() )
library(tidyverse)

source('code/analysis/parameters.R')

annual_data <- read_rds('data/temp/annual_data.rds')
allotments <- read_csv('data/temp/allotment_info.csv')

annual_data <- annual_data %>% 
  left_join(allotments, by = 'uname')

decades <- 
  data.frame( year = 1980:2030 ) %>% 
  mutate( decade = cut( year , c(1980, 1990, 2000, 2010, 2020, 2030))) %>% 
  mutate( decade = factor(decade, labels = c('1980s', '1990s', '2000s', '2010s', '2020s'))) 

# Annual Production ------------------------------------------- # 

annual_prod_dominance <- annual_data %>% 
  rename( 'type' = name) %>% 
  filter( unit == 'production', type != 'herbAGB') %>% 
  left_join(decades, by = 'year') %>% 
  group_by( type, uname, decade, ecogroup) %>% 
  summarise(avg = mean(value, na.rm = T))  %>% 
  pivot_wider(names_from = type, values_from = avg) %>% 
  mutate( annual_dom = afgAGB > pfgAGB ) %>%
  group_by( decade, ecogroup, annual_dom ) %>%
  summarise( n = n() )  %>% 
  group_by( decade, ecogroup ) %>% 
  mutate( frac = n/sum(n)) %>% 
  ungroup() %>% 
  arrange( ecogroup, decade ) %>% 
  mutate( Dominance = factor(annual_dom, labels = c('more perennials', 'more annuals'))) %>% 
  select( decade, ecogroup, Dominance , n ) %>% 
  pivot_wider( names_from = Dominance, values_from = n , values_fill = 0) %>%
  arrange( ecogroup, decade ) %>%
  mutate( n_total = `more perennials` + `more annuals`) %>% 
  mutate( frac_more_perennials = `more perennials`/n_total ) %>% 
  mutate( frac_more_annuals = `more annuals`/n_total )

annual_prod_dominance %>% 
  write_csv('output/tables/annual_prod_dominance.csv')

letter_lab <- 
  expand.grid( decade = '1980s', perc_more_annuals = 105, 
               ecogroup = c( "AZ/NM Highlands", "E Cold Deserts", 
                             "Forested Mts", 
                             "Mediterranean California", 
                             "N Great Plains", "S Great Plains", 
                             "W Cold Deserts", "Warm Deserts"))  %>% 
  mutate( lab = LETTERS[1:8])


ann_prod <- annual_prod_dominance %>% 
  filter( ecogroup != 'Marine West Coast Forest') %>% 
  mutate( perc_more_annuals = frac_more_annuals*100) %>% 
  mutate( 
    label = paste0( round( frac_more_annuals, 2)*100, '%', '(',`more annuals`,')')
  ) %>%
  ggplot( aes( x = decade, y = perc_more_annuals )) + 
  geom_text( data = letter_lab, aes( label = lab), 
             nudge_x = -0.4, nudge_y = 0.2, size = 3) + 
  facet_wrap( ~ ecogroup , nrow = 4) + 
  geom_bar(stat = 'identity') + 
  geom_text( aes( label = label), nudge_y = 4, size = 2.5) + 
  ylab( 'Percent of Allotments') + 
  xlab( 'Decade') + 
  theme_bw() + 
  scale_y_continuous(labels = scales::percent_format(scale = 1)) + 
  ggtitle('Annual Production > Perennial Production')

ggsave( ann_prod , 
        filename = 'output/figures/Fig_Supp_annual_production_gt_perennial_by_decade.png', 
          height = 8, width = 6, units = 'in', dpi = 600)




# Annual Cover ---------------------------------------- 
annual_cover_dominance <- annual_data %>% 
  rename( 'type' = name) %>% 
  filter( unit == 'cover') %>% 
  left_join(decades, by = 'year') %>% 
  group_by( type, uname, decade, ecogroup) %>% 
  summarise(avg = mean(value, na.rm = T))  %>% 
  pivot_wider(names_from = type, values_from = avg) %>%
  mutate( annual_dom = AFGC > PFGC ) %>%
  group_by( decade, ecogroup, annual_dom ) %>%
  summarise( n = n() )  %>% 
  group_by( decade, ecogroup ) %>% 
  mutate( frac = n/sum(n)) %>% 
  ungroup() %>% 
  arrange( ecogroup, decade ) %>% 
  mutate( Dominance = factor(annual_dom, labels = c('more perennials', 'more annuals'))) %>% 
  select( decade, ecogroup, Dominance , n ) %>% 
  pivot_wider( names_from = Dominance, values_from = n , values_fill = 0) %>%
  arrange( ecogroup, decade ) %>%
  mutate( n_total = `more perennials` + `more annuals`) %>% 
  mutate( frac_more_perennials = `more perennials`/n_total ) %>% 
  mutate( frac_more_annuals = `more annuals`/n_total ) 

annual_cover_dominance %>% 
  write_csv('output/tables/annual_cover_dominance.csv')

ann_cover <- annual_cover_dominance %>% 
  filter( ecogroup != 'Marine West Coast Forest') %>% 
  mutate( 
    label = paste0( round( frac_more_annuals, 2)*100, '%', '(',`more annuals`,')')
  ) %>% 
  mutate( perc_more_annuals = frac_more_annuals*100 ) %>% 
  ggplot( aes( x = decade, y = perc_more_annuals )) + 
  geom_bar(stat = 'identity') + 
  geom_text( data = letter_lab, aes( label = lab), 
             nudge_x = -0.4, nudge_y = 0.2, size = 3) + 
  facet_wrap( ~ ecogroup , nrow = 4) + 
  geom_text( aes( label = label), nudge_y = 5, size = 2.5) + 
  ylab( 'Percent of Allotments') + 
  xlab( 'Decade') + 
  theme_bw() + 
  ggtitle('Annual Cover > Perennial Cover')  + 
  scale_y_continuous(labels = scales::percent_format(scale = 1))

ggsave(ann_cover , filename = 'output/figures/Fig_Supp_annual_cover_gt_perennial_by_decade.png', 
          height = 8, width = 6, units = 'in', dpi = 600)

annual_cover_dominance %>% 
  filter(ecogroup != 'Mediterranean California') %>% 
  group_by(decade ) %>% 
  summarise( n = sum(n_total), `more annuals` = sum(`more annuals`)) %>% 
  mutate( `more annuals`/n)

# Trees ---------------------------------------------------
tree_bins <-
  annual_data %>% 
  rename( 'type' = name) %>% 
  filter( unit == 'cover', type == 'TREE') %>% 
  left_join(decades, by = 'year') %>% 
  group_by( type, uname, decade, ecogroup) %>% 
  summarise(avg = mean(value, na.rm = T)) %>% 
  ungroup() %>% 
  mutate( tree_bins = cut( avg, c(0, 1, 2, 5, 10, 20, 100))) %>% 
  mutate( tree_bins = factor(tree_bins, labels = c('0-1', '1-2', '2-5', '5-10', '10-20', '>20'))) %>% 
  group_by( decade, ecogroup, tree_bins ) %>%
  summarise( n = n() )  %>% 
  group_by( decade, ecogroup ) %>% 
  mutate( frac = n/sum(n)) %>% 
  ungroup() %>% 
  arrange( ecogroup, decade , tree_bins) 

tree_bins %>% 
  arrange( ecogroup, tree_bins, decade )  %>% 
  write_csv('output/tables/tree_cover_by_decade.csv')

tree_bins %>% 
  filter( ecogroup != 'Marine West Coast Forest') %>% 
  filter( !is.na(tree_bins )) %>% 
  ggplot( aes( x = decade, y = frac, fill = tree_bins )) + 
  geom_bar(stat = 'identity') + 
  facet_wrap( ~ ecogroup )   + 
  scale_fill_brewer(type = 'seq',palette = 2, name = 'Tree Cover Class (%)')  + 
  ylab( 'Fraction of Allotments') + 
  xlab( 'Decade') + 
  theme_bw() + 
  ggtitle('Average Tree Cover by Decade') + 
  ggsave( 'output/figures/Fig_Supp_tree_cover_by_decade.png', 
          height = 6, width = 8, units = 'in', dpi = 600)

