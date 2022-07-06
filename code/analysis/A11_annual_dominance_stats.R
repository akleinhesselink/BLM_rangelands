# Quantify major changes 
rm(list = ls() )
library(tidyverse)

source('code/analysis/parameters.R')

annual_data <- read_csv('data/temp/annual_data.csv')
allotments <- read_csv('data/temp/allotment_info.csv')

annual_data <- annual_data %>% 
  left_join(allotments, by = 'uname')

decades <- 
  data.frame( year = 1980:2030 ) %>% 
  mutate( decade = cut( year , c(1981, 1991, 2001, 2011, 2021, 2031))) %>% 
  mutate( decade = factor(decade, labels = c('1980s', '1990s', '2000s', '2010s', '2020s'))) 

letter_lab <- 
  expand.grid( decade = '1980s', perc_more_annuals = 105, 
               ecoregion = c( "AZ/NM Highlands", "E Cold Deserts", 
                              "Forested Mts", 
                              "Mediterranean California", 
                              "N Great Plains", "S Great Plains", 
                              "W Cold Deserts", "Warm Deserts"))  %>% 
  mutate( lab = LETTERS[1:8])

# Annual Cover ---------------------------------------- 
annual_cover_dominance_uname <- annual_data %>% 
  rename( 'type' = name) %>% 
  filter( unit == 'cover') %>% 
  left_join(decades, by = 'year') %>% 
  group_by( type, uname, decade, ecoregion) %>% 
  summarise(avg = mean(value, na.rm = T))  %>% 
  pivot_wider(names_from = type, values_from = avg) %>%
  mutate( annual_dom = AFG > PFG ) %>% 
  filter( !is.na( AFG ))

total_annual_cover_dominance <- 
  annual_cover_dominance_uname %>% 
  filter( ecoregion != 'Marine West Coast Forests') %>%
  ungroup() %>% 
  mutate( ecoregion = 'Total') %>%
  group_by( ecoregion, decade , annual_dom ) %>% 
  summarise( n = n())

annual_cover_dominance_ecoregion <- annual_cover_dominance_uname %>% 
  group_by( decade, ecoregion, annual_dom ) %>%
  summarise( n = n())  %>% 
  bind_rows( total_annual_cover_dominance  ) %>% 
  group_by( decade, ecoregion ) %>%   
  mutate( frac = n/sum(n)) %>% 
  arrange( ecoregion, decade ) %>% 
  ungroup() %>% 
  mutate( Dominance = factor(annual_dom, labels = c('more perennials', 'more annuals'))) %>% 
  select( decade, ecoregion, Dominance , n) %>% 
  pivot_wider( names_from = Dominance, values_from = n, values_fill = 0) %>%
  arrange( ecoregion, decade ) %>%
  mutate( n_total = `more perennials` + `more annuals`) %>% 
  mutate( frac_more_perennials = `more perennials`/n_total ) %>% 
  mutate( frac_more_annuals = `more annuals`/n_total ) %>%  
  select( decade, ecoregion, `more annuals`, n_total, frac_more_annuals)

# Make plot  ------------------------- 
ann_cover <- annual_cover_dominance_ecoregion %>% 
  filter( ecoregion != 'Marine West Coast Forest', ecoregion != 'Total') %>% 
  mutate( 
    label = paste0( round( frac_more_annuals, 2)*100, '%', '(',`more annuals`,')')
  ) %>% 
  mutate( perc_more_annuals = frac_more_annuals*100 ) %>% 
  ggplot( aes( x = decade, y = perc_more_annuals )) + 
  geom_bar(stat = 'identity') + 
  geom_text( data = letter_lab, aes( label = lab), 
             nudge_x = -0.4, nudge_y = 0.2, size = 3) + 
  facet_wrap( ~ ecoregion , nrow = 4) + 
  geom_text( aes( label = label), nudge_y = 5, size = 2.5) + 
  ylab( 'Percent of Allotments') + 
  xlab( 'Decade') + 
  theme_bw() + 
  ggtitle('Annual Cover > Perennial Cover')  + 
  scale_y_continuous(labels = scales::percent_format(scale = 1))

ggsave(ann_cover , filename = 'output/figures/Fig_Supp_annual_cover_gt_perennial_by_decade.png', 
       height = 8, width = 6, units = 'in', dpi = 600)


# Make table 
annual_cover_dominance_area <- 
  annual_cover_dominance_uname %>% 
  left_join(allotments %>% select( uname, hectares ), by = 'uname') %>% 
  group_by( decade, ecoregion, annual_dom ) %>%
  summarise( hectares = sum(hectares))  %>% 
  group_by( decade, ecoregion ) %>%   
  arrange( ecoregion, decade ) %>% 
  ungroup() %>% 
  mutate( Dominance = factor(annual_dom, labels = c('more perennials', 'more annuals'))) %>% 
  select( decade, ecoregion, Dominance , hectares) %>%
  filter( Dominance == 'more annuals') 

total_annual_cover_dominance_area <- 
  annual_cover_dominance_area %>% 
  filter( ecoregion != 'Marine West Coast Forests') %>%
  ungroup() %>% 
  mutate( ecoregion = 'Total') %>%
  group_by( ecoregion, decade , Dominance ) %>% 
  summarise( hectares = sum(hectares))

level_order <- 
  c( "AZ/NM Highlands (562)", "E Cold Deserts (4574)", "Forested Mts (5086)", 
     "Mediterranean California (232)", 
     "N Great Plains (5066)", "S Great Plains (838)", 
     "W Cold Deserts (3470)", "Warm Deserts (1171)", "Total (21009)")       

annual_cover_dominance_ecoregion <- 
  annual_cover_dominance_uname %>% 
  group_by( decade, ecoregion, annual_dom ) %>%
  summarise( n = n() )  %>% 
  bind_rows( total_annual_cover_dominance  ) %>% 
  group_by( decade, ecoregion ) %>% 
  mutate( frac = n/sum(n)) %>% 
  ungroup() %>% 
  arrange( ecoregion, decade ) %>% 
  mutate( Dominance = factor(annual_dom, labels = c('more perennials', 'more annuals'))) %>% 
  select( decade, ecoregion, Dominance , n ) %>% 
  pivot_wider( names_from = Dominance, values_from = n , values_fill = 0) %>%
  arrange( ecoregion, decade ) %>%
  mutate( n_total = `more perennials` + `more annuals`) %>% 
  mutate( frac_more_perennials = `more perennials`/n_total ) %>% 
  mutate( frac_more_annuals = `more annuals`/n_total )    %>% 
  select( decade, ecoregion, n_total, `more annuals`, frac_more_annuals )
  
annual_cover_dominance_ecoregion_table <- 
  annual_cover_dominance_ecoregion %>% 
  filter( ecoregion != 'Marine West Coast Forest') %>% 
  arrange( ecoregion, decade ) %>%
  left_join( 
    bind_rows(annual_cover_dominance_area, 
              total_annual_cover_dominance_area) %>% select( - Dominance) ) %>%   
  mutate( hectares = replace_na(hectares, 0)) %>% 
  select( decade, ecoregion, `more annuals`, n_total, frac_more_annuals, hectares) %>% 
  mutate( frac_more_annuals = paste0( round( 100*frac_more_annuals), '%'), 
          `Total area (1x10^3 ha)` = as.character( round( `hectares`/1e3, 2) )) %>% 
  mutate( `more annuals`  = as.character( `more annuals`)) %>% 
  mutate( ecoregion_label = paste0( ecoregion, ' (', n_total, ')')) %>%  
  select( - hectares) %>%  
  pivot_longer(cols = c(`more annuals` , frac_more_annuals, `Total area (1x10^3 ha)`))

annual_cover_dominance_ecoregion_table %>% 
  pivot_wider( names_from = name , values_from = value) %>% 
  mutate( new_val = paste0( `more annuals`, ' (', frac_more_annuals, ')')) %>% 
  select( ecoregion_label, decade, new_val, `Total area (1x10^3 ha)`) %>% 
  pivot_longer( cols = c( `new_val`:`Total area (1x10^3 ha)` )) %>% 
  unite( 'newName' , c(name, decade)) %>% 
  pivot_wider( names_from = newName, values_from = value ) %>%  
  select( ecoregion_label, contains( 'new_val'), contains('area')) %>% 
  mutate( ecoregion_label = factor(ecoregion_label)) %>%  
  mutate( ecoregion_label = factor( ecoregion_label, levels = level_order, ordered = T)) %>%   
  arrange( ecoregion_label) %>% 
  rename( "Ecoregion" = ecoregion_label) %>% 
  kableExtra::kbl(caption = 'Table A13: Allotments with annual cover greater than perennial cover by decade.') %>% 
  kableExtra::kable_classic_2(html_font = 'times new roman', font_size = 12) %>%  
  kableExtra::save_kable(file = 'output/tables/annual_cover_dominance_stats.html')

# Annual Production ------------------------------------------- # 
annual_prod_dominance_uname <- 
  annual_data %>% 
  rename( 'type' = name) %>% 
  filter( unit == 'production', type != 'herbAGB') %>% 
  left_join(decades, by = 'year') %>% 
  group_by( type, uname, decade, ecoregion) %>% 
  summarise(avg = mean(value, na.rm = T))  %>% 
  pivot_wider(names_from = type, values_from = avg) %>% 
  mutate( annual_dom = afgNPP > pfgNPP ) %>%
  filter( !is.na( afgNPP ))

total_annual_prod_dominance <- 
  annual_prod_dominance_uname %>% 
  filter( ecoregion != 'Marine West Coast Forests') %>%
  ungroup() %>% 
  mutate( ecoregion = 'Total') %>%
  group_by( ecoregion, decade , annual_dom ) %>% 
  summarise( n = n())


annual_prod_dominance_area <- 
  annual_prod_dominance_uname %>% 
  left_join(allotments %>% select( uname, hectares ), by = 'uname') %>% 
  group_by( decade, ecoregion, annual_dom ) %>%
  summarise( hectares = sum(hectares))  %>% 
  group_by( decade, ecoregion ) %>%   
  arrange( ecoregion, decade ) %>% 
  ungroup() %>% 
  mutate( Dominance = factor(annual_dom, labels = c('more perennials', 'more annuals'))) %>% 
  select( decade, ecoregion, Dominance , hectares) %>%
  filter( Dominance == 'more annuals') 

total_annual_prod_dominance_area <- 
  annual_prod_dominance_area %>% 
  filter( ecoregion != 'Marine West Coast Forests') %>%
  ungroup() %>% 
  mutate( ecoregion = 'Total') %>%
  group_by( ecoregion, decade , Dominance ) %>% 
  summarise( hectares = sum(hectares))

annual_prod_dominance_ecoregion <- 
  annual_prod_dominance_uname %>% 
  group_by( decade, ecoregion, annual_dom ) %>%
  summarise( n = n() )  %>% 
  bind_rows( total_annual_prod_dominance  ) %>% 
  group_by( decade, ecoregion ) %>% 
  mutate( frac = n/sum(n)) %>% 
  ungroup() %>% 
  arrange( ecoregion, decade ) %>% 
  mutate( Dominance = factor(annual_dom, labels = c('more perennials', 'more annuals'))) %>% 
  select( decade, ecoregion, Dominance , n ) %>% 
  pivot_wider( names_from = Dominance, values_from = n , values_fill = 0) %>%
  arrange( ecoregion, decade ) %>%
  mutate( n_total = `more perennials` + `more annuals`) %>% 
  mutate( frac_more_perennials = `more perennials`/n_total ) %>% 
  mutate( frac_more_annuals = `more annuals`/n_total )    %>% 
  select( decade, ecoregion, n_total, `more annuals`, frac_more_annuals )

annual_prod_dominance_ecoregion_table <- 
  annual_prod_dominance_ecoregion %>% 
  filter( ecoregion != 'Marine West Coast Forest') %>% 
  arrange( ecoregion, decade ) %>%
  left_join( 
    bind_rows(annual_prod_dominance_area, 
              total_annual_prod_dominance_area) %>% select( - Dominance) ) %>%   
  mutate( hectares = replace_na(hectares, 0)) %>% 
  select( decade, ecoregion, `more annuals`, n_total, frac_more_annuals, hectares) %>% 
  mutate( frac_more_annuals = paste0( round( 100*frac_more_annuals), '%'), 
          `Total area (1x10^3 ha)` = as.character( round( `hectares`/1e3, 2) )) %>% 
  mutate( `more annuals`  = as.character( `more annuals`)) %>% 
  mutate( ecoregion_label = paste0( ecoregion, ' (', n_total, ')')) %>%  
  select( - hectares) %>%  
  pivot_longer(cols = c(`more annuals` , frac_more_annuals, `Total area (1x10^3 ha)`))

annual_prod_dominance_ecoregion_table %>% 
  pivot_wider( names_from = name , values_from = value) %>% 
  mutate( new_val = paste0( `more annuals`, ' (', frac_more_annuals, ')')) %>% 
  select( ecoregion_label, decade, new_val, `Total area (1x10^3 ha)`) %>% 
  pivot_longer( cols = c( `new_val`:`Total area (1x10^3 ha)` )) %>% 
  unite( 'newName' , c(name, decade)) %>% 
  pivot_wider( names_from = newName, values_from = value ) %>%  
  select( ecoregion_label, contains( 'new_val'), contains('area')) %>% 
  mutate( ecoregion_label = factor(ecoregion_label)) %>%  
  mutate( ecoregion_label = factor( ecoregion_label, levels = level_order, ordered = T)) %>%   
  arrange( ecoregion_label) %>% 
  rename( "Ecoregion" = ecoregion_label) %>% 
  kableExtra::kbl(caption = 'Table A14: Allotments with annual production greater than perennial production by decade.') %>% 
  kableExtra::kable_classic_2(html_font = 'times new roman', font_size = 12) %>%  
  kableExtra::save_kable(file = 'output/tables/annual_production_dominance_stats.html')


letter_lab <- 
  expand.grid( decade = '1980s', perc_more_annuals = 105, 
               ecoregion = c( "AZ/NM Highlands", "E Cold Deserts", 
                              "Forested Mts", 
                              "Mediterranean California", 
                              "N Great Plains", "S Great Plains", 
                              "W Cold Deserts", "Warm Deserts"))  %>% 
  mutate( lab = LETTERS[1:8])

# Make plot ------------------------------ # 
ann_prod <- annual_prod_dominance_ecoregion %>% 
  filter( ecoregion != 'Marine West Coast Forest', ecoregion != 'Total') %>% 
  mutate( perc_more_annuals = frac_more_annuals*100) %>% 
  mutate( 
    label = paste0( round( frac_more_annuals, 2)*100, '%', '(',`more annuals`,')')
  ) %>%
  ggplot( aes( x = decade, y = perc_more_annuals )) + 
  geom_text( data = letter_lab, aes( label = lab), 
             nudge_x = -0.4, nudge_y = 0.2, size = 3) + 
  facet_wrap( ~ ecoregion , nrow = 4) + 
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

# Make Table --------------- # 


annual_prod_dominance_area <- 
  annual_prod_dominance_uname %>% 
  left_join(allotments %>% select( uname, hectares ), by = 'uname') %>% 
  group_by( decade, ecoregion, annual_dom ) %>%
  summarise( hectares = sum(hectares))  %>% 
  group_by( decade, ecoregion ) %>%   
  arrange( ecoregion, decade ) %>% 
  ungroup() %>% 
  mutate( Dominance = factor(annual_dom, labels = c('more perennials', 'more annuals'))) %>% 
  select( decade, ecoregion, Dominance , hectares) %>%
  filter( Dominance == 'more annuals') 

total_annual_prod_dominance_area <- 
  annual_prod_dominance_area %>% 
  filter( ecoregion != 'Marine West Coast Forests') %>%
  ungroup() %>% 
  mutate( ecoregion = 'Total') %>%
  group_by( ecoregion, decade , Dominance ) %>% 
  summarise( hectares = sum(hectares))

annual_prod_dominance_ecoregion %>% 
  filter( ecoregion != 'Marine West Coast Forest') %>% 
  arrange( ecoregion, decade ) %>%
  left_join( 
    bind_rows(annual_prod_dominance_area, 
              total_annual_prod_dominance_area) %>% select( - Dominance) ) %>% 
  mutate( hectares = replace_na(hectares, 0)) %>% 
  select( decade, ecoregion, `more annuals`, n_total, frac_more_annuals, hectares) %>% 
  mutate( frac_more_annuals = paste0( round( 100*frac_more_annuals, 2), '%'), 
          `Total area (1x10^3 ha)` = as.character( round( `hectares`/1e3, 2) )) %>% 
  mutate( `more annuals`  = as.character( `more annuals`)) %>% 
  mutate( ecoregion_label = paste0( ecoregion, ' (', n_total, ')')) %>%  
  select( - hectares) %>%  
  pivot_longer(cols = c(`more annuals` , frac_more_annuals, `Total area (1x10^3 ha)`)) %>% 
  pivot_wider( names_from = decade, values_from = value ) %>% 
  select( ecoregion_label, name, `1980s`:`2010s`) %>% 
  ungroup() %>% 
  mutate( ecoregion_label = factor( ecoregion_label, levels = level_order, ordered = T)) %>%
  arrange( ecoregion_label) %>% 
  mutate( name = ifelse(name == "more annuals",  'Allotments (n)', name )) %>% 
  mutate( name = ifelse( name == "frac_more_annuals", '% of Allotments', name)) %>% 
  rename( "Ecoregion" = ecoregion_label, 
          "Annual Dominated" = name) %>%
  kableExtra::kbl(caption = 'Table A14: Allotments with annual production greater than perennial production by decade.') %>% 
  kableExtra::kable_classic_2(html_font = 'times new roman', font_size = 12) %>%  
  kableExtra::save_kable(file = 'output/tables/annual_prod_dominance_stats.html')




# Trees ---------------------------------------------------
tree_bins <-
  annual_data %>% 
  rename( 'type' = name) %>% 
  filter( unit == 'cover', type == 'TRE') %>% 
  left_join(decades, by = 'year') %>% 
  group_by( type, uname, decade, ecoregion) %>% 
  summarise(avg = mean(value, na.rm = T)) %>% 
  ungroup() %>% 
  mutate( tree_bins = cut( avg, c(0, 1, 2, 5, 10, 20, 100))) %>% 
  mutate( tree_bins = factor(tree_bins, labels = c('0-1', '1-2', '2-5', '5-10', '10-20', '>20'))) %>% 
  group_by( decade, ecoregion, tree_bins ) %>%
  summarise( n = n() )  %>% 
  group_by( decade, ecoregion ) %>% 
  mutate( frac = n/sum(n)) %>% 
  ungroup() %>% 
  arrange( ecoregion, decade , tree_bins) 

tree_bins %>% 
  arrange( ecoregion, tree_bins, decade )  %>% 
  write_csv('output/tables/tree_cover_by_decade.csv')

tree_bins %>% 
  filter( ecoregion != 'Marine West Coast Forest') %>% 
  filter( !is.na(tree_bins )) %>% 
  ggplot( aes( x = decade, y = frac, fill = tree_bins )) + 
  geom_bar(stat = 'identity') + 
  facet_wrap( ~ ecoregion )   + 
  scale_fill_brewer(type = 'seq',palette = 2, name = 'Tree Cover Class (%)')  + 
  ylab( 'Fraction of Allotments') + 
  xlab( 'Decade') + 
  theme_bw() + 
  ggtitle('Average Tree Cover by Decade') 
