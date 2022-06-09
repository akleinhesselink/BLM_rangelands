
annual_agb <- read_csv('output/AFG_agb_group_trends.csv')
perennial_agb <- read_csv('output/PFG_agb_group_trends.csv')

annual_agb %>% 
  left_join( perennial_agb , by= 'uname')  %>% 
  ggplot( aes( x = allot_trend.x, y = allot_trend.y )) + 
  geom_point()  + 
  geom_smooth( method = 'lm', se = F) + 
  facet_wrap( ~ ecogroup.x ) + 
  ylab( 'Annual AGB Trend') + 
  xlab( 'Perennial AGB Trend')
  

annual_agb %>% 
  left_join( perennial_agb , by= 'uname')  %>% 
  ggplot( aes( x = office_trend.x, y = office_trend.y )) + 
  geom_point()  + 
  geom_smooth( method = 'lm', se = F) + 
  facet_wrap( ~ ecogroup.x ) + 
  ylab( 'Annual AGB Trend') + 
  xlab( 'Perennial AGB Trend')



annual_agb %>% 
  left_join( perennial_agb , by= 'uname')  %>% 
  ggplot( aes( x = ecogroup_trend.x, y = ecogroup_trend.y )) + 
  geom_point()  + 
  geom_smooth( method = 'lm', se = F) + 
  ylab( 'Annual AGB Trend') + 
  xlab( 'Perennial AGB Trend')



