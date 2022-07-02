all <- read_csv('data/temp/annual_data.csv') 
allotments <- read_csv( 'data/temp/allotment_info.csv')

all <- all  %>% left_join(allotments)

tree_mod <- read_rds('output/TREE_cover_trend_model.rds')
afg_mod <- read_rds( 'output/AFG_cover_trend_model.rds')
afgNPP_mod <- read_rds( 'output/AFG_NPP_trend_model.rds')

n_allot_all <- all %>%
  summarise( 
  n_distinct(uname)
) 

n_allot_tree <- tree_mod@frame %>% 
  summarise( n_distinct(uname))
n_allot_afg <- afg_mod@frame %>% 
  summarise( n_distinct(uname))
n_allot_afgNPP <- afgNPP_mod@frame %>% 
  summarise( n_distinct(uname))

n_allot_all$`n_distinct(uname)` - n_allot_tree$`n_distinct(uname)`
n_allot_all$`n_distinct(uname)` - n_allot_afg$`n_distinct(uname)`
n_allot_all$`n_distinct(uname)` - n_allot_afgNPP$`n_distinct(uname)`

load('data/analysis_data/cover.rda')
attach(cover)

AFG %>% summarise( n_distinct(uname))


AFG %>%
  filter(year < 1996 ) %>%
  group_by(ecoregion) %>%
  summarise(AFG = mean(value)) %>%
  left_join(BGR %>%
              filter(year > 1996) %>%
              group_by(ecoregion) %>%
              summarise(BGR = mean(value))) %>%
  left_join(PFG %>%
              filter(year > 1996) %>%
              group_by(ecoregion) %>%
              summarise(PFG = mean(value))) %>%
  left_join(SHR %>%
               filter(year > 1996) %>%
               group_by(ecoregion) %>%
               summarise(SHR = mean(value))) %>%
  left_join(TRE %>%
              filter(year > 1996) %>%
              group_by(ecoregion) %>%
              summarise(TRE = mean(value))) %>% 
  pivot_longer(AFG:TRE ) %>%
  ggplot(aes( y = value , x = ecoregion )) + 
  geom_bar( aes( fill =  name), stat = 'identity', position = position_dodge()) + 
  theme( axis.text.x = element_text(angle = -40, hjust = 0))

# 

AFG %>%
  filter(year > 2015) %>%
  group_by(ecoregion) %>%
  summarise(AFG = mean(value)) %>%
  left_join(BGR %>%
              filter(year > 2015) %>%
              group_by(ecoregion) %>%
              summarise(BGR = mean(value))) %>%
  left_join(PFG %>%
              filter(year > 2015) %>%
              group_by(ecoregion) %>%
              summarise(PFG = mean(value))) %>%
  left_join(SHR %>%
              filter(year > 2015) %>%
              group_by(ecoregion) %>%
              summarise(SHR = mean(value))) %>%
  left_join(TRE %>%
              filter(year > 2015) %>%
              group_by(ecoregion) %>%
              summarise(TRE = mean(value))) %>% 
  pivot_longer(AFG:TRE ) %>%
  ggplot(aes( y = value , x = ecoregion )) + 
  geom_bar( aes( fill =  name), stat = 'identity', position = position_dodge()) +  
  theme( axis.text.x = element_text(angle = -40, hjust = 0))


cover_change <- lapply( cover, function( x ) {  
  x %>% 
    filter( year > 2015 | year < 1996) %>% 
    mutate( era = cut(year, breaks = c(1990, 2010 , 2022), labels = c('early 90s', 'late 10s'))) %>% 
    group_by( era, ecoregion) %>% 
    summarise( avg = median(value)) %>% 
    as.data.frame() 
})

do.call( rbind, cover_change ) %>% 
  mutate( type = str_extract( row.names( . ), '[A-Z]+')) %>% 
  pivot_wider( names_from = era, values_from = avg ) %>% 
  mutate( prop_change = (`late 10s` - `early 90s` ) /`early 90s`)  %>% 
  write_csv('output/tables/average_cover_change_per_ecoregion.csv')

load('data/analysis_data/prod.rda')

prod_change <- lapply( prod[1:2], function( x ) {  
  x %>% 
    filter( year > 2015 | year < 1996) %>% 
    mutate( era = cut(year, breaks = c(1990, 2010 , 2022), labels = c('early 90s', 'late 10s'))) %>% 
    group_by( era, ecoregion) %>% 
    summarise( avg = median(value)) %>% 
    as.data.frame() 
})

do.call( rbind, prod_change ) %>% 
  mutate( type = str_extract( row.names( . ), '[A-z]+')) %>% 
  pivot_wider( names_from = era, values_from = avg ) %>% 
  mutate( prop_change = (`late 10s` - `early 90s` ) /`early 90s`)  %>% 
  write_csv('output/tables/average_prod_change_per_ecoregion.csv')



