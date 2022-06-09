rm(list = ls())
library(tidyverse)
AIM_allotments <- read_csv('data/temp/AIM_species_by_allotment.csv')
SpeciesState <- read_csv('data/temp/clean_speciesState_table.csv')

# Add states so table can be joined with SpeciesState 
AIM_allotments <- AIM_allotments %>% 
  left_join( 
    data.frame( ecogroup = c( 'AZ/NM Highlands', 
                              'E Cold Deserts', 
                              'Forested Mts', 
                              'Mediterranean California', 
                              'N Great Plains', 
                              'S Great Plains', 
                              'W Cold Deserts', 
                              'Warm Deserts') , 
                SpeciesState = c( 'AZ', 
                                  'UT', 
                                  'ID', 
                                  'CA', 
                                  'MT', 
                                  'NM', 
                                  'NV', 
                                  'AZ')  )) 


AIM_allotments <- AIM_allotments %>%   
  left_join( SpeciesState, by = c('Species' = 'SpeciesCode', 
                                  'SpeciesState' = 'SpeciesState')) 

AIM_allotments %>% 
  filter( str_detect(ScientificName, 'Bromus')) %>% 
  group_by( ecogroup, ScientificName ) %>% 
  summarise( n() ) %>% View 

library(tidyverse)
pfg_mod <- read_rds('output/PFG_agb_trend_model.rds')

nd <- pfg_mod@frame %>% 
  filter( ecogroup == 'Warm Deserts') %>% 
  select( ecogroup, office_label, year2) %>% 
  group_by(ecogroup, office_label, year2 ) %>% 
  distinct() 

pfg_mod@call

yhat <- predict(pfg_mod, newdata = nd, re.form  = ~ (1|ecogroup:office_label), type = 'response')
center <- attributes(pfg_mod@frame$value2)$`scaled:center`
scale <- attributes( pfg_mod@frame$value2)$`scaled:scale`

nd$yhat_bt <- exp( yhat*scale + center  )
nd %>% 
  group_by(ecogroup, year2) %>%
  summarise( mean(yhat_bt)) %>% View 

load('data/analysis_data/agb.rda')

agb$pfgAGB %>% 
  mutate( dec = cut( year , c(1980, 1991, 2001, 2011, 2022))) %>% 
  group_by( ecogroup, dec ) %>% 
  summarise( v = mean( value )) %>% 
  ggplot( aes( x = dec, y =v, group = ecogroup ) ) + 
  geom_point() + 
  geom_line() + 
  facet_wrap( ~ ecogroup)

agb$pfgAGB %>% 
  group_by( year, ecogroup )
  select( value, value1, value2)

exp( -1.78*scale + center  )

load('data/analysis_data/cover.rda')

library(lme4) 
library(optimx)
library(dfoptim)


control_lmer$optCtrl$eval.max <- 1e8
control_lmer$optCtrl$iter.max <- 1e8

cover$WOODY 

woodyModel <- lmer( value2 ~ year2*ecogroup + (year2|uname) +  (year2|ecogroup:office_label), data = cover$WOODY)

summary(woodyModel)
emmeans::emtrends(woodyModel, var = 'year2', specs = ~ ecogroup)

woodyModelSimple <- lm( value2 ~ year2*ecogroup , data = cover$WOODY)

emmeans::emtrends(woodyModelSimple, var = 'year2', specs = ~ ecogroup)
