rm(list = ls())
library(lme4)
library(tidyverse)
library(knitr)
library(kableExtra)

model_files <- dir('output', pattern = 'cover_trend_model.rds', full.names = T)
model_type <- str_extract( basename(model_files), '[A-Za-z]+')

titles <- c('AFG' = 'Annual', 
            'PFG' = 'Perennial', 
            'BG' = 'Bare Ground', 
            'SHR' = 'Shrub', 
            'TREE' = 'Tree', 
            'afgNPP' = 'Annual', 
            'pfgNPP' = 'Perennial')

model_title <- titles[model_type]

for( i in 1:length(model_files) ) { 
  
  m <- read_rds( model_files[i] )
  
  out <- summary(m)
  curTitle <- paste0( '<b>Table A', i+2, ':</b> ', model_title[i], ' Cover Model Summary. 
                      Linear mixed effects model fit with <b>lmer</b> in R.
                      Reference level (Intercept) is the AZ/NM Highlands ecoregion. 
                      Response modeled on log-scale.')
  
  random_effects <- out$varcor  %>% data.frame() %>% 
    select( -vcov) %>% 
    mutate( cor = ifelse( !is.na(var1) & !is.na(var2), sdcor, NA)) %>% 
    group_by(grp) %>% 
    mutate( cor = ifelse(var1 == 't', unique(cor[!is.na(cor)]), cor)) %>% 
    filter( is.na(var2)) %>% 
    ungroup() %>%
    mutate( Group = str_replace_all(grp, c('uname' = 'allotment', 'ecoregion:OFFICE' = 'Ecoregion:Office'))) %>% 
    mutate( var1 = str_replace_all( var1, c('^t'= 'trend')) ) %>%     
    select( Group, var1, sdcor , cor ) %>% 
    rename( 'Std.Dev.' = sdcor, ) %>% 
    mutate( var1 = replace_na(var1, '')) %>% 
    kbl( caption = 'Random Effects', digits = 4) 
  
  row.names(out$coefficients) <- str_replace_all( row.names( out$coefficients), c('^t' = 'trend', 'ecoregion' = ''))
  
  coefficients <- out$coefficients %>% 
    .[, -c(3)] %>% 
    kbl(caption =  'Coefficients', digits = c(3,4))
  
  ngroups <- c( out$ngrps, out$devcomp$dims['N'])  %>% t() %>% data.frame() %>% 
    rename( 'Ecoregion:Office' = ecoregion.OFFICE, 
            'Allotments' = uname) %>% 
    kbl( caption = curTitle, escape = F)
  
  out <- list(ngroups, random_effects,  coefficients)

  file_name <- file.path('output/tables', paste0( model_type[i], '_cover.html')) 
  
  table_output <- lapply( out , function(x) x %>% kable_classic_2(html_font = 'times new roman', font_size = 12))  

  table_output %>%  
    save_kable(file = file_name )
}

# Print out Biomass models:
model_files <- dir('output', pattern = 'NPP_trend_model.rds', full.names = T)
model_type <- str_extract( basename(model_files), '[A-Za-z]+')

model_title <- titles[model_type]

for( i in 1:length(model_files) ) { 
  
  m <- read_rds( model_files[i] )
  
  out <- summary(m)
  curTitle <- paste0( '<b>Table A', i+7, ':</b> ', model_title[i], ' Production Model Summary. 
    Linear mixed effects model fit with <b>lmer</b> in R.
                      Reference level (Intercept) is the AZ/NM Highlands ecoregion. 
                      Response modeled on log-scale.')
  
  random_effects <- out$varcor  %>% data.frame() %>% 
    select( -vcov) %>% 
    mutate( cor = ifelse( !is.na(var1) & !is.na(var2), sdcor, NA)) %>% 
    group_by(grp) %>% 
    mutate( cor = ifelse(var1 == 't', unique(cor[!is.na(cor)]), cor)) %>% 
    filter( is.na(var2)) %>% 
    ungroup() %>%
    mutate( Group = str_replace_all(grp, c('uname' = 'allotment', 'ecoregion:OFFICE' = 'Ecoregion:Office'))) %>% 
    mutate( var1 = str_replace_all( var1, c('^t'= 'trend')) ) %>%     
    select( Group, var1, sdcor , cor ) %>% 
    rename( 'Std.Dev.' = sdcor, ) %>% 
    mutate( var1 = replace_na(var1, '')) %>% 
    kbl( caption = 'Random Effects', digits = 4) 
  
  row.names(out$coefficients) <- str_replace_all( row.names( out$coefficients), c('^t' = 'trend', 'ecoregion' = ''))
  
  coefficients <- out$coefficients %>% 
    .[, -c(3)] %>% 
    kbl(caption =  'Coefficients', digits = c(3,4))

  ngroups <- c( out$ngrps, out$devcomp$dims['N'])  %>% t() %>% data.frame() %>% 
    rename( 'Ecoregion:Office' = ecoregion.OFFICE, 
            'Allotments' = uname) %>% 
    kbl( caption = curTitle, escape = F)
  
  out <- list(ngroups, random_effects,  coefficients)
  
  file_name <- file.path('output/tables', paste0( model_type[i], '_NPP.html')) 
  
  table_output <- lapply( out, 
                          function(x) {  
                            x %>% 
                              kable_classic_2(html_font = 'times new roman', font_size = 12)
                          })

  table_output %>%  
    save_kable(file = file_name )
}
