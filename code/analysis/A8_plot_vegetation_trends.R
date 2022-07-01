rm(list = ls())
library(tidyverse)
library(lme4)
library(emmeans)
library(gridExtra)

source('code/analysis/functions.R')
source('code/analysis/parameters.R')

# Cover trends:  ------------------------------------------ #
cover_model_files <-
  dir(path = 'output',
      pattern = '.*_cover_trend_model.rds',
      full.names = T)

cover_models <- lapply(cover_model_files, read_rds)
types  <- c(str_extract(cover_model_files, pattern = '[A-Z]+'))

types <-
  factor(types, labels = names( my_colors))

names(cover_models) <- types

trend_table_cover <- mapply(
  x = cover_models,
  y = types,
  FUN = function(x, y)
    ecoregion_trends_as_df(x, y),
  SIMPLIFY = F
)

trend_table_cover <- do.call(rbind, trend_table_cover)

trend_table_cover <-
  trend_table_cover %>%
  mutate(type = factor(type))

trend_table_cover <- trend_table_cover %>%
  mutate( ecoregion = factor(ecoregion)) %>% 
  mutate(ecoregion = factor(ecoregion, labels = ecoregion_labels)) 

trend_table_cover$measure <- 'Cover'

# Production Models and Trends--------------------------- #
npp_model_files <-
  dir(path = 'output',
      pattern = '.*_NPP_trend_model.rds',
      full.names = T)

npp_models <- lapply(npp_model_files, read_rds)

types_npp  <-
  c(str_extract(basename(npp_model_files), pattern = '[A-Z]+'))

types_npp <- factor(types_npp, labels = c('Annual', 'Perennial'))
names( npp_models ) <- types_npp 

trend_table_npp <-
  mapply(
    x = npp_models,
    y = types_npp,
    FUN = function(x, y)
      ecoregion_trends_as_df(x, y),
    SIMPLIFY = F
  )

trend_table_npp <- do.call(rbind, trend_table_npp)

trend_table_npp <- trend_table_npp %>%
  mutate( ecoregion = factor(ecoregion)) %>%
  mutate(ecoregion = factor(ecoregion, labels = ecoregion_labels))

trend_table_npp$measure <- 'Production'

# Plot Cover and Production Trends together
trend_table <-
  trend_table_cover %>%
  bind_rows(trend_table_npp) %>%
  mutate(measure = factor(
    measure,
    levels = c('Cover', 'Production'),
    ordered = T
  )) %>%
  mutate(type = factor(
    type,
    levels = sort(names(my_colors), decreasing = T),
    ordered = T
  )) %>%
  mutate(sig = ifelse((asymp.LCL > 0 | asymp.UCL < 0), "*", ''))

# Plot -------------------------
trend_plot1 <- 
  trend_table %>%
  plot_trend_coefficients_vertical(my_colors = my_colors) +
  facet_grid(ecoregion  ~ measure, switch = 'y', scales = 'free_x') +
  theme(legend.title = element_text()) +
  scale_color_manual(name = 'Functional Type', values = my_colors) +
  scale_x_continuous(name = 'Trend Slope (+/- 95%CI)') +
  scale_color_manual(name = 'Functional Type',
                     values = my_colors,
                     guide = guide_legend())

ggsave(trend_plot1, 
    filename = 'output/figures/Fig_4_veg_trends_by_Ecoregion.png',
    height = 8,
    width = 7,
    units = 'in',
    dpi = 'print'
  )

#  Save Tables -------------------------------------------- #
ttlist <- trend_table %>%
  split(f = .$type:.$measure)

ttlist <- ttlist[which(unlist(lapply(ttlist, nrow)) != 0)]

for (i in names(ttlist)) {
  ttlist[[i]] %>% 
    kableExtra::kbl(digits = 3, caption = i) %>% 
    kableExtra::kable_classic_2() %>%
    kableExtra::save_kable(file = file.path('output/tables/',
                                            paste0(i, '_trend_estimates.html')))
}

# Plot observed and predicted over time
## Plot Allotment Cover over time :

frames <- lapply( cover_models, function(x) x@frame)
types <- names( frames  )

yhat <- lapply( cover_models, FUN = function( x) {
  data.frame( yhat = predict( x, re.form = ~ (1|uname) + (1|ecoregion:OFFICE)))
}) 

names( yhat ) <- types 

cover_data <- cbind( do.call( rbind, frames) , do.call( rbind, yhat ) ) %>% 
  mutate( type = str_extract( rownames(.), '^[A-z]+')) %>% 
  mutate( measure = 'Cover') %>% 
  mutate( year = t + 1991 )

# Get NPP data 
frames_npp <- lapply( npp_models, function( x) x@frame )
types_npp <- names(frames_npp)
yhat_npp <- lapply( npp_models, FUN = function( x) {
  data.frame( yhat = predict( x, re.form = ~ (1|uname) + (1|ecoregion:OFFICE)))
}) 

npp_data <- cbind( do.call( rbind, frames_npp) , do.call( rbind, yhat_npp ) ) %>% 
  mutate( type = str_extract( rownames(.), '^[A-z]+')) %>% 
  mutate( measure = 'Production') %>% 
  mutate( year = t + 1991)

# bind cover and npp 
all_data_summary <- cover_data %>% 
  bind_rows(npp_data) %>% 
  mutate( value = exp(log_value)) %>% 
  group_by( year, ecoregion, measure , type ) %>%
  summarise( yhat_bt  = exp(mean(yhat)),   
             lcl = quantile( value, 0.25) , ucl = quantile( value, 0.75))


# Add indicator for significant trends from the trend table 
all_data_summary <- 
  all_data_summary %>% 
  ungroup() %>% 
  mutate( ecoregion = factor( ecoregion, labels = ecoregion_labels)) %>% 
  left_join(trend_table %>% select( ecoregion, type, measure, sig) , by = c('ecoregion', 'type',  'measure')) %>% 
  mutate(significant =  ifelse(sig == '*', T, F))

ylab = "Cover (%)"

cover_time_series <- 
  all_data_summary %>% 
  filter( measure == 'Cover') %>% 
  ggplot( aes( x = year) ) + 
  geom_ribbon(aes( ymin = lcl, ymax = ucl, fill = type), color = NA, alpha = 0.5) + 
  geom_line(data = all_data_summary %>% 
              filter( significant, measure == 'Cover') , 
            aes( y = yhat_bt ), color = 'black' , alpha = 0.5) + 
  facet_grid(type ~ ecoregion, scales = 'free_y') +
  scale_fill_manual(name = 'Cover Type', values = my_colors) +
  scale_x_continuous(breaks = c(1995, 2005, 2015)) +
  theme_bw() + 
  scale_y_log10(name = ylab) +
  theme(strip.text.y = element_text(angle = 0),
        axis.text.x = element_text(angle = -45, hjust = 0)) +
  xlab('Year') +
  theme(legend.position = "none") 

ggsave(
  cover_time_series , 
  filename = 'output/figures/Fig_2_cover_series_by_ecoregion.png',
  height = 7,
  width = 10,
  units = 'in',
  dpi = 'print'
)

# NPP time series 
ylab <- expression(Aboveground ~ Production ~ "(" * kg ~ ha ^ -1 * ")")
production_time_series <- 
  all_data_summary %>% 
  filter( measure == 'Production') %>% 
  mutate( type = factor(type, levels = c('Annual', 'Perennial'))) %>%
  ggplot( aes( x = year) ) + 
  geom_ribbon(aes( ymin = lcl, ymax = ucl, fill = type), color = NA, alpha = 0.5) + 
  geom_line(data = all_data_summary %>% 
              filter( significant, measure == 'Production') , 
            aes( y = yhat_bt, group = type ), color = 'black' , alpha = 0.5) + 
  facet_grid( ~ ecoregion, scales = 'free_y') +
  scale_fill_manual(name = 'Type', values = my_colors[c(1,3)]) +
  scale_x_continuous(breaks = c(1995, 2005, 2015)) +
  theme_bw() + 
  scale_y_log10(name = ylab) +
  theme(strip.text.y = element_text(angle = 0),
        axis.text.x = element_text(angle = -45, hjust = 0)) +
  xlab('Year')

ggsave( production_time_series, 
        filename = 'output/figures/Fig_3_production_series_by_ecoregion.png',
        height = 3,
        width = 10,
        units = 'in',
        dpi = 'print'
)

