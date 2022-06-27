rm(list = ls() )
library(tidyverse)
library(lme4)
library(optimx)
library(dfoptim)

source('code/analysis/functions.R')
source('code/analysis/parameters.R')

control_lmer$optCtrl$eval.max <- 1e9
control_lmer$optCtrl$iter.max <- 1e9

variance_analysis <- function( df, extraction_number){ 
  
  mydat <- df %>% 
    select( year, uname, rep, AFG:pfgNPP, OFFICE, ecoregion) %>% 
    mutate( pixel = factor( paste(uname, rep, sep = '_'))) %>%
    mutate( uname = factor(uname)) %>% 
    mutate( across( .cols = c(AFG:pfgNPP), .fns = log, .names = "log_{.col}" )) %>% 
    mutate( year = as.numeric(year)) %>% 
    mutate( t =  year - min(year)) 

  mydat <- mydat %>% 
    select(t, year, uname, OFFICE, ecoregion, pixel, starts_with('log') )
  
  mydat <- mydat %>% 
    select( t:pixel, log_AFG:log_pfgNPP) %>% 
    pivot_longer(cols = log_AFG:log_pfgNPP)
  
  mydat_filtered <- mydat %>% 
    group_by( name, pixel ) %>% 
    filter( !is.na(value), is.finite( value )) %>% 
    mutate( n_valid = n_distinct(year)) %>% 
    filter( n_valid > 20  ) %>% 
    group_by(name, uname ) %>% 
    mutate( npixels = n_distinct(pixel)) %>%  
    arrange( name, pixel, t ) %>%
    ungroup() %>% 
    filter( npixels > 2) %>% 
    group_by( name, ecoregion, OFFICE ) %>% 
    mutate( n_allots = n_distinct(uname)) %>% 
    filter( n_allots > 2)
  
  mydat_split <- 
    mydat_filtered %>% 
    split( .$name)
  
  attach( mydat_split)
  
  form <- value ~ t*ecoregion + (t|ecoregion:OFFICE) + (t|uname) + (t|pixel)
  
  m_afg <- lmer(data = log_AFG, form, control = control_lmer)
  m_bgr <- lmer(data = log_BGR, form, control = control_lmer)
  m_pfg <- lmer(data = log_PFG, form, control = control_lmer)
  m_shr <- lmer(data = log_SHR, form, control = control_lmer)
  m_tre <- lmer(data = log_TRE, form, control = control_lmer)
  
  m_afgNPP <- lmer(data = log_afgNPP, form, control = control_lmer)
  m_pfgNPP <- lmer(data = log_pfgNPP, form, control = control_lmer)
  

  write_rds(m_afg, paste0( 'data/temp/afg_pixel_trends_model_extraction_n', extraction_number, '.rds'))
  write_rds(m_bgr, paste0( 'data/temp/bgr_pixel_trends_model_extraction_n', extraction_number, '.rds'))
  write_rds(m_pfg, paste0( 'data/temp/pfg_pixel_trends_model_extraction_n', extraction_number, '.rds'))
  write_rds(m_shr, paste0( 'data/temp/shr_pixel_trends_model_extraction_n', extraction_number, '.rds'))
  write_rds(m_tre, paste0( 'data/temp/tre_pixel_trends_model_extraction_n', extraction_number, '.rds'))
  write_rds(m_afgNPP, paste0( 'data/temp/afgNPP_pixel_trends_model_extraction_n', extraction_number, '.rds'))
  write_rds(m_pfgNPP, paste0( 'data/temp/pfgNPP_pixel_trends_model_extraction_n', extraction_number, '.rds'))
  
  trend_summary <- 
    emtrends(m_afg  , ~ ecoregion, var = "t") %>% 
    data.frame()  %>%
    mutate( type = 'Annual') %>% 
    bind_rows( 
      emtrends(m_bgr, ~ ecoregion, var = 't') %>% 
        data.frame() %>% 
        mutate( type = 'Bare')
    ) %>% 
    bind_rows( 
      emtrends(m_pfg, ~ ecoregion, var = 't') %>% 
        data.frame() %>% 
        mutate( type = 'Perennial')
    ) %>% 
    bind_rows(
      emtrends(m_shr, ~ ecoregion, var = 't') %>% 
        data.frame() %>%
        mutate( type = 'Shrub')
    ) %>%
    bind_rows(
      emtrends(m_tre, ~ ecoregion, var = 't') %>% 
        data.frame() %>%
        mutate( type = 'Tree')
    ) %>%
    bind_rows(
      emtrends(m_afgNPP, ~ ecoregion, var = 't') %>% 
        data.frame() %>%
        mutate( type = 'afgNPP')
    ) %>%
    bind_rows(
      emtrends(m_pfgNPP, ~ ecoregion, var = 't') %>% 
        data.frame() %>%
        mutate( type = 'pfgNPP')
    ) 
  
  u_ecoregion <- sort( unique(trend_summary$ecoregion), decreasing = T)
  u_type <- sort( unique(trend_summary$type), decreasing = T)

  trend_plot_title <- paste0('Ecoregion Trends from pixel extraction n:', extraction_number)
  
  write_csv( trend_summary, paste0( 'output/tables/Ecoregion_trend_summary_pixel_extraction:', 
                                    extraction_number, '.csv'))
  # p1 <- trend_summary %>% 
  #   mutate( type = factor(type, levels = u_type, ordered = T)) %>% 
  #   mutate( ecoregion = factor(ecoregion, levels = u_ecoregion, ordered = T)) %>% 
  #   ggplot( aes( color = type, x = ecoregion, y= t.trend, ymax = asymp.LCL, ymin = asymp.UCL)) + 
  #   geom_point(position = position_dodge(width = 0.5)) + 
  #   geom_errorbar(position = position_dodge(width = 0.5)) + 
  #   geom_hline(aes( yintercept = 0)) + 
  #   ggtitle(trend_plot_title) + 
  #   coord_flip() + 
  #   scale_color_manual(name = 'Functional Type', values = my_colors) +
  #   scale_y_continuous(name = 'Trend Slope (+/- 95%CI)') +
  #   scale_color_manual(name = 'Functional Type',
  #                      values = my_colors,
  #                      guide = guide_legend(reverse = T)) 
  # ggsave( p1, 
  #         filename = paste0( 'output/figures/', trend_plot_title, '.png'), 
  #         height = 8,
  #         width = 7,
  #         units = 'in',
  #         dpi = 'print')
  # 
  # -- Extract trend variance and plot -------------------- # 
  extract_trend_variances <- function( model, label, measure){ 
    
    ngroups = sapply( ranef(model), nrow)
    
    as.data.frame( VarCorr(model)) %>% 
      mutate( type = label ) %>% 
      mutate( measure = measure ) %>%
      filter( var1 == 't', is.na(var2)) %>% 
      mutate( ngrps = ngroups[grp]) %>% 
      select( grp, var1, vcov, sdcor, type , measure, ngrps) %>% 
      rename( 'scale'  = grp)
  }
  
  extract_ranef_tables <- function( model, label, measure){ 
    
  ranef(model)  
    
  }
  
  
  trend_variance_table <- 
    extract_trend_variances(m_afg, 'Annual', 'cover') %>% 
    bind_rows(
      extract_trend_variances(m_bgr, 'Bare', 'cover')  
    ) %>% 
    bind_rows(
      extract_trend_variances(m_pfg, 'Perennial', 'cover')  
    ) %>%     
    bind_rows(
      extract_trend_variances(m_shr, 'Shrub', 'cover')  
    ) %>% 
    bind_rows(
      extract_trend_variances(m_tre, 'Tree', 'cover')  
    ) %>% 
    bind_rows(
      extract_trend_variances(m_afgNPP, 'AFG', 'production')  
    ) %>% 
    bind_rows(
      extract_trend_variances(m_pfgNPP, 'PFG', 'production')  
    ) %>% 
    mutate( scale = str_extract(scale, '[A-Za-z]+$')) %>% 
    mutate( scale = factor(scale, levels = c('OFFICE', 'uname', 'pixel'), ordered = T)) %>% 
    mutate( scale = factor( scale, labels = c('office', 'allotment', 'pixel'))) %>% 
    mutate( type_label = paste( type, measure )) %>% 
    group_by( type_label ) %>% 
    arrange( type_label, desc(scale)) %>% 
    mutate( bar_lab = paste0( 100*round( vcov/sum(vcov), 2), '%'), y_pos = cumsum(vcov) - 0.5*vcov ) %>% 
    ungroup() %>% 
    mutate( extraction_number = extraction_number)
  
  trend_variance_table %>% 
    write_csv(paste0('output/tables/trend_variance_table', extraction_number, '.csv'))
}
# 
# df1 <- read_csv('data/RAP_EE_exports/drive-download-20220623T143258Z-001/RAP_allotment_pixel_samples1.csv') %>% 
#   separate( `system:index`, c('year', 'allot', 'rep') , sep = '_') %>% 
#   left_join( read_csv('data/temp/allotment_info.csv'), by = 'uname')
# 
# variance_analysis(df1, extraction_number = 1)
# 
# df2 <- read_csv('data/RAP_EE_exports/drive-download-20220623T143258Z-001/RAP_allotment_pixel_samples2.csv') %>% 
#   separate( `system:index`, c('year', 'allot', 'rep') , sep = '_') %>% 
#   left_join( read_csv('data/temp/allotment_info.csv'), by = 'uname')
# 
# variance_analysis(df2, extraction_number = 2)
# 
# df3 <- read_csv('data/RAP_EE_exports/drive-download-20220623T143258Z-001/RAP_allotment_pixel_samples3.csv') %>% 
#   separate( `system:index`, c('year', 'allot', 'rep') , sep = '_') %>% 
#   left_join( read_csv('data/temp/allotment_info.csv'), by = 'uname')
# 
# variance_analysis(df3, extraction_number = 3)
# 
# df6 <- read_csv('data/RAP_EE_exports/drive-download-20220624T013905Z-001/RAP_allotment_pixel_samples6.csv') %>% 
#   separate( `system:index`, c('year', 'allot', 'rep') , sep = '_') %>% 
#   left_join( read_csv('data/temp/allotment_info.csv'), by = 'uname')
# 
# variance_analysis(df6, extraction_number = 6)
# 

df4 <- read_csv('data/RAP_EE_exports/drive-download-20220624T051641Z-001/RAP_allotment_pixel_samples4.csv') %>% 
  separate( `system:index`, c('year', 'allot', 'rep') , sep = '_') %>% 
  left_join( read_csv('data/temp/allotment_info.csv'), by = 'uname')

variance_analysis(df4, extraction_number = 4)

trend_variance_table <- read_csv('output/tables/trend_variance_table4.csv')
var_cols <- scales::hue_pal()(3)

trend_variance_table <- 
  trend_variance_table %>% 
  mutate( scale = factor(scale, levels = c('office', 'allotment', 'pixel'), ordered = T))

varplot <- 
  trend_variance_table %>% 
  filter( extraction_number == 4) %>% 
  ggplot(aes( x = type_label, y = vcov, fill = scale )) + 
  geom_bar(stat = 'identity')  + 
  geom_text( aes( label = bar_lab, y = y_pos), size = 3) + 
  scale_fill_manual(values = var_cols, name = 'Scale') + 
  ylab( "Trend Variance") + 
  theme_bw() + 
  theme(axis.title.x = element_blank(), 
        axis.text.x = element_text( angle = -50, hjust = 0), 
        strip.text = element_text(hjust = 0)) 



ggsave( varplot, 
        filename = paste0( 'output/figures/scale_variance_plot_extraction', extraction_number, '.png'), 
        width = 6.5, 
        height = 4, 
        units = 'in', dpi = 600
)