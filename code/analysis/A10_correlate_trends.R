rm(list = ls())
library(tidyverse)
library(sf)
library(lme4)
library(gridExtra)

source('code/analysis/functions.R')
source('code/analysis/parameters.R')

format_ranef_trends <- function( model, type, model_scale){ 

  re_df <- ranef(model)
  scales <- names(re_df)
  
  helper_scale_fun <- function( temp_df, type, scale ) { 
    
    if( any(names(temp_df) == 'year2') ) { 
      temp_df <- temp_df %>% 
        rename( 't' = year2 )
    }
    

    temp_df %>% 
      mutate( scale = scale) %>% 
      mutate( ID = rownames(.)) %>% 
      mutate( type = type ) %>% 
      rename( "trend" = t )  %>% 
      select( trend, scale, ID, type )
  }
  
   
  out <- mapply(FUN = helper_scale_fun, temp_df = re_df, type = type, scale = scales, SIMPLIFY = F) %>% 
    do.call( rbind, . ) %>% 
    mutate( model_scale = model_scale )
  
  return(out)

}

m_afg <- read_rds('data/temp/afg_pixel_trends_model_extraction_n4.rds')
m_pfg <- read_rds('data/temp/pfg_pixel_trends_model_extraction_n4.rds')
m_bgr <- read_rds('data/temp/bgr_pixel_trends_model_extraction_n4.rds')
m_shr <- read_rds('data/temp/shr_pixel_trends_model_extraction_n4.rds')

afg_ranef <- format_ranef_trends(m_afg, type = 'Annual', model_scale = 'Pixel')  
bgr_ranef <- format_ranef_trends(m_bgr, type = 'Bare', model_scale = 'Pixel')  
pfg_ranef <- format_ranef_trends(m_pfg, type = 'Perennial', model_scale = 'Pixel')
shr_ranef <- format_ranef_trends(m_shr, type = 'Shrub', model_scale = 'Pixel')  

make_pairwise <- function( ranef1, ranef2){ 
  n1 <- unique(ranef1$type)[1]
  n2 <- unique(ranef2$type)[1]
  ranef1 %>% 
    left_join(ranef2, by = c('scale', 'ID', 'model_scale')) %>% 
    mutate( pair = paste0( c(n1,n2), collapse = '_')) %>% 
    filter( complete.cases(.))
}

# six pairs 
afg_pfg <- make_pairwise(afg_ranef, pfg_ranef)
afg_bgr <- make_pairwise(afg_ranef, bgr_ranef)
afg_shr <- make_pairwise(afg_ranef, shr_ranef)
bgr_pfg <- make_pairwise(bgr_ranef, pfg_ranef)
bgr_shr <- make_pairwise(bgr_ranef, shr_ranef)
pfg_shr <- make_pairwise(pfg_ranef, shr_ranef)

cortest_pairs <- function( pair_df  ) { 
  
  pairNames <- str_split( unique( pair_df$pair ), pattern = '_')[[1]] 
  
  res <- pair_df %>% 
    group_by( scale, pair, model_scale) %>% 
    summarise(n = n(), corResPearson = list(cor.test(trend.x, trend.y))) %>% 
    ungroup() 
  
  bind_cols( res, 
             lapply( res$corResPearson, 
                     function( x) data.frame(p.val = x$p.val, r = x$estimate)) %>% 
               do.call( rbind, . )) %>% 
    mutate(significant = ifelse( p.val < 0.05, '*', 'n.s.'))
}


xvals <- afg_bgr %>% 
  filter( pair == "Annual_Bare", scale == 'pixel')  %>% 
  select( trend.x, trend.y) %>% pull( trend.x)
yvals <- afg_bgr %>% 
  filter( pair == "Annual_Bare", scale == 'pixel')  %>% 
  select( trend.x, trend.y) %>% pull( trend.y)

pixel_model_cortests <- bind_rows(
  cortest_pairs(afg_bgr), 
  cortest_pairs(afg_pfg), 
  cortest_pairs(afg_shr), 
  cortest_pairs(bgr_pfg), 
  cortest_pairs(bgr_shr), 
  cortest_pairs(pfg_shr)
)

# Get allotment level models --------------------------------------- # 
m_afg_allotment <- read_rds('output/AFG_cover_trend_model.rds')
m_pfg_allotment <- read_rds('output/PFG_cover_trend_model.rds')
m_bgr_allotment <- read_rds('output/BG_cover_trend_model.rds')
m_shr_allotment <- read_rds('output/SHR_cover_trend_model.rds')

afg_ranef2 <- format_ranef_trends(m_afg_allotment, type = 'Annual', model_scale = 'Allotment')
pfg_ranef2 <- format_ranef_trends(m_pfg_allotment, type = 'Perennial', model_scale = 'Allotment')
bgr_ranef2 <- format_ranef_trends(m_bgr_allotment, type = 'Bare', model_scale = 'Allotment')
shr_ranef2 <- format_ranef_trends(m_shr_allotment, type = 'Shrub', model_scale = 'Allotment')

get_fixTrends <- function( model , type ) { 
  emtrends(model, ~ ecoregion, "t") %>% 
    data.frame() %>% 
    mutate( scale = 'ecoregion', model_scale = 'Allotment', type = type) %>% 
    rename( "ID" = ecoregion) %>%
    rename( 'trend' = t.trend) %>% 
    select( type , scale, ID, trend, model_scale)
    
}

afg_trends <- 
  get_fixTrends(m_afg_allotment, 'Annual') %>% 
  bind_rows( afg_ranef2) 

pfg_trends <- 
  get_fixTrends(m_pfg_allotment, 'Perennial') %>% 
  bind_rows( pfg_ranef2) 

bgr_trends <- 
  get_fixTrends(m_bgr_allotment, 'Bare') %>% 
  bind_rows( bgr_ranef2) 

shr_trends <- 
  get_fixTrends(m_shr_allotment, 'Shrub') %>% 
  bind_rows(shr_ranef2) 

# make pairs 
afg_pfg2 <- make_pairwise(afg_trends, pfg_trends)
afg_bgr2 <- make_pairwise(afg_trends, bgr_trends)
afg_shr2 <- make_pairwise(afg_trends, shr_trends)
bgr_pfg2 <- make_pairwise(bgr_trends, pfg_trends)
bgr_shr2 <- make_pairwise(bgr_trends, shr_trends)
pfg_shr2 <- make_pairwise(pfg_trends, shr_trends)

# get correlations 
allotment_model_cortests <- bind_rows(
  cortest_pairs(afg_bgr2), 
  cortest_pairs(afg_pfg2), 
  cortest_pairs(afg_shr2), 
  cortest_pairs(bgr_pfg2), 
  cortest_pairs(bgr_shr2), 
  cortest_pairs(pfg_shr2)
)
allotment_model_cortests %>% 
  filter( scale == 'ecoregion')

unit_scale <- function( y ) { 
  (y - min(y,na.rm = T))/diff(range(y, na.rm = T)) 
}

all_pairs <- list(afg_bgr, afg_pfg, 
                  afg_shr, bgr_pfg, 
                  bgr_shr, pfg_shr, 
                  afg_bgr2, afg_pfg2, 
                  afg_shr2, bgr_pfg2, 
                  bgr_shr2, pfg_shr2) %>% 
  do.call( bind_rows , . )  %>% 
  mutate( scale_lab = factor( scale, levels = c('ecoregion', 'ecoregion:OFFICE', 'uname', 'pixel'), ordered = T)) %>% 
  mutate( scale_lab = factor( scale_lab, labels = c('Ecoregion', 'Office', 'Allotment', 'Pixel'))) 


allotment_model_cortests %>% write_csv('output/tables/allotment_model_correlations.csv')
pixel_model_cortests %>% write_csv('output/tables/pixel_model_correlations.csv')

corplot_labels <- allotment_model_cortests %>% 
  bind_rows(pixel_model_cortests) %>% 
  mutate( lab = paste( "~ italic(r) ~'='~", round(r, 2) )) %>% 
  mutate( lab = ifelse( p.val > 0.05, '', lab ))

all_pairs <- 
  all_pairs %>% 
  left_join(corplot_labels %>% select( scale, pair, model_scale, p.val) ) %>% 
  mutate( slope = ifelse(p.val < 0.05, T, F)) %>% 
  select( -p.val)

fig_seven_dat <- 
  all_pairs %>% 
  filter( ( model_scale == 'Pixel' & scale == 'pixel') | model_scale == 'Allotment') %>% 
  filter( pair %in% c('Annual_Perennial', 'Annual_Bare')) %>% 
  mutate( ylab = paste0( str_extract( pair, '[A-Za-z]+$'), ' Trend')) %>% 
  group_by( ylab, pair, model_scale, scale ) %>%
  mutate( across( .cols = c('trend.y', 'trend.x'), .fns = unit_scale ))


fig_seven_labels <- 
  fig_seven_dat %>%
  ungroup() %>% 
  distinct( ylab, scale, model_scale, pair, scale_lab ) %>% 
  left_join(corplot_labels %>% ungroup() ) %>% 
  select( ylab, pair, lab, model_scale, scale, scale_lab)

letter_lab <- 
  expand.grid( trend.y = 1, trend.x = 0, 
               ylab = c('Bare Trend', 'Perennial Trend') , 
               scale_lab = c('Ecoregion', 'Office', 'Allotment', 'Pixel')) %>% 
  arrange( ylab ) %>%
  mutate( lab = LETTERS[1:8])


fig7 <- fig_seven_dat %>% 
  ggplot( aes( x = trend.x, y = trend.y ) )  + 
  geom_point(color = 'black', alpha = 0.5) +
  geom_smooth(data = fig_seven_dat %>% filter( slope),  method = 'lm', se = F, size = 0.5) + 
  geom_text( data = fig_seven_labels, 
             aes( label = lab,  
                  x = Inf, y = -Inf), 
             hjust = 1.1, nudge_x = 0,
             vjust = -0.5, parse = T, size = 3) + 
  geom_text(data = letter_lab , aes( label = lab), size = 3)  + 
  facet_grid(  ylab ~ scale_lab, switch = 'y' ) + 
  theme_bw() +
  coord_equal(1) + 
  scale_x_continuous(breaks = c(0.2, 0.5, 0.8))  +
  scale_y_continuous(breaks = c(0.2, 0.5, 0.8))  + 
  xlab( 'Annual Cover Trend')  + 
  #scale_x_continuous(breaks = c( 0, 0.2, 0.4, 0.6, 0.8, 1) , labels = function(x) { sprintf( "%.1f" , x )})
  theme(strip.placement = 'outside', 
        strip.background.y = element_blank(), 
        axis.title.y = element_blank(), 
        axis.text = element_text( size = 7), 
        strip.text.y = element_text( size = 10)) 

ggsave(fig7, filename = 'output/figures/Fig_7_trend_correlation.png', 
       width = 7, height = 4, units = 'in', dpi = 600)


make_panel <- 
  function( sel_pair, pair_df, label_df, title_text  ) { 

  figdat <- 
    pair_df %>% 
    filter( !( model_scale == 'Pixel' & scale != 'pixel')) %>% 
    filter( pair == sel_pair ) %>% 
    mutate( ylab = paste0( str_extract( pair, '[A-Za-z]+$'), ' Trend')) %>% 
    mutate( xlab = paste0( str_extract( pair, '^[A-Za-z]+'), ' Trend')) %>% 
    group_by(xlab, ylab, pair, model_scale, scale ) %>% 
    mutate( across( .cols = c('trend.y', 'trend.x'), .fns = unit_scale ))

  fig_labels <- 
    figdat %>%
    ungroup() %>% 
    distinct( xlab, ylab, scale, model_scale, pair, scale_lab ) %>% 
    left_join(corplot_labels %>% ungroup() ) %>% 
    select( xlab, ylab, pair, lab, model_scale, scale, scale_lab)
  
  xlab <- unique( figdat$xlab )
  
  figdat %>% 
    ggplot( aes( x = trend.x, y = trend.y ) )  + 
    geom_point(color = 'black', alpha = 0.5) +
    geom_smooth(data = figdat %>% filter( slope),  method = 'lm', se = F, size = 0.5) + 
    geom_text( data = fig_labels,
               aes( label = lab,
                    x = Inf, y = -Inf),
               hjust = 1.1, nudge_x = 0,
               vjust = -0.5, parse = T, size = 3) +
    xlab( xlab) + 
    facet_grid(  ylab ~ scale_lab, switch = 'y' ) + 
    theme_bw() +
    coord_equal(1) + 
    scale_x_continuous(breaks = c(0.2, 0.5, 0.8))  +
    scale_y_continuous(breaks = c(0.2, 0.5, 0.8))  + 
    #scale_x_continuous(breaks = c( 0, 0.2, 0.4, 0.6, 0.8, 1) , labels = function(x) { sprintf( "%.1f" , x )})
    theme(strip.placement = 'outside', 
          strip.background.y = element_blank(), 
          axis.title.y = element_blank(), 
          axis.text = element_text( size = 7), 
          strip.text.y = element_text( size = 10)) + 
    ggtitle(title_text)
}

# First one in pair goes on y-axis 

corplot_labels

#p1 <- make_panel(sel_pair = 'Annual_Bare', all_pairs, label_df = corplot_labels, title_text = 'A) Annual vs. Bare')
#p2 <- make_panel(sel_pair = 'Annual_Perennial', all_pairs, label_df = corplot_labels, title_text = 'B) Annual vs. Perennial')
p3 <- make_panel(sel_pair = 'Annual_Shrub', all_pairs, label_df = corplot_labels, title_text = 'A) Annual vs. Shrub')
p4 <- make_panel(sel_pair = 'Bare_Perennial', all_pairs, label_df = corplot_labels, title_text = 'B) Bare vs. Perennial')
p5 <- make_panel(sel_pair = 'Bare_Shrub', all_pairs, label_df = corplot_labels, title_text = 'C) Bare vs. Shrub')
p6 <- make_panel(sel_pair = 'Perennial_Shrub', all_pairs, label_df  = corplot_labels, title_text = 'D) Perennial vs. Shrub')

ggsave(plot = grid.arrange( p3, p4, p5, p6, nrow = 4, ncol = 1), 
       filename =  'output/figures/Fig_A3_trend_correlations.png', 
       width = 8, 
       height = 9, 
       units = 'in', 
       dpi = 600)


