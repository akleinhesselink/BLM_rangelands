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
    summarise( corRes = list(cor.test(trend.x, trend.y))) %>% 
    ungroup() 
  
  bind_cols( res, 
             lapply( res$corRes, 
                     function(x ) data.frame( n = x$parameter, p.val = x$p.value, r = x$estimate)) %>% 
               do.call( rbind, . ) ) %>% 
    mutate(significant = ifelse( p.val < 0.05, '*', 'n.s.'))
}

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
  emtrends(model, ~ ecoregion, "year2") %>% 
    data.frame() %>% 
    mutate( scale = 'ecoregion', model_scale = 'Allotment', type = type) %>% 
    rename( "ID" = ecoregion) %>%
    rename( 'trend' = year2.trend) %>% 
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
afg_shr2 <- make_pairwise(afg_trends, shr_ranef)
bgr_pfg2 <- make_pairwise(bgr_trends, pfg_ranef)
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

unit_scale <- function( y ) { 
  (y - min(y,na.rm = T))/diff(range(y, na.rm = T)) 
}
pixel_model_cortests <- bind_rows(
  cortest_pairs(afg_bgr), 
  cortest_pairs(afg_pfg), 
  cortest_pairs(afg_shr), 
  cortest_pairs(bgr_pfg), 
  cortest_pairs(bgr_shr), 
  cortest_pairs(pfg_shr)
)
all_pairs <- list(afg_bgr, afg_pfg, 
                  afg_shr, bgr_pfg, 
                  bgr_shr, pfg_shr, 
                  afg_bgr2, afg_pfg2, 
                  afg_shr2, bgr_pfg2, 
                  bgr_shr2, pfg_shr2) %>% 
  do.call( bind_rows , . )  %>% 
  mutate( scale_lab = factor( scale, levels = c('ecoregion', 'ecoregion:OFFICE', 'uname', 'pixel'), ordered = T)) %>% 
  mutate( scale_lab = factor( scale_lab, labels = c('Ecoregion', 'Office', 'Allotment', 'Pixel'))) 

corplot_labels <- allotment_model_cortests %>% 
  bind_rows(pixel_model_cortests) %>% 
  mutate( lab = paste( "~italic(r)~'='~", round(r, 2) )) %>% 
  mutate( lab = ifelse( p.val > 0.05, '', lab ))

all_pairs <- 
  all_pairs %>% 
  left_join(corplot_labels %>% select( scale, pair, model_scale, p.val) ) %>% 
  mutate( slope = ifelse(p.val < 0.05, T, F)) %>% 
  select( -p.val)

fig_seven_dat <- 
  all_pairs %>% 
  filter( !( model_scale == 'Pixel' & scale != 'pixel')) %>% 
  filter( pair %in% c('Perennial_Annual', 'Bare_Annual')) %>% 
  mutate( xlab = paste0( str_extract( pair, '[A-Za-z]+'), ' Trend')) %>% 
  group_by( xlab, pair, model_scale, scale ) %>% 
  mutate( across( .cols = c('trend.y', 'trend.x'), .fns = unit_scale ))

fig_seven_labels <- 
  fig_seven_dat %>%
  ungroup() %>% 
  distinct( xlab, scale, model_scale, pair, scale_lab ) %>% 
  left_join(corplot_labels %>% ungroup() ) %>% 
  select( xlab, pair, lab, model_scale, scale, scale_lab)


letter_lab <- 
  expand.grid( trend.y = 0, trend.x = 1, 
               xlab = c('Bare Trend', 'Perennial Trend') , 
               scale_lab = c('Ecoregion', 'Office', 'Allotment', 'Pixel')) %>% 
  arrange( xlab ) %>%
  mutate( lab = LETTERS[1:8])


fig7 <- fig_seven_dat %>% 
  ggplot( aes( x = trend.y, y = trend.x ) )  + 
  geom_point(color = 'black', alpha = 0.5) +
  geom_smooth(data = fig_seven_dat %>% filter( slope),  method = 'lm', se = F, size = 0.5) + 
  geom_text( data = fig_seven_labels, 
             aes( label = lab,  
                  x = Inf, y = -Inf), 
             hjust = 1.1, nudge_x = 0,
             vjust = -0.5, parse = T, size = 3) + 
  geom_text(data = letter_lab , aes( label = lab), size = 3)  + 
  facet_grid(  xlab ~ scale_lab, switch = 'y' ) + 
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
    mutate( ylab = paste0( str_extract( pair, '[A-Za-z]+'), ' Trend')) %>% 
    mutate( xlab = paste0( str_extract( pair, '[A-Za-z]+$'), ' Trend')) %>% 
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
    ggplot( aes( x = trend.y, y = trend.x ) )  + 
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
all_pairs$pair %>% unique
p1 <- make_panel(sel_pair = 'Bare_Annual', all_pairs, label_df = corplot_labels, title_text = 'Annual vs. Bare')
p2 <- make_panel(sel_pair = 'Perennial_Annual', all_pairs, label_df = corplot_labels, title_text = 'Annual vs. Perennial')
p3 <- make_panel(sel_pair = 'Perennial_Bare', all_pairs, label_df = corplot_labels, title_text = 'Bare vs. Perennial')
p4 <- make_panel(sel_pair = 'Perennial_Shrub', all_pairs, label_df = corplot_labels, title_text = 'Shrub vs. Perennial')
p5 <- make_panel(sel_pair = 'Shrub_Annual', all_pairs, label_df = corplot_labels, title_text = 'Annual vs. Shrub')
p6 <- make_panel(sel_pair = 'Shrub_Bare', all_pairs, label_df  = corplot_labels, title_text = 'Bare vs. Shrub')


ggsave(plot = grid.arrange( p1, p2, p3, p4, p5, p6, nrow = 3, ncol = 2), 
       filename =  'output/figures/Fig_S5_trend_correlations.png', 
       width = 14, 
       height = 8, 
       units = 'in', 
       dpi = 600)



get_pairs <- function(vars = c('AFG', 'BG'), cutoff = c(0.3, 0.3)){ 
  pixel_file <- paste0( 'data/old_RAP_EE_exports/', paste( tolower(vars), collapse = '_'), '_paired_samples.csv')
  pix_dat <- read_csv(pixel_file)
  col_names <- names(pix_dat) 
  
  col_names[ str_detect( col_names, vars[1]) ] <- vars[1]
  col_names[ str_detect( col_names, vars[2]) ] <- vars[2]
  names( pix_dat ) <- col_names
  
  
  out <- 
    ecoregion_year2 %>%
    bind_rows(
      office_year2 
    ) %>% 
    bind_rows(
      allotment_year2  
    ) %>%
    bind_rows( 
        pix_dat %>% 
        filter( abs( .data[[vars[[1]]]])  < cutoff[1]) %>% 
        filter( abs( .data[[vars[[2]]]])   < cutoff[2]) %>% 
        mutate( scale = 'Pixel' )
    ) %>% 
    mutate( scale = factor(scale, 
                           levels = c('Ecoregion', 'Office', 'Allotment', 'Pixel'), 
                           ordered = T)) %>% 
    group_by( scale ) %>%
    mutate_at( vars, unit_scale)  
  
  return(out ) 
}



model_fls <- dir('output', pattern = 'cover_trend_model.rds' ,full.names =T )
model_names <- str_extract(basename(model_fls), pattern = '[A-Za-z]+_[a-z]+')

model_list <- data.frame( model_names = model_names , file = model_fls) %>% 
  separate( model_names , c('type', 'unit')) %>%
  mutate( type = str_to_upper(type )) %>% 
  filter( type != 'HERB' , unit != 'agb')

out <- list()

i <- 1 

for( i in 1:nrow( model_list )){ 
  
  temp <- ranef( read_rds(file = model_list$file[i])) 
  temp <- lapply( temp, as.data.frame ) 
  
  temp <- format_ranefs(temp, model_list$type[i])
  fixef_temp <- get_ecoregion_trends(read_rds(file = model_list$file[[i]]))
  fixef_temp <- data.frame( Group = names( fixef_temp), 
              scale = 'Ecoregion', 
              type = model_list$type[i], year2 = fixef_temp  )
  
  temp <- do.call(bind_rows, temp )    
  
  temp <- 
    temp %>% 
    bind_rows(fixef_temp)
  
  out[[i ]] <- temp 
  
} 

out_all <- do.call(bind_rows, out ) 

office_year2 <- out_all %>% 
  pivot_longer(cols=c(`(Intercept)`, year2), names_to = 'param', values_to = 'value' ) %>% 
  filter( scale == 'Office', param == 'year2') %>% 
  pivot_wider(names_from = type, values_from = value )

allotment_year2 <- out_all %>% 
  pivot_longer(cols=c(`(Intercept)`, year2), names_to = 'param', values_to = 'value' ) %>% 
  filter( scale == 'Allotment', param == 'year2') %>% 
  pivot_wider(names_from = type, values_from = value )

ecoregion_year2 <- out_all %>% 
  pivot_longer(cols=c(`(Intercept)`, year2), names_to = 'param', values_to = 'value' ) %>% 
  filter( scale == 'Ecoregion', param == 'year2') %>% 
  pivot_wider(names_from = type, values_from = value )

p1 <- make_panel(vars = c('AFG', 'BG'), title_text = 'A) Annuals vs. Bare Ground') 
p2 <- make_panel(vars = c('AFG', 'PFG'), title_text = 'B) Annuals vs. Perennials') 
p3 <- make_panel(vars = c('AFG', 'SHR'), title_text = 'C) Annuals vs. Shrubs', c(0.3, 0.2)) 
p4 <- make_panel(vars = c('PFG', 'BG'), title_text = 'D) Perennials vs. Bare Ground') 
p5 <- make_panel(vars = c('PFG', 'SHR'), title_text = 'E) Perennials vs. Shrubs', c(0.3, 0.2)) 
p6 <- make_panel(vars = c('SHR', 'BG'), title_text = 'F) Shrubs vs. Bare Ground' , c(0.2, 0.3)) 

afg_v_BG_PFG <- p1$data %>% 
  select( Group, scale, param, AFG, BG, dum, pval, .group) %>% 
  pivot_longer(cols = c(BG), names_to = 'type')  %>% 
  bind_rows(
    p2$data %>% 
    select( Group, scale, param, AFG, PFG, dum, pval, .group) %>% 
    pivot_longer(cols = c(PFG), names_to = 'type') 
  )  %>% 
  mutate( type = factor( type, labels = c("Bare Ground Trend", "Perennial Cover Trend")))


labels <- afg_v_BG_PFG %>% ungroup %>% 
  distinct( type, scale, dum, pval ) %>% 
  mutate( AFG = 0.2, value = 0.1 )


x <- afg_v_BG_PFG %>% 
  group_by(scale,type ) %>% 
  summarise( 
    res = list( cor.test( AFG, value ))
  ) 

x <- x %>% 
  rowwise( ) %>% 
  mutate( r = res$estimate[1] ) %>% 
  mutate( pval = res$p.value[1]) %>% 
  mutate( label = paste( "~italic(r)~'='~", round( r, 2))) %>% 
  filter( pval < 0.05) %>% 
  mutate( AFG = 0.2, value = 0.01) 
  

letter_lab <- 
  expand.grid( AFG = 0, value = 1, 
               type = c('Bare Ground Trend', 'Perennial Cover Trend') , 
               scale = c('Ecoregion', 'Office', 'Allotment', 'Pixel')) %>% 
  arrange( type ) %>%
  mutate( lab = LETTERS[1:8])

fig6 <- afg_v_BG_PFG %>% 
  ggplot( aes( x = AFG, y = value) ) + 
  facet_grid( type ~ scale, switch = 'y' ) + 
  geom_point(alpha = 0.5) + 
  geom_smooth( data = afg_v_BG_PFG %>% filter( dum) , se = F, method = 'lm') + 
  geom_text( data = x, 
             aes( label = label ), parse = T) + 
  geom_text(data = letter_lab , aes( label = lab), size = 3)  + 
  theme_bw() + 
  xlab( 'Annual Cover Trend')  + 
  #scale_x_continuous(breaks = c( 0, 0.2, 0.4, 0.6, 0.8, 1) , labels = function(x) { sprintf( "%.1f" , x )})
  theme(strip.placement = 'outside', 
        strip.background.y = element_blank(), 
        axis.title.y = element_blank(), 
        axis.text = element_text( size = 7), 
        strip.text.y = element_text( size = 10)) 


ggsave(fig6, filename = 'output/figures/Fig_6_trend_correlation.png', 
       width = 7, height = 4, units = 'in', dpi = 600)





ggsave(plot = grid.arrange( p1, p2, p3, p4, p5, p6, nrow = 3, ncol = 2), 
       filename =  'output/figures/Fig_S5_trend_correlations.png', 
       width = 14, 
       height = 8, 
       units = 'in', 
       dpi = 600)



a <- read_csv('app/data/allotment_info.csv')

p2$data %>% filter( scale %in% c('Allotment')) %>% 
  mutate( uname = as.numeric(Group)) %>%
  left_join(a, by = 'uname') %>%
  ggplot( aes( x = AFG, y = PFG )) + 
  geom_point() + facet_wrap( ~ ecoregion )


p2$data %>% filter( scale %in% c('Allotment')) %>% 
  mutate( uname = as.numeric(Group)) %>%
  left_join(a, by = 'uname') %>%
  ggplot( aes( x = AFG, y = SHR )) + geom_point() + facet_wrap( ~ ecoregion )


p2$data %>% filter( scale %in% c('Pixel'))

