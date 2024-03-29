# Miscellaneous helper functions shared by scripts 
require(tidyverse)
require(emmeans)
require(lme4)

decade_function <- function(x ) { 
  paste0( 
    str_pad( 
      floor(( x %% 100 ) %/% 10 )*10, 
      width = 2, 
      side = 'left', 
      pad = '0'), 's')
}


plot_trends <- function( dataset, my_colors ){ 
  # plot annual state of allotment (e.g. cover, production) 
  # by year. 
  # Different colors for different types (e.g. annual, perennial)

  dataset %>% 
    ggplot( aes( x = year , y= value, fill = type, color = type)) + 
    stat_summary(fun.max = function(x) quantile(x, 0.75),
                 fun.min = function(x) quantile(x, 0.25), 
                 geom = 'ribbon', alpha = 0.4, color = NA) + 
    stat_summary(fun = function( x ) median(x), geom = 'line') + 
    scale_fill_manual(values = my_colors) +   
    scale_color_manual(values = my_colors) + 
    theme_bw() + 
    theme( axis.title.x =  element_blank() , legend.title = element_blank() ) 
}


back_transform <- function(x, attributes_obj, log = T ){ 
  
  center <- attributes_obj$`scaled:center`
  sd <- attributes_obj$`scaled:scale`
  
  if( log ){ 
    exp( (sd*x) + center) 
  }else{ 
    sd*x + center 
  }
}


plot_single_allotment_trends <- function( dataset, my_colors ){ 
  dataset %>% 
    ggplot( aes( x = year , y= value, fill = type, color = type)) + 
    geom_line() + 
    scale_color_manual(values = my_colors) + 
    theme_bw() + 
    theme( axis.title.x =  element_blank() , legend.title = element_blank() ) 
}


ecoregion_trends_as_df <- function(trend_model, type){ 
  
  trends <- emmeans::emtrends(trend_model, ~ ecoregion, var = 't') %>% 
    data.frame(type = type)
}

plot_ecoregion_trend_coefficients <- function( trend_summary, my_colors ){ 
  
  trend_summary %>%
    ggplot(aes( x = ecoregion, color = type, 
                y = year2.trend, ymin = asymp.LCL, ymax = asymp.UCL )) + 
    geom_point(position = position_dodge(width = 1)) + 
    geom_errorbar(position = position_dodge( width = 1)) + 
    geom_hline(aes( yintercept = 0), lty = 2, color = 'darkgray') + 
    scale_color_manual(values = my_colors, name = 'Trend') + 
    theme_bw() + 
    theme(axis.text.x =  element_text(angle = 45 , hjust = 1), axis.title.x = element_blank()) + 
    ylab( 'Annual Coefficient')    
  
}

plot_trend_coefficients <- function(beta_table, my_colors) { 
  # beta_table = table of trend coefficients for each ecoregion 
  beta_table %>% 
    ggplot( aes( x = type, 
                 y = year2.trend, 
                 ymin = asymp.LCL, 
                 ymax = asymp.UCL, 
                 color = type )) + 
    geom_point() + 
    geom_errorbar() + 
    geom_hline(aes(yintercept = 0 ), linetype = 2) + 
    facet_grid( ~ ecoregion ) + 
    scale_color_manual(name = 'Functional Type', 
                       values = my_colors) + 
    scale_y_continuous(name = 'Annual Rate') + 
    theme_bw() +   
    theme(axis.text.x = element_blank(),
          axis.title.x = element_blank(), 
          axis.ticks.x = element_blank())
}

plot_trend_coefficients_vertical <- function(my_trend_table, my_colors ) { 
  
  my_trend_table %>%   
    ggplot(aes( x = t.trend, y = type, color = type  )) + 
    geom_vline( aes( xintercept = 0 ), linetype = 2, alpha = 0.5) + 
    geom_point() + 
    geom_errorbar(aes( xmin = asymp.LCL, xmax = asymp.UCL)) + 
    facet_grid( ecoregion ~ . , switch = 'y') + 
    scale_x_continuous(name = 'Trend Coefficient') + 
    scale_color_manual(values = my_colors) + 
    theme_bw() + 
    theme(axis.text.y = element_blank(), 
          axis.title.y = element_blank(), 
          axis.ticks.y = element_blank(), 
          strip.text.y.left = element_text( angle = 0), 
          legend.title = element_blank())
}

# Functions for summarizing trends at the Ecogroup and BLM Admin Level
get_ecoregion_trends <- function( model ){ 
  
  fixeffects <- emtrends(model, ~ ecoregion, 't') %>% 
    as.data.frame()
  
  ecoregion_effect <- fixeffects$t.trend
  names(ecoregion_effect) <-  c( str_trim( fixeffects$ecoregion ) )
  
  return( ecoregion_effect ) 
} 


get_blm_random_effects <- function( model ) { 
  
  re <- ranef(model)
  
  #state <- re$admin_st
  office <- re$`ecoregion:OFFICE`
  allotment <- re$uname
  
  out <- list(office, allotment ) 
  names( out ) <- c("office", "allotment")

  
  out$office$office_label <- row.names( out$office)
  out$allotment$uname <- as.integer( row.names( out$allotment ))

  
  out$office$office_trend <- out$office$t
  out$allotment$allot_trend <- out$allotment$t
  
  return( out )
} 



blm_trend_summary <- function( my_data, ecoregion_trends, group_trends ){ 
  
  if( all( 
    !is.null( nrow( group_trends$allotment) ), 
    !is.null( nrow( group_trends$office)))){ 
    my_data %>% 
      ungroup() %>% 
      distinct(ecoregion, OFFICE, uname) %>%
      mutate( office_label = paste0(ecoregion, ':', OFFICE)) %>% 
      mutate( ecoregion_trend = ecoregion_trends[ecoregion]) %>%
      left_join( group_trends$office, by = 'office_label') %>% 
      left_join( group_trends$allotment, by = 'uname') %>%
      dplyr::select( uname, ecoregion, office_label, 
                     ecoregion_trend, office_trend, allot_trend ) %>%
      rowwise() %>% 
      mutate( full_trend = ecoregion_trend + office_trend + allot_trend )
  }
  
}


back_trans_frames <- function(m, type = "NAME"){ 
  dat <- m@frame
  dat$value <- back_transform( dat$value2, attributes_obj = attributes(dat$value2))
  dat$year <- back_transform( dat$year2, 
                              attributes_obj = c(attributes(dat$year2), 'scaled:scale' = 1), log = F)
  dat$type <- type 
  return( dat )   
}


scale_comparison_df <- function( x ) { 
  
  x %>% 
    distinct(type, uname, allot_trend) %>%
    pivot_wider(names_from = type, values_from = allot_trend ) %>% 
    mutate( scale = 'Allotment') %>% 
    rename( label = uname) %>% 
    mutate( label = as.character(label)) %>% 
    bind_rows(
      x %>% 
        distinct(type, office_label, office_trend) %>%
        pivot_wider(names_from = type, values_from = office_trend ) %>% 
        mutate( scale = 'Field Office') %>%
        rename( label = office_label)
    ) %>% 
    bind_rows(
      x %>% 
        distinct(type, ecoregion, ecoregion_trend) %>%
        pivot_wider(names_from = type, values_from = ecoregion_trend ) %>% 
        mutate( scale = 'Ecoregion') %>% 
        rename( label = ecoregion)
    ) %>% 
    bind_rows(
      x %>% 
        distinct(uname, full_trend, type ) %>%
        pivot_wider(names_from = type, values_from = full_trend ) %>% 
        mutate( scale = 'Total') %>% 
        rename( label = uname) %>% 
        mutate( label = as.character( label )) 
    ) 
  
}

get_rhos <- function( x, vars = c('AFGC', 'PFGC') ) { 
  
  x %>% filter( scale != 'Total') %>% 
    group_by( scale ) %>% 
    summarise( rho = list(cor.test( eval(parse( text = vars[1])), 
                                    eval(parse( text = vars[2]))))) %>% 
    rowwise() %>%
    mutate( label  = paste0('r==', sprintf('%.2f', rho$estimate)))
  
}

make_corplot_by_scale <- function( x, types = c('AFG', 'BG') ){ 
  
  correlations <- 
    x %>% 
    filter(type %in% types) %>% 
    select(ecoregion, office_label, uname, type, allot_trend, 
           office_trend, ecoregion_trend, full_trend )
  
  scale_comparison <- scale_comparison_df(correlations)
  scale_rhos <- get_rhos(scale_comparison, vars = types)
  
  return( 
    scale_comparison %>% 
      filter( scale != 'Total' )  %>% 
      ggplot( aes_string( x = types[1], y = types[2])  ) + 
      geom_point() + 
      stat_smooth(method = 'lm', se = F, size = 0.5) + 
      geom_text( data = scale_rhos, 
                 aes( x = Inf, y = Inf, label = label), vjust = 2, hjust = 1.1, parse = T) +  
      facet_wrap( ~ factor( scale, levels = c('Ecoregion', 'Field Office', 'Allotment'), ordered = T), 
                  scale = 'free') +
      theme_bw() 
  )
}


ecoregion_detail_plot <- function( x, 
                                  sel_type = 'Bare', 
                                  sel_ecoregion = 'Warm Deserts', 
                                  my_colors = 'black'){ 
  
  
  title <- paste0(  paste( sel_type, sep = '&' ), 'Cover: ', 
                   paste( sel_ecoregion))   
  
  
  if(length(sel_type) > 1 ){   
    
  gg_out <- x %>% 
      filter( type %in% sel_type, 
              ecoregion == sel_ecoregion ) %>% 
      ggplot( aes( x = year, y = value, color = type )) + 
      geom_line(size = 0.2, alpha = 0.5, aes( group = paste(uname, type))) + 
      scale_y_log10(name = 'Cover (%)') + 
      scale_x_continuous(breaks = c(1995, 2005, 2015)) + 
      scale_color_manual(values = my_colors) + 
      ggtitle(title) + 
      theme_bw() 
    
  }else if( length(sel_type) == 1 ){
  gg_out <- x %>% 
      filter( type == sel_type, ecoregion == sel_ecoregion ) %>% 
      ggplot( aes( x = year, y = value, group = uname)) + 
      geom_line(size = 0.2, alpha = 0.5) + 
      scale_y_log10(name = 'Cover (%)') + 
      scale_x_continuous(breaks = c(1995, 2005, 2015)) + 
      ggtitle(title) + 
      theme_bw()
  }
  if( n_distinct( gg_out$data$district_label) > 2 ){
    gg_out <- 
      gg_out + 
      facet_wrap( ~ district_label)
  }
  
  return( gg_out )
}
