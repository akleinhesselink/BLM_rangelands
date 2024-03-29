# Analyses 

rm(list = ls() )
library(tidyverse)
library(sf)
library(lubridate)
library(lme4)
library(emmeans)

source('code/analysis/A1_regional_map.R')
source('code/analysis/A2_format_vegetation_data.R')
source('code/analysis/A3_basic_data_summary.R')
source('code/analysis/A4_fit_veg_trend_models.R')
source('code/analysis/A5_print_model_tables_to_html.R')
source('code/analysis/A6_join_rates_to_shapes.R')
source('code/analysis/A7_field_office_rate_table.R')
source('code/analysis/A8_plot_vegetation_trends.R')
source('code/analysis/A9_multiscale_trend_variance_models.R')
source('code/analysis/A10_correlate_trends.R')
source('code/analysis/A11_annual_dominance_stats.R')
source('code/analysis/A12_tree_non_parametric.R')

source('code/analysis/A13_AIM_genera_table.R') # Depends on other AIM code 
source('code/analysis/A14_new_regional_map_example.R') 