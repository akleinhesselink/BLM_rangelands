rm(list = ls())
library(tidyverse)
library(sf)

#  Load annual data exported from Earth Engine 
cover <- read_csv( 'data/RAP_EE_exports/allotment_cover_by_year.csv') %>% 
  select( uname, year, AFG, BGR, LTR, PFG, SHR, TRE) %>% 
  mutate( unit = 'cover')

prod <- read_csv('data/RAP_EE_exports/allotment_prod_by_year.csv') %>%
  select( uname, year, contains('NPP')) %>% 
  mutate( unit = 'production')

elevation <- read_csv('data/RAP_EE_exports/allotment_elevation.csv')  %>% 
  select( uname, mean )

# Join the response data together into one long data frame 
annual_data <- prod %>% 
  pivot_longer( contains('NPP'), values_to = 'value' ) %>% 
  bind_rows(
    cover %>% 
      pivot_longer( AFG:TRE, values_to = 'value')
  ) 

write_csv(annual_data, 'data/temp/annual_data.csv')

# Write Elevation 
elevation <- elevation %>% rename( 'elevation' = mean )

read_csv('data/temp/allotment_info.csv') %>% 
  left_join( elevation, by = 'uname') %>% 
  write_csv( 'data/temp/allotment_info.csv')
