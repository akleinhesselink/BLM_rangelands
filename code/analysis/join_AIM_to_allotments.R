rm(list = ls())
library(tidyverse)
library(sf)
library(lubridate)

spp_attributes <- read_csv('data/AIM/AIM_species_data/CASI_attributed_PJA5May22s.csv')
spp_attributes <- spp_attributes %>% 
  mutate( order = row_number() )  %>% 
  filter(!str_detect( SCIENTIFIC , '=>')) 


allotment_shapes <- read_sf('data/temp/allotments/allotments_with_cover_trends.shp')
AIM_species_raw <- read_csv('data/AIM/AIM_species_data/BLM_species_cover_2022-05-07_AK_edit.csv')
AIM_plots <- read_sf('data/temp/AIM_plots_May17/aim_plots.shp')
AIM_LMF <- read_sf( 'data/temp/AIM_LMF_plots_May17/AIM_LMF_plots.shp')

AIM_species_raw <- AIM_species_raw %>% 
  filter( State != 'AK')

AIM_plots <- AIM_plots %>% 
  filter( State != "AK")

AIM_LMF <- AIM_LMF %>% 
  filter( State != "AK")

AIM_species_raw$PrimaryKey <- str_trim(AIM_species_raw$PrimaryKey)
AIM_species_raw$PlotID <- str_trim(AIM_species_raw$PlotID)

AIM_plots$PrimaryKey <- str_trim(AIM_plots$PrimaryKey)
AIM_plots$PlotID <- str_trim(AIM_plots$PlotID)

AIM_LMF$PrimaryKey <- str_trim( AIM_LMF$PrimaryKey ) 

AIM_plots_combined <- AIM_plots %>% 
  select( PrimaryKey, PlotID, State , geometry) %>% 
  bind_rows( AIM_LMF %>% select( PrimaryKey, PLOTKEY, State, geometry)) 

AIM_species <- AIM_species_raw %>% 
  left_join(AIM_plots_combined, by = "PrimaryKey")

AIM_species %>% 
  filter( State.x == State.y) %>% 
  nrow()

AIM_species %>% 
  filter( State.x != State.y ) %>% 
  nrow()

AIM_species %>% 
  filter( is.na( State.y)) %>% 
  nrow()

AIM_species1 <- AIM_species %>% 
  filter( !st_is_empty(geometry)) %>% 
  st_set_geometry( value = 'geometry') %>% 
  st_set_crs( value = 'EPSG:4269')

AIM_species2 <- AIM_species %>% 
  filter( st_is_empty(geometry))  %>% 
  st_as_sf( coords = c('Longitude_NAD83', 'Latitude_NAD83')) %>% 
  st_set_crs( value = 'EPSG:4269')

AIM_species <- 
  AIM_species1 %>% 
  bind_rows(AIM_species2)

AIM_species %>% 
  filter( st_is_empty(geometry))

AIM_species_allotments <- 
  AIM_species %>% 
  st_transform( crs = st_crs( allotment_shapes )) %>% 
  st_join( allotment_shapes, join = st_within ) 

AIM_species_allotments %>% 
  st_drop_geometry() %>% 
  filter( !is.na(uname)) %>% 
  summarise( n_distinct(PlotID.x))

AIM_species_allotments %>% 
  st_drop_geometry() %>% 
  filter( !is.na(uname)) %>% 
  summarise( n_distinct(PrimaryKey))

AIM_species_allotments %>% 
  st_drop_geometry()  %>% 
  filter( !is.na(uname)) %>% 
  summarise( n_distinct(uname))

AIM_species_allotments %>% 
  st_drop_geometry() %>%
  filter( !is.na(uname)) %>% 
  mutate( loc = paste( Latitude_NAD83, Longitude_NAD83)) %>% 
  summarise( n_distinct(loc))

allotment_veg <- 
  AIM_species_allotments %>%
  st_drop_geometry() %>% 
  filter(!is.na(uname)) %>% 
  group_by( uname ) %>% 
  mutate( nPrimaryKeys = n_distinct(PrimaryKey)) %>%
  group_by( ecoregn, uname, Species, nPrimaryKeys) %>%
  summarise( avg_cover = mean(AH_SpeciesCover, na.rm = T), frequency = n() ) %>% 
  left_join( spp_attributes, by = c('Species' = 'SYMBOL'))

allotments_surveyed_per_ecoregion <- 
  AIM_species_allotments %>% 
  st_drop_geometry() %>% 
  filter( !is.na(uname) ) %>% 
  group_by( ecoregn ) %>% 
  summarise( num_allotments_surveyed = n_distinct(uname))

avg_cover <- 
  allotment_veg %>% 
  group_by( ecoregn , Species ) %>%
  summarise( avg_cover = mean(avg_cover, na.rm = T), 
             frequency = sum( frequency )/sum(nPrimaryKeys), 
             n_allots = n_distinct(uname)) %>% 
  group_by( ecoregn ) %>% 
  arrange(ecoregn, desc( n_allots)) 

avg_cover <- avg_cover %>% 
  left_join(allotments_surveyed_per_ecoregion) %>%
  mutate( allotment_freq = n_allots/num_allotments_surveyed) 

avg_cover %>% 
  write_csv(file = 'data/temp/AIM_species_by_ecoregion.csv')

allotment_veg %>% 
  write_csv(file = 'data/temp/AIM_species_by_allotment.csv')




