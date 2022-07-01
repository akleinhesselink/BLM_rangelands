rm(list = ls() )

library(raster)
library(tidyverse)

focal_allot <- raster::brick( 'data/RAP_EE_exports/example_allotment_cover.tif')

afgc0 <- 
  focal_allot$AFG %>% 
  as.data.frame(xy = T) 

afgc1 <- 
  focal_allot$AFG_1 %>% 
  as.data.frame(xy = T) %>%
  rename( 'AFG' = 'AFG_1')

afgc2 <- 
  focal_allot$AFG_2 %>% 
  as.data.frame(xy = T) %>%
  rename( 'AFG' = 'AFG_2')

unloadNamespace( 'raster')
library(tidyverse)
library(sf)
source('code/analysis/functions.R')
source('code/analysis/parameters.R')

allotment_info <- read_csv('data/temp/allotment_info.csv') %>% 
  filter( ecoregion != 'Marine West Coast Forest')

#BLM_districts <- read_rds('data/temp/cleaned_BLM_district_shapes.rds')
BLM_districts <- st_read( 'data/spatial/BLM_National_Administrative_Units/admu.gdb/', layer = 'blm_natl_admu_dist_poly_webpub') %>%
  filter( ADMIN_ST != "AK") %>% 
  st_cast('MULTIPOLYGON') %>% 
  st_make_valid()  %>% 
  st_transform( crs = 'epsg:5070')

#BLM_offices <- read_rds('data/temp/cleaned_BLM_field_office_shapes.rds')
BLM_offices <- st_read('data/temp/BLM_field_offices_cleaned/field_offices.shp', 
         layer = 'field_offices')

ecoregions <- read_rds('data/temp/simplified_ecoregion_shapefile.rds') %>% 
  filter( ecoregion != 'Marine West Coast Forest') 
allotment_shapes <- sf::read_sf('data/temp/allotments/allotments_clean.shp')

allotment_shapes_ecoregion <- 
  allotment_shapes %>% 
  select(uname) %>%
  left_join(allotment_info %>% select(uname, ecoregion), by = 'uname') %>% 
  filter( !is.na(ecoregion)) %>% 
  mutate( geometry = st_buffer(geometry, 200)) %>% 
  st_simplify(dTolerance = 200) %>%
  st_make_valid() %>% 
  group_by( ecoregion ) %>% 
  summarise( geometry = st_union( geometry)) %>% 
  st_make_valid() 
  
allotment_shapes_ecoregion <- 
  allotment_shapes_ecoregion %>%
  mutate(ecoregion = factor(ecoregion)) %>%
  mutate(ecoregion = factor(
    ecoregion,
    labels = c(
      'AZ/NM Highlands',
      'E Cold Deserts',
      'Forested Mts',
      'Mediterranean California',
      'N Great Plains',
      'S Great Plains',
      'W Cold Deserts',
      'Warm Deserts'
    )
  ))

# Make Maps 
BLM_districts <- BLM_districts %>% 
  filter( !PARENT_NAME  %in% c('NORTHEASTERN STATES DISTRICT OFFICE', 
                               'SOUTHEASTERN STATES DISTRICT OFFICE' )) %>% 
  st_transform(crs = st_crs(allotment_shapes_ecoregion)) %>% 
  st_simplify(dTolerance = 1000) %>% 
  st_make_valid()

BLM_offices <- BLM_offices %>%
  filter( ADMU_NA != 'OKLAHOMA FIELD OFFICE') %>% 
  st_transform(crs = st_crs(allotment_shapes_ecoregion)) %>% 
  st_simplify(dTolerance = 100) %>% 
  st_make_valid()

# States shapefile 
state_layer <- tigris::states(resolution = '20m') %>% 
  filter( STUSPS %in% WESTERN_STATES ) %>% 
  st_transform(crs = st_crs(allotment_shapes_ecoregion)) %>% 
  st_simplify(dTolerance = 100) %>%
  st_make_valid()

names( ecoregion_colors)[ order(names( ecoregion_colors)) ]  <- 
  c(
    'AZ/NM Highlands',
    'E Cold Deserts',
    'Forested Mts',
    'Mediterranean California',
    'N Great Plains',
    'S Great Plains',
    'W Cold Deserts',
    'Warm Deserts'
    )

display_proj <- st_crs( '+proj=laea +lat_0=40.00 +lon_0=-109.00 +x_0=0 +y_0=0')

state_layer <- state_layer %>% 
  st_transform(display_proj)

allotment_shapes_ecoregion <- allotment_shapes_ecoregion %>% 
  st_transform(display_proj)

BLM_offices <- BLM_offices %>% 
  st_transform(display_proj)

BLM_districts <- BLM_districts %>% 
  st_transform(display_proj)

full_map <- 
  state_layer %>% 
    ggplot() + 
    geom_sf( fill = 'white', size = 0.2) + 
    geom_sf( data = allotment_shapes_ecoregion, aes( fill = ecoregion), 
             alpha = 0.9, color= NA) + 
    scale_fill_manual( name = 'Ecoregion', values = ecoregion_colors) + 
    # geom_sf( data= allotment_shapes_ecoregion, aes( fill = ecoregion), size = 0.1,
    #         fill = NA, alpha = 0.9) +
    scale_color_manual(name = 'Ecoregion', values = ecoregion_colors) + 
    #geom_sf(data = BLM_districts, fill = NA, color = NA) + 
    #xlim( c(-126, -95)) + 
    #ylim( c(31.5, 48.5)) + 
    geom_sf(data = BLM_offices %>% distinct(geometry), 
            fill = NA, size = 0.1, alpha = 0.5) + 
    geom_sf(fill = NA, size = 0.2, alpha = 0.5) + 
    theme( legend.position = c(0.9, 0.3), 
           legend.box.background = element_rect(color = 1, size = 0.2)) 

ggsave(full_map, 
       filename = 'output/figures/Fig_1_Ecoregion_Map.png',
          width = 10, height = 5.5, units = 'in', dpi = 400)


# Ecoregion Map 
west_cold_deserts_allots <- 
  allotment_shapes %>% 
  filter( ecoregn == 'W Cold Deserts') %>%
  st_transform(crs = "EPSG:4326") 

state_layer <- state_layer %>% st_transform(crs = "EPSG:4326")

regional_map <- 
  state_layer %>% 
  ggplot() + 
  geom_sf( fill = 'white', size = 0.2, color = 'white') + 
  geom_sf( data = west_cold_deserts_allots, 
           fill = ecoregion_colors["W Cold Deserts"], 
           color = 'white', size = 0.05) + 
  xlim( c(-121, -111)) + 
  ylim( c(36.2, 45.5)) + 
  geom_sf(data = BLM_offices %>% distinct(geometry), 
          fill = NA, size = 0.1, alpha = 0.8) + 
  geom_sf(fill = NA, size = 0.5, alpha = 0.5) + 
  geom_sf(data = BLM_offices %>% 
            filter( ADM_UNI == "IDT01000") %>% distinct(geometry) , 
          fill = NA, size = 0.4, color = 'blue', alpha = 0.3) + 
  ggspatial::annotation_scale( location = 'br')  + 
  theme( legend.position = 'none', 
         panel.grid = element_blank(), 
         #axis.text = element_blank(), 
         #axis.ticks = element_blank(), 
         panel.border = element_rect(fill = NA), 
         legend.box.background = element_rect(color = 1, size = 0.2)) 

ggsave( 
  regional_map  + 
    theme(axis.text = element_text( size = 6)),
  filename = 'output/figures/West_Cold_Desert_Map.png', 
  width = 4, 
  height = 5, 
  units = 'in'
  )

# field office map
# focal allotment loaded at the head of script 
afgc0$AFG[afgc0$AFG == 0 ] <- NA

afgc1$AFG[afgc1$AFG == 0 ] <- NA

afgc2$AFG[afgc2$AFG == 0 ] <- NA

unloadNamespace('raster')
source('code/analysis/parameters.R')

BLM_offices <- 
  BLM_offices %>% 
  st_transform( crs = 'EPSG:4326') %>% 
  filter( str_detect( ADM_UNI, 'ID|NV'))

#
Jarbidge_FO <- 
  BLM_offices %>% 
  filter( ADM_UNI == "IDT01000" ) 


Jarbidge_allotments <- 
  allotment_shapes %>% 
  filter( ADMIN_S == "ID", ADM_OFC == 'T01000') %>% 
  st_transform(crs = 'EPSG:4326')

Jarbidge_FO <- 
  Jarbidge_allotments %>% 
  dplyr::select( geometry) %>%
  bind_rows(
  Jarbidge_FO) %>% 
  summarise( geometry = st_union(geometry))  %>%
  st_make_valid()

jarbidge_map <- Jarbidge_FO %>% 
  ggplot() + 
  geom_sf(size = 0.5, 
          fill = 'white') + 
  geom_sf(data = Jarbidge_allotments, 
          size = 0.2, 
          fill = ecoregion_colors['W Cold Deserts'], 
          color = 'white')  + 
  geom_sf(data = Jarbidge_allotments %>% filter( uname == 4304), 
          size = 0.8, 
          fill = ecoregion_colors['W Cold Deserts'], 
          color = 'blue', show.legend = F)  + 
  geom_tile( data = afgc0, aes( x = x, y= y, fill = AFG ), 
             color = NA, show.legend = F) + 
  scico::scale_fill_scico(palette = 'bamako', 
                          direction = -1, 
                          na.value = NA) + 
  xlim( c(-115.8, -114.7) ) + 
  ylim( c(41.9, 43.0)) + 
  geom_sf(size = 0.5, 
          color = 'black', 
          fill = NA) + 
  theme_bw() + 
  theme(panel.grid = element_blank(), 
        axis.text = element_blank(),
        axis.ticks = element_blank(), 
    axis.title =  element_blank()) +
  ggspatial::annotation_scale( location = 'br') 

ggsave(jarbidge_map,  filename = 'output/figures/JarbidgeFieldOffice.png', 
          width = 4, height = 5)

# -------------------------------- # 

# single allotment --------- # 
single_allot1 <- Jarbidge_FO %>% 
  ggplot() + 
  geom_sf(size = 0.5, 
          fill = 'white') + 
  geom_sf(data = Jarbidge_allotments, 
          size = 0.2, 
          alpha = 0.5, 
          fill = ecoregion_colors['W Cold Deserts'], 
          color = 'white')  + 
  geom_tile( data = afgc0, aes( x = x, y= y, fill = AFG ), 
             color = NA, show.legend = F, alpha = 0.8) +   
  geom_sf( data = Jarbidge_allotments %>% filter( uname == 4304),
           fill = ecoregion_colors['West Cold Deserts'], color = 'black', size = 0.5) + 
  scico::scale_fill_scico(palette = 'bamako', 
                          direction = -1, 
                          na.value = NA) + 
  xlim( c(-115.08, -114.94) ) + 
  ylim( c(42.135, 42.26)) + 
  #xlim( c(-115.08, -114.8) ) + 
  #ylim( c(42.085, 42.26)) + 
  geom_sf(size = 0.5, 
          color = 'black', 
          fill = NA) + 
  theme_bw() + 
  theme(#panel.border = element_blank(), 
        panel.grid = element_blank(), 
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title =  element_blank())
  #ggspatial::annotation_scale( location = 'br')  

ggsave( single_allot1, filename = 'output/figures/SingleAllotment.png', bg = 'transparent', 
          width = 4, height = 5)

# Floating image on white 
allotment_year_one_floating <- Jarbidge_FO %>%
  ggplot() + 
  geom_sf( data = Jarbidge_allotments %>% filter( uname == 4304),
           fill = ecoregion_colors['West Cold Deserts'], color = 'black', size = 0.5) + 
  geom_tile( data = afgc0, aes( x = x, y= y, fill = AFG ), 
             color = NA, show.legend = F, alpha = 0.8) +   
  geom_sf( data = Jarbidge_allotments %>% filter( uname == 4304),
           fill = NA, color = 'black', size = 0.6) +
  scico::scale_fill_scico(palette = 'bamako', 
                          direction = -1, 
                          na.value = NA) + 
  xlim( c(-115.08, -114.94) ) + 
  ylim( c(42.135, 42.26)) + 
  theme_bw() + 
  theme( panel.border = element_blank(),
         axis.line = element_blank(), 
         panel.grid = element_blank(), 
         panel.background = element_rect(fill = "transparent",colour = NA), # or theme_blank()
         plot.background = element_rect(fill = 'transparent', colour = NA),
         axis.text = element_blank(),
         axis.ticks = element_blank(),
         axis.title =  element_blank())
#ggspatial::annotation_scale( location = 'br')   + 
ggsave(allotment_year_one_floating, filename = 'output/figures/SingleAllotment_year1.png', 
          width = 4, height = 5, bg = 'transparent')


allotment_year_two_floating <- 
  Jarbidge_FO %>%
  ggplot() + 
  geom_sf( data = Jarbidge_allotments %>% filter( uname == 4304),
           fill = ecoregion_colors['West Cold Deserts'], color = 'black', size = 0.5) + 
  geom_tile( data = afgc1, aes( x = x, y= y, fill = AFG ), 
             color = NA, show.legend = F, alpha = 0.8) +   
  geom_sf( data = Jarbidge_allotments %>% filter( uname == 4304),
           fill = NA, color = 'black', size = 0.6) +
  scico::scale_fill_scico(palette = 'bamako', 
                          direction = -1, 
                          na.value = NA) + 
  xlim( c(-115.08, -114.94) ) + 
  ylim( c(42.135, 42.26)) + 
  theme_bw() + 
  theme( panel.border = element_blank(),
    axis.line = element_blank(), 
         panel.grid = element_blank(), 
         panel.background = element_rect(fill = "transparent",colour = NA), # or theme_blank()
         plot.background = element_rect(fill = 'transparent', colour = NA),
         axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title =  element_blank()) 
  #ggspatial::annotation_scale( location = 'br')   + 


ggsave( allotment_year_two_floating, filename = 'output/figures/SingleAllotment_year2.png', 
        width = 4, height = 5, bg = 'transparent')

#

allotment_year_three_floating <- 
  Jarbidge_FO %>%
  ggplot() + 
  geom_sf( data = Jarbidge_allotments %>% filter( uname == '5339'),
           fill = ecoregion_colors['West Cold Deserts'], color = 'black', size = 0.5) + 
  geom_tile( data = afgc2, aes( x = x, y= y, fill = AFG ), 
             color = NA, show.legend = F, alpha = 0.8) +   
  geom_sf( data = Jarbidge_allotments %>% filter( uname == '5339'),
           fill = NA, color = 'black', size = 0.1) +
  scico::scale_fill_scico(palette = 'bamako', 
                          direction = -1, 
                          na.value = NA) + 
  xlim( c(-115.08, -114.94) ) + 
  ylim( c(42.135, 42.26)) + 
  theme_bw() + 
  theme( panel.border = element_blank(), 
         axis.line = element_blank(), 
         panel.grid = element_blank(), 
         axis.text = element_blank(),
         axis.ticks = element_blank(),
         axis.title =  element_blank(), 
         panel.background = element_rect(fill = "transparent",colour = NA), # or theme_blank()
         plot.background = element_rect(fill = 'transparent', colour = NA)) +
  ggspatial::annotation_scale( location = 'br') 


ggsave(allotment_year_three_floating , filename = 'output/figures/SingleAllotment_year3.png', 
          width = 4, height = 5, bg = 'transparent')




