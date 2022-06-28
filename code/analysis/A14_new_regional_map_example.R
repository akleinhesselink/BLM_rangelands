rm(list = ls())
# load allotment image: 
library(raster)
focal_allot <- raster::brick( 'data/RAP_EE_exports/example_AFG_allotment_trends_5305.tif')
focal_allot <- projectRaster(focal_allot , crs = CRS('EPSG:4326')) 

afgc0 <- as.data.frame( focal_allot$AFG_scale , xy = T) 

focal_allot <- raster::brick( 'data/RAP_EE_exports/example_TREE_allotment_trends_5305.tif')
focal_allot <- projectRaster(focal_allot , crs = CRS('EPSG:4326')) 

tree0 <- as.data.frame( focal_allot$TRE_scale, xy = T)

unloadNamespace('raster')
# 
library(sf)
library(tidyverse)
library(ggspatial)

ecoregions <- read_sf('data/spatial/us_eco_l3_state_boundaries/us_eco_l3_state_boundaries.shp')

W_COLD_DESERT <- 
  ecoregions %>%
  filter( NA_L2NAME == 'COLD DESERTS', US_L3CODE %in% c(10,12,13,80)) %>% 
  st_union()  %>% 
  st_transform( crs=  'EPSG:4326') %>%
  st_make_valid()

annual_data <- read_rds('data/temp/annual_data.rds')
allotments <- read_csv('data/temp/allotment_info.csv')
state_layer <- read_rds('data/temp/western_states_outlines_shapefile.rds')  
shps <- read_sf('data/temp/BLM_allotments_cleaned/allotments.shp')
rate_shps <- read_sf( 'data/temp/cover_rates/allotments_with_rates.shp') 
fo <- read_rds('data/temp/cleaned_BLM_field_office_shapes.rds') 
fo_rates <- read_csv('output/tables/field_office_rates.csv')


fill_scale <- scico::scale_fill_scico(palette = 'roma', 
                                      midpoint = 0, 
                                      direction = -1,
                                      na.value = 'gray', 
                                      name = 'Rate of Increase\nin Cover') 

fill_scale_tree <- scico::scale_fill_scico(palette = 'roma', 
                                           midpoint = 0, 
                                           direction = -1,
                                           na.value = 'gray', 
                                           name = 'Rate of Increase\nin Tree Cover') 
state_layer <- state_layer %>% 
  st_transform(crs = 'epsg:4326')

BLM_offices <- fo %>% 
  st_transform(crs = 'epsg:4326') %>% 
  st_make_valid()

allotments <- allotments %>% 
  filter( ecogroup == 'W Cold Deserts') %>% 
  filter(admin_st != 'WA') 
  
annual_data <- 
  allotments %>% 
  dplyr::select( uname, allot_name, admu_name, parent_cd, parent_name, ecogroup) %>% 
  left_join(annual_data, by = 'uname') 

west_cold_deserts_allots <- rate_shps %>% 
  filter( ecogroup == 'W Cold Deserts') %>%
  st_transform(crs = "EPSG:4326")  %>% 
  st_make_valid()
  
field_office_rate_layer <- fo_rates %>% 
  filter( ecogroup == 'W Cold Deserts') %>% 
  distinct( admu_name, Annual_Cover, Tree_Cover) %>% 
  left_join(fo, by = c('admu_name' = 'ADMU_NAME')) %>% 
  sf::st_as_sf() %>% 
  st_transform( crs = 'EPSG:4326') %>% 
  st_make_valid()

field_office_rate_layer <- 
  field_office_rate_layer %>% 
  st_intersection( W_COLD_DESERT)  # clip to west cold desert 


regional_map <- 
  state_layer %>% 
  ggplot() + 
  geom_sf( fill = 'white', size = 0.2, color = 'white') + 
  xlim( c(-121, -111)) + 
  ylim( c(36.2, 45.5)) + 
  theme(panel.grid = element_blank(), 
        legend.position = 'right',
         panel.border = element_rect(fill = NA), 
         legend.box.background = element_rect(color = 1, size = 0.2)) 


no_axis_no_legend_theme <-  theme(legend.position = 'none',      
                       panel.border = element_blank(),
                       axis.line = element_blank(), 
                       panel.grid = element_blank(), 
                       axis.text = element_blank(),
                       axis.ticks = element_blank(), 
                       axis.title =  element_blank())

no_axis_with_legend_theme <-  theme(legend.position = 'right',      
                                  panel.border = element_blank(),
                                  axis.line = element_blank(), 
                                  panel.grid = element_blank(), 
                                  axis.text = element_blank(),
                                  axis.ticks = element_blank(), 
                                  axis.title =  element_blank())

fo_boundaries  <- geom_sf(data = BLM_offices %>% distinct(Shape), 
                   fill = NA, size = 0.1, alpha = 0.8) 

state_boundaries <- geom_sf(fill = NA, size = 0.5, alpha = 0.5) 

jarbidge_highlight <- geom_sf(data = BLM_offices %>% distinct(Shape) %>% 
          filter( ADM_UNIT_CD == "IDT01000"), 
        fill = NA, size = 0.4, color = 'blue', alpha = 0.3) 
  
regional_map_annual_allotment_scale <- 
  regional_map + 
  geom_sf( data = west_cold_deserts_allots, 
         aes(fill = Annual), 
         color = 'white', size = 0.05, alpha = 0.9) + 
  fo_boundaries + 
  state_boundaries +
  jarbidge_highlight + 
  fill_scale + 
  no_axis_no_legend_theme + 
  coord_sf(clip = 'off') 

regional_map_tree_allotment_scale <- regional_map + 
  geom_sf( data = west_cold_deserts_allots, 
           aes(fill = Tree), 
           color = 'white', size = 0.05, alpha = 0.9) + 
  fo_boundaries + 
  state_boundaries +
  jarbidge_highlight + 
  fill_scale + 
  no_axis_no_legend_theme + 
  coord_sf(clip = 'off') 

regional_map_annual <- regional_map + 
#  fo_boundaries + 
  geom_sf( data = field_office_rate_layer, 
           aes(fill = Annual_Cover), 
           color = 'white', size = 0.1, alpha = 0.9) + 
  geom_sf( data = W_COLD_DESERT, color = 'gray', size = 0.5 , fill = NA) + 
  state_boundaries +
  jarbidge_highlight + 
  fill_scale + 
  no_axis_no_legend_theme + 
  coord_sf(clip = 'off') + 
  ggspatial::annotation_scale(location = 'bl')


regional_map_tree <- regional_map + 
  geom_sf( data = field_office_rate_layer, 
           aes(fill = Tree_Cover), 
           color = 'white', size = 0.1, alpha = 0.9) + 
  #fo_boundaries + 
  state_boundaries +
  jarbidge_highlight + 
  fill_scale + 
  no_axis_no_legend_theme + 
  coord_sf(clip = 'off')  + 
  ggspatial::annotation_scale(location = 'bl')

# Annual Cover 
ggsave(regional_map_annual, 
       filename = 'output/figures/Figure7A_annuals_no_legend.png', 
          width = 4, height = 5, units = 'in', dpi = 600)

# Tree Cover
ggsave(regional_map_tree, 
       filename = 'output/figures/Figure7D_trees_no_legend.png', 
       width = 4, height = 5, units = 'in', dpi = 600 )

## Single field office in Southern Idaho: ----------------------------- # 
BLM_offices <- 
  read_rds('data/temp/cleaned_BLM_field_office_shapes.rds') %>% 
  st_make_valid() %>% 
  st_transform( crs = 'EPSG:4326') %>% 
  filter( str_detect( ADM_UNIT_CD , 'ID|NV'))

#
Jarbidge_FO <- 
  BLM_offices %>% 
  filter( ADM_UNIT_CD == "IDT01000" ) 

Jarbidge_allotments <- 
  west_cold_deserts_allots %>% 
  filter( ADMIN_ST == "ID", ADM_OFC_CD == 'T01000') %>% 
  st_transform(crs = 'EPSG:4326')

Jarbidge_FO <- 
  Jarbidge_allotments %>% 
  dplyr::select( geometry) %>%
  bind_rows(
    Jarbidge_FO %>% 
      dplyr::select(Shape) %>% 
      rename('geometry' = Shape) ) %>% 
  summarise( geometry = st_union(geometry))  %>%
  st_make_valid()

fo_xlimits <- c(-115.76, -114.72) 
fo_ylimits <- c(41.90, 42.98)

allotment_highlight <- 
  geom_sf(data = Jarbidge_allotments %>% filter( uname == 5305), 
        size = 0.8, 
        fill = NA, 
        color = 'blue', 
        show.legend = F)

Jarbidge_fo_map <- 
  Jarbidge_FO %>% 
    ggplot() + 
    geom_sf( data = state_layer, fill = 'white', size = 0.2, color = 'black') + 
    geom_sf( data = west_cold_deserts_allots, fill = NA, 
             color = 'lightgray', size = 0.1, alpha = 0.7)  + 
    geom_sf(data = BLM_offices %>% distinct(Shape), 
            fill = NA, size = 0.1, alpha = 0.8) + 
    xlim( fo_xlimits ) + 
    ylim( fo_ylimits) + 
    theme_bw() + 
    no_axis_no_legend_theme + 
    coord_sf(clip = 'off') 
    
# Annual cover 
Jarbidge_FO_annual_trends <- Jarbidge_fo_map + 
  geom_sf(data = Jarbidge_allotments, 
        size = 0.2, 
        aes( fill = Annual ) , 
        color = 'white', alpha = 0.8)  + 
  geom_sf(size = 0.5, 
          color = 'black', 
          fill = NA)  + 
  allotment_highlight + 
  fill_scale + 
  ggspatial::annotation_scale( location = 'bl', )  + 
  coord_sf(clip = 'off') 


ggsave( Jarbidge_FO_annual_trends, 
        filename = 'output/figures/Figure7B_FieldOfficeTrends.png',
        width = 4, 
        height = 5, 
        dpi = 600 )

# Tree Cover 
Jarbidge_tree_trends <- 
  Jarbidge_fo_map + 
  geom_sf(data = Jarbidge_allotments, 
          size = 0.2, 
          aes( fill = Tree) , 
          color = 'white', alpha = 0.8)  + 
  geom_sf(size = 0.5, 
          color = 'black', 
          fill = NA)  + 
  allotment_highlight + 
  fill_scale + 
  ggspatial::annotation_scale( location = 'bl', )  + 
  no_axis_no_legend_theme + 
  coord_sf(clip = 'off')

ggsave(Jarbidge_tree_trends, 
       filename = 'output/figures/Figure7E_FieldOfficeTrends.png',
        width = 4, 
       height = 5, dpi = 600 )

# single allotment --------- # 
# Floating image on white 
allotment_xlimits <- c(-115.069, -115.0025) 
allotment_ylimits <- c(42.0260, 42.095) 

fill_scale_allotment <- scico::scale_fill_scico(palette = 'roma', 
                                      midpoint = 0, 
                                      direction = -1,
                                      na.value = NA, 
                                      name = 'Rate of Increase\nin Cover') 

annual_fill <- geom_tile( data = afgc0, aes( x = x, y= y, fill = AFG_scale ), 
           color = NA, show.legend = F, alpha = 0.8) 

tree_fill <- geom_tile( data = tree0, aes( x = x, y= y, fill = TRE_scale ), 
                          color = NA, show.legend = F, alpha = 0.9) 

allotment_border <- geom_sf( data = Jarbidge_allotments %>% 
                               filter( uname == '5305'),
           fill = NA, color = 'black', size = 1, alpha = 0.9) 
  
allotment_theme_no_legend <- 
  theme_bw()  + 
  theme( legend.position = "none", 
         panel.border = element_blank(),
         axis.line = element_blank(), 
         panel.grid = element_blank(), 
         panel.background = element_rect(fill = "transparent",colour = NA), # or theme_blank()
         plot.background = element_rect(fill = 'transparent', colour = NA),
         axis.text = element_blank(),
         axis.ticks = element_blank(),
         axis.title =  element_blank())
  
  
single_allotment_map <- 
  Jarbidge_FO %>%
    ggplot() + 
    geom_sf( data = Jarbidge_allotments, color = 'black', fill = NA, size = 0.2) + 
    xlim( allotment_xlimits) + 
    ylim( allotment_ylimits) + 
    coord_sf(clip = 'off') + 
    allotment_theme_no_legend

single_allotment_annual_trend <- single_allotment_map + 
  annual_fill + 
  allotment_border + 
  fill_scale_allotment +
  ggspatial::annotation_scale( location = 'bl', )  + 
  coord_sf(clip = 'off') 


ggsave(single_allotment_annual_trend, 
       filename ='output/figures/Figure7C_allotment_trends.png', 
       width = 4, 
       height = 5, 
       bg = 'transparent', dpi = 600)

# Tree Cover 
single_allotment_tree_trend <- 
  single_allotment_map + 
  tree_fill + 
  allotment_border + 
  fill_scale_allotment + 
  ggspatial::annotation_scale( location = 'bl') +
  coord_sf(clip = 'off') 

ggsave(single_allotment_tree_trend, 
       filename = 'output/figures/Figure7F_allotment_trends.png', 
       width = 4, 
       height = 5, 
       bg = 'transparent', dpi = 600)


