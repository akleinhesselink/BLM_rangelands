# app with template
rm(list = ls())
library(tidyverse)
library(leaflet)
library(sf)
library(shiny)
library(plotly)
library(shinydashboard)


load('app/data/mapdata.rda')
load('app/data/vegdata.rda')
source('app/parameters.R')
source('app/functions.R')


allotment_map <- function( ctrs, shps , choices ){ 
  
  trend_choice <- paste0( '`', paste( choices['type'], 
                                      choices['unit'], sep = '_'), '`')
  
  rng <- ctrs %>% 
    pull(eval(parse( text = trend_choice))) %>% 
    range(na.rm = T)
  
  pal <- colorNumeric(
    palette = viridis::viridis(n = 256), 
    #palette = c('blue', 'yellow', 'red'),
    domain = rng, 
    na.color = NA)
  
  legend_label <- 
    paste( 
      paste(
        choices['type'], choices['unit']
      ), 
      'trend') 
  
  print(legend_label)
  
  ctrs %>% 
    leaflet(options = leafletOptions(minZoom = 4, maxZoom = 11)) %>%
    setView(center_ll[1], center_ll[2], zoom = 5) %>% 
    setMaxBounds( lng1 = bounds[1], 
                  lat1 = bounds[2], 
                  lng2 = bounds[3], 
                  lat2 = bounds[4]) %>%
    addProviderTiles(providers$CartoDB.Positron) %>% 
    addCircles(
      layerId = ~ uname, 
      color = ~ pal( Annual_cover ), 
      weight = 4, 
      highlight = highlightOptions(
        fillColor = "Cyan",
        fillOpacity = 0.8,
        bringToFront = TRUE
      ),
      group = "Annual Cover",
      label = labels,
      labelOptions = labelOptions(
        style = list("font-weight" = "normal", "padding" = "5px 10px"),
        textsize = "15px",
        direction = "auto")
    ) %>% 
    addLegend_decreasing("bottomright",
                         pal = pal,
                         values = ~ eval( parse( text = trend_choice)),
                         title = legend_label,
                         labFormat = labelFormat(suffix = '%', digits = 2),
                         opacity = 0.9, na.label = 'Gray', decreasing = T) %>%
    groupOptions(group = 'Annual Cover', zoomLevels = 4:5) %>% 
    groupOptions(group = 'Shapes', zoomLevels = 6:12) %>%
    addPolygons( 
      data = shps, 
      layerId = ~uname, 
      color = ~ pal( eval( parse( text = trend_choice))),       
      highlight = highlightOptions(
        fillColor = "Cyan",
        fillOpacity = 0.9,
        bringToFront = TRUE
      ),
      weight = 2,
      group = "Shapes",
      label = labels,
      labelOptions = labelOptions(
        style = list("font-weight" = "normal", "padding" = "5px 10px"),
        textsize = "15px",
        direction = "auto"
      ))
  
}

choices <- c(type = 'Tree', unit = 'cover', scale= 'Ecoregion')

rng <- c(-2, 2)
pal <- colorNumeric(
  palette = viridis::viridis(n = 256), 
  #palette = c('blue', 'yellow', 'red'),
  domain = rng, 
  na.color = NA)




map <- leaflet(allotment_ctrs, options = leafletOptions(minZoom = 4, maxZoom = 11)) %>%
  # Base groups
  setView(center_ll[1], center_ll[2], zoom = 5) %>% 
  setMaxBounds( lng1 = bounds[1], 
                lat1 = bounds[2], 
                lng2 = bounds[3], 
                lat2 = bounds[4]) %>%
  addProviderTiles(providers$CartoDB.Positron) %>% 
  # Overlay groups
  addCircles(color = ~ pal( Annual_cover), group = "Annual Cover") %>%
  addCircles(color = ~ pal( Perennial_cover), group = "Perennial Cover") %>%
  addCircles(color = ~ pal( Perennial_cover), group = "Bare Ground Cover") %>%
  addCircles(color = ~ pal( Perennial_cover), group = "Shrub Cover") %>%
  addCircles(color = ~ pal( Perennial_cover), group = "Tree Cover") %>%
  # Layers control
  addLayersControl(
    overlayGroups = c("Annual Cover", 
                      "Perennial Cover", 
                      "Bare Ground Cover", 
                      "Shrub Cover", 
                      "Tree Cover"),
    options = layersControlOptions(collapsed = T)
  ) %>% 
  hideGroup(c( "Perennial Cover", "Bare Ground Cover", "Shrub Cover", "Tree Cover"))

map

