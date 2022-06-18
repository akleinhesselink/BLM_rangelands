rm(list = ls())

library(sf)
library(tidyverse)
library(lubridate)

m2_per_ACRE <- 4046.8564224
m2_per_ha <- 1e4
ACRE_per_ha <- 2.47105

mdate <- date( file.info('data/spatial/BLM_National_Grazing_Allotments_2022/gra.gdb/a00000001.gdbindexes')$mtime )

allotment_info <- sf::read_sf('data/spatial/BLM_National_Grazing_Allotments_2022/gra.gdb/', 
                   layer = 'gra_allot_poly') %>% 
  st_drop_geometry()

sort( unique( allotment_info$ADMIN_ST ) )

allotment_info <- allotment_info %>% 
  mutate( ADMIN_ST = ifelse( ADMIN_ST == ' ', str_extract(ST_ALLOT, '^[A-Z]{2}'), ADMIN_ST)) %>% 
  mutate( ST_ALLOT_new = ifelse(
    !str_extract(ST_ALLOT, '^[A-Z]{2}') %in% c('AZ', 'CA', 'CO', 'ID', 'MT', 'NM', 'NV', 'OR', 'UT', 'WY') ,
    paste0( ADMIN_ST, ALLOT_NO ), ST_ALLOT ))  

allotment_info <- allotment_info %>% 
  mutate( ST_ALLOT_new = ifelse( !str_detect( ST_ALLOT_new, "[0-9]{5}$" ), 
                                 paste0( ADMIN_ST, str_pad( ALLOT_NO, 5, side = "left", pad = 0)), 
                                 ST_ALLOT_new))  


all( str_length(allotment_info$ST_ALLOT_new) == 7 )

allotment_info <- 
  allotment_info %>% 
  ungroup() %>% 
  mutate( ALLOT_NAME_new = str_trim( str_squish( str_to_upper(str_remove_all(ALLOT_NAME, "[\\.]+" ))))) %>% 
  arrange( ST_ALLOT_new, last_edited_date) %>% 
  mutate( ID = as.numeric( factor( paste(ST_ALLOT_new, ALLOT_NAME_new, ADMIN_ST, ADM_UNIT_CD, ADM_OFC_CD)))) 

allotments <- 
  allotment_info %>% 
  left_join( sf::read_sf('data/spatial/BLM_National_Grazing_Allotments_2022/gra.gdb/') %>% 
               select( SHAPE, GlobalID) %>% 
               st_transform( crs = 'epsg:5070'), 
           by = 'GlobalID')

allotments <- allotments %>% 
  st_as_sf() %>% 
  st_cast( 'MULTIPOLYGON') %>% 
  filter( !st_is_empty(SHAPE)) %>% 
  st_make_valid()

# Group by allot number and name 
# assign unique ID
if(!dir.exists('data/temp/allotments/')){ 
  dir.create('data/temp/allotments/') 
}

st_write( allotments %>% select( ID, last_edited_date, ALLOT_NAME_new, ST_ALLOT_new, ADMIN_ST, ADM_OFC_CD, ADM_UNIT_CD) , 
          dsn = 'data/temp/allotments/step_one.shp', 
          layer = 'step_one', append = F)


readline(prompt="
# Read into QGIS and clean with GRASS 
# 1. Union/v.clean with GRASS
# 2. Fix geometries 
# 3. Remove duplicates 
# 4. Combine features by ID
# 5. Dissolve internal boundaries with same ID
# press [Enter] when done to complete R script.")

allotments_clean <- sf::read_sf('data/temp/allotments/step_two.shp') # read in cleaned shapefile

# Read in BLM districts and offices
# Use spatial join to find missing 
# states, districts and offices in the allotment data 
st_layers('data/spatial/BLM_National_Administrative_Units/admu.gdb/' )

mdate_admin_boundaries <- file.info('data/spatial/BLM_National_Administrative_Units/admu.gdb/a00000001.gdbindexes')$mtime
mdate_admin_boundaries <- date( mdate_admin_boundaries)

districts <- st_read( 'data/spatial/BLM_National_Administrative_Units/admu.gdb/', 
                      layer = 'blm_natl_admu_dist_poly_webpub') %>% 
  filter( ADMIN_ST != "AK") %>% 
  st_cast('MULTIPOLYGON') %>% 
  st_make_valid()  %>% 
  st_transform( crs = 'epsg:5070')

offices <- st_read('data/spatial/BLM_National_Administrative_Units/admu.gdb/', 
                   layer = 'blm_natl_admu_field_poly_webpub') %>%  
  filter( ADMIN_ST != "AK") %>% 
  st_cast('MULTIPOLYGON') %>% 
  st_make_valid() %>% 
  st_transform( crs = 'epsg:5070')

# Add Las Cruces District/Office: missing from field office Layer
# but is present in District layer 
Las_Cruces <- districts %>% 
  filter( ADMIN_ST == 'NM') %>% 
  filter( PARENT_CD == 'NML00000') 

# Patch whole in Paria River district/Kanab Field office 
# Hole in Kanab office where Grand staircase escalante is 
Paria_River <- districts %>% 
  filter( ADMIN_ST == 'UT' ) %>% 
  filter( PARENT_CD == 'UTP00000')

offices <- offices %>% 
  rbind( 
    Las_Cruces %>%
      mutate( ADM_UNIT_CD = PARENT_CD) %>%
      mutate( ADMU_NAME = PARENT_NAME) %>%
      mutate( BLM_ORG_TYPE = 'Field' ) %>% 
      mutate( APPRV_DT = NA, EFF_DT = NA, ADMU_ST_URL = NA) ) %>% 
  rbind( Paria_River %>% 
           mutate( ADM_UNIT_CD = 'UTP02000' ) %>% 
           mutate( ADMU_NAME = 'KANAB') %>%
           mutate( BLM_ORG_TYPE = 'Field') %>% 
           mutate( APPRV_DT = NA, EFF_DT = NA, ADMU_ST_URL = NA))


# Standardize admin names 
# A couple of ADMU_UNIT_CD's have more than one entry
# group by ADMU_UNIT_CD and take top row within group (1 per group)

offices_fix_names <- 
  offices %>% 
  mutate( ADMU_NAME = str_trim( str_remove(str_to_upper(str_trim(str_squish(ADMU_NAME))), 'FIELD OFFICE|(OFFICE)')), 
          PARENT_NAME = str_trim( str_remove( str_to_upper( str_trim(str_squish( PARENT_NAME))), '(DISTRICT OFFICE)|(DISTRICT)|(OFFICE)'))) %>% 
  select( ADM_UNIT_CD, ADMU_NAME, PARENT_CD, PARENT_NAME, ADMIN_ST)  %>% 
  mutate( ADMU_NAME = paste( ADMU_NAME, 'FIELD OFFICE'), 
          PARENT_NAME = paste( PARENT_NAME, 'DISTRICT')) 

offices_fix_names <- 
  offices_fix_names %>% 
  group_by(ADM_UNIT_CD, ADMU_NAME , PARENT_CD, PARENT_NAME, ADMIN_ST) %>% 
  summarise( Shape = st_union( Shape))

if(!dir.exists('data/temp/BLM_field_offices_cleaned/')){ 
  dir.create('data/temp/BLM_field_offices_cleaned/') 
}

offices_fix_names %>%  
  filter( ADMIN_ST!="AK") %>% 
  st_cast('MULTIPOLYGON') %>% 
  st_write('data/temp/BLM_field_offices_cleaned/field_offices.shp', 
           layer = 'field_offices', append = F)

office_info <- offices_fix_names %>% st_drop_geometry()

allotments_clean <- 
  allotments_clean %>% 
  left_join( office_info, by = c('ADM_UNI'= 'ADM_UNIT_CD')) 

centers_missing_admin_info <- allotments_clean %>%  
  filter( is.na(ADMU_NAME)) %>% 
  st_centroid()

spatial_join_admin <- 
  centers_missing_admin_info %>% 
  select( - c(ADMU_NAME:ADMIN_ST)) %>% 
  st_join( offices_fix_names, by = st_within() ) 

allotments_clean <- 
  allotments_clean %>% 
  left_join(spatial_join_admin %>% st_drop_geometry(), 
            by = c('fid', 'cat', 'ID'), suffix = c('', '.X')) %>% 
  mutate( ADMU_NAME = ifelse( is.na(ADMU_NAME), ADMU_NAME.X, ADMU_NAME)) %>% 
  mutate( PARENT_CD = ifelse( is.na(PARENT_CD), PARENT_CD.X, PARENT_CD)) %>% 
  mutate( PARENT_NAME = ifelse( is.na(PARENT_NAME), PARENT_NAME.X, PARENT_NAME)) %>% 
  mutate( ADMIN_ST = ifelse( is.na(ADMIN_ST), ADMIN_ST.X, ADMIN_ST)) %>% 
  mutate( ADM_UNI = ifelse( ADM_UNI == 'UTP00000', ADM_UNIT_CD, ADM_UNI)) %>% 
  mutate( ADM_UNI = ifelse( is.na(ADM_UNI), ADM_UNIT_CD, ADM_UNI)) %>%
  select( fid:ADMIN_ST) %>% 
  ungroup()

allotments_clean <- allotments_clean %>% 
  rename( 'ALLOT_NAME' = ALLOT_N, 
          'ALLOT_NO' = ST_ALLO, 
          'ADM_OFC_CD' = ADM_OFC, 
          'ADM_UNIT_CD' = ADM_UNI, 
          'OFFICE' = ADMU_NAME, 
          'DISTRICT' = PARENT_NAME, 
          'DISTRICT_CD' = PARENT_CD) %>% 
  select( -fid, -cat, -lst_dt_, - ADMIN_S) %>% 
  mutate( hectares = as.numeric(st_area(geometry))/m2_per_ha) %>% 
  filter( hectares > 1) %>% 
  arrange(ID) %>% 
  mutate( uname = row_number())


offices_fix_names %>% 
  st_cast( 'POLYGON') %>% 
  st_make_valid() %>% 
  st_write('data/temp/BLM_field_offices_cleaned/field_offices.shp', append = F, 
           layer = 'offices')


# Join RAS data 
RAS_info <- read_csv('data/RAS_data/Allotment Information Report-All Allotments.csv') %>% 
  mutate( `Allotment Number` = str_squish(str_trim(`Allotment Number`)))  %>% 
  rename( 'Field Office'  = `Field Office...9`) %>% 
  select( - `Field Office...10`)

RAS_info <- 
  RAS_info %>% 
  distinct( `Allotment Number`, `Allotment Name`, `Public Acres`, `Authorization Number`, `Admin State`, `Admin Office`) %>% 
  group_by( `Allotment Number`, `Allotment Name` ) %>% 
  arrange( desc( `Authorization Number`)) %>% 
  filter( row_number() == 1 )

allotments_clean <- 
  allotments_clean %>% 
  left_join(RAS_info, by = c('ALLOT_NO' = 'Allotment Number'))

# Add Ecoregion information 
# input -------------------------- # 
ER <- read_sf('data/spatial/us_eco_l3_state_boundaries/us_eco_l3_state_boundaries.shp')
#--------------------------------- # 

ctrs <- allotments_clean %>% st_centroid() %>% 
  select( uname)
# add ecoregion column to allotment table: 
# Get ecoregion
ER <- ER %>% 
  st_transform(crs = st_crs(ctrs))
# 
ctrs <- ctrs %>% 
  st_join(ER, left = T, join = st_within )

# Custom Ecoregion classification, keep n similar between ecoregions 
# Use Level I Ecoregions in all cases except: 
# 1. Break deserts to level II ecoregions: warm deserts and cold deserts
# 2. Lump Southern Semi-Arid Highlands with Temperate Sierras = "S. Mts."
# 3. Split Cold Deserts (ERII) into West and East  
# 4. Split Great Plains (I) into Northern and Southern 

ctrs <- 
  ctrs %>%
  st_drop_geometry() %>% 
  mutate( ecoregion = NA_L1NAME ) %>% 
  mutate( ecoregion = 
            ifelse( ecoregion %in% 
                      c('TEMPERATE SIERRAS', 
                        'SOUTHERN SEMI-ARID HIGHLANDS'), 
                    'AZ/NM HIGHLANDS', 
                    ecoregion)) %>%
  mutate( ecoregion =  
            ifelse( ecoregion == 'NORTH AMERICAN DESERTS', 
                    NA_L2NAME, 
                    ecoregion))  %>%
  mutate( ecoregion = 
            ifelse( ecoregion == "COLD DESERTS",
                    NA_L3NAME,
                    ecoregion)) %>% 
  mutate( ecoregion = 
            ifelse( ecoregion %in% 
                      c('Wyoming Basin', 
                        'Colorado Plateaus', 
                        'Arizona/New Mexico Plateau'),
                    'E COLD DESERTS', 
                    ecoregion)) %>% 
  mutate( ecoregion = 
            ifelse( ecoregion %in% 
                      c( 'Central Basin and Range', 
                         'Northern Basin and Range', 
                         'Columbia Plateau', 
                         'Snake River Plain'),
                    'W COLD DESERTS', 
                    ecoregion)) %>% 
  mutate( ecoregion = 
            ifelse( ecoregion == 'GREAT PLAINS', 
                    NA_L2NAME, 
                    ecoregion)) %>% 
  mutate( ecoregion = 
            ifelse( ecoregion %in% 
                      c('WEST-CENTRAL SEMI-ARID PRAIRIES', 'TEMPERATE PRAIRIES'), 
                    'N GREAT PLAINS', 
                    ecoregion)) %>% 
  mutate( ecoregion = 
            ifelse( ecoregion == 'SOUTH CENTRAL SEMI-ARID PRAIRIES', 
                    'S GREAT PLAINS', 
                    ecoregion))  %>% 
  mutate( ecoregion = 
            ifelse( ecoregion == 'NORTHWESTERN FORESTED MOUNTAINS', 'Forested Mts', ecoregion))


allotment_ecoregions <- 
  ctrs %>%
  mutate( ecoregion = stringr::str_to_title(ecoregion)) %>%
  mutate( ecoregion = ifelse( ecoregion == 'Az/Nm Highlands' , 'AZ/NM Highlands',  ecoregion )) %>%
  select( uname, ecoregion, US_L3CODE:L1_KEY) 

allotments_clean <- 
  allotments_clean %>% 
  left_join(allotment_ecoregions %>% 
              select( ecoregion , contains('NAME'), -STATE_NAME), by = 'uname') %>% 
  st_make_valid() %>% 
  select( uname, ALLOT_NAME:NA_L1NAME)

allotments_clean %>% 
  st_drop_geometry() %>% 
  write_csv('data/temp/allotment_info.csv')

allotments_clean %>% 
  st_write( 'data/temp/allotments/allotments_clean.shp', 
            layer = 'allotments_clean', append = F)


# Save ecoregions 
EG <- ER %>% 
  left_join( allotment_ecoregions, by = 'US_L3CODE') %>%
  filter( !is.na(ecoregion )) %>% 
  select( ecoregion, geometry) %>%
  st_simplify(preserveTopology = T, dTolerance = 500) %>% 
  st_make_valid() %>% 
  group_by( ecoregion) %>%
  summarise( geometry = st_union( geometry )) %>%
  st_make_valid()

EG %>%
  write_rds(file = 'data/temp/simplified_ecoregion_shapefile.rds')


