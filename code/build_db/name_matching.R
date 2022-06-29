rm(list = ls())
library(tidyverse)
library(stringdist)

mismatches <- read_csv('~/Desktop/compare_names.csv')

word_match_list = function(...) {
  words = c(...)
  word_options = paste(words, collapse = "|") # combine the words w/ | between them
  paste0("\\b(?:", word_options, ")\\b") # add extra regex formatting that makes it work
}

remove_terms <- word_match_list(c('FORK', 'UNIT', 'CANYON', 'RANCH','INDIVIDUAL', 'INDIVID', 'INDIV', 'ALLOTMENT', 'ALLOT', 'COMMUNITY', 'COMMONS', 'COMMON', 'TRACT', 'PASTURE'))
remove_punct <- "[:punct:]+"
remove_spaces <- ' '
replacements <- c('CREEK'='CR',
                  'SPRING'= 'SPR', 
                  'WEST' = 'W', 
                  'SOUTH' = 'S ', 
                  'NORTH'= 'N', 
                  'EAST' = 'E',
                  'MOUNTAIN' = 'MT', 
                  'MOUNT' = 'MT',
                  'MTN' = 'MT', 
                  'ROAD'= 'RD')

mismatches <- 
  mismatches %>% 
  mutate( ALLOT_NAME.x = str_trim( str_squish( str_remove_all(string = ALLOT_NAME.x, remove_punct)))) %>% 
  mutate( ALLOT_NAME.y = str_trim( str_squish( str_remove_all(string = ALLOT_NAME.y, remove_punct)))) %>% 
  mutate( ALLOT_NAME.x = str_trim( str_squish( str_remove_all(string = ALLOT_NAME.x, remove_terms)))) %>% 
  mutate( ALLOT_NAME.y = str_trim( str_squish( str_remove_all(string = ALLOT_NAME.y, remove_terms)))) %>% 
  mutate( ALLOT_NAME.x = str_trim( str_squish( str_replace_all(string = ALLOT_NAME.x, replacements)))) %>% 
  mutate( ALLOT_NAME.y = str_trim( str_squish( str_replace_all(string = ALLOT_NAME.y, replacements)))) %>% 
  mutate( ALLOT_NAME.x = str_trim( str_squish( str_remove_all(string = ALLOT_NAME.x, ' ')))) %>% 
  mutate( ALLOT_NAME.y = str_trim( str_squish( str_remove_all(string = ALLOT_NAME.y, ' '))))

mismatches$d1 <- stringdist(mismatches$ALLOT_NAME.x , mismatches$ALLOT_NAME.y)

mismatches %>% 
  filter( d1 > 0 ) %>% 
  arrange( desc(d1)) %>% 
  view


mismatches$ALLOT_NAME.x_simple <- str_trim(str_squish( str_remove_all(mismatches$ALLOT_NAME.x, "INDIVIDUAL|ALLOTMENT|[-\\.\\/\\&]+")))
mismatches$ALLOT_NAME.y_simple <- str_trim(str_squish( str_remove_all(mismatches$ALLOT_NAME.y, "INDIVIDUAL|ALLOTMENT|[-\\.\\/\\&]+")))


mismatches$d2 <- stringdist(mismatches$ALLOT_NAME.x_simple, 
                            mismatches$ALLOT_NAME.y_simple)

mismatches$d3 <- stringdist(str_sub(mismatches$ALLOT_NAME.x_simple, 1,10), 
                            str_sub(mismatches$ALLOT_NAME.y_simple, 1,10))


mismatches$ALLOT_NAME.x_simple2 <- str_replace_all(mismatches$ALLOT_NAME.x_simple, c('MOUNTAIN' = 'MTN', 'MOUNT '= "MTN ", 'W ' = "WEST "))

mismatches %>% 
  arrange( desc(d1), desc(d2)) %>% View 
