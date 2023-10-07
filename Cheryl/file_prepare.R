library(sf)
library(dplyr)
library(jsonlite)

# set current working directory to where this app.R file stores
current_directory <- dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(current_directory)

###############################
# wirte study area shape file #
###############################
# vic_shape = st_read("../Data/VIC_localities_GDA94/vic_localities.shp")
mel_lga_shape = st_read('../Data/blocks-for-census-of-land-use-and-employment-clue-shape/blocks-for-census-of-land-use-and-employment-clue.shp')
# plot(vic_shape[vic_shape$LOC_NAME == 'South Yarra',]$geometry)
mel_lga_shape = mel_lga_shape[, -1]


# temp = mel_lga_shape[mel_lga_shape$clue_area == 'Carlton', ]

# A function for merging two polygons into one
merge_polygons = function(df, polygons_name, merge_name) {
  merge_df <- df %>%
    mutate(clue_area = ifelse(clue_area == polygons_name, merge_name, clue_area)) %>%
    group_by(clue_area) %>%
    summarise(geometry = st_union(geometry))
  
  return(merge_df)
}

# A function for merging two polygons into one
merge_two_polygon = function(df, polygon_name.a, polygon_name.b, merge_name) {
  merge_df <- df %>%
    mutate(clue_area = ifelse(clue_area %in% c(polygon_name.a, polygon_name.b), merge_name, clue_area)) %>%
    group_by(clue_area) %>%
    summarise(geometry = st_union(geometry))
  
  return(merge_df)
}

# since original data are in unit level, we merge polygons together to form suburb polygons
mel_lga_shape = merge_polygons(mel_lga_shape, 'Carlton', 'Carlton')

plot(mel_lga_shape$geometry)

# suburb names to use in this project
suburbs_lga_mel = c('Carlton', 'Carlton North', 'East Melbourne', 
                    'South Yarra', 'Melbourne', 'Southbank', 
                    'South Wharf', 'Docklands', 'Port Melbourne',
                    'West Melbourne', 'North Melbourne','Kensington', 
                    'Parkville', 'Flemington')

# merge 'West Melbourne (Industrial)' and 'West Melbourne (Residential)' into 'West Melbourne'
mel_lga_shape = merge_two_polygon(mel_lga_shape, 'West Melbourne (Industrial)', 
                                  'West Melbourne (Residential)', 'West Melbourne')

# merge flemington and kensington into 'Keinsington'
mel_lga_shape = merge_two_polygon(mel_lga_shape, 'Melbourne (CBD)', 'Melbourne (Remainder)', 
                                  'Melbourne')

plot(mel_lga_shape$geometry)

# write into shape file for later use
st_write(mel_lga_shape, "../Data/Mel_LGA_Suburbs_GDA94/melbourne_combined.shp")


