library(sf)
library(dplyr)

# set current working directory to where this app.R file stores
current_directory <- dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(current_directory)

###############################
# wirte study area shape file #
###############################
vic_shape = st_read("Data/VIC_localities_GDA94/vic_localities.shp")

plot(vic_shape$geometry)

# suburb names to use in this project
suburbs_lga_mel = c('Carlton', 'Carlton North', 'East Melbourne', 
                    'South Yarra', 'Melbourne', 'Southbank', 
                    'South Wharf', 'Docklands', 'Port Melbourne',
                    'West Melbourne', 'North Melbourne','Kensington', 
                    'Parkville', 'Flemington')

# filter the dataset to city of melbourne lga
mel_shape = vic_shape[vic_shape$LOC_NAME %in% suburbs_lga_mel, ]

# drop unnecessary columns
mel_shape = mel_shape[, c(-3, -5)]

# A function for merging two separate polygons
merge_two_polygon = function(df, polygon_name.a, polygon_name.b, merge_name) {
  df_merged <- df %>%
    mutate(LOC_NAME = ifelse(LOC_NAME %in% c(polygon_name.a, polygon_name.b), 
                             merge_name, LOC_NAME)) %>%
    group_by(LOC_NAME) %>%
    summarise(geometry = st_union(geometry))
    # st_union(by_feature = TRUE)
  
  return(df_merged)
}

# merge Carlton and Carlton North to 'Carlton'
mel_shape = merge_two_polygon(mel_shape, 'Carlton', 'Carlton North', 'Carlton')

# merge Flemington and Kensington to 'Kensington'
mel_shape = merge_two_polygon(mel_shape, 'Flemington', 'Kensington', 'Kensington')

# write into shape
st_write(mel_shape_combined, "melbourne_combined.shp")

################
# Read in Data #
################
# Victoria street line data
traffic_lines <- st_read("Data/transportation/Traffic_volume.geojson")

head(traffic_lines)

unique(traffic_lines$RMA_DESC)

# transform to the same coordinate system
traffic_lines_gda94 <- st_transform(traffic_lines, crs = 4283)

plot(traffic_lines_gda94$geometry)

# LGA melbourne shape data
city_mel_sf <- st_read("Data/Mel_LGA_Suburbs_GDA94/melbourne_suburbs.shp")

plot(city_mel_sf$geometry)


#####################
# Data Manipulation #
#####################




##################
# USER INTERFACE #
##################





################
# SHINY SERVER #
################




#############
# RUN SHINY #
#############
shinyApp(ui, server)