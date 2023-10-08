library(sf)
library(dplyr)
library(shiny)
library(leaflet)

# set current working directory to where this app.R file stores
current_directory <- dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(current_directory)
################
# Read in Data #
################
# Victoria street line data
traffic_lines <- st_read("../Data/transportation/Traffic_volume.geojson")

# filtered to columns that would like to keep
traffic_lines <- traffic_lines[, c(1, 7, 11, 15, 20, 22, 24, 32, 33, 35, 36, 37, 38, 42, 43, 44, 49)]

# LGA melbourne shape data
city_mel_gda94 <- st_read("../Data/Mel_LGA_Suburbs_GDA94/melbourne_combined.shp")

# extract the geometry center of the city of melbourne for initialize view 
city_mel_whole_shape <- st_read("../Data/city_of_mel_boundary/mel_boundary.shp")

#####################
# Data Manipulation #
#####################
# transform to the same coordinate system
traffic_lines_wgs84 <- st_transform(traffic_lines, crs = 4326)

city_mel_wgs84 <- st_transform(city_mel_gda94, crs = 4326)

city_mel_whole_shape_wgs84 <- st_transform(city_mel_whole_shape, crs = 4326)


# get the centroid
center_point <- st_centroid(city_mel_whole_shape_wgs84$geometry)


# filter street lines to the city of melbourne via spatial join intersection
mel_st_lines_sf <- st_intersection(traffic_lines_wgs84, city_mel_wgs84)

# merged_geom <- mel_st_lines_sf %>%
#   group_by(OBJECTID_1) %>%  # 每一行都作为一个单独的组
#   summarize(geometry = st_union(geometry)) %>%
#   ungroup()
# 
# mel_st_lines_sf <- st_as_sf(
#   left_join(select(as.data.frame(mel_st_lines_sf), -geometry),
#             as.data.frame(merged_geom), by = "OBJECTID_1")
#                             )

# Convert the geometry column back to multilinestring
mel_st_lines_sf <- st_cast(mel_st_lines_sf, "MULTILINESTRING")

# Define a color palette with 6 bins based on log(mel_st_lines_sf$ALLVEHS_AADT) since
# the data is high in variance
pal <- colorBin(palette = 'RdYlGn', domain = log(mel_st_lines_sf$ALLVEHS_AADT), bins = 6, reverse = TRUE)

# define a label design for each street line triggered during hovering event on map
myStLabel <- paste(
  'Street Name and Section Id: <strong>', paste(mel_st_lines_sf$LOCAL_ROAD_NM, mel_st_lines_sf$OBJECTID_1), '</strong><br/>', 
  'Annual Average Daily Traffic Volume: <strong>', mel_st_lines_sf$ALLVEHS_AADT, '</strong><br/>')%>%
  lapply(htmltools::HTML)

##################
# USER INTERFACE #
##################
# the first and main panel of the UI
traffic_vol_tab <- tabPanel(
  title = 'Traffic Volume (Street Level)',
  mainPanel(
    leafletOutput('traffic_vol', width = "1870px", height = "970px")
  )
)


# The layout of the UI
ui <- navbarPage(
  id = 'mypage',
  title='Transportation',
  traffic_vol_tab
)


################
# SHINY SERVER #
################
server <- function(input, output, session) { 
  
  ##### traffic_vol tab ####
  output$traffic_vol <- renderLeaflet({
    
    map <- leaflet() %>%
      addProviderTiles(providers$Esri.WorldGrayCanvas)%>%
      setView(lng = as.numeric(st_coordinates(center_point)[, 'X']), 
              lat = as.numeric(st_coordinates(center_point)[, 'Y']), zoom = 14)%>%
      addPolygons(data = city_mel_wgs84,
                  color = 'blue',
                  opacity = 0.3,
                  fillOpacity = 0.2,
                  weight = 4
                  )%>%
      addPolylines(data = mel_st_lines_sf,
                   color = ~pal(log(mel_st_lines_sf$ALLVEHS_AADT)),
                   opacity = 0.8,
                   weight = 4,
                   # add a highlight effect when hovering on lines
                   highlightOptions = highlightOptions(
                     weight = 6,
                     color = "white",
                     bringToFront = TRUE),
                   label = myStLabel
                   )
      
  })
  
}



#############
# RUN SHINY #
#############
shinyApp(ui, server)