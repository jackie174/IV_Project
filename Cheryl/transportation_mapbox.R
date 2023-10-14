library(sf)
library(dplyr)
library(shiny)
library(leaflet)
library(mapboxapi)

# mb_access_token("pk.eyJ1IjoiY2hlcnlsLWNoZW5jY2MiLCJhIjoiY2wyZGJtaHk2MHhweDNjbzIyaWk2ODlqdCJ9.nSmaPBChoCWG7b-VQmpKsA")

# set current working directory to where this app.R file stores
current_directory <- dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(current_directory)
################
# Read in Data #
################
# Victoria street line data
mel_traffic_lines <- st_read("../Data/transportation/mel_traffic_vol.geojson")

# LGA melbourne shape data
# city_mel_gda94 <- st_read("../Data/Mel_LGA_Suburbs_GDA94/melbourne_combined.shp")
mel_suburbs_gda94 <- st_read("../Data/Mel_LGA_Suburbs_GDA94/mel_suburbs_edit.geojson")

# extract the geometry center of the city of melbourne for initialize view 
city_mel_whole_shape <- st_read("../Data/city_of_mel_boundary/mel_boundary.shp")

#####################
# Data Manipulation #
#####################
# # filtered to columns that would like to keep
# mel_traffic_lines <- mel_traffic_lines[, c(1, 7, 11, 15, 20, 22, 24, 32, 33, 35, 36, 37, 38, 42, 43, 44, 49)]

# # filter to city of melbourne only
# traffic_lines <- traffic_lines[traffic_lines$LGA_SHORT_NM == 'MELBOURNE', ]

# transform to the same coordinate system
mel_traffic_lines_wgs84 <- st_transform(mel_traffic_lines, crs = 4326)

mel_suburbs_wgs84 <- st_transform(mel_suburbs_gda94, crs = 4326)

city_mel_whole_shape_wgs84 <- st_transform(city_mel_whole_shape, crs = 4326)


# get the centroid
center_point <- st_centroid(city_mel_whole_shape_wgs84$geometry)


# filter street lines to the city of melbourne via spatial join intersection
# mel_st_lines_sf <- st_join(traffic_lines_wgs84, city_mel_wgs84, left = TRUE)

# mel_st_lines_sf_no_gaps <- st_join(traffic_lines_wgs84, city_mel_wgs84, join = st_within)

mel_st_lines_sf <- st_intersection(mel_traffic_lines_wgs84, mel_suburbs_wgs84)






# Convert the geometry column back to multilinestring
mel_st_lines_sf <- st_cast(mel_st_lines_sf, "MULTILINESTRING")

# # check whether na is included in clue_area
# any(is.na(mel_st_lines_sf$clue_area))
# 
# mel_st_lines_sf <- mel_st_lines_sf %>%
#   filter(!is.na(clue_area))

###################
duplicated_rows = duplicated(mel_st_lines_sf[, -18])
# 
temp = mel_st_lines_sf[duplicated_rows, ]
# temp_view = mel_st_lines_sf[mel_st_lines_sf$OBJECTID_1 == 135, ]
# ##################


# Define a color palette with 6 bins based on log(mel_st_lines_sf$ALLVEHS_AADT) since
# the data is high in variance
my_colors <- c("red4", "red2", "darkorange", "gold", "yellowgreen", "forestgreen", 'darkgreen')
pal <- colorBin(palette = my_colors, domain = log(mel_st_lines_sf$ALLVEHS_AADT), bins = 7, reverse = TRUE)

# define a label design for each street line triggered during hovering event on map
myStLabel <- paste(
  'Street Name: <strong>', mel_st_lines_sf$LOCAL_ROAD_NM, '</strong><br/>', 
  'Section Id: <strong>', mel_st_lines_sf$OBJECTID_1, '</strong><br/>',
  'Flow: <strong>', mel_st_lines_sf$FLOW, '</strong><br/>',
  'Street Type: <strong>', mel_st_lines_sf$RMA_DESC, '</strong><br/>',
  'Clue Area Name: <strong>', mel_st_lines_sf$clue_area, '</strong><br/>',
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
      addMapboxTiles(style_url = "mapbox://styles/cheryl-chenccc/clnmbvidc005701r1bkvb5jew",
                     access_token = 'pk.eyJ1IjoiY2hlcnlsLWNoZW5jY2MiLCJhIjoiY2wyZGJtaHk2MHhweDNjbzIyaWk2ODlqdCJ9.nSmaPBChoCWG7b-VQmpKsA',
                     username = "cheryl-chenccc")%>%
      setView(lng = as.numeric(st_coordinates(center_point)[, 'X']), 
              lat = as.numeric(st_coordinates(center_point)[, 'Y']), zoom = 14)%>%
      addPolygons(data = mel_suburbs_wgs84,
                  color = 'gray',
                  opacity = 0.7,
                  fillOpacity = 0.1,
                  weight = 4
      )%>%
      addPolylines(data = mel_st_lines_sf,
                   color = ~pal(log(mel_st_lines_sf$ALLVEHS_AADT)),
                   opacity = 0.8,
                   weight = 6,
                   # add a highlight effect when hovering on lines
                   highlightOptions = highlightOptions(
                     weight = 8,
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




