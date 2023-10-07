library(sf)
library(dplyr)


################
# Read in Data #
################
# Victoria street line data ！！！！！！！！！！！！！！！！！！！！！！
traffic_lines <- st_read("../Data/transportation/Traffic_volume.geojson")

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