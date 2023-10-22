library(RColorBrewer)
library(dplyr)
library(echarts4r)
library(ggiraph)
library(ggplot2)
library(hrbrthemes)
library(htmltools)
library(htmlwidgets)
library(jsonlite)
library(leaflet)
library(lubridate)
library(mapboxapi)
library(maps)
library(plotly)
library(readxl)
library(scales)
library(sf)
library(shiny)
library(shinyWidgets)
library(shinyjs)
library(shinythemes)
library(tidyr)
library(tidyverse)
library(viridis)
library(markdown)

# Load the GEOM90007 Tableau in Shiny library
source('tableau-in-shiny-v1.0.R')
################################# CLEANING DATA START ######################################################

######################Relation Data #####################
suburbs_lga_mel = c(
  'Carlton',
  'East Melbourne',
  'South Yarra',
  'Melbourne',
  'Southbank',
  'Docklands',
  'Port Melbourne',
  'West Melbourne',
  'North Melbourne',
  'Kensington',
  'Parkville'
)

# ---------------------- Carpark Data Processing
# Reading and processing carpark data
carpark_data <-
  read.csv("../Data/relation/off-street-car-parks-with-capacity-and-type1.csv",
           header = TRUE)
# ... [All your carpark data processing code, with variable names prefixed by carpark_]

carpark_data$`Clue.Small.Area` <-
  gsub("Melbourne \\(CBD\\)",
       "Melbourne",
       carpark_data$`Clue.Small.Area`)
carpark_data$`Clue.Small.Area` <-
  gsub("Melbourne \\(Remainder\\)",
       "Melbourne",
       carpark_data$`Clue.Small.Area`)
carpark_data$`Clue.Small.Area` <-
  gsub(
    "West Melbourne \\(Residential\\)",
    "West Melbourne",
    carpark_data$`Clue.Small.Area`
  )
carpark_data$`Clue.Small.Area` <-
  gsub("West Melbourne \\(Industrial\\)",
       "West Melbourne",
       carpark_data$`Clue.Small.Area`)
carpark_data$`Clue.Small.Area` <-
  gsub("Carlton North", "Carlton", carpark_data$`Clue.Small.Area`)
carpark_data$`Clue.Small.Area` <-
  gsub("South Wharf", "Southbank", carpark_data$`Clue.Small.Area`)
carpark_data$`Clue.Small.Area` <-
  gsub("Flemington", "Kensington", carpark_data$`Clue.Small.Area`)
carpark_data_2020 <- carpark_data %>% filter(Census.Year == 2020)
carpark_result <- carpark_data_2020 %>%
  group_by(`Clue.Small.Area`) %>%
  summarise(total_parking_spaces = sum(`Parking.Spaces`))
# ... [Continue your carpark data processing]
total_sum_parking <-
  sum(carpark_data_2020$`Parking.Spaces`, na.rm = TRUE)


carpark_result$parking_percentage <-
  (carpark_result$total_parking_spaces / total_sum_parking) * 100

max_parking_percentage <-
  max(carpark_result$parking_percentage, na.rm = TRUE)
# ---------------------- Employment Data Processing

employment_data <-
  read.csv("../Data/relation/employment-by-block-by-space-use2.csv",
           header = TRUE)
# ... [All your employment data processing code, with variable names prefixed by employment_]

employment_data$`Clue.Small.Area` <-
  gsub("Melbourne \\(CBD\\)",
       "Melbourne",
       employment_data$`Clue.Small.Area`)
employment_data$`Clue.Small.Area` <-
  gsub("Melbourne \\(Remainder\\)",
       "Melbourne",
       employment_data$`Clue.Small.Area`)
employment_data$`Clue.Small.Area` <-
  gsub(
    "West Melbourne \\(Residential\\)",
    "West Melbourne",
    employment_data$`Clue.Small.Area`
  )
employment_data$`Clue.Small.Area` <-
  gsub(
    "West Melbourne \\(Industrial\\)",
    "West Melbourne",
    employment_data$`Clue.Small.Area`
  )
employment_data$`Total.Jobs.In.Block`[is.na(employment_data$`Total.Jobs.In.Block`)] <-
  0

employment_data_2020 <-
  employment_data %>% filter(Census.Year == 2020)
employment_result <- employment_data_2020 %>%
  
  group_by(`Clue.Small.Area`) %>%
  summarise(total_jobs = sum(`Total.Jobs.In.Block`, na.rm = TRUE))
# ... [Continue your employment data processing]

total_sum_jobs <-
  sum(employment_data_2020$`Total.Jobs.In.Block`, na.rm = TRUE)


employment_result$jobs_percentage <-
  (employment_result$total_jobs / total_sum_jobs) * 100


max_jobs_percentage <-
  max(employment_result$jobs_percentage, na.rm = TRUE)

# ---------------------- Transport Data Processing

transport_data <- employment_data
transport_data_2020 <-
  transport_data %>% filter(Census.Year == 2020)
transport_result <- transport_data_2020 %>%
  group_by(`Clue.Small.Area`) %>%
  summarise(transport_count = n())

total_sum_transport <-
  sum(transport_result$transport_count, na.rm = TRUE)

transport_result$transport_percentage <-
  (transport_result$transport_count / total_sum_transport) * 100

transport_max_transport_count <-
  max(transport_result$transport_count, na.rm = TRUE)
# ---------------------- Block Data Processing

bus_stop_data <-
  read.csv('../Data/blocks-for-census-of-land-use-and-employment-clue.csv')
bus_stop_data <- bus_stop_data %>%
  mutate(lat = as.numeric(sapply(strsplit(
    as.character(Geo.Point), ", "
  ), "[", 1)),
  lng = as.numeric(sapply(strsplit(
    as.character(Geo.Point), ", "
  ), "[", 2)))

bus_stop_data <-
  st_as_sf(bus_stop_data,
           coords = c("lng", "lat"),
           crs = 4326)
bus_stop_data2 <- read.csv('../Data/relation/bus-stops1.csv')
bus_stop_data2 <- bus_stop_data2 %>%
  mutate(lat = as.numeric(sapply(strsplit(
    as.character(Geo.Point.2D), ", "
  ), "[", 1)),
  lng = as.numeric(sapply(strsplit(
    as.character(Geo.Point.2D), ", "
  ), "[", 2)))

bus_stop_data2 <-
  st_as_sf(bus_stop_data2,
           coords = c("lng", "lat"),
           crs = 4326)

distance_threshold <- 500
bus_stop_buffered_data <-
  st_buffer(bus_stop_data, distance_threshold)
bus_stop_joined_data <-
  st_join(bus_stop_data2, bus_stop_buffered_data, join = st_within)

bus_stop_joined_data <- bus_stop_joined_data %>%
  group_by(clue_area) %>%
  summarise(total_stops = n()) %>%
  filter(!is.na(clue_area))
bus_stop_joined_data$clue_area <-
  gsub("Melbourne \\(CBD\\)",
       "Melbourne",
       bus_stop_joined_data$clue_area)
bus_stop_joined_data$clue_area <-
  gsub("Melbourne \\(Remainder\\)",
       "Melbourne",
       bus_stop_joined_data$clue_area)
bus_stop_joined_data$clue_area <-
  gsub(
    "West Melbourne \\(Residential\\)",
    "West Melbourne",
    bus_stop_joined_data$clue_area
  )
bus_stop_joined_data$clue_area <-
  gsub("West Melbourne \\(Industrial\\)",
       "West Melbourne",
       bus_stop_joined_data$clue_area)

bus_stop_data <- data.frame(name = bus_stop_joined_data$clue_area,
                            val = bus_stop_joined_data$total_stops)
bus_stop_joined_data <- bus_stop_data %>%
  group_by(`name`) %>%
  summarise(bus_stop_count = sum(val, na.rm = TRUE))


# ---------------------- cleaning data for liquor_fig5
liquor_data <-
  read.csv("../Data/relation/Metropolitan-as-at-31-December-2020.csv",
           header = FALSE)

liquor_data$V8_lower <- tolower(trimws(liquor_data$V8))
suburbs_lga_mel_lower <- tolower(suburbs_lga_mel)

liquor_filtered_data <-
  liquor_data[liquor_data$V8_lower %in% suburbs_lga_mel_lower,]

lookup <- setNames(suburbs_lga_mel, suburbs_lga_mel_lower)
liquor_filtered_data$V8 <- lookup[liquor_filtered_data$V8_lower]
liquor_filtered_data$V8_lower <- NULL
liquor_filtered_data <- liquor_filtered_data %>%
  group_by(V8) %>%
  summarise(total_liquor_store = n())

# ---------------------- cleaning data for population_fig7

population_fig71_data <-
  read.csv(
    "../Data/relation/city-of-melbourne-population-forecasts-by-small-area-2020-2040.csv"
  )

population_fig71_data$Geography <-
  gsub("Melbourne \\(CBD\\)",
       "Melbourne",
       population_fig71_data$Geography)
population_fig71_data$Geography <-
  gsub(
    "West Melbourne \\(Residential\\)",
    "West Melbourne",
    population_fig71_data$Geography
  )
population_fig71_data$Geography <-
  gsub(
    "South Yarra \\(inc\\. Melbourne Remainder\\)",
    "South Yarra",
    population_fig71_data$Geography
  )
population_fig71_data$Geography <-
  gsub(
    "West Melbourne \\(Industrial\\)",
    "West Melbourne",
    population_fig71_data$Geography
  )

population_fig71_data <-
  population_fig71_data[!population_fig71_data$Geography %in% c("Greater Melbourne", "City of Melbourne"), ]

population_fig71_data_2020 <-
  population_fig71_data %>% filter(Year == 2021)
population_fig71_data_2020$Value[is.na(population_fig71_data_2020$Value)] <-
  0
population_fig71_result <- population_fig71_data_2020 %>%
  group_by(Geography) %>%
  summarise(total_population = sum(Value))

population_fig71_result <- population_fig71_result %>%
  mutate(label = paste(Geography, total_population, sep = " "))

population_fig71_result <- population_fig71_result %>%
  filter(!is.na(total_population))



scaling_factor <-
  100 / log(max(population_fig71_result$total_population) + 1)

population_fig71_result <- population_fig71_result %>%
  mutate(Scaled_population = log(total_population + 1) * scaling_factor)

population_fig71_data_final <- data.frame(
  individual = population_fig71_result$label,
  group = population_fig71_result$Geography,
  value = population_fig71_result$Scaled_population
)


total_sum_population <-
  sum(population_fig71_result$total_population, na.rm = TRUE)

population_fig71_result$population_percentage <-
  (population_fig71_result$total_population / total_sum_population) * 100


# --------------------------------- Crime Data Processing
data_crime <-
  read_excel("../Data/crime/LGA_Recorded_Offences_Year_Ending_June_2023.xlsx",
             sheet = "Table 03")
carpark_data$`Clue.Small.Area` <-
  gsub("Carlton North", "Carlton", carpark_data$`Clue.Small.Area`)
carpark_data$`Clue.Small.Area` <-
  gsub("South Wharf", "Southbank", carpark_data$`Clue.Small.Area`)
carpark_data$`Clue.Small.Area` <-
  gsub("Flemington", "Kensington", carpark_data$`Clue.Small.Area`)
filter_data <-
  data_crime[grep("Melbourne", data_crime$`Local Government Area`, ignore.case = TRUE), ]
filter_data <- filter_data %>% filter(Year == 2020)
crime_result <- filter_data %>%
  group_by(`Suburb/Town Name`) %>%
  summarise(total_offence_count = sum(`Offence Count`))

# 计算工作数据集的总和
total_crime <- sum(crime_result$total_offence_count, na.rm = TRUE)

crime_result$crime_percentage <-
  (crime_result$total_offence_count / total_crime) * 100
# --------------------------------- Traffic Data Processing

# Victoria street line data
mel_traffic_lines <-
  st_read("../Data/transportation/mel_traffic_vol.geojson")
# LGA melbourne shape data
mel_suburbs_gda94 <-
  st_read("../Data/Mel_LGA_Suburbs_GDA94/mel_suburbs_edit.geojson")
# extract the geometry center of the city of melbourne for initialize view
city_mel_whole_shape <-
  st_read("../Data/city_of_mel_boundary/mel_boundary.shp")

# transform to the same coordinate system
mel_traffic_lines_wgs84 <-
  st_transform(mel_traffic_lines, crs = 4326)
mel_suburbs_wgs84 <- st_transform(mel_suburbs_gda94, crs = 4326)

mel_st_lines_sff <-
  st_intersection(mel_traffic_lines_wgs84, mel_suburbs_wgs84)

# Convert the geometry column back to multilinestring
mel_st_lines_sff <- st_cast(mel_st_lines_sff, "MULTILINESTRING")
mel_st_lines_sff <- mel_st_lines_sff %>% distinct()
traffic_result <- mel_st_lines_sff %>%
  group_by(`clue_area`) %>%
  summarise(total_traffic_count = sum(`ALLVEHS_AADT`))

total_traffic <-
  sum(traffic_result$total_traffic_count, na.rm = TRUE)

traffic_result$traffic_percentage <-
  (traffic_result$total_traffic_count / total_traffic) * 100
# -----------------------------------Produce new Data

suburb_realtion <- data.frame(Suburb = suburbs_lga_mel)
suburb_realtion <- suburb_realtion %>%
  left_join(crime_result, by = c("Suburb" = "Suburb/Town Name"))
suburb_realtion <- suburb_realtion %>%
  left_join(traffic_result, by = c("Suburb" = "clue_area"))
suburb_realtion <- suburb_realtion %>%
  left_join(carpark_result, by = c("Suburb" = "Clue.Small.Area"))
suburb_realtion <- suburb_realtion %>%
  left_join(bus_stop_joined_data, by = c("Suburb" = "name"))
suburb_realtion <- suburb_realtion %>%
  left_join(liquor_filtered_data, by = c("Suburb" = "V8"))
suburb_realtion <- suburb_realtion %>%
  left_join(employment_result, by = c("Suburb" = "Clue.Small.Area"))
suburb_realtion <- suburb_realtion %>%
  left_join(transport_result, by = c("Suburb" = "Clue.Small.Area"))
suburb_realtion <- suburb_realtion %>%
  left_join(population_fig71_result, by = c("Suburb" = "Geography"))
suburb_realtion$total_liquor_store[is.na(suburb_realtion$total_liquor_store)] <-
  0
suburb_realtion <- suburb_realtion %>%
  mutate_if(is.numeric, ~ round(., 2))




######################Traffic Data #################
# Victoria street line data
mel_traffic_lines <-
  st_read("../Data/transportation/mel_traffic_vol.geojson")

# LGA melbourne shape data
mel_suburbs_gda94 <-
  st_read("../Data/Mel_LGA_Suburbs_GDA94/mel_suburbs_edit.geojson")

# extract the geometry center of the city of melbourne for initialize view
city_mel_whole_shape <-
  st_read("../Data/city_of_mel_boundary/mel_boundary.shp")

stats_crash_at_streets <-
  st_read('../Data/transportation/stats_crash_at_streets.geojson')
##################### t
# Data Manipulation #
##################### t
# transform to the same coordinate system
mel_traffic_lines_wgs84 <-
  st_transform(mel_traffic_lines, crs = 4326)

mel_suburbs_wgs84 <- st_transform(mel_suburbs_gda94, crs = 4326)

city_mel_whole_shape_wgs84 <-
  st_transform(city_mel_whole_shape, crs = 4326)

stats_crash_at_streets <-
  st_transform(stats_crash_at_streets, crs = 4326)


####### city_mel_whole_shape data

# get the centroid
center_point <- st_centroid(city_mel_whole_shape_wgs84$geometry)



####### city_mel streets line traffic data

mel_st_lines_sf <-
  st_intersection(mel_traffic_lines_wgs84, mel_suburbs_wgs84)

# Convert the geometry column back to multilinestring
mel_st_lines_sf <- st_cast(mel_st_lines_sf, "MULTILINESTRING")

temp <-
  st_drop_geometry(mel_st_lines_sf) %>% select(-clue_area) %>% distinct()

mel_st_lines_sf <- mel_st_lines_sf[rownames(temp),]

# Define a color palette with 6 bins based on log(mel_st_lines_sf$ALLVEHS_AADT) since
# the data is high in variance
my_colors <-
  c("red4",
    "red2",
    "darkorange",
    "goldenrod",
    "yellowgreen",
    "forestgreen",
    'darkgreen')
pal <-
  colorBin(
    palette = my_colors,
    domain = log(mel_st_lines_sf$ALLVEHS_AADT),
    bins = 7,
    reverse = TRUE
  )

# define a label design for each street line triggered during hovering event on map
myStLabel <- paste(
  'Street Name: <strong>',
  mel_st_lines_sf$LOCAL_ROAD_NM,
  '</strong><br/>',
  'Section Id: <strong>',
  mel_st_lines_sf$OBJECTID_1,
  '</strong><br/>',
  'Flow: <strong>',
  mel_st_lines_sf$FLOW,
  '</strong><br/>',
  'Street Type: <strong>',
  mel_st_lines_sf$RMA_DESC,
  '</strong><br/>',
  'Clue Area Name: <strong>',
  mel_st_lines_sf$clue_area,
  '</strong><br/>',
  'Annual Average Daily Traffic Volume: <strong>',
  mel_st_lines_sf$ALLVEHS_AADT,
  '</strong><br/>'
) %>%
  lapply(htmltools::HTML)



##### bubble chart data
# For plotly bubble chart
temp_df <- st_drop_geometry(mel_st_lines_sf) %>%
  select(c(
    LOCAL_ROAD_NM,
    RMA_DESC,
    ALLVEHS_AADT,
    EHV,
    GROWTH_RATE,
    clue_area
  ))

# prepare final plotly bubble chart use dataframe
bubble_df <- temp_df %>%
  group_by(clue_area, LOCAL_ROAD_NM) %>%
  summarise(
    ALLVEHS_AADT = sum(ALLVEHS_AADT),
    RMA_DESC = first(RMA_DESC),
    # 保留第一个 RMA_DESC
    EHV = round(mean(EHV), 3),
    GROWTH_RATE = round(mean(GROWTH_RATE), 3)
  ) %>%
  ungroup()

# define proper bubble size for viz
bubble_df$size <- round(bubble_df$EHV * 3 * 10 ^ 2)

# define colors for bubble chart
bubble_colors <-
  c('#4AC6B7', '#1972A4', '#965F8A', '#FF7070', '#C61951')


####### suburb selection plot data
suburb_summary <- mel_st_lines_sf %>%
  group_by(clue_area) %>%
  summarize(
    total_ALLVEHS_AADT = sum(ALLVEHS_AADT, na.rm = TRUE),
    total_TRUCKS_AADT = sum(ifelse(is.na(TRUCKS_AADT), 0, TRUCKS_AADT)),
    mean_HHF_PMPEAK_AADT = round(mean(ALLVEH_PMPEAK_AADT, na.rm = TRUE)),
    mean_HHF_AMPEAK_AADT = round(mean(ALLVEH_AMPEAK_AADT, na.rm = TRUE)),
    avg_GROWTH_RATE = round(mean(GROWTH_RATE, na.rm = TRUE), 4)
  )

# drop geometry
suburb_summary <- st_drop_geometry(suburb_summary)


suburb_summary_rank <- suburb_summary %>%
  mutate_at(vars(-clue_area), list(rank = ~ rank(-., ties.method = "min")))

suburb_summary_rank <-
  suburb_summary_rank %>% mutate(avg_GROWTH_RATE = round(avg_GROWTH_RATE * 100, 2))

suburb_summary_rank$avg_GROWTH_RATE <-
  paste(suburb_summary_rank$avg_GROWTH_RATE, "%", sep = "")


######################Crime Data #################

#------------------------------  Setup
#load data
melb_clue <-
  st_read("../Data/Mel_LGA_Suburbs_GDA94/mel_suburbs_edit.geojson") %>% st_transform(crs = 4326)
crime_data <- read.csv('../Data/crime/crime_clue.csv', header = T)

#pre-processing
melb_clue$center <- sf::st_centroid(melb_clue$geometry)
colnames(melb_clue)[colnames(melb_clue) == "clue_area"] <- "Clue"
colnames(crime_data)[colnames(crime_data) == "Suburb.Town.Name"] <-
  "Clue"
colnames(crime_data)[colnames(crime_data) == "Offence.Division"] <-
  "Offence.Type"


crime_data$Offence.Count <-
  as.numeric(gsub(",", "", crime_data$Offence.Count))
crime_data$Clue <- sub("Flemington", "Kensington", crime_data$Clue)
crime_data$Clue <- sub("Carlton North", "Carlton", crime_data$Clue)
crime_data$Clue <- sub("South Wharf", "Southbank", crime_data$Clue)

crimeClueType <-
  crime_data %>% group_by(Clue, Offence.Type) %>% summarise(Total_Offences = sum(Offence.Count, na.rm = TRUE))

crimeYearType <-
  crime_data %>% group_by(Year, Offence.Type) %>% summarise(Total_Offences = sum(Offence.Count, na.rm = TRUE))
crimeType <-
  crime_data %>% group_by(Offence.Type) %>% summarise(Total_Offences = sum(Offence.Count, na.rm = TRUE))

crimeByClue <- crime_data %>% group_by(Clue) %>%
  summarise(
    Total_Offences = sum(Offence.Count, na.rm = TRUE),
    content = htmltools::HTML(
      htmltools:::as.character.shiny.tag.list(
        htmlwidgets:::as.tags.htmlwidget(
          data.frame(x = as.character(Year),
                     y = Offence.Count) %>%
            group_by(x) %>%
            summarise(total = sum(y, na.rm = TRUE)) %>%
            e_charts(x, width = 450, height = 300) %>%
            e_bar(total, legend = FALSE, color = 'skyblue') %>%
            e_tooltip(
              trigger = "axis",
              formatter = htmlwidgets::JS(
                "function(params) {
                       var year = params[0].name;
                      var value = params[0].value[1];
                      return 'Year: ' + year + '<br/>Offence Count: ' + value;}"
              )
              
            ) %>%
            e_x_axis(
              axisLabel = list(
                interval = 0,
                rotate = 60,
                fontSize = 8,
                lineHeight = 40
              ),
              name = "Year"
            ) %>%
            e_title("Offence Count Per Year")
          
        )
      )
    )
  )

# crime_grouped <- crime_data %>% group_by(Suburb.Town.Name,Offence.Division,Year) %>% summarise(Total_Offence_Count = sum(Offence.Count, na.rm = TRUE))

merge_data <-
  merge(crimeByClue,
        melb_clue,
        by.x = "Clue",
        by.y = "Clue",
        all = FALSE)

#------------------------------  Map
merge_data$Color <- factor(
  ifelse(
    merge_data$Total_Offences < 5000,
    "<5000",
    ifelse(
      merge_data$Total_Offences < 20000,
      "5000~20000",
      ifelse(merge_data$Total_Offences < 50000, "20000~50000", ">50000")
    )
  ),
  levels = c("<5000", "5000~20000", "20000~50000", ">50000")
)
cpol <-
  colorFactor(
    "BuPu",
    na.color = 'grey',
    ordered = TRUE,
    domain = merge_data$Color
  )

html <- leaflet(merge_data) %>%
  addMapboxTiles(style_url = "mapbox://styles/cheryl-chenccc/clo0z5zuo005d01rfep1c4hqj",
                 access_token = 'pk.eyJ1IjoiY2hlcnlsLWNoZW5jY2MiLCJhIjoiY2wyZGJtaHk2MHhweDNjbzIyaWk2ODlqdCJ9.nSmaPBChoCWG7b-VQmpKsA',
                 username = "cheryl-chenccc") %>%
  setView(
    lng = as.numeric(st_coordinates(center_point)[, 'X']),
    lat = as.numeric(st_coordinates(center_point)[, 'Y']),
    zoom = 14
  ) %>%
  addPolygons(
    data = melb_clue,
    group = merge_data$Clue,
    color = 'white',
    opacity = 0.7,
    fillOpacity = 0.6,
    # fillColor = 'lightgrey',
    weight = 4,
    dashArray = "10",
    #fillColor =~color_palette(Total_Offence_Count),
    fillColor = cpol(merge_data$Color),
    # fillOpacity = 0.6,
    # weight = 4,
    stroke = T,
    popup = merge_data$content,
    popupOptions = popupOptions(minWidth = 450, maxHeight = 300),
    label = ~ lapply(
      paste(
        "<b>Suburb Name: </b>",
        merge_data$Clue,
        "<br>",
        "<b>Total Offences:</b>",
        merge_data$Total_Offences
      ),
      HTML
    ),
    
    highlightOptions = highlightOptions(
      weight = 5,
      color = "white",
      dashArray = "",
      fillOpacity = 1,
      bringToFront = TRUE
    )
  ) %>%
  onRender("function(el,x) {
  this.on('popupopen', function() {HTMLWidgets.staticRender();})
  }") %>%
  addLegend(
    position = "bottomright",
    pal = cpol,
    values = ~ Color,
    labels = ~ Clue,
    title = "Total Offence Count (2014-2023)"
  ) %>%
  # addLayersControl(
  #   overlayGroups = ~Clue,
  #   options = layersControlOptions(collapsed = FALSE)
  # ) %>%
  tagList(htmlwidgets::getDependency("echarts4r", "echarts4r"))





#################################CLEANING DATA DONE#########################################################

#####################
# USER INTERFACE ####
#####################

######################### Home Page Tab Start ##############################################################
homepage <- tabPanel(title = 'Home',
                     mainPanel(
                       id = 'home',
                       width = 12,
                       tableauPublicViz(id = 'tableauViz',
                                        url = 'https://public.tableau.com/views/IVasmt3/CityofMelbourneAnalysis-publictransportcrime2?:language=en-US&:display_count=n&:origin=viz_share_link',
                                        height = "850px")
                     ))
######################### Home Page Tab Done ##############################################################

######################### First Nav Tab Start ##############################################################
traffic_tab <-   tabPanel(
  id = 'Traffic',
  title = 'Traffic',
  width = 12,
  h5("Average Daily Traffic Volume In City Of Melbourne",
     style = "text-align: center; font-weight: bold; font-size: 1.5em;"),
  div(
    class = 'my-map-container',
    style = "position: relative;",
    leafletOutput('map', height = "800px"),
    uiOutput("float_window"),
    # Render the UI element here
    uiOutput('float_filter'),
    uiOutput('overview')
  )
)
######################### First Nav Tab Done ###############################################################

######################### Second Nav Tab Start #############################################################
# Define a tab panel titled 'Type and Rating'
crime_tab <- tabPanel(
  title = 'Crime',
  id = 'Crime',
  mainPanel(
    id = "crime_panel",
    width = 12,
    # Define a set of tabs within the main panel
    tabsetPanel(
      # Define the first tab panel containing Pie Charts
      tabPanel(
        title = 'Crime Offence Map',
        h5("Total Offence count in City of Melbourne from 2014-2023",
           style = "text-align: center; font-weight: bold; font-size: 1.5em;"),
        uiOutput('plot_map', height = '750px')
      ),
      # Define the second tab panel containing Bar Charts
      tabPanel('Crime Analysis',
               mainPanel(
                 width = 12,
                 div(
                   style = "position:relative; z-index:1;",
                   
                   # First row with two inputs side by side
                   div(
                     style = "position:relative; z-index:1;",
                     
                     # CLUE Area input at 25%
                     div(
                       style = "position:absolute; top:0px; left:calc(4%); font-size:14px; z-index:2; color:white;' >",
                       selectInput(
                         "clueInput",
                         "Select A Suburb:",
                         choices = unique(crime_data$Clue),
                         selected = "Carlton"
                       )
                     ),
                     
                     # Crime Type input at 75%
                     div(
                       style = "position:absolute; top:0px; left:calc(45%); font-size:14px; z-index:2; color:white;' >",
                       pickerInput(
                         "crimeType",
                         "Select A Crime Type:",
                         choices = unique(crime_data$Offence.Type),
                         selected = unique(crime_data$Offence.Type),
                         multiple = TRUE
                       )
                     ),
                     
                     div(style = "clear:both;") # Clear the float
                   ),
                   
                   # Second row with the plots
                   
                 ),
                 div(style = "margin-top: 70px;",
                     #plotlyOutput("plot", height=750)
                     uiOutput("inputCheck", height = '750px'))
               ))
    )
  )
)
######################### Second Nav Tab Done ##############################################################
######################### Third Nav Tab Start #############################################################
# Define a tab panel titled 'Type and Rating'
relation_tab <- tabPanel(title = 'Relation',
                         
                         mainPanel(
                           id = "relation",
                           width = 12,
                           # Define a set of tabs within the main panel
                           tabsetPanel(
                             tabPanel(
                               "Traffic Factor Analysis",
                               div(
                                 style = "position:relative; z-index:1;",
                                 
                                 div(HTML(
                                   paste0(
                                     "<div style='position:absolute; top:0px; left:calc(70% - 20px); font-size:14px; z-index:2;' title='Sort the number by Traffic Volumn and Suburb name'>",
                                     as.character(actionLink(
                                       inputId = "sort_button1",
                                       label = "Sort",
                                       icon = icon("sort")
                                     )),
                                     "</div>"
                                   )
                                 )),
                                 div(style = "margin-top: 10px;",
                                     plotlyOutput("plot_traffic", height = '750px'))
                               )
                             ),
                             
                             tabPanel(
                               "Crime Factor Analysis",
                               div(
                                 style = "position:relative; z-index:1;",
                                 
                                 div(HTML(
                                   paste0(
                                     "<div style='position:absolute; top:0px; left:calc(70% - 20px); font-size:14px; z-index:2; color:white;' title='Sort the number by Crime number and Suburb name'>",
                                     "<a href='#' id='sort_button2' onclick='Shiny.setInputValue(\"sort_button2\", Math.random());'><i class='fa fa-sort'></i> Sort</a>",
                                     "</div>"
                                   )
                                 )),
                                 div(style = "margin-top: 10px;",
                                     plotlyOutput("plot_crime", height = '750px'))
                               )
                               
                               
                             ),
                             tabPanel(
                               "Correlation",
                               div(
                                 style = "position:relative; z-index:1;",
                                 
                                 div(HTML(
                                   paste0(
                                     "<div style='position:absolute; top:0px; left:calc(70% - 20px); font-size:14px; z-index:2;' title='Select the factor'>",
                                     as.character(
                                       selectInput(
                                         'factor',
                                         label = 'Select A Factor',
                                         choices = c(
                                           'Traffic: Parking',
                                           'Traffic: Transport',
                                           'Traffic: Bus Stop',
                                           'Traffic: Population',
                                           'Crime: Employment',
                                           'Crime: Liquor',
                                           'Crime: Population'
                                           
                                         ),
                                         selected = 'Crime: Population',
                                       )
                                     ),
                                     "</div>"
                                   )
                                 )),
                                 div(style = "margin-top: 10px;",
                                     plotlyOutput("plot_relation", height = '750px'))
                                 
                                 
                               )
                             )
                           )
                         ))


######################### Third Nav Tab Done ##############################################################

##########################  UI ######################
ui <- navbarPage(
  header = setUpTableauInShiny(),
  id = 'mypage',
## UI CSS ######################################
  tags$head(tags$style(
    HTML(
      "
     .navbar .navbar-brand {
      color: skyblue !important;
      font-weight: bold !important;
      font-size: 30px !important;
     }
    .shiny-tab-input {
      font-size: 20px !important;
    }

    .sort_button {
      display: inline !important;
      margin: 0px !important;
      color: grey !important;
      text-align: right !important;

    }
    .shiny-html-output.shiny-bound-output.shiny-output-error {
    display: none;
    }
    
    .my-table-text {
      font-size: 14px;
      color: black;
      text-align: justify;
      opacity: 0.85;
      position:absolute;
      top: 320px;
      left: 25px;
      right: 25px;
    }


    .my-bubble-chart-text {
      font-size: 14px;
      color: black;
      text-align: justify;
      opacity: 0.85;
    }

    .my-overview {
      position: absolute;
      top: 20%;
      left: 30%;
      width: 800px;
      height: 500px;
      background-color:rgb(224, 220, 220, 1);
      border: 0px #ccc;
      padding: 20px;
    }
    



    .my-reset-button {
      position: absolute;
      bottom: 5%;
      left: 82%;
      background-color: skyblue; 
      color: white; 
      border: 2px solid white; 
      border-radius: 5px; 
      padding: 5px 5px;
      font-size: 12px; 
    }

    .my-reset-button:hover {
      background-color: #EC7F0C; 

    }
      .card {
      justify-content: center;
      align-items: center;
      border: 1px solid #ddd;
      border-radius: 8px;
      padding: 20px;
      box-shadow: 2px 2px 12px #aaa;
      margin: 20px;
      text-align: center;
      background-color: #e0e0e0;
      color: white;
      }
        #about {
      width: 100% !important;
    }

    "
    )
  ))
## UI CSS ###################################### 
  ,
  # this is needed to be able to change the selected tab from code
  title = 'City of Melbourne',
  theme = shinythemes::shinytheme("cyborg"),
  fluid = TRUE,
  collapsible = TRUE,
  homepage,
  traffic_tab,
  crime_tab,
  relation_tab,
  # To avoid Waring
  footer = tags$div(""),
  navbarMenu(
    "More",
    tabPanel("About",mainPanel(htmltools::includeMarkdown("readMe.md")))
  )
)

###################
# SHINY SERVER ####
###################
server <- function(input, output, session) {
  observeEvent(input$mypage, {
    runjs('dispatchEvent(new Event("resize"))')
  })
  
  #########################Crime Part ##############################
  #------------------------------  Map
  
  # Crime Map
  output$plot_map <- renderUI({
    html[[1]]$sizingPolicy$defaultHeight <- 750
    html %>%
      browsable()
  })
  
  #------------------------------  Filter
  clue_filter <- reactive({
    filtered_data <- crime_data %>%
      filter(Clue %in% input$clueInput,
             Offence.Type %in% input$crimeType) %>%
      group_by(Year, Offence.Type) %>%
      summarise(Total_Offences = sum(Offence.Count, na.rm = TRUE))
    return(filtered_data)
  })
  
  bar_filter <- reactive({
    filtered_data <- crime_data %>%
      filter(Clue %in% input$clueInput,
             Offence.Type %in% input$crimeType) %>%
      group_by(Clue, Offence.Type) %>%
      summarise(Total_Offences = round(mean(Offence.Count, na.rm = TRUE)))
    return(filtered_data)
  })
  
  line_filter <- reactive({
    filtered_data <- crime_data %>%
      filter(Offence.Type %in% input$crimeType) %>%
      group_by(Offence.Type) %>%
      summarise(Total_Offences = round(mean(Offence.Count, na.rm = TRUE)))
    return(filtered_data)
  })
  
  #------------------------------  Plot1
  #plot1
  output$plot <- renderPlotly({
    filtered_data <- clue_filter()
    plot1 <- ggplotly (
      ggplot(
        filtered_data,
        aes(x = Year, y = Total_Offences, fill = Offence.Type)
      ) +
        #geom_point_interactive(aes(tooltip = Total_Offences, data_id = continent), size = 2)+
        geom_area(
          alpha = 0.6 ,
          linewidth = .5,
          colour = "white"
        ) +
        scale_fill_viridis(discrete = T) +
        theme_ipsum() +
        ggtitle("Crime Types in seleced clue area") +
        theme_minimal() +
        theme(
          panel.background = element_rect(fill = "black"),
          plot.background = element_rect(fill = "black"),
          axis.text = element_text(color = "white"),
          axis.title = element_text(color = "white"),
          legend.text = element_text(color = "white"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          plot.title = element_text(color = "white", hjust = 0.5)  # Centering the title
        ) +
        ggtitle("Total Offence Count In Selected Suburb")
    )
    bar_data <- bar_filter()
    line_data <- line_filter()
    
    plot2 <- ggplot() +
      geom_bar(
        data = bar_data,
        aes(x = Offence.Type, y = Total_Offences),
        fill = "lightblue",
        alpha = 0.5,
        linewidth = 0.5,
        color = "white",
        stat = "identity",
        position = "dodge",
        width = 0.7
      ) +
      geom_line(
        data = line_data,
        aes(
          x = Offence.Type,
          y = Total_Offences,
          color = "Mean Offences in City of Melbourne",
          group = 1,
          alpha = 0.5
        )
      ) +
      geom_point(
        data = line_data,
        aes(x = Offence.Type, y = Total_Offences),
        color = "white",
        shape = 19,
        size = 3
      ) +
      #geom_text(data = bar_data, aes(x = Offence.Type, y = Total_Offences, label = Total_Offences), color = "white", size = 4, position = position_dodge(width = 0.7), vjust = 5) +
      #geom_text(data = line_data, aes(x = Offence.Type, y = Total_Offences, label = Total_Offences), color = "white", size = 4, vjust = -1, textposition = "top center") +
      scale_color_manual(values = c("white"), name = "Mean Offences in City of Melbourne") +
      theme_minimal() +
      theme(
        axis.text.x = element_text(angle = -45, hjust = 1),
        panel.background = element_rect(fill = "black"),
        plot.background = element_rect(fill = "black"),
        axis.text = element_text(color = "white"),
        axis.title = element_text(color = "white"),
        legend.text = element_text(color = "white"),
        legend.background = element_rect(fill = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title = element_text(color = "white", hjust = 0.5)  # Centering the title
      ) +
      ggtitle("Mean Offence Count in selected Clue area VS City of Melbourne")

    plot2 <- ggplotly(plot2)
    
    fig <- subplot(plot1,
                   plot2,
                   nrows = 1,
                   shareX = FALSE,
                   shareY = FALSE)
    fig <- fig %>% layout(
      title = list(text = '', y = 0.998),
      plot_bgcolor = '#e5ecf6',
      xaxis = list(
        zerolinecolor = '#ffff',
        zerolinewidth = 2,
        gridcolor = 'ffff'
      ),
      yaxis = list(
        zerolinecolor = '#ffff',
        zerolinewidth = 2,
        gridcolor = 'ffff'
      ),
      margin = list(
        l = 50,
        r = 50,
        b = 10,
        t = 50
      ) 
    )
    # Update title
    annotations = list(
      list(
        x = 0.2,
        y = 1.0,
        text = "Total Offence Count In Selected Suburb",
        xref = "paper",
        yref = "paper",
        xanchor = "center",
        yanchor = "bottom",
        showarrow = FALSE,
        font = list(size = 20)
      ),
      list(
        x = 0.8,
        y = 1,
        text = "Mean Offence Count: Selected Suburb Vs City Of Melbourne",
        xref = "paper",
        yref = "paper",
        xanchor = "center",
        yanchor = "bottom",
        showarrow = FALSE,
        font = list(size = 20)
      )
    )
    
    fig <-
      fig %>% layout(
        showlegend = TRUE,
        annotations = annotations,
        plot_bgcolor = 'black',
        paper_bgcolor = 'black',
        font = list(color = "white")
      )
    fig
    
  })
  
  
  #------------------------------  Check Input is null
  output$inputCheck <- renderUI({
    if (is.null(input$crimeType) || length(input$crimeType) == 0) {
      # if (is.null(input$crimeType)) {
      return(tags$div(class = "card", h3(
        sprintf("Please at least select one Crime Type") %>% lapply(htmltools::HTML)
      )))
    } else {
      return(plotlyOutput("plot", height = "750px"))
    }
  })
  
  
  #############################Crime Part ########################
  
  ###########################Traffic Part#########
  
  # 在server函数中创建reactiveValues对象
  values <- reactiveValues(
    clicked_line = NULL,
    clicked_polygon = NULL,
    clicked_reset = NULL,
    clicked_overview = FALSE
  )
  
  # A reactive data filter to filter out data belongs to the selected state and date range
  getFilteredStreetData <- reactive({
    line = values$clicked_line
    if (!is.null(line)) {
      filtered_df <- stats_crash_at_streets %>%
        filter(OBJECTID_1 == line$OBJECTID_1)
      return(filtered_df)
    }
    
    
    
    
  })
  
  
  getFilteredPolygonData <- reactive({
    polygon = values$clicked_polygon
    
    if (!is.null(polygon)) {
      # filter to clicked polygon
      filtered_df <- suburb_summary_rank %>%
        filter(clue_area == polygon$clue_area)
      return(filtered_df)
    }
  })
  
  
  observeEvent(input$map_shape_click, {
    click_event <- input$map_shape_click
    if (input$mypage == "crime") {
      # print("crime-----")
      selected_clue <- click_event$id
      data_filtered <- data[data$Clue == selected_clue,]
      
      # bar for selected CLUE
      output$barplot <- renderPlot({
        ggplot(data_filtered, aes(x = Clue, y = Total_Offences)) +
          geom_bar(stat = "identity") +
          ggtitle(paste("Bar Plot for CLUE area: ", selected_clue))
      })
    }
    
    click <- input$map_shape_click
    if (input$mypage == "Traffic") {
      # print("-----1")
      if (class(click$id) == 'integer') {
        # print(-----2)
        # the line has been clicked
        line <-
          mel_st_lines_sf[mel_st_lines_sf$OBJECTID_1 == click$id,]
        
        values$clicked_line <- line
        
        values$clicked_polygon <- NULL
        
      } else {
        # the polygon has been clicked
        updateSelectInput(session, 'suburb', selected = input$map_shape_click)
        
        polygon <-
          mel_suburbs_wgs84[mel_suburbs_wgs84$clue_area == click$id,]
        
        values$clicked_polygon <- polygon
        
        values$clicked_line <- NULL
      }
    }
  })
  
  
  observeEvent(input$subtraff_overview, {
    values$clicked_overview <- input$subtraff_overview
    
  })
  
  
  # 添加点击按钮关闭窗口的事件
  observeEvent(input$reset_button, {
    values$clicked_line <- NULL
    
    values$clicked_polygon <- NULL
    
    updateSelectInput(session, 'suburb', selected = "")
    
    updateCheckboxInput(session, 'subtraff_overview', value = FALSE)
    
    values$clicked_reset <- NULL
    
  })
  

  observeEvent(input$suburb, {
    temp = mel_suburbs_wgs84[mel_suburbs_wgs84$clue_area == input$suburb,]
    
    if (dim(temp)[1] == 0) {
      values$clicked_polygon <- NULL
    } else {
      # Clear the selection sidebar, stop the selection
      values$clicked_polygon <-
        mel_suburbs_wgs84[mel_suburbs_wgs84$clue_area == input$suburb,]
    }
    
    # Change the "state" input on the map tab to the state selected on the state map; session is the parameter of the server() function
    updateSelectInput(session, 'suburb', selected = input$suburb)
  })
  
  
  ##### map tab
  output$map <- renderLeaflet({
    map <- leaflet() %>%
      addMapboxTiles(style_url = "mapbox://styles/cheryl-chenccc/clo0z5zuo005d01rfep1c4hqj",
                     access_token = 'pk.eyJ1IjoiY2hlcnlsLWNoZW5jY2MiLCJhIjoiY2wyZGJtaHk2MHhweDNjbzIyaWk2ODlqdCJ9.nSmaPBChoCWG7b-VQmpKsA',
                     username = "cheryl-chenccc") %>%
      setView(
        lng = as.numeric(st_coordinates(center_point)[, 'X']),
        lat = as.numeric(st_coordinates(center_point)[, 'Y']),
        zoom = 14
      ) %>%
      addPolygons(
        data = mel_suburbs_wgs84,
        color = 'white',
        opacity = 0.7,
        fillOpacity = 0.3,
        fillColor = 'lightgrey',
        weight = 4,
        dashArray = "10",
        highlightOptions = highlightOptions(fillColor = 'rgb(205, 203, 203)',
                                            fillOpacity = 0.9),
        layerId = ~ clue_area
      ) %>%
      addPolylines(
        data = mel_st_lines_sf,
        color = ~ pal(log(mel_st_lines_sf$ALLVEHS_AADT)),
        opacity = 0.8,
        weight = 6,
        # add a highlight effect when hovering on lines
        highlightOptions = highlightOptions(
          weight = 8,
          color = "white",
          bringToFront = TRUE
        ),
        label = myStLabel,
        layerId = ~ OBJECTID_1
      )
    
    
    
  })
  
  
  # Add a reactive to track whether a shape is clicked
  output$float_window <- renderUI({
    polygon <- values$clicked_polygon
    line <- values$clicked_line
    
    if (is.null(polygon) & is.null(line)) {
      return()
    }
    
    if (!is.null(values$clicked_polygon)) {
      div(
        style = "position: absolute; top: 10px; right: 10px; width: 450px; height: 450px; background-color: rgb(145, 145, 145, 0.9);",
        uiOutput('float_window_head'),
        div(style = "position: absolute; top: 8%; width: 100%; height: 92%; background-color: rgb(224, 220, 220, 0.9); padding: 10px;",
            plotlyOutput(outputId = 'stat_polygon_plot'),
            helpText('Note that this dataset has only contain the traffic volume data in 2020. 
                     However, since the data is daily average data, it is still useful for analysis.',
                     class = 'my-table-text')
        )
        
      )
    } else {
      div(
        style = "position: absolute; top: 10px; right: 10px; width: 450px; height: 450px; background-color: rgb(145, 145, 145);",
        uiOutput('float_window_head'),
        div(style = "position: absolute; top: 8%; width: 100%; height: 92%; background-color: rgb(224, 220, 220, 0.8); padding: 10px;",
            plotlyOutput(outputId = 'stat_line_plot'))
        
      )
    }
  })
  
  
  output$overview <- renderUI({
    if (values$clicked_overview) {
      div(
        class = 'my-overview',
        plotlyOutput('myBubbleChart'),
        helpText(
          'A bubble chart illustrates the possible relationship
                   between predicated Traffic Volume Growth Rate and Daily
                   Average Traffic Volume (2020 Traffic Volume Data).
                   The size of the bubbles represent the Equivalent Heavy Vehicle (EHV)
                   Ratio (i.e. %Heavy Vehicle + 3 * %Light Vehicle). Thus, the
                   larger the bubble, the heavier traffic.',
          class = 'my-bubble-chart-text'
        )
      )
    }
    
    
  })
  
  output$float_filter <- renderUI({
    div(
      style = "position: absolute; top: 10px; left: 60px; width: 310px; height: 120px; background-color: rgb(145, 145, 145); border: 0px #ccc; padding: 10px",
      selectInput(
        inputId = 'suburb',
        label = tags$span("Suburbs In City Of Melbourne", style = "color: white;"), 
        choices = c(
          "(Please Select)" = "",
          'Carlton',
          'Docklands',
          "East Melbourne",
          "Kensington",
          "Melbourne",
          "North Melbourne",
          "Parkville",
          "Port Melbourne",
          "South Yarra",
          "Southbank",
          "West Melbourne"
        ),
        selected = ""
      ),
      checkboxInput(
        inputId = 'subtraff_overview',
        label = tags$span('Suburb Streets Traffic Growth Rate', style = "color: white;"),
        value = FALSE
      ),
      # action button itself is a div()
      actionButton(
        inputId = "reset_button",
        label = "Reset",
        class = "my-reset-button"
      )
    )
  })
  
  output$float_window_head <- renderUI({
    polygon <- values$clicked_polygon
    line <- values$clicked_line
    
    
    if (!is.null(polygon)) {
      HTML(as.character(
        div(style = "position: relative; left: 2%; color: white; font-weight: bold; font-size: 20px; padding: 6px 6px;",
            polygon$clue_area)
      ))
    } else {
      HTML(as.character(
        div(style = "position: relative; left: 2%; color: white; font-weight: bold; font-size: 20px; padding: 6px 6px;",
            line$LOCAL_ROAD_NM)
      ))
    }
    
  })
  
  
  output$stat_polygon_plot <- renderPlotly({
    # hist(rnorm(100))
    polygon <- values$clicked_polygon
    
    if (!is.null(polygon)) {
      temp = getFilteredPolygonData()
      
      fig <- plot_ly(
        type = 'table',
        columnwidth = c(90, 55, 55),
        header = list(
          values = c(
            paste0('<b>', temp$clue_area, '</b>'),
            '<b>Value</b>',
            '<b>Rank</b>'
          ),
          line = list(color = 'black'),
          fill = list(color = 'rgb(145, 145, 145)'),
          align = c('left', 'center'),
          font = list(color = 'white', size = 14)
        ),
        cells = list(
          values = rbind(
            c(
              'Avg num of vehicles in suburb per day',
              'Avg num of trucks in suburb per day',
              'Avg highest vehicle flow btw 12PM-12AM',
              'Avg highest vehicle flow btw 12AM-12PM',
              'Avg logarithmic annual growth rate of volume'
            ),
            c(
              temp$total_ALLVEHS_AADT,
              temp$total_TRUCKS_AADT,
              temp$mean_HHF_PMPEAK_AADT,
              temp$mean_HHF_AMPEAK_AADT,
              temp$avg_GROWTH_RATE
            ),
            c(
              temp$total_ALLVEHS_AADT_rank,
              temp$total_TRUCKS_AADT_rank,
              temp$mean_HHF_PMPEAK_AADT_rank,
              temp$mean_HHF_AMPEAK_AADT_rank,
              temp$avg_GROWTH_RATE_rank
            )
          ),
          line = list(color = 'black'),
          fill = list(color = c('rgb(145, 145, 145)', 'white')),
          align = c('left', 'center'),
          font = list(color = c('white', 'black'), size = 14)
        )
      )

      fig <- fig %>% layout(
        paper_bgcolor = 'transparent',
        plot_bgcolor = 'transparent',
        margin = list(
          l = 10,
          r = 10,
          t = 10
          # b = 10
        )
      )
      
      fig
    }
  })
  
  
  
  output$stat_line_plot <- renderPlotly({
    line <- values$clicked_line
    
    if (!is.null(line)) {
      temp = getFilteredStreetData()
      
      # if no data returned for the streets
      if (dim(temp)[1] == 0) {
        fig <- plot_ly()
        
   
        fig <- fig %>% layout(
          annotations = list(
            list(
              text = "No data collected <br>for this section of <br>this street yet....",
              x = -0.05,
              xref = "paper",
 
              xanchor = 'left',
              y = 0.55,
              yref = "paper",
              yanchor = 'middle',
              showarrow = FALSE,
              
              font = list(size = 40, color = "rgb(141, 140, 139)")  
            )
          ),
          paper_bgcolor = 'rgba(0,0,0,0)',
          plot_bgcolor = 'rgba(0,0,0,0)',
          xaxis = list(
            showgrid = FALSE,
            showline = FALSE,
            showticklabels = FALSE,
            zeroline = FALSE
          ),
          yaxis = list(
            showgrid = FALSE,
            showline = FALSE,
            showticklabels = FALSE,
            zeroline = FALSE
          )
        )
        
        fig
        
      } else {
        fig <- plot_ly(
          temp,
          x = ~ ACCIDENT_YEAR,
          y = ~ INJ_OR_FATAL,
          name = 'Fatal or Injured',
          type = 'scatter',
          mode = 'lines',
          line = list(
            color = 'rgb(141, 140, 139)',
            width = 4,
            dash = 'dot'
          ),
          marker = list(color = 'rgb(141, 140, 139)', size = 8)
        )
        fig <- fig %>% add_trace(
          y = ~ WEEKDAYS,
          name = 'Weekdays',
          line = list(
            color = 'rgb(44, 29, 9)',
            width = 4,
            dash = 'dash'
          ),
          marker = list(color = 'rgb(44, 29, 9)', size = 8)
        )
        fig <- fig %>% add_trace(
          y = ~ ACCIDENTS,
          name = 'Daily Avg',
          line = list(
            color = 'rgb(205, 12, 24)',
            width = 4,
            dash = 'solid'
          ),
          marker = list(color = 'rgb(205, 12, 24)', size = 8)
        )
        fig <- fig %>% layout(
          title = list(
            text = "Street Accidents Against Year",
            x = 0.5,
            xanchor = 'center'
          ),
          xaxis = list(
            title = "Year",
            showgrid = FALSE,
            showline = TRUE,
            showticklabels = TRUE,
            zeroline = FALSE,
            ticks = 'outside',
            tickcolor = 'rgb(204, 204, 204)',
            tickwidth = 2,
            ticklen = 5,
            tickfont = list(
              family = 'Arial',
              size = 12,
              color = 'rgb(82, 82, 82)'
            )
          ),
          yaxis = list (
            title = "Counts",
            showgrid = FALSE,
            zeroline = FALSE,
            showline = TRUE,
            showticklabels = TRUE,
            ticks = 'outside',
            tickcolor = 'rgb(204, 204, 204)',
            tickwidth = 2,
            ticklen = 5,
            tickfont = list(
              family = 'Arial',
              size = 12,
              color = 'rgb(82, 82, 82)'
            )
          ),
          paper_bgcolor = 'rgba(0,0,0,0)',
          plot_bgcolor = 'rgba(0,0,0,0)',
          legend = list(orientation = 'h', y = -0.2)  # 设置图例位置在下方
        )
        fig
      }
    }
    
  })
  
  
  output$myBubbleChart <- renderPlotly({
    fig <-
      plot_ly(
        bubble_df,
        x = ~ ALLVEHS_AADT,
        y = ~ GROWTH_RATE,
        color = ~ clue_area,
        size = ~ size,
        colors = bubble_colors,
        type = 'scatter',
        mode = 'markers',
        sizes = c(min(bubble_df$size), max(bubble_df$size)),
        marker = list(
          symbol = 'circle',
          sizemode = 'diameter',
          line = list(width = 2, color = '#FFFFFF')
        ),
        text = ~ paste(
          'Street:',
          LOCAL_ROAD_NM,
          '<br>Traffic Volume Growth Rate:',
          GROWTH_RATE,
          '<br>Street Type:',
          RMA_DESC,
          '<br>EHV Ratio:',
          EHV,
          '<br>Avg Daily Traffic Volume:',
          paste0(round(ALLVEHS_AADT / 1000, 1), 'K')
        )
      )
    
    fig <-
      fig %>% layout(
        title = 'Growth Rate Vs Avg Daily Vol, 2020',
        xaxis = list(
          title = 'Average Daily Traffic Volume',
          gridcolor = 'rgb(255, 255, 255)',
          range = c(2, 6.5),
          showgrid = FALSE,
          type = 'log',
          gridwidth = 1
        ),
        yaxis = list(
          title = 'Growth Rate',
          gridcolor = 'rgb(255, 255, 255)',
          showgrid = FALSE,
          range = c(-0.05, 0.1),
          zeroline = FALSE,
          gridwith = 1
        ),
        paper_bgcolor = 'transparent',
        plot_bgcolor = 'transparent'
        
      )
    
    fig
  })
  
  ###############################Traffic Part ##############
  #########################Relation Part #########################
  sort_state <- reactiveVal("desc")
  select_factor <- reactiveVal("Traffic: Population")
  name_map <- list(
    population = "Number of population",
    liquor = "Number of liquor stores",
    Employment = "Number of employments",
    bus = "Number of bus stops",
    transport = "Number of transportation land",
    parking = "Number of parking spaces (Off Street)"
  )
  
  getSelectData <- reactive({
    suburb_realtion <- suburb_realtion %>% arrange(crime_percentage)
    y <- suburb_realtion$Suburb
    x_crime <- suburb_realtion$traffic_percentage
    x_population <- suburb_realtion$total_population
    feature <- "Traffic"
    factor <- 'population'
    selected_name <- name_map[[factor]]
    
    if (input$factor == 'Crime: Population') {
      suburb_realtion <- suburb_realtion %>% arrange(crime_percentage)
      y <- suburb_realtion$Suburb
      x_crime <- suburb_realtion$crime_percentage
      x_population <- suburb_realtion$total_population
      feature <- "Crime"
      selected_name <- name_map[[factor]]
      
    } else if (input$factor == 'Crime: Liquor') {
      suburb_realtion <- suburb_realtion %>% arrange(crime_percentage)
      y <- suburb_realtion$Suburb
      x_crime <- suburb_realtion$crime_percentage
      x_population <- suburb_realtion$total_liquor_store
      feature <- "Crime"
      factor <- 'liquor'
      selected_name <- name_map[[factor]]
      
    } else if (input$factor == 'Crime: Employment') {
      suburb_realtion <- suburb_realtion %>% arrange(crime_percentage)
      y <- suburb_realtion$Suburb
      x_crime <- suburb_realtion$crime_percentage
      x_population <- suburb_realtion$total_jobs
      feature <- "Crime"
      factor <- 'Employment'
      selected_name <- name_map[[factor]]
      
    } else if (input$factor == 'Traffic: Population') {
      suburb_realtion <- suburb_realtion %>% arrange(traffic_percentage)
      y <- suburb_realtion$Suburb
      x_crime <- suburb_realtion$traffic_percentage
      x_population <- suburb_realtion$total_population
      
      
    } else if (input$factor == 'Traffic: Bus Stop') {
      suburb_realtion <- suburb_realtion %>% arrange(traffic_percentage)
      y <- suburb_realtion$Suburb
      x_crime <- suburb_realtion$traffic_percentage
      x_population <- suburb_realtion$bus_stop_count
      factor <- 'bus'
      selected_name <- name_map[[factor]]
      
    }
    else if (input$factor == 'Traffic: Transport') {
      suburb_realtion <- suburb_realtion %>% arrange(traffic_percentage)
      y <- suburb_realtion$Suburb
      x_crime <- suburb_realtion$traffic_percentage
      x_population <- suburb_realtion$transport_count
      factor <- 'trasport'
      selected_name <- name_map[[factor]]
      
      
    } else if (input$factor == 'Traffic: Parking') {
      suburb_realtion <- suburb_realtion %>% arrange(traffic_percentage)
      y <- suburb_realtion$Suburb
      x_crime <- suburb_realtion$traffic_percentage
      x_population <- suburb_realtion$total_parking_spaces
      factor <- 'parking'
      selected_name <- name_map[[factor]]
      
    } else{
      suburb_realtion <- suburb_realtion %>% arrange(traffic_percentage)
      y <- suburb_realtion$Suburb
      x_crime <- suburb_realtion$traffic_percentage
      x_population <- suburb_realtion$total_population
    }
    
    list(
      y = y,
      x_crime = x_crime,
      x_population = x_population,
      feature = feature,
      factor = factor,
      selected_name = selected_name
    )
    
  })
  
  
  # Observe for a click event on the sort button
  observeEvent(input$sort_button2, {
    # Check the current state of sorting
    if (sort_state() == "desc") {
      sort_state("inc")
    } else {
      sort_state("desc")
    }
  })
  # Observe for a click event on the sort button
  observeEvent(input$sort_button1, {
    # Check the current state of sorting
    if (sort_state() == "desc") {
      sort_state("inc")
    } else {
      sort_state("desc")
    }
  })
  sortedTrafficData <- reactive({
    # Sorting logic based on sort_state
    if (sort_state() == "inc") {
      suburb_realtion <-
        suburb_realtion %>% arrange(desc(suburb_realtion$Suburb))
    } else if (sort_state() == "desc") {
      suburb_realtion <-
        suburb_realtion %>% arrange(suburb_realtion$total_traffic_count)
    }
    suburb_realtion
  })
  
  sortedCrimeData <- reactive({
    # Sorting logic based on sort_state
    if (sort_state() == "inc") {
      suburb_realtion <-
        suburb_realtion %>% arrange(desc(suburb_realtion$Suburb))
    } else if (sort_state() == "desc") {
      suburb_realtion <-
        suburb_realtion %>% arrange(suburb_realtion$total_offence_count)
    }
    suburb_realtion
  })
  
  # Observe for a click event on the sort button
  
  output$plot_crime <- renderPlotly({
    suburb_realtion_sort <- sortedCrimeData()
    suburb_realtion_sort$Suburb <-
      factor(suburb_realtion_sort$Suburb, levels = suburb_realtion_sort$Suburb)
    
    employment_fig2 <-
      ggplot(suburb_realtion_sort, aes(x = Suburb, y = total_jobs)) +
      geom_segment(aes(xend = Suburb,
                       yend = 0,), color = "#CCFFCC", ) +
      geom_point(size = 4, color = "skyblue") +
      coord_flip() +
      theme_bw() +
      xlab("") +
      theme(
        plot.title = element_text(
          size = 11,
          face = "bold",
          hjust = 0.5,
          color = "#606060"
        ),
        plot.title.position = "panel",
        axis.title.x = element_text(
          color = "black",
          size = 11,
          face = "bold",
          margin = margin(
            t = 10,
            r = 0,
            b = 0,
            l = 0
          )
        ),
        axis.title.y = element_text(
          color = "black",
          size = 11,
          face = "bold",
          margin = margin(
            t = 0,
            r = 10,
            b = 0,
            l = 0
          )
        ),
        axis.text.x = element_text(
          color = "#606060",
          size = 10,
          face = "bold",
          margin = margin(
            t = 10,
            r = 0,
            b = 0,
            l = 0
          )
        ),
        axis.text.y = element_text(
          color = "#606060",
          size = 10,
          angle = 0,
          hjust = 0,
          face = "bold",
          margin = margin(
            t = 0,
            r = -20,
            b = 0,
            l = 10
          )
        )
      )
    
    employment_fig2 <-
      ggplotly(employment_fig2) %>% layout(
        title = "employment",
        # Add this line for the title
        xaxis = list(title = 'Number of Jobs', showgrid = FALSE),
        yaxis = list(title = 'Suburb Name', showgrid = FALSE),
        plot_bgcolor = 'black',
        paper_bgcolor = 'black',
        font = list(color = "white")
      ) %>%
      style(hoverinfo = "none", traces = 1)
    
    liquor_fig5 <-
      ggplot(suburb_realtion_sort,
             aes(x = Suburb, y = total_liquor_store)) +
      geom_segment(aes(xend = Suburb,
                       yend = 0,), color = "#CCFFCC") +
      geom_point(size = 4, color = "skyblue") +
      coord_flip() +
      theme_bw() +
      xlab("") +
      theme(
        plot.title = element_text(
          size = 11,
          face = "bold",
          hjust = 0.5,
          color = "#606060"
        ),
        plot.title.position = "panel",
        axis.title.x = element_text(
          color = "black",
          size = 11,
          face = "bold",
          margin = margin(
            t = 10,
            r = 0,
            b = 0,
            l = 0
          )
        ),
        axis.title.y = element_text(
          color = "black",
          size = 11,
          face = "bold",
          margin = margin(
            t = 0,
            r = 10,
            b = 0,
            l = 0
          )
        ),
        axis.text.x = element_text(
          color = "#606060",
          size = 10,
          face = "bold",
          margin = margin(
            t = 10,
            r = 0,
            b = 0,
            l = 0
          )
        ),
        axis.text.y = element_text(
          color = "#606060",
          size = 10,
          angle = 0,
          hjust = 0,
          face = "bold",
          margin = margin(
            t = 0,
            r = -20,
            b = 0,
            l = 10
          )
        )
      )
    
    liquor_fig5 <-
      ggplotly(liquor_fig5) %>% layout(
        title = "liquor",
        # Add this line for the title
        xaxis = list(title = 'Numbr of Liquor Store', showgrid = FALSE),
        yaxis = list(title = 'Suburb Name', showgrid = FALSE),
        plot_bgcolor = 'black',
        paper_bgcolor = 'black',
        font = list(color = "white")
      )  %>%
      style(hoverinfo = "none", traces = 1)
    
    fig <-
      subplot(
        employment_fig2,
        liquor_fig5,
        nrows = 1,
        titleY = TRUE,
        titleX = TRUE,
        margin = 0.1
      )
    
    fig <- fig %>% layout(
      title = list(text = 'Potential Influencing Factors on Crime', y = 0.998),
      plot_bgcolor = '#e5ecf6',
      xaxis = list(
        zerolinecolor = '#ffff',
        zerolinewidth = 2,
        gridcolor = 'ffff'
      ),
      yaxis = list(
        zerolinecolor = '#ffff',
        zerolinewidth = 2,
        gridcolor = 'ffff'
      ),
      margin = list(
        l = 50,
        r = 50,
        b = 10,
        t = 50
      ) 
    )
    # Update title
    annotations = list(
      list(
        x = 0.2,
        y = 1.0,
        text = "Number of Jobs",
        xref = "paper",
        yref = "paper",
        xanchor = "center",
        yanchor = "bottom",
        showarrow = FALSE,
        font = list(size = 20)
      ),
      list(
        x = 0.8,
        y = 1,
        text = "Number of Liqour Store",
        xref = "paper",
        yref = "paper",
        xanchor = "center",
        yanchor = "bottom",
        showarrow = FALSE,
        font = list(size = 20)
      )
    )
    
    fig <-
      fig %>% layout(
        showlegend = FALSE,
        annotations = annotations,
        plot_bgcolor = 'black',
        paper_bgcolor = 'black',
        font = list(color = "white")
      )
    fig
  })
  output$plot_traffic <- renderPlotly({
    suburb_realtion_sort <- sortedTrafficData()
    
    suburb_realtion_sort$Suburb <-
      factor(suburb_realtion_sort$Suburb, levels = suburb_realtion_sort$Suburb)
    
    carpark_fig1 <-
      ggplot(suburb_realtion_sort,
             aes(x = Suburb, y = total_parking_spaces)) +
      geom_segment(aes(xend = Suburb,
                       yend = 0,), color = "#CCFFCC", ) +
      geom_point(size = 4, color = "skyblue") +
      coord_flip() +
      theme_bw() +
      xlab("") +
      theme(
        plot.title = element_text(
          size = 11,
          face = "bold",
          hjust = 0.5,
          color = "#606060"
        ),
        plot.title.position = "panel",
        axis.title.x = element_text(
          color = "black",
          size = 11,
          face = "bold",
          margin = margin(
            t = 10,
            r = 0,
            b = 0,
            l = 0
          )
        ),
        axis.title.y = element_text(
          color = "black",
          size = 11,
          face = "bold",
          margin = margin(
            t = 0,
            r = 10,
            b = 0,
            l = 0
          )
        ),
        axis.text.x = element_text(
          color = "#606060",
          size = 10,
          face = "bold",
          margin = margin(
            t = 10,
            r = 0,
            b = 0,
            l = 0
          )
        ),
        axis.text.y = element_text(
          color = "#606060",
          size = 10,
          angle = 0,
          hjust = 0,
          face = "bold",
          margin = margin(
            t = 0,
            r = -20,
            b = 0,
            l = 10
          )
        )
      )
    
    carpark_fig1 <-
      ggplotly(carpark_fig1) %>% layout(
        xaxis = list(title = 'Number of Parking', showgrid = FALSE),
        yaxis = list(title = 'Suburb Name', showgrid = FALSE),
        plot_bgcolor = 'black',
        paper_bgcolor = 'black',
        font = list(color = "white")
      ) %>%
      style(hoverinfo = "none", traces = 1)
    
    transport_fig3 <-
      ggplot(suburb_realtion_sort, aes(x = Suburb, y = transport_count)) +
      geom_segment(aes(xend = Suburb,
                       yend = 0,), color = "#CCFFCC", ) +
      geom_point(size = 4, color = "skyblue") +
      coord_flip() +
      theme_bw() +
      xlab("") +
      theme(
        plot.title = element_text(
          size = 11,
          face = "bold",
          hjust = 0.5,
          color = "#606060"
        ),
        plot.title.position = "panel",
        axis.title.x = element_text(
          color = "black",
          size = 11,
          face = "bold",
          margin = margin(
            t = 10,
            r = 0,
            b = 0,
            l = 0
          )
        ),
        axis.title.y = element_text(
          color = "black",
          size = 11,
          face = "bold",
          margin = margin(
            t = 0,
            r = 10,
            b = 0,
            l = 0
          )
        ),
        axis.text.x = element_text(
          color = "#606060",
          size = 10,
          face = "bold",
          margin = margin(
            t = 10,
            r = 0,
            b = 0,
            l = 0
          )
        ),
        axis.text.y = element_text(
          color = "#606060",
          size = 10,
          angle = 0,
          hjust = 0,
          face = "bold",
          margin = margin(
            t = 0,
            r = -20,
            b = 0,
            l = 10
          )
        )
      )
    
    transport_fig3 <-
      ggplotly(transport_fig3) %>% layout(
        xaxis = list(title = 'Number of Transport Use ', showgrid = FALSE),
        yaxis = list(title = 'Suburb Name', showgrid = FALSE),
        plot_bgcolor = 'black',
        paper_bgcolor = 'black',
        font = list(color = "white")
      ) %>%
      style(hoverinfo = "none", traces = 1)
    
    
    bus_stop_fig4 <-
      ggplot(suburb_realtion_sort, aes(x = Suburb, y = bus_stop_count)) +
      geom_segment(aes(xend = Suburb,
                       yend = 0,), color = "#CCFFCC", ) +
      geom_point(size = 4, color = "skyblue") +
      coord_flip() +
      theme_bw() +
      xlab("") +
      theme(
        plot.title = element_text(
          size = 11,
          face = "bold",
          hjust = 0.5,
          color = "#606060"
        ),
        plot.title.position = "panel",
        axis.title.x = element_text(
          color = "black",
          size = 11,
          face = "bold",
          margin = margin(
            t = 10,
            r = 0,
            b = 0,
            l = 0
          )
        ),
        axis.title.y = element_text(
          color = "black",
          size = 11,
          face = "bold",
          margin = margin(
            t = 0,
            r = 10,
            b = 0,
            l = 0
          )
        ),
        axis.text.x = element_text(
          color = "#606060",
          size = 10,
          face = "bold",
          margin = margin(
            t = 10,
            r = 0,
            b = 0,
            l = 0
          )
        ),
        axis.text.y = element_text(
          color = "#606060",
          size = 10,
          angle = 0,
          hjust = 0,
          face = "bold",
          margin = margin(
            t = 0,
            r = -20,
            b = 0,
            l = 10
          )
        )
      )
    
    bus_stop_fig4 <-
      ggplotly(bus_stop_fig4) %>% layout(
        xaxis = list(title = 'Number of Bus Stop', showgrid = FALSE),
        yaxis = list(title = 'Suburb Name', showgrid = FALSE),
        plot_bgcolor = 'black',
        paper_bgcolor = 'black',
        font = list(color = "white")
      ) %>%
      style(hoverinfo = "none", traces = 1)
    
    
    population_fig6 <-
      ggplot(suburb_realtion_sort, aes(x = Suburb, y = total_population)) +
      geom_segment(aes(xend = Suburb,
                       yend = 0,), color = "#CCFFCC") +
      geom_point(size = 4, color = "skyblue") +
      coord_flip() +
      theme_bw() +
      xlab("") +
      theme(
        plot.title = element_text(
          size = 11,
          face = "bold",
          hjust = 0.5,
          color = "#606060"
        ),
        plot.title.position = "panel",
        axis.title.x = element_text(
          color = "black",
          size = 11,
          face = "bold",
          margin = margin(
            t = 10,
            r = 0,
            b = 0,
            l = 0
          )
        ),
        axis.title.y = element_text(
          color = "black",
          size = 11,
          face = "bold",
          margin = margin(
            t = 0,
            r = 10,
            b = 0,
            l = 0
          )
        ),
        axis.text.x = element_text(
          color = "#606060",
          size = 10,
          face = "bold",
          margin = margin(
            t = 10,
            r = 0,
            b = 0,
            l = 0
          )
        ),
        axis.text.y = element_text(
          color = "#606060",
          size = 10,
          angle = 0,
          hjust = 0,
          face = "bold",
          margin = margin(
            t = 0,
            r = -20,
            b = 0,
            l = 10
          )
        )
      ) +
      scale_y_continuous(labels = comma) 
    
    population_fig6 <- ggplotly(population_fig6) %>%
      layout(
        xaxis = list(title = 'Number of Population', showgrid = FALSE),
        yaxis = list(
          title = 'Suburb Name',
          showgrid = FALSE,
          tickformat = ','
        ),
        plot_bgcolor = 'black',
        paper_bgcolor = 'black',
        font = list(color = "white")
      ) %>%
      style(hoverinfo = "none", traces = 1)
    
    
    fig <-
      subplot(
        population_fig6,
        bus_stop_fig4,
        transport_fig3,
        carpark_fig1,
        nrows = 2,
        titleY = TRUE,
        titleX = TRUE,
        margin = 0.1
      )
    
    fig <- fig %>% layout(
      title = list(text = 'Potential Influencing Factors on Traffic', y = 0.998),
      plot_bgcolor = '#e5ecf6',
      xaxis = list(
        zerolinecolor = '#ffff',
        zerolinewidth = 2,
        gridcolor = 'ffff'
      ),
      yaxis = list(
        zerolinecolor = '#ffff',
        zerolinewidth = 2,
        gridcolor = 'ffff'
      ),
      margin = list(
        l = 50,
        r = 50,
        b = 10,
        t = 50
      )  
    )
    # Update title
    annotations = list(
      list(
        x = 0.2,
        y = 1.0,
        text = "Number of Population",
        xref = "paper",
        yref = "paper",
        xanchor = "center",
        yanchor = "bottom",
        showarrow = FALSE,
        font = list(size = 20)
      ),
      list(
        x = 0.8,
        y = 1,
        text = "Number of Bus Stop ",
        xref = "paper",
        yref = "paper",
        xanchor = "center",
        yanchor = "bottom",
        showarrow = FALSE,
        font = list(size = 20)
      ),
      list(
        x = 0.2,
        y = 0.4,
        text = "Transport Use Land",
        xref = "paper",
        yref = "paper",
        xanchor = "center",
        yanchor = "bottom",
        showarrow = FALSE,
        font = list(size = 20)
      ),
      list(
        x = 0.8,
        y = 0.4,
        text = "Number of Car Park(off street)",
        xref = "paper",
        yref = "paper",
        xanchor = "center",
        yanchor = "bottom",
        showarrow = FALSE,
        font = list(size = 20)
      )
    )
    
    fig <-
      fig %>% layout(
        showlegend = FALSE,
        annotations = annotations,
        plot_bgcolor = 'black',
        paper_bgcolor = 'black',
        font = list(color = "white")
      )
    fig
  })
  output$plot_relation <- renderPlotly({
    selectData <- getSelectData()
    y <- selectData$y
    x_crime <- selectData$x_crime
    x_population <- selectData$x_population
    feature <- selectData$feature
    selected_name <- selectData$selected_name
    name_value <-
      ifelse(
        feature == "Traffic",
        "Percentage of Traffic based on City of Melbourne",
        ifelse(
          feature == "Crime",
          "Percentage of Crime based on City of Melbourne",
          "Other Name"
        )
      ) 
    title_value <-
      ifelse(
        feature == "Traffic",
        "Percentage Of Traffic & Potentional Factor Based On City Of Melbourne",
        ifelse(
          feature == "Crime",
          "Percentage Of Crime & Potentional Factor Based On City Of Melbourne",
          "Other Name"
        )
      )  
    fig1 <-
      plot_ly(
        x = ~ x_crime,
        y = ~ reorder(y, x_crime),
        name = name_value,
        type = 'bar',
        orientation = 'h',
        hoverinfo = 'y+x',
        marker = list(
          color = 'rgba(204,255,204, 0.5)',
          # Deep red
          line = list(color = 'rgba(204,255,204, 1.0)', width = 1)
        )
      )
    fig1 <-
      fig1 %>% layout(
        yaxis = list(
          showgrid = FALSE,
          showline = FALSE,
          showticklabels = TRUE,
          domain = c(0, 0.85),
          tickcolor = "white",
          tickfont = list(color = "white")
        ),
        xaxis = list(
          zeroline = FALSE,
          showline = FALSE,
          showticklabels = TRUE,
          showgrid = TRUE,
          tickcolor = "white",
          tickfont = list(color = "white")
        )
      )
    
    fig1 <- fig1 %>% add_annotations(
      xref = 'x1',
      yref = 'y',
      x = x_crime * 2.1 + 3,
      y = y,
      text = paste(round(x_crime, 2), '%'),
      font = list(
        family = 'Arial',
        size = 13,
        color = 'rgb(204,255,204)'
      ),
      showarrow = FALSE
    )
    
    fig2 <-
      plot_ly(
        x = ~ x_population,
        y = ~ reorder(y, x_crime),
        name = selected_name,
        type = 'scatter',
        mode = 'lines+markers',
        line = list(color = 'skyblue'),
        marker = list(color = 'deepskyblue'),
        hoverinfo = 'y+x'
      )  # Skyblue
    fig2 <-
      fig2 %>% layout(
        yaxis = list(
          showgrid = FALSE,
          showline = TRUE,
          showticklabels = FALSE,
          linecolor = 'rgba(102, 102, 102, 0.8)',
          linewidth = 2,
          tickcolor = "white",
          tickfont = list(color = "white"),
          domain = c(0, 0.85)
        ),
        xaxis = list(
          zeroline = FALSE,
          showline = FALSE,
          showticklabels = TRUE,
          showgrid = TRUE,
          tickcolor = "white",
          tickfont = list(color = "white"),
          side = 'top',
          dtick = 25000
        )
      )
    
    fig2 <- fig2 %>% add_annotations(
      xref = 'x2',
      yref = 'y',
      x = x_population,
      y = y,
      text = paste(x_population, 'M'),
      font = list(
        family = 'Arial',
        size = 13,
        color = 'white'
      ),
      showarrow = FALSE
    )
    
    
    fig <- subplot(fig1, fig2)
    fig <-
      fig %>% layout(
        title = list(text = title_value, font = list(
          color = "white", size = 16
        )),
        legend = list(
          x = 0.029,
          y = 1.038,
          font = list(size = 13, color = "white")
        ),
        margin = list(
          l = 100,
          r = 20,
          t = 70,
          b = 70
        ),
        paper_bgcolor = 'black',
        plot_bgcolor = 'black'
      )
    fig <- fig %>% add_annotations(
      xref = 'paper',
      yref = 'paper',
      x = -0.14,
      y = -0.15,
      text = paste(''),
      font = list(
        family = 'Arial',
        size = 13,
        color = 'rgb(150,150,150)'
      ),
      showarrow = FALSE
    )
    
    fig
  })
  
  #########################Relation Part #########################
  
}
######################Help function for relation ##################



#############
# Run Shiny #
#############
shinyApp(ui, server, options = list(launch.browser = TRUE))
