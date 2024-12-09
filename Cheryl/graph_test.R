library(sf)
library(dplyr)
library(shiny)
library(leaflet)
library(mapboxapi)
# library(shinyjs)
library(shinythemes)
library(plotly)
library(lubridate)
library(tidyr)


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
mel_suburbs_gda94 <- st_read("../Data/Mel_LGA_Suburbs_GDA94/mel_suburbs_edit.geojson")

# extract the geometry center of the city of melbourne for initialize view 
city_mel_whole_shape <- st_read("../Data/city_of_mel_boundary/mel_boundary.shp")

# get last five years vic crash data (with point location)
vic_crash_data <- st_read('../Data/transportation/Road_Crashes_for_five_Years_Victoria.geojson')

#####################
# Data Manipulation #
#####################
# transform to the same coordinate system
mel_traffic_lines_wgs84 <- st_transform(mel_traffic_lines, crs = 4326)

mel_suburbs_wgs84 <- st_transform(mel_suburbs_gda94, crs = 4326)

city_mel_whole_shape_wgs84 <- st_transform(city_mel_whole_shape, crs = 4326)

vic_crash_data_wgs84 <- st_transform(vic_crash_data, crs = 4326)


####### city_mel_whole_shape data #######

# get the centroid
center_point <- st_centroid(city_mel_whole_shape_wgs84$geometry)



####### city_mel streets line traffic data #######

mel_st_lines_sf <- st_intersection(mel_traffic_lines_wgs84, mel_suburbs_wgs84)

# Convert the geometry column back to multilinestring
mel_st_lines_sf <- st_cast(mel_st_lines_sf, "MULTILINESTRING")

temp <- st_drop_geometry(mel_st_lines_sf) %>% select(-clue_area) %>% distinct()

mel_st_lines_sf <- mel_st_lines_sf[rownames(temp), ]


# Define a color palette with 6 bins based on log(mel_st_lines_sf$ALLVEHS_AADT) since
# the data is high in variance
my_colors <- c("red4", "red2", "darkorange", "goldenrod", "yellowgreen", "forestgreen", 'darkgreen')
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



##### bubble chart data ######### 

# For plotly bubble chart
temp_df <- st_drop_geometry(mel_st_lines_sf) %>% 
  select(c(LOCAL_ROAD_NM, RMA_DESC, ALLVEHS_AADT, EHV, GROWTH_RATE, clue_area))

# prepare final plotly bubble chart use dataframe
bubble_df <- temp_df %>%
  group_by(clue_area, LOCAL_ROAD_NM) %>%
  summarise(
    ALLVEHS_AADT = sum(ALLVEHS_AADT),
    RMA_DESC = first(RMA_DESC),  # 保留第一个 RMA_DESC
    EHV = round(mean(EHV), 3),
    GROWTH_RATE = round(mean(GROWTH_RATE), 3)
  ) %>%
  ungroup()

# define proper bubble size for viz
bubble_df$size <- round(bubble_df$EHV * 3 * 10^2)

# define colors for bubble chart
bubble_colors <- c('#4AC6B7', '#1972A4', '#965F8A', '#FF7070', '#C61951')



##### crash data ########
# 选择指定的列并进行日期提取
selected_columns <- vic_crash_data_wgs84 %>%
  select(
    ACCIDENT_DATE = ACCIDENT_DATE,
    ALCOHOLTIME,
    DAY_OF_WEEK,
    LIGHT_CONDITION,
    ROAD_GEOMETRY,
    SPEED_ZONE,
    LONGITUDE,
    LATITUDE,
    LGA_NAME,
    INJ_OR_FATAL,
    FATALITY,
    SERIOUSINJURY,
    BICYCLIST,
    PEDESTRIAN,
    MOTORIST,
    ALCOHOL_RELATED,
    HEAVYVEHICLE
  )

# 提取日期部分
selected_columns$ACCIDENT_DATE <- as.Date(selected_columns$ACCIDENT_DATE)

# 筛选 LGA_NAME 为 "MELBOURNE" 的行
mel_crash_data_wgs84 <- selected_columns[selected_columns$LGA_NAME == "MELBOURNE", ]

# join street (line) and crash data (point)

# keep required columns in mel_st_lines_sf only
mel_st_for_join <- mel_st_lines_sf %>%
  select(OBJECTID_1, LOCAL_ROAD_NM, clue_area)

# perform spatial join
crash_at_streets <- st_join(mel_st_for_join, mel_crash_data_wgs84, join = st_intersects)

# 输出结果
crash_at_streets <- crash_at_streets %>%
  filter(!is.na(LONGITUDE) & !is.na(LATITUDE) & !is.na(ACCIDENT_DATE))

# Extract crash year into a column
crash_at_streets$ACCIDENT_YEAR <- year(crash_at_streets$ACCIDENT_DATE)

# drop unnecessary columns
crash_at_streets <- st_drop_geometry(crash_at_streets)

crash_at_streets <- crash_at_streets %>%
  select(-LGA_NAME, -LONGITUDE, -LATITUDE, -ACCIDENT_DATE)

# statistics about crash at streets df
stats_crash_at_streets <- crash_at_streets %>%
  group_by(OBJECTID_1, ACCIDENT_YEAR, LOCAL_ROAD_NM, clue_area) %>%
  summarize(
    ACCIDENTS = n(),
    INJ_OR_FATAL = sum(INJ_OR_FATAL),
    FATALITY = sum(FATALITY),
    SERIOUSINJURY = sum(SERIOUSINJURY),
    BICYCLIST = sum(BICYCLIST),
    PEDESTRIAN = sum(PEDESTRIAN),
    MOTORIST = sum(MOTORIST),
    ALCOHOL_RELATED = sum(ALCOHOL_RELATED == 'Yes'),
    LIGHT_CONDITION_Unk = sum(LIGHT_CONDITION == 'Unk.'),
    LIGHT_CONDITION_DarkLightsUnknown = sum(LIGHT_CONDITION == 'Dark Street lights unknown'),
    LIGHT_CONDITION_DuskDawn = sum(LIGHT_CONDITION == 'Dusk/Dawn'),
    ROAD_GEOMETRY_CIntersection = sum(ROAD_GEOMETRY == 'Cross intersection'),
    ROAD_GEOMETRY_MultiIntersection = sum(ROAD_GEOMETRY == 'Multiple intersection'),
    ROAD_GEOMETRY_TIntersection = sum(ROAD_GEOMETRY == 'T intersection'),
    ROAD_GEOMETRY_YIntersection = sum(ROAD_GEOMETRY == 'Y intersection'),
    SPEED_ZONE_NotKnown = sum(SPEED_ZONE == 'Not known'),
    WEEKDAYS = sum(DAY_OF_WEEK %in% c('Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday')),
    WEEKENDS = sum(DAY_OF_WEEK %in% c('Saturday', 'Sunday'))
  ) %>%
  ungroup()

# To add missing year data
# 你的OBJECTID_1的列表
objectid_1_list <- unique(stats_crash_at_streets$OBJECTID_1)
all_years <- c(2015, 2016, 2017, 2018, 2019)

all_years_df <- expand.grid(OBJECTID_1 = objectid_1_list,
                            ACCIDENT_YEAR = all_years)

# 当前temp中有很多na值
temp <- all_years_df %>%
  left_join(stats_crash_at_streets, by = c("OBJECTID_1", "ACCIDENT_YEAR"))%>%
  mutate(across(-c(OBJECTID_1, ACCIDENT_YEAR, LOCAL_ROAD_NM, clue_area), ~replace(., is.na(.), 0))) %>%
  mutate_all(~ replace(., is.na(.), 0))

# 找到每个 OBJECTID_1 对应的非零 LOCAL_ROAD_NM 和 clue_area 值
non_zero_local_road <- temp %>%
  group_by(OBJECTID_1) %>%
  filter(LOCAL_ROAD_NM != 0, clue_area != 0) %>%
  select(OBJECTID_1, LOCAL_ROAD_NM, clue_area)%>%
  unique()


# 使用非零值填充对应 OBJECTID_1 的 LOCAL_ROAD_NM 列
stats_crash_at_streets <- temp %>%
  left_join(non_zero_local_road, by = "OBJECTID_1") %>%
  select(-LOCAL_ROAD_NM.x, -clue_area.x) %>%
  rename(LOCAL_ROAD_NM = LOCAL_ROAD_NM.y, clue_area = clue_area.y) %>%
  select(OBJECTID_1, ACCIDENT_YEAR, LOCAL_ROAD_NM, clue_area, everything())

########################################################

# month <- c('January', 'February', 'March', 'April', 'May', 'June', 'July',
#            'August', 'September', 'October', 'November', 'December')
# high_2000 <- c(32.5, 37.6, 49.9, 53.0, 69.1, 75.4, 76.5, 76.6, 70.7, 60.6, 45.1, 29.3)
# low_2000 <- c(13.8, 22.3, 32.5, 37.2, 49.9, 56.1, 57.7, 58.3, 51.2, 42.8, 31.6, 15.9)
# high_2007 <- c(36.5, 26.6, 43.6, 52.3, 71.5, 81.4, 80.5, 82.2, 76.0, 67.3, 46.1, 35.0)
# low_2007 <- c(23.6, 14.0, 27.0, 36.8, 47.6, 57.7, 58.9, 61.2, 53.3, 48.5, 31.0, 23.6)
# high_2014 <- c(28.8, 28.5, 37.0, 56.8, 69.7, 79.7, 78.5, 77.8, 74.1, 62.6, 45.3, 39.9)
# low_2014 <- c(12.7, 14.3, 18.6, 35.5, 49.9, 58.0, 60.0, 58.6, 51.7, 45.2, 32.2, 29.1)

# data <- data.frame(month, high_2000, low_2000, high_2007, low_2007, high_2014, low_2014)

#The default order will be alphabetized unless specified as below:
# data$month <- factor(data$month, levels = data[["month"]])
temp = stats_crash_at_streets[stats_crash_at_streets$OBJECTID_1 == 8899, ]
fig <- plot_ly(temp, 
               x = ~ACCIDENT_YEAR, y = ~WEEKENDS, 
               name = 'Weekends', 
               type = 'scatter', mode = 'lines',
               line = list(color = 'rgb(44, 29, 9)', width = 4, dash = 'dot'),
               marker = list(color = 'rgb(44, 29, 9)', size = 8)) 
fig <- fig %>% add_trace(y = ~WEEKDAYS, name = 'Weekdays', 
                         line = list(color = 'rgb(141, 140, 139)', width = 4, dash = 'dash'),
                         marker = list(color = 'rgb(141, 140, 139)', size = 8)) 
fig <- fig %>% add_trace(y = ~ACCIDENTS, name = 'Accidents', 
                         line = list(color = 'rgb(205, 12, 24)', width = 4, dash = 'solid'),
                         marker = list(color = 'rgb(205, 12, 24)', size = 8)) 
fig <- fig %>% layout(title = list(text = "Street accident counts Vs year",
                                   x = 0.5,
                                   xanchor = 'center'),
                      xaxis = list(title = "Year", 
                                   showgrid = FALSE, 
                                   showline = TRUE,
                                   showticklabels = TRUE,
                                   zeroline = FALSE,
                                   ticks = 'outside',
                                   tickcolor = 'rgb(204, 204, 204)',
                                   tickwidth = 2,
                                   ticklen = 5,
                                   tickfont = list(family = 'Arial',
                                                   size = 12,
                                                   color = 'rgb(82, 82, 82)')
                                   ),
                      yaxis = list (title = "Accident counts", 
                                    showgrid = FALSE, 
                                    zeroline = FALSE,
                                    showline = TRUE, 
                                    showticklabels = TRUE,
                                    ticks = 'outside',
                                    tickcolor = 'rgb(204, 204, 204)',
                                    tickwidth = 2,
                                    ticklen = 5,
                                    tickfont = list(family = 'Arial',
                                                    size = 12,
                                                    color = 'rgb(82, 82, 82)')
                                    )
                      )

fig
         
stats_crash_at_streets[stats_crash_at_streets$OBJECTID_1 == 8899, ]
########################################################

fig <- plot_ly(bubble_df, x = ~ALLVEHS_AADT, y = ~GROWTH_RATE, color = ~clue_area, size = ~size, colors = colors,
               type = 'scatter', mode = 'markers', sizes = c(min(bubble_df$size), max(bubble_df$size)),
               marker = list(symbol = 'circle', sizemode = 'diameter',
                             line = list(width = 2, color = '#FFFFFF')),
               text = ~paste('Street:', LOCAL_ROAD_NM, 
                             '<br>Traffic Volume Growth Rate:', GROWTH_RATE, 
                             '<br>Street Type:', RMA_DESC,
                             '<br>EHV Ratio:', EHV,
                             '<br>Avg Daily Traffic Volume:', paste0(round(ALLVEHS_AADT/1000, 1), 'K')
                             )
               )

fig <- fig %>% layout(title = 'Growth Rate Vs Avg Daily Vol, 2020',
                      xaxis = list(title = 'Average Daily Traffic Volume',
                                   gridcolor = 'rgb(255, 255, 255)',
                                   range = c(2, 6.5),
                                   type = 'log',
                                   gridwidth = 1),
                      yaxis = list(title = 'Growth Rate',
                                   gridcolor = 'rgb(255, 255, 255)',
                                   range = c(-0.05, 0.1),
                                   zeroline = FALSE,
                                   gridwith = 1),
                      paper_bgcolor = 'transparent',
                      plot_bgcolor = 'transparent'
                      )

fig












