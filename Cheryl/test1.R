library(sf)
library(dplyr)
library(shiny)
library(leaflet)
library(mapboxapi)
library(shinythemes)
library(plotly)
library(lubridate)


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

# st_write(stats_crash_at_streets, '../Data/transportation/stats')


##################
# USER INTERFACE #
##################
# The layout of the UI
ui <- navbarPage(
  id = 'mypage',
  title = 'Traffic in City of Melbourne',
  theme = shinythemes::shinytheme("cyborg"),
  tabPanel(
    id = 'tab_panel',
    class ='my-tab',
    title = '',
    div(
      class = 'my-map-container',
      style = "position: relative;",
      leafletOutput('map', width = "100%", height = "950px"),
      uiOutput("float_window"),  # Render the UI element here
      uiOutput('float_filter'),
      uiOutput('overview')
    )
  ),
  
  # 在这里嵌入自定义CSS样式
  tags$style(HTML('
    .my-tab {
      background-color: black;
      color: white;
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
      background-color:rgb(224, 220, 220, 0.95);
      border: 0px #ccc;
      padding: 20px;
    }
    
    /* 自定义按钮样式 */
    .my-reset-button {
      position: absolute;
      bottom: 5%;
      left: 82%;
      background-color: blue; /* 设置按钮的背景颜色 */
      color: white; /* 设置按钮的文本颜色 */
      border: 2px solid white; /* 设置按钮的边框样式 */
      border-radius: 5px; /* 设置按钮的边框圆角 */
      padding: 5px 5px; /* 设置按钮内边距 */
      font-size: 12px; /* 设置文本字体大小 */
    }

    /* 鼠标悬停时的按钮样式 */
    .my-reset-button:hover {
      background-color: #d9534f; /* 设置鼠标悬停时的背景颜色 */
    }
    
   
    
    /* 设置导航栏背景颜色为黑色 */
    .navbar-default {
      background-color: black;
    }
    
    /* 设置标题文本颜色为白色 */
    .navbar-default .navbar-brand {
      color: white;
    }

    /* 设置标签文本颜色为白色 */
    .navbar-default .nav > li > a {
      color: white;
    }
    
    
    .nav-tabs > .active > a {
      background-color: black;
    }
        
    /* 设置标签文本颜色为白色 */
    .nav-tabs > li > a {
      color: white;
    }
  '))
)

################
# SHINY SERVER #
################
server <- function(input, output, session) { 
  
  # 在server函数中创建reactiveValues对象
  values <- reactiveValues(clicked_shape = NULL, 
                           clicked_reset = NULL,
                           clicked_overview = NULL)
  
  # A reactive data filter to filter out data belongs to the selected state and date range
  getFilteredStreetData <- reactive({
    
    click <- values$clicked_shape
    # print('$$$$$$$')
    # print(class(click$OBJECTID_1))
    if (class(click$OBJECTID_1) == 'integer') {
      # filter to clicked street
      filtered_df <- stats_crash_at_streets %>% 
        filter(OBJECTID_1 == click$OBJECTID_1)
      
      print('-------')
      print(filtered_df)
      
    } else {
      filtered_df = stats_crash_at_streets
    }
    
    
    return(filtered_df)
  })
  
  
  # 监听地图的点击事件
  observeEvent(input$map_shape_click, {
    
    click <- input$map_shape_click
    
    if (class(click$id) == 'integer') {
      # the line has been clicked
      clicked_shape <- mel_st_lines_sf[mel_st_lines_sf$OBJECTID_1 == click$id, ]
    } else {
      # the polygon has been clicked
      updateSelectInput(session, 'suburb', selected=input$map_shape_click)
      
      clicked_shape <- mel_suburbs_wgs84[mel_suburbs_wgs84$clue_area == click$id, ]
    }
    
    values$clicked_shape <- clicked_shape
    # print('----------')
    # print(class(clicked_shape))
    # print(clicked_shape$OBJECTID_1)
  })
  
  observeEvent(input$subtraff_overview, {
    # print(input$subtraff_overview)
    values$clicked_overview <-input$subtraff_overview
    
    # updateCheckboxInput(session, )
  })
  
  
  
  
  # 添加点击按钮关闭窗口的事件
  observeEvent(input$reset_button, {
    values$clicked_shape <- NULL
    
    updateSelectInput(session, 'suburb', selected= "")
    
    updateCheckboxInput(session, 'subtraff_overview', value = FALSE)
  })
  
  # 监听filter select事件
  observeEvent(input$suburb, {
    
    click <- input$map_shape_click
    
    
    if (!identical(click$id, input$suburb)) { 
      values$clicked_shape = click$id
    }
    
    
    # Clear the selection sidebar, stop the selection
    # session$sendCustomMessage(type='nation_map_set', message=character(0))
    values$clicked_shape <- mel_suburbs_wgs84[mel_suburbs_wgs84$clue_area == input$suburb, ]
    
    # Change the "state" input on the map tab to the state selected on the state map; session is the parameter of the server() function
    updateSelectInput(session, 'suburb', selected=input$suburb)
  })
  
  
  ##### map tab ####
  output$map <- renderLeaflet({
    
    map <- leaflet() %>%
      addMapboxTiles(style_url = "mapbox://styles/cheryl-chenccc/clnxv1txn003101r8cimzhqm7",
                     access_token = 'pk.eyJ1IjoiY2hlcnlsLWNoZW5jY2MiLCJhIjoiY2wyZGJtaHk2MHhweDNjbzIyaWk2ODlqdCJ9.nSmaPBChoCWG7b-VQmpKsA',
                     username = "cheryl-chenccc"
      )%>%
      setView(lng = as.numeric(st_coordinates(center_point)[, 'X']),
              lat = as.numeric(st_coordinates(center_point)[, 'Y']),
              zoom = 14)%>%
      addPolygons(data = mel_suburbs_wgs84,
                  color = 'white',
                  opacity = 0.7,
                  fillOpacity = 0.3,
                  fillColor = 'lightgrey',
                  weight = 4,
                  dashArray = "10",
                  highlightOptions = highlightOptions(
                    fillColor = 'rgb(205, 203, 203)',
                    fillOpacity = 0.9
                  ),
                  layerId = ~clue_area
      )%>%
      addPolylines(data = mel_st_lines_sf,
                   color = ~pal(log(mel_st_lines_sf$ALLVEHS_AADT)),
                   opacity = 0.8,
                   weight = 6,
                   # add a highlight effect when hovering on lines
                   highlightOptions = highlightOptions(
                     weight = 8,
                     color = "white",
                     bringToFront = TRUE
                   ),
                   label = myStLabel,
                   layerId = ~OBJECTID_1
      )
    
    
    
  })
  
  
  # Add a reactive to track whether a shape is clicked
  output$float_window <- renderUI({
    # 这里会报错因为一开始的时候未点击任何地方clicked_shape为空，但是由于selectinput
    # 什么都不点击的default是'Please select'
    # print('$$$$$')
    # print(values$clicked_shape)
    # print(nrow(values$clicked_shape))
    
    # print('%%%%%')
    # print(values$clicked_shape$id)
    # print(input$suburb)
    # print(nrow(values$clicked_shape))
    # print(nrow(values$clicked_shape) >0)
    # print('%%%%%')
    
    if ( (!is.null(values$clicked_shape)) & (nrow(values$clicked_shape) > 0) ){
      div(
        # id = "shape_info",
        style = "position: absolute; top: 10px; right: 10px; width: 420px; height: 650px; background-color: rgba(145, 145, 145, 0.7); border: 1px solid #ccc; padding: 10px",
        h3("统计信息"),
        plotlyOutput(outputId = 'stat_plot')
      )
    } else {
      # div()
    }
  })
  
  
  output$overview <- renderUI({
    if (values$clicked_overview) {
      div(class = 'my-overview',
          # h3('Overview'),
          plotlyOutput('myBubbleChart'),
          helpText('A bubble chart illustrates the possible relationship 
                   between predicated Traffic Volume Growth Rate and Daily
                   Average Traffic Volume (2020 Traffic Volume Data).
                   The size of the bubbles represent the Equivalent Heavy Vehicle
                   Ratio (i.e. %Heavy Vehicle + 3 * %Light Vehicle). Thus, the 
                   larger the bubble, the heavier traffic.', 
                   class = 'my-bubble-chart-text')
      )
    } else {
      # div()
    }
    
    
  })
  
  output$float_filter <- renderUI({
    div(
      style = "position: absolute; top: 10px; left: 60px; width: 300px; height: 120px; background-color: rgb(145, 145, 145); border: 0px #ccc; padding: 10px",
      selectInput(
        inputId= 'suburb',
        label = 'Suburbs in City of Melbourne',
        choices = c("(Please Select)" = "",'Carlton', 'Docklands', "East Melbourne", "Kensington",
                    "Melbourne", "North Melbourne", "Parkville", "Port Melbourne",
                    "South Yarra", "Southbank", "West Melbourne"),
        selected = ""
      ),
      checkboxInput(
        inputId = 'subtraff_overview',
        label = 'Suburbs Traffic Overview',
        value = FALSE
      ),
      # action button itself is a div()
      actionButton(inputId = "reset_button", 
                   label = "Reset", 
                   class = "my-reset-button")
    )
  })
  
  
  
  
  
  output$stat_plot <- renderPlotly({
    req(values$clicked_shape)
    
    clicked_shape <- values$clicked_shape
    
    if ('MULTILINESTRING' %in% st_geometry_type(clicked_shape$geometry)) {
      # hist(rnorm(100))
      temp = getFilteredStreetData()
      print('=========')
      print(dim(temp))
      print(class(getFilteredStreetData()))
      fig <- plot_ly(temp, 
                     x = ~ACCIDENT_YEAR, y = ~WEEKENDS, 
                     name = 'Weekends', 
                     type = 'scatter', mode = 'lines',
                     line = list(color = 'rgb(141, 140, 139)', width = 4, dash = 'dot'),
                     marker = list(color = 'rgb(141, 140, 139)', size = 8)) 
      fig <- fig %>% add_trace(y = ~WEEKDAYS, name = 'Weekdays', 
                               line = list(color = 'rgb(44, 29, 9)', width = 4, dash = 'dash'),
                               marker = list(color = 'rgb(44, 29, 9)', size = 8)) 
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
      
    } else {
      getFilteredStreetData()
      boxplot(rnorm(100))
    }
  })
  
  
  output$myBubbleChart <- renderPlotly({
    fig <- plot_ly(bubble_df, x = ~ALLVEHS_AADT, y = ~GROWTH_RATE, color = ~clue_area, size = ~size, colors = bubble_colors,
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
                                       showgrid = FALSE,
                                       type = 'log',
                                       gridwidth = 1),
                          yaxis = list(title = 'Growth Rate',
                                       gridcolor = 'rgb(255, 255, 255)',
                                       showgrid = FALSE,
                                       range = c(-0.05, 0.1),
                                       zeroline = FALSE,
                                       gridwith = 1),
                          paper_bgcolor = 'transparent',
                          plot_bgcolor = 'transparent'
    )
    
    fig
  })
  
}



#############
# RUN SHINY #
#############
shinyApp(ui, server)



