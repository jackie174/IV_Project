library(sf)
library(dplyr)
library(shiny)
library(leaflet)
library(mapboxapi)
library(shinyjs)


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
# transform to the same coordinate system
mel_traffic_lines_wgs84 <- st_transform(mel_traffic_lines, crs = 4326)

mel_suburbs_wgs84 <- st_transform(mel_suburbs_gda94, crs = 4326)

city_mel_whole_shape_wgs84 <- st_transform(city_mel_whole_shape, crs = 4326)


# get the centroid
center_point <- st_centroid(city_mel_whole_shape_wgs84$geometry)

mel_st_lines_sf <- st_intersection(mel_traffic_lines_wgs84, mel_suburbs_wgs84)


# Convert the geometry column back to multilinestring
mel_st_lines_sf <- st_cast(mel_st_lines_sf, "MULTILINESTRING")


mel_st_lines_sf <- mel_st_lines_sf %>% distinct()

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
  title = 'Transportation',
  tabPanel(
    id = 'tab_panel',
    # shinyjs::useShinyjs(),
    title = 'Traffic Volume (Street Level)',
    div(
      style = "position: relative;", 
      leafletOutput('traffic_vol', width = "100%", height = "950px"),
      uiOutput("float_window")  # Render the UI element here
      
    )
    # ,
    # uiOutput('reset')
  ),
  # 在这里嵌入自定义CSS样式
  tags$style(HTML('
    /* 自定义按钮样式 */
    .my-custom-button {
      background-color: red; /* 设置按钮的背景颜色 */
      color: white; /* 设置按钮的文本颜色 */
      border: 2px solid #d9534f; /* 设置按钮的边框样式 */
      border-radius: 5px; /* 设置按钮的边框圆角 */
      padding: 5px 5px; /* 设置按钮内边距 */
      font-size: 14px; /* 设置文本字体大小 */
    }

    /* 鼠标悬停时的按钮样式 */
    .my-custom-button:hover {
      background-color: #d9534f; /* 设置鼠标悬停时的背景颜色 */
    }
  '))
)

################
# SHINY SERVER #
################
server <- function(input, output, session) { 
  # 使用shinyjs来初始化
  
  
  # 在server函数中创建reactiveValues对象
  values <- reactiveValues(clicked_shape = NULL, clicked_close = NULL)
  
  # 监听地图的点击事件
  observeEvent(input$traffic_vol_shape_click, {
    
    click <- input$traffic_vol_shape_click
    # print('--------------')
    # print(click)
    # print('==============')
    if (class(click$id) == 'integer') {
      clicked_shape <- mel_st_lines_sf[mel_st_lines_sf$OBJECTID_1 == click$id, ]
    } else {
      clicked_shape <- mel_suburbs_wgs84[mel_suburbs_wgs84$clue_area == click$id, ]
    }
    
    values$clicked_shape <- clicked_shape
  })
  
  
  # observeEvent(input$close_button, {
  #   print('yes')
  #   values$clicked_close = input$close_button
  #   
  # })
  # 添加点击按钮关闭窗口的事件
  observeEvent(input$close_button, {
    # 隐藏float_window
    temp = input$close_button
    print('$$$$')
    print(temp == 1)
    values$clicked_close <- input$close_button
    # shinyjs::reset("tab_panel")
    # updateActionButton(inputId = 'close_button', label = NULL)
    # reset('shape_info')
    # print(input$close_button)
    # shinyApp(ui,server)
    # session$sendCustomMessage(type='close_button_set', message=character(0))
  })
  
  
  ##### traffic_vol tab ####
  output$traffic_vol <- renderLeaflet({
    
    map <- leaflet() %>%
      addMapboxTiles(style_url = "mapbox://styles/cheryl-chenccc/clnmbvidc005701r1bkvb5jew",
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
                    fillColor = 'rgb(43, 49, 59)',
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
    if (!is.null(values$clicked_shape) & (is.null(values$clicked_close))) {
      div(
        id = "shape_info",
        style = "position: absolute; top: 10px; right: 10px; width: 300px; background-color: rgba(255, 255, 255, 0.5); border: 1px solid #ccc; padding: 10px",
        h3("统计信息"),
        plotOutput(outputId = 'stat_plot'),
        useShinyjs(),
        div(id = "cross_sign_container",
            style = "position: absolute; top: 0; right: 0;",
            actionButton(inputId = "close_button", label = "Close", class = "my-custom-button")
        )
        
      )
    } else {
      div()
      # values$clicked_close <- 0
      # reactiveValues(clicked_shape = NULL, clicked_close = NULL)
    }
  })
  
  
  output$stat_plot <- renderPlot({
    req(values$clicked_shape)
    
    clicked_shape <- values$clicked_shape
    
    if ('MULTILINESTRING' %in% st_geometry_type(clicked_shape$geometry)) {
      hist(rnorm(100))
    } else {
      boxplot(rnorm(100))
    }
  })
  
  # output$empty_ui <- renderUI({
  #   req(values$clicked_close) 
  #   
  # })
  # output$reset <- renderUI({
  #   times <- input$close_button
  #   div(id=letters[(times %% length(letters)) + 1],
  #       
  #       )
  # })
  
}





#############
# RUN SHINY #
#############
shinyApp(ui, server)



