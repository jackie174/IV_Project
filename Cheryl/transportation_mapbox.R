library(sf)
library(dplyr)
library(shiny)
library(leaflet)
library(mapboxapi)
library(shinythemes)
library(plotly)
library(lubridate)

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

stats_crash_at_streets <- st_read('../Data/transportation/stats_crash_at_streets.geojson')
#####################
# Data Manipulation #
#####################
# transform to the same coordinate system
mel_traffic_lines_wgs84 <- st_transform(mel_traffic_lines, crs = 4326)

mel_suburbs_wgs84 <- st_transform(mel_suburbs_gda94, crs = 4326)

city_mel_whole_shape_wgs84 <- st_transform(city_mel_whole_shape, crs = 4326)

stats_crash_at_streets <- st_transform(stats_crash_at_streets, crs = 4326)


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


####### suburb selection plot data ########
# 创建一个以'clue_area'为分组的新DataFrame
suburb_summary <- mel_st_lines_sf %>%
  group_by(clue_area) %>%
  # 统计不同的RMA_DESC type各自有多少个
  summarize(
    # 整个suburb的sum(ALLVEHS_AADT)
    total_ALLVEHS_AADT = sum(ALLVEHS_AADT, na.rm = TRUE),
    # 整个suburb的sum(TRUCKS_AADT)
    total_TRUCKS_AADT = sum(ifelse(is.na(TRUCKS_AADT), 0, TRUCKS_AADT)),
    # 整个suburb的mean(ALLVEHS_PMPEAK_AADT)
    mean_HHF_PMPEAK_AADT = round(mean(ALLVEH_PMPEAK_AADT, na.rm = TRUE)),
    # 整个suburb的mean(ALLVEHS_AMPEAK_AADT)
    mean_HHF_AMPEAK_AADT = round(mean(ALLVEH_AMPEAK_AADT, na.rm = TRUE)),
    # 整个suburb的avg(GROWTH_RATE)
    avg_GROWTH_RATE = round(mean(GROWTH_RATE, na.rm = TRUE), 4)
  )

# drop geometry
suburb_summary <- st_drop_geometry(suburb_summary)

# 计算每一列的排名并存储在新列中
suburb_summary_rank <- suburb_summary %>%
  mutate_at(vars(-clue_area), list(rank = ~rank(-., ties.method = "min")))

suburb_summary_rank <- suburb_summary_rank %>% mutate(avg_GROWTH_RATE = round(avg_GROWTH_RATE * 100, 2))

# 如果需要，将百分比符号添加到Avg_GROWTH_RATE列
suburb_summary_rank$avg_GROWTH_RATE <- paste(suburb_summary_rank$avg_GROWTH_RATE, "%", sep = "")

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
    .shiny-html-output.shiny-bound-output.shiny-output-error {
    display: none;
    }
    
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
      background-color:rgb(224, 220, 220, 1);
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
  values <- reactiveValues(clicked_line = NULL, 
                           clicked_polygon = NULL,
                           clicked_reset = NULL,
                           clicked_overview = FALSE)
  
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
  
  
  # 监听地图的点击事件
  observeEvent(input$map_shape_click, {
    
    click <- input$map_shape_click
    
    if (class(click$id) == 'integer') {
      # the line has been clicked
      line <- mel_st_lines_sf[mel_st_lines_sf$OBJECTID_1 == click$id, ]
      
      values$clicked_line <- line
      
      values$clicked_polygon <- NULL
      
    } else {
      # the polygon has been clicked
      updateSelectInput(session, 'suburb', selected=input$map_shape_click)
      
      polygon <- mel_suburbs_wgs84[mel_suburbs_wgs84$clue_area == click$id, ]
      
      values$clicked_polygon <- polygon
      
      values$clicked_line <- NULL
    }
    
  })
  
  observeEvent(input$subtraff_overview, {
    
    values$clicked_overview <-input$subtraff_overview
    
  })
  
  
  # 添加点击按钮关闭窗口的事件
  observeEvent(input$reset_button, {
    values$clicked_line <- NULL
    
    values$clicked_polygon <- NULL
    
    updateSelectInput(session, 'suburb', selected= "")
    
    updateCheckboxInput(session, 'subtraff_overview', value = FALSE)
    
    values$clicked_reset <- NULL
    
  })
  
  # 监听filter select事件
  observeEvent(input$suburb, {
    
    temp = mel_suburbs_wgs84[mel_suburbs_wgs84$clue_area == input$suburb, ]
    
    if (dim(temp)[1] == 0) {
      values$clicked_polygon <- NULL
    } else {
      # Clear the selection sidebar, stop the selection
      values$clicked_polygon <- mel_suburbs_wgs84[mel_suburbs_wgs84$clue_area == input$suburb, ]
    }
    
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
    
    polygon <- values$clicked_polygon
    line <- values$clicked_line
    
    if (is.null(polygon) & is.null(line)) {
      return()
    } 
    
    if ( !is.null(values$clicked_polygon) ){
      div(
        style = "position: absolute; top: 10px; right: 10px; width: 450px; height: 450px; background-color: rgb(145, 145, 145);",
        uiOutput('float_window_head'),
        div(
          style = "position: absolute; top: 8%; width: 100%; height: 92%; background-color: rgb(224, 220, 220, 0.8); padding: 10px;",
          plotlyOutput(outputId = 'stat_polygon_plot')
        )
        
      )
    } else {
      div(
        style = "position: absolute; top: 10px; right: 10px; width: 450px; height: 450px; background-color: rgb(145, 145, 145);",
        uiOutput('float_window_head'),
        div(
          style = "position: absolute; top: 8%; width: 100%; height: 92%; background-color: rgb(224, 220, 220, 0.8); padding: 10px;",
          plotlyOutput(outputId = 'stat_line_plot')
        )
        
      )
    }
  })
  
  
  output$overview <- renderUI({
    if (values$clicked_overview) {
      div(class = 'my-overview',
          plotlyOutput('myBubbleChart'),
          helpText('A bubble chart illustrates the possible relationship 
                   between predicated Traffic Volume Growth Rate and Daily
                   Average Traffic Volume (2020 Traffic Volume Data).
                   The size of the bubbles represent the Equivalent Heavy Vehicle (EHV)
                   Ratio (i.e. %Heavy Vehicle + 3 * %Light Vehicle). Thus, the 
                   larger the bubble, the heavier traffic.', 
                   class = 'my-bubble-chart-text')
      )
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
  
  output$float_window_head <- renderUI({
    
    polygon <- values$clicked_polygon
    line <- values$clicked_line
    
    
    if (!is.null(polygon)) {
      HTML(as.character(div(style = "position: relative; left: 2%; color: white; font-weight: bold; font-size: 20px; padding: 6px 6px;",
                            polygon$clue_area)
      )
      )
    } else {
      HTML(as.character(div(style = "position: relative; left: 2%; color: white; font-weight: bold; font-size: 20px; padding: 6px 6px;",
                            line$LOCAL_ROAD_NM)
      )
      )
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
        # padding
        header = list(
          values = c(paste0('<b>', temp$clue_area,'</b>'), '<b>Value</b>','<b>Rank</b>'),
          line = list(color = 'black'),
          fill = list(color = 'rgb(145, 145, 145)'),
          align = c('left','center'),
          font = list(color = 'white', size = 14)
        ),
        cells = list(
          values = rbind(
            c('Avg # vehicles on all streets per day',
              'Avg # trucks on all streets per day',
              'Avg highest vehicle flow 12PM-12AM',
              'Avg highest vehicle flow 12AM-12PM',
              'Avg logarithmic annual growth rate of volume'),
            c(temp$total_ALLVEHS_AADT,
              temp$total_TRUCKS_AADT,
              temp$mean_HHF_PMPEAK_AADT,
              temp$mean_HHF_AMPEAK_AADT,
              temp$avg_GROWTH_RATE),
            c(temp$total_ALLVEHS_AADT_rank,
              temp$total_TRUCKS_AADT_rank,
              temp$mean_HHF_PMPEAK_AADT_rank,
              temp$mean_HHF_AMPEAK_AADT_rank,
              temp$avg_GROWTH_RATE_rank)),
          line = list(color = 'black'),
          fill = list(color = c('rgb(145, 145, 145)', 'white')),
          align = c('left', 'center'),
          font = list(color = c('white','black'), size = 14)
        ))
      # 设置整个图表的背景为透明
      fig <- fig %>% layout(
        paper_bgcolor = 'transparent',
        plot_bgcolor = 'transparent',
        margin = list(l = 10, r = 10, t = 30, b = 10)
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
        
        # 添加一个文本注释，替代图形
        fig <- fig %>% layout(
          annotations = list(
            list(
              text = "No data collected <br>for this section of <br>this street yet....",
              x = -0.05,  # 水平位置
              xref = "paper",  # 相对于整个图表的水平坐标
              xanchor = 'left',  # 水平锚点位置
              y = 0.55,  # 垂直位置
              yref = "paper",  # 相对于整个图表的垂直坐标
              yanchor = 'middle',  # 垂直锚点位置
              showarrow = FALSE,  # 不显示箭头
              font = list(size = 40, color = "rgb(141, 140, 139)")  # 文本的字体大小和颜色
            )
          ),
          paper_bgcolor = 'rgba(0,0,0,0)',
          plot_bgcolor = 'rgba(0,0,0,0)',
          xaxis = list(showgrid = FALSE, 
                       showline = FALSE,
                       showticklabels = FALSE,
                       zeroline = FALSE),
          yaxis = list(showgrid = FALSE, 
                       showline = FALSE,
                       showticklabels = FALSE,
                       zeroline = FALSE)
        )
        
        fig
        
      } else {
        
        fig <- plot_ly(temp, 
                       x = ~ACCIDENT_YEAR, y = ~INJ_OR_FATAL, 
                       name = 'Fatal or Injured', 
                       type = 'scatter', mode = 'lines',
                       line = list(color = 'rgb(141, 140, 139)', width = 4, dash = 'dot'),
                       marker = list(color = 'rgb(141, 140, 139)', size = 8)) 
        fig <- fig %>% add_trace(y = ~WEEKDAYS, name = 'Weekdays', 
                                 line = list(color = 'rgb(44, 29, 9)', width = 4, dash = 'dash'),
                                 marker = list(color = 'rgb(44, 29, 9)', size = 8)) 
        fig <- fig %>% add_trace(y = ~ACCIDENTS, name = 'Daily Avg', 
                                 line = list(color = 'rgb(205, 12, 24)', width = 4, dash = 'solid'),
                                 marker = list(color = 'rgb(205, 12, 24)', size = 8)) 
        fig <- fig %>% layout(
          title = list(text = "Street Accidents Against Year",
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
          yaxis = list (title = "Counts", 
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
          ),
          # 设置背景为透明
          paper_bgcolor = 'rgba(0,0,0,0)',
          plot_bgcolor = 'rgba(0,0,0,0)',
          legend = list(orientation = 'h', y =-0.2)  # 设置图例位置在下方
        )
        fig
      }
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



