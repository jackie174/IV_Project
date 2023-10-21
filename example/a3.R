library(shiny)
library(tidyverse)
library(jsonlite)
library(plotly)
library(ggplot2)
library(dplyr)
library(hrbrthemes)
library(tidyr)
library(sf)
library(shinythemes)
library(readxl)
library(scales)
library(shinyjs)
# Load the GEOM90007 Tableau in Shiny library
source('tableau-in-shiny-v1.0.R')
############################################################################################################
############################################ CLEAN DATA START ##############################################
############################################################################################################

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
# 对"Geography"列中的特定值进行替换
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

# 计算每个Clue.Small.Area的停车百分比
carpark_result$parking_percentage <-
  (carpark_result$total_parking_spaces / total_sum_parking) * 100
# 计算最大的停车百分比
max_parking_percentage <-
  max(carpark_result$parking_percentage, na.rm = TRUE)
# ---------------------- Employment Data Processing 

employment_data <-
  read.csv("../Data/relation/employment-by-block-by-space-use2.csv",
           header = TRUE)
# ... [All your employment data processing code, with variable names prefixed by employment_]
# 对"geography"列中的特定值进行替换
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
# 计算工作数据集的总和
total_sum_jobs <-
  sum(employment_data_2020$`Total.Jobs.In.Block`, na.rm = TRUE)

# 计算每个Clue.Small.Area的工作百分比
employment_result$jobs_percentage <-
  (employment_result$total_jobs / total_sum_jobs) * 100

# 计算最大的工作百分比
max_jobs_percentage <-
  max(employment_result$jobs_percentage, na.rm = TRUE)

# ---------------------- Transport Data Processing 

transport_data <- employment_data
transport_data_2020 <-
  transport_data %>% filter(Census.Year == 2020)
transport_result <- transport_data_2020 %>%
  group_by(`Clue.Small.Area`) %>%
  summarise(transport_count = n())
# 计算工作数据集的总和
total_sum_transport <-
  sum(transport_result$transport_count, na.rm = TRUE)

# 计算每个Clue.Small.Area的工作百分比
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
  read.csv("../Data/relation/Metropolitan-as-at-31-December-20202.csv",
           header = FALSE)

liquor_data$V9_lower <- tolower(liquor_data$V9)
suburbs_lga_mel_lower <- tolower(suburbs_lga_mel)
liquor_filtered_data <-
  liquor_data[liquor_data$V9_lower %in% suburbs_lga_mel_lower, ]

lookup <- setNames(suburbs_lga_mel, suburbs_lga_mel_lower)
liquor_filtered_data$V9 <- lookup[liquor_filtered_data$V9_lower]
liquor_filtered_data$V9_lower <- NULL
liquor_filtered_data <- liquor_filtered_data %>%
  group_by(V9) %>%
  summarise(total_liquor_store = n())

# ---------------------- cleaning data for population_fig7

population_fig71_data <-
  read.csv(
    "../Data/relation/city-of-melbourne-population-forecasts-by-small-area-2020-2040.csv"
  )

# 对"Geography"列中的特定值进行替换
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

# 删除包含"Greater Melbourne"和"City of Melbourne"的所有行
population_fig71_data <-
  population_fig71_data[!population_fig71_data$Geography %in% c("Greater Melbourne", "City of Melbourne"),]

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

# 计算工作数据集的总和
total_sum_population <-
  sum(population_fig71_result$total_population, na.rm = TRUE)

# 计算每个Clue.Small.Area的工作百分比
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
  data_crime[grep("Melbourne", data_crime$`Local Government Area`, ignore.case = TRUE),]
filter_data <- filter_data %>% filter(Year == 2020)
crime_result <- filter_data %>%
  group_by(`Suburb/Town Name`) %>%
  summarise(total_offence_count = sum(`Offence Count`))

# 计算工作数据集的总和
total_crime <- sum(crime_result$total_offence_count, na.rm = TRUE)

# 计算每个Clue.Small.Area的工作百分比
crime_result$crime_percentage <-
  (crime_result$total_offence_count / total_crime) * 100
# --------------------------------- Traffic Data Processing 

# Victoria street line data
mel_traffic_lines <- st_read("../Data/transportation/mel_traffic_vol.geojson")
# LGA melbourne shape data
mel_suburbs_gda94 <- st_read("../Data/Mel_LGA_Suburbs_GDA94/mel_suburbs_edit.geojson")
# extract the geometry center of the city of melbourne for initialize view 
city_mel_whole_shape <- st_read("../Data/city_of_mel_boundary/mel_boundary.shp")

# transform to the same coordinate system
mel_traffic_lines_wgs84 <- st_transform(mel_traffic_lines, crs = 4326)
mel_suburbs_wgs84 <- st_transform(mel_suburbs_gda94, crs = 4326)

mel_st_lines_sf <- st_intersection(mel_traffic_lines_wgs84, mel_suburbs_wgs84)

# Convert the geometry column back to multilinestring
mel_st_lines_sf <- st_cast(mel_st_lines_sf, "MULTILINESTRING")
mel_st_lines_sf <- mel_st_lines_sf %>% distinct()
traffic_result <- mel_st_lines_sf %>%
  group_by(`clue_area`) %>%
  summarise(total_traffic_count = sum(`ALLVEHS_AADT`))

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
  left_join(liquor_filtered_data, by = c("Suburb" = "V9"))
suburb_realtion <- suburb_realtion %>%
  left_join(employment_result, by = c("Suburb" = "Clue.Small.Area"))
suburb_realtion <- suburb_realtion %>%
  left_join(transport_result, by = c("Suburb" = "Clue.Small.Area"))
suburb_realtion <- suburb_realtion %>%
  left_join(population_fig71_result, by = c("Suburb" = "Geography"))
suburb_realtion$total_liquor_store[is.na(suburb_realtion$total_liquor_store)] <-
  0
# 将所有数值列的小数点保留到两位
suburb_realtion <- suburb_realtion %>%
  mutate_if(is.numeric, ~ round(., 2))




######################Traffic Dat #################


######################Crime Dat #################


#################################CLEANING DATA DONE#########################################################


############################# HELP FUNCTION START ###########################################################
############################# HELP FUNCTION DONE ###########################################################


############################################################################################################
############################################ CLEAN DATA DONE ###############################################
############################################################################################################

##################
# USER INTERFACE #
##################

######################### Home Page Tab Start ##############################################################
homepage <- tabPanel(title = 'Home',
                     mainPanel(
                       id = 'home',
                       width = 12,
                       tableauPublicViz(id = 'tableauViz',
                                        url = 'https://public.tableau.com/views/ivasmt3/CityofMelbourneAnalysis-publictransportcrime?:language=zh-CN&publish=yes&:display_count=n&:origin=viz_share_link',
                                        height = "800px")
                     ))
######################### Home Page Tab Done ##############################################################

######################### First Nav Tab Start ##############################################################
traffic_tab <- tabPanel(title = 'Traffic',
                        mainPanel(id = 'traffic',
                                  width = 12,))
######################### First Nav Tab Done ###############################################################

######################### Second Nav Tab Start #############################################################
# Define a tab panel titled 'Type and Rating'
crime_tab <- tabPanel(
  title = 'Crime',
  
  mainPanel(
    id = "crime",
    width = 12,
    # Define a set of tabs within the main panel
    tabsetPanel(
      # Define the first tab panel containing Pie Charts
      tabPanel("map"),
      # Define the second tab panel containing Bar Charts
      tabPanel(
        "static",
        # Define the main panel containing the chart plots))))
  ))))      
######################### Second Nav Tab Done ##############################################################
######################### Third Nav Tab Start #############################################################
# Define a tab panel titled 'Type and Rating'
relation_tab <- tabPanel(title = 'Relation',
                         
   mainPanel(
     id = "relation",
     width = 12,
     # Define a set of tabs within the main panel
     tabsetPanel(
       tabPanel("Traffic Related",
                div(
                  style = "position:relative; z-index:1;",
                  
                  div(HTML(
                    paste0(
                      "<div style='position:absolute; top:0px; left:calc(75% - 20px); font-size:14px; z-index:2;' title='Sort the number by Traffic Volumn and Suburb name'>",
                      as.character(actionLink(
                        inputId = "sort_button1",
                        label = "Sort",
                        icon = icon("sort")
                      )),
                      "</div>"
                    )
                  )),
                  div(style = "margin-top: 30px;", 
                      plotlyOutput("plot_traffic", height = '750')
                  )
                )
       ),
       
       tabPanel("Crime Related",
                div(
                  style = "position:relative; z-index:1;",
                  
                  div(HTML(
                    paste0(
                      "<div style='position:absolute; top:0px; left:calc(75% - 20px); font-size:14px; z-index:2; color:white;' title='Sort the number by Crime number and Suburb name'>",
                      as.character(actionLink(
                        inputId = "sort_button2",
                        label = "Sort",
                        icon = icon("sort")
                      )),
                      "</div>"
                    )
                  )),
                  div(style = "margin-top: 30px;", 
                      plotlyOutput("plot_crime", height = '750')
                  )
                )
                )
     )))
   
        
######################### Third Nav Tab Done ##############################################################
ui <- navbarPage(
  header = setUpTableauInShiny(),
  id = 'mypage',
  tags$head(
    tags$script(HTML("
  $(document).on('click', function(event) {
      let clickedClass = event.target.attributes[1] ? event.target.attributes[1].nodeValue : null;

      Shiny.setInputValue('global_click', {
          x: event.pageX,
          y: event.pageY,
          target: event.target.tagName,
          class: clickedClass
      });

      // 注意这里的引号使用
      let elem = document.querySelector('[data-value=\"Crime Related\"]');
      let isActive = elem && elem.classList.contains('active');
      Shiny.setInputValue('data_value_active_status', isActive);
  });

")))
  
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
  navbarMenu("More",
             tabPanel(
               "Data Table",  mainPanel(dataTableOutput("viewTable"))
             ))
)

################
# SHINY SERVER #
################
server <- function(input, output, session) {
  observeEvent(input$mypage, {
    runjs('dispatchEvent(new Event("resize"))')
  })
  #########################Relation Part #########################
  sort_state <- reactiveVal("desc")
    # Observe for a click event on the sort button
    observeEvent(input$sort_button2, {
    # Check the current state of sorting
      if(sort_state() == "desc") {
        sort_state("inc")
      } else {
        sort_state("desc")
      }
    })
    # Observe for a click event on the sort button
    observeEvent(input$sort_button1, {
      # Check the current state of sorting
      if(sort_state() == "desc") {
        sort_state("inc")
      } else {
        sort_state("desc")
      }
    })
    sortedTrafficData <- reactive({
      
      # Sorting logic based on sort_state
      if(sort_state() == "inc") {
        suburb_realtion <- suburb_realtion %>% arrange(desc(suburb_realtion$Suburb))
      } else if(sort_state() == "desc") {
        suburb_realtion <- suburb_realtion %>% arrange(suburb_realtion$total_traffic_count)
      }
      suburb_realtion
    })
    
  sortedCrimeData <- reactive({
    
    # Sorting logic based on sort_state
    if(sort_state() == "inc") {
      suburb_realtion <- suburb_realtion %>% arrange(desc(suburb_realtion$Suburb))
    } else if(sort_state() == "desc") {
      suburb_realtion <- suburb_realtion %>% arrange(suburb_realtion$total_offence_count)
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
      geom_segment(aes(
        xend = Suburb,
        yend = 0,
        color = "#FFCCCC",
      )) +
      geom_point(size = 4, color = "skyblue") +
      coord_flip() +
      theme_bw() +
      xlab("") +
      theme(
        panel.grid.major.y = element_blank(),
        panel.border = element_blank(),
        axis.ticks.y = element_blank(),
        plot.title = element_text(
          size = 16,
          face = "bold",
          hjust = 0.5,
          color = "#606060"
        ),
        plot.title.position = "panel",
        axis.title.x = element_text(
          color = "black",
          size = 16,
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
          size = 16,
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
          size = 12,
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
          size = 12,
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
        title = "employment",  # Add this line for the title
        xaxis = list(title = 'Number of Jobs', showgrid = FALSE),
        yaxis = list(title = 'Suburb Name', showgrid = FALSE),
        plot_bgcolor = 'black',
        paper_bgcolor = 'black',
        font = list(color = "white")
      ) %>% 
      style(hoverinfo = "none", traces = 1) 
    
    liquor_fig5 <-
      ggplot(suburb_realtion_sort, aes(x = Suburb, y = total_liquor_store)) +
      geom_segment(aes(
        xend = Suburb,
        yend = 0,
        color = "#FFCCCC",
      )) +
      geom_point(size = 4, color = "skyblue") +
      coord_flip() +
      theme_bw() +
      xlab("") +
      theme(
        plot.title = element_text(
          size = 16,
          face = "bold",
          hjust = 0.5,
          color = "#606060"
        ),
        plot.title.position = "panel",
        axis.title.x = element_text(
          color = "black",
          size = 16,
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
          size = 16,
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
          size = 12,
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
          size = 12,
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
        title = "liquor",  # Add this line for the title
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
        t = 80
      )  # 使用t属性来调整图的顶部边距
    )
    # Update title
    annotations = list(
      list(
        x = 0.2,
        y = 1.0,
        text = "Number of jobs",
        xref = "paper",
        yref = "paper",
        xanchor = "center",
        yanchor = "bottom",
        showarrow = FALSE
      ),
      list(
        x = 0.8,
        y = 1,
        text = "Number of Liqour Store",
        xref = "paper",
        yref = "paper",
        xanchor = "center",
        yanchor = "bottom",
        showarrow = FALSE
      )
    )
    
    fig <-
      fig %>% layout(
        showlegend = FALSE,
        # 这里将隐藏图例
        annotations = annotations,
        plot_bgcolor = 'black',
        paper_bgcolor = 'black',
        font = list(color = "white")
      ) %>% onRender(
        "
    function(el) {
      el.on('plotly_click', function(data) {
          var containerId = el.id;
    var subplot = data.points[0].subplot;

    console.log('Clicked container:', containerId);
    console.log('Clicked subplot:', subplot);
        var yValue = data.points[0].x;
        var subplot = data.points[0].subplot;
console.log(data);
        console.log(data.points);
        console.log('Clicked value:', yValue);
        console.log('Clicked subplot:', subplot);

        if (subplot === 'xy') {  // Assuming 'xy' is the subplot id of the first plot
          Shiny.setInputValue('pointClicked_firstPlot', data.points);
        } else if (subplot === 'xy2') {  // Assuming 'xy2' is the subplot id of the second plot
          Shiny.setInputValue('pointClicked_secondPlot', data.points);
        }
      });

      el.on('plotly_relayout', function(data) {
        if (!data['xaxis.autorange'] && !data['yaxis.autorange']) { 
          console.log('Plot area clicked');
          Shiny.setInputValue('plotAreaClicked', true, {priority: 'event'});
        }
      });
    }
    "
      )
    fig
  })
  output$plot_traffic <- renderPlotly({
    suburb_realtion_sort <- sortedTrafficData()
    
    suburb_realtion_sort$Suburb <-
      factor(suburb_realtion_sort$Suburb, levels = suburb_realtion_sort$Suburb)
    
    carpark_fig1 <-
      ggplot(suburb_realtion_sort, aes(x = Suburb, y = total_parking_spaces)) +
      geom_segment(aes(
        xend = Suburb,
        yend = 0,
        color = "#FFCCCC",
      )) +
      geom_point(size = 4, color = "skyblue") +
      coord_flip() +
      theme_bw() +
      xlab("") +
      theme(
        plot.title = element_text(
          size = 16,
          face = "bold",
          hjust = 0.5,
          color = "#606060"
        ),
        plot.title.position = "panel",
        axis.title.x = element_text(
          color = "black",
          size = 16,
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
          size = 16,
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
          size = 12,
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
          size = 12,
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
      geom_segment(aes(
        xend = Suburb,
        yend = 0,
        color = "#FFCCCC",
      )) +
      geom_point(size = 4, color = "skyblue") +
      coord_flip() +
      theme_bw() +
      xlab("") +
      theme(
        plot.title = element_text(
          size = 16,
          face = "bold",
          hjust = 0.5,
          color = "#606060"
        ),
        plot.title.position = "panel",
        axis.title.x = element_text(
          color = "black",
          size = 16,
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
          size = 16,
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
          size = 12,
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
          size = 12,
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
      geom_segment(aes(
        xend = Suburb,
        yend = 0,
        color = "#FFCCCC",
      )) +
      geom_point(size = 4, color = "skyblue") +
      coord_flip() +
      theme_bw() +
      xlab("") +
      theme(
        plot.title = element_text(
          size = 16,
          face = "bold",
          hjust = 0.5,
          color = "#606060"
        ),
        plot.title.position = "panel",
        axis.title.x = element_text(
          color = "black",
          size = 16,
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
          size = 16,
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
          size = 12,
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
          size = 12,
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
      geom_segment(aes(
        xend = Suburb,
        yend = 0,
        color = "#FFCCCC"
      )) +
      geom_point(size = 4, color = "skyblue") +
      coord_flip() +
      theme_bw() +
      xlab("") + 
      theme(
        plot.title = element_text(
          size = 16,
          face = "bold",
          hjust = 0.5,
          color = "#606060"
        ),
        plot.title.position = "panel",
        axis.title.x = element_text(
          color = "black",
          size = 16,
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
          size = 16,
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
          size = 12,
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
          size = 12,
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
      )+
      scale_y_continuous(labels = comma)  # 使用这个来格式化y轴的标签
    
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
      )%>% 
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
      )  # 使用t属性来调整图的顶部边距
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
        showarrow = FALSE
      ), 
      list(
        x = 0.8,
        y = 1,
        text = "Number of Bus Stop ",
        xref = "paper",
        yref = "paper",
        xanchor = "center",
        yanchor = "bottom",
        showarrow = FALSE
      ),
      list(
        x = 0.2,
        y = 0.4,
        text = "Transport Use Land",
        xref = "paper",
        yref = "paper",
        xanchor = "center",
        yanchor = "bottom",
        showarrow = FALSE
      ),
      list(
        x = 0.8,
        y = 0.4,
        text = "Number of Car Park(off street)",
        xref = "paper",
        yref = "paper",
        xanchor = "center",
        yanchor = "bottom",
        showarrow = FALSE
      )
    )
    
    fig <-
      fig %>% layout(
        showlegend = FALSE,
        # 这里将隐藏图例
        annotations = annotations,
        plot_bgcolor = 'black',
        paper_bgcolor = 'black',
        font = list(color = "white")
      )  %>% onRender(
        "
    function(el) {
      el.on('plotly_click', function(data) {
        var yValue = data.points[0].x;
        var subplot = data.points[0].subplot;

        console.log(data.points);
        console.log('Clicked value:', yValue);
        console.log('Clicked subplot:', subplot);

        if (subplot === 'xy') {  // Assuming 'xy' is the subplot id of the first plot
          Shiny.setInputValue('pointClicked_firstPlot', data.points);
        } else if (subplot === 'xy2') {  // Assuming 'xy2' is the subplot id of the second plot
          Shiny.setInputValue('pointClicked_secondPlot', data.points);
        }
      });

      el.on('plotly_relayout', function(data) {
        if (!data['xaxis.autorange'] && !data['yaxis.autorange']) { 
          console.log('Plot area clicked');
          Shiny.setInputValue('plotAreaClicked', true, {priority: 'event'});
        }
      });
    }
    "
      )
    fig
  })
  

  observeEvent(input$global_click, {
    
    cat("Clicked at:", input$global_click$x, input$global_click$y, "\n")
    cat("Element:", input$global_click$target, "\n")
    cat("Class:", input$global_click$class, "\n")
  })
  observeEvent(input$data_value_active_status, {
    if (input$data_value_active_status) {
      print("Element with data-value 'Crime Related' is active!")
    } else {
      print("Element with data-value 'Crime Related' is not active or does not exist.")
    }
  })
  # observeEvent(input$phrase_status, {
  #   if (input$phrase_status) {
  #     print("The phrase 'Potential Influencing Factors on Crime' is not present on the page!")
  #   } else {
  #     print("The phrase 'Potential Influencing Factors on Crime' is found on the page.")
  #   }
  # })
  
  #########################Relation Part #########################

}
######################Help function for relation ##################



#############
# Run Shiny #
#############
shinyApp(ui, server, options = list(launch.browser = TRUE))
