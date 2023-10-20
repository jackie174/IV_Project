library(shiny)
library(tidyverse)
library(jsonlite)
library(plotly)
library(ggplot2)
library(dplyr)
library(hrbrthemes)
library(tidyr)
library(sf)
################# Realtion data ###############
suburbs_lga_mel = c('Carlton', 'Carlton North', 'East Melbourne', 
                    'South Yarra', 'Melbourne', 'Southbank', 
                    'South Wharf', 'Docklands', 'Port Melbourne',
                    'West Melbourne', 'North Melbourne','Kensington', 
                    'Parkville', 'Flemington')

# ---------------------- Carpark Data Processing ----------------------

# Reading and processing carpark data
carpark_data <- read.csv("../Data/relation/off-street-car-parks-with-capacity-and-type1.csv", header=TRUE)
# ... [All your carpark data processing code, with variable names prefixed by carpark_]
# 对"Geography"列中的特定值进行替换
carpark_data$`Clue.Small.Area` <- gsub("Melbourne \\(CBD\\)", "Melbourne", carpark_data$`Clue.Small.Area`)
carpark_data$`Clue.Small.Area` <- gsub("Melbourne \\(Remainder\\)", "Melbourne", carpark_data$`Clue.Small.Area`)
carpark_data$`Clue.Small.Area` <- gsub("West Melbourne \\(Residential\\)", "West Melbourne", carpark_data$`Clue.Small.Area`)
carpark_data$`Clue.Small.Area` <- gsub("West Melbourne \\(Industrial\\)", "West Melbourne", carpark_data$`Clue.Small.Area`)

carpark_data_2020 <- carpark_data %>% filter(Census.Year == 2020)
carpark_result <- carpark_data_2020 %>%
  group_by(`Clue.Small.Area`) %>%
  summarise(total_parking_spaces = sum(`Parking.Spaces`))
# ... [Continue your carpark data processing]
total_parking <- sum(carpark_data_2020$`Parking.Spaces`, na.rm = TRUE)

# 计算每个Clue.Small.Area的停车百分比
carpark_result$parking_percentage <- (carpark_result$total_parking_spaces / total_parking) * 100
# 计算最大的停车百分比
max_parking_percentage <- max(carpark_result$parking_percentage, na.rm = TRUE)
# ---------------------- Employment Data Processing ----------------------

employment_data <- read.csv("../Data/relation/employment-by-block-by-space-use2.csv", header=TRUE)
# ... [All your employment data processing code, with variable names prefixed by employment_]
# 对"geography"列中的特定值进行替换
employment_data$`Clue.Small.Area` <- gsub("Melbourne \\(CBD\\)", "Melbourne", employment_data$`Clue.Small.Area`)
employment_data$`Clue.Small.Area` <- gsub("Melbourne \\(Remainder\\)", "Melbourne", employment_data$`Clue.Small.Area`)
employment_data$`Clue.Small.Area` <- gsub("West Melbourne \\(Residential\\)", "West Melbourne", employment_data$`Clue.Small.Area`)
employment_data$`Clue.Small.Area` <- gsub("West Melbourne \\(Industrial\\)", "West Melbourne", employment_data$`Clue.Small.Area`)
employment_data$`Total.Jobs.In.Block`[is.na(employment_data$`Total.Jobs.In.Block`)] <- 0

employment_data_2020<- employment_data %>% filter(Census.Year == 2020)
employment_result <- employment_data_2020 %>%
  
  group_by(`Clue.Small.Area`) %>%
  summarise(total_jobs = sum(`Total.Jobs.In.Block`, na.rm = TRUE))
# ... [Continue your employment data processing]
# 计算工作数据集的总和
total_jobs <- sum(employment_data_2020$`Total.Jobs.In.Block`, na.rm = TRUE)

# 计算每个Clue.Small.Area的工作百分比
employment_result$jobs_percentage <- (employment_result$total_jobs / total_jobs) * 100

# 计算最大的工作百分比
max_jobs_percentage <- max(employment_result$jobs_percentage, na.rm = TRUE)

# ---------------------- Transport Data Processing ----------------------

transport_data <- employment_data
transport_data_2020 <- transport_data %>% filter(Census.Year == 2020)
transport_result <- transport_data_2020 %>%
  group_by(`Clue.Small.Area`) %>%
  summarise(transport_count =n())
# 计算工作数据集的总和
total_transport <- sum(transport_result$transport_count, na.rm = TRUE)

# 计算每个Clue.Small.Area的工作百分比
transport_result$transport_percentage <- (transport_result$transport_count / total_transport) * 100

transport_max_transport_count <- max(transport_result$transport_count, na.rm = TRUE)
# ---------------------- Block Data Processing ----------------------

bus_stop_data <- read.csv('../Data/blocks-for-census-of-land-use-and-employment-clue.csv')
bus_stop_data <- bus_stop_data %>%
  mutate(
    lat = as.numeric(sapply(strsplit(as.character(Geo.Point), ", "), "[", 1)),
    lng = as.numeric(sapply(strsplit(as.character(Geo.Point), ", "), "[", 2))
  )

bus_stop_data <- st_as_sf(bus_stop_data, coords = c("lng", "lat"), crs = 4326)
bus_stop_data2 <- read.csv('../Data/relation/bus-stops1.csv')
bus_stop_data2 <- bus_stop_data2 %>%
  mutate(
    lat = as.numeric(sapply(strsplit(as.character(Geo.Point.2D), ", "), "[", 1)),
    lng = as.numeric(sapply(strsplit(as.character(Geo.Point.2D), ", "), "[", 2))
  )

bus_stop_data2 <- st_as_sf(bus_stop_data2, coords = c("lng", "lat"), crs = 4326)

distance_threshold <- 500
bus_stop_buffered_data <- st_buffer(bus_stop_data, distance_threshold)
bus_stop_joined_data <- st_join(bus_stop_data2, bus_stop_buffered_data, join = st_within)

bus_stop_joined_data <- bus_stop_joined_data %>%
  group_by(clue_area) %>%
  summarise(total_stops = n()) %>%
  filter(!is.na(clue_area))
bus_stop_joined_data$clue_area <- gsub("Melbourne \\(CBD\\)", "Melbourne", bus_stop_joined_data$clue_area)
bus_stop_joined_data$clue_area <- gsub("Melbourne \\(Remainder\\)", "Melbourne", bus_stop_joined_data$clue_area)
bus_stop_joined_data$clue_area <- gsub("West Melbourne \\(Residential\\)", "West Melbourne", bus_stop_joined_data$clue_area)
bus_stop_joined_data$clue_area <- gsub("West Melbourne \\(Industrial\\)", "West Melbourne", bus_stop_joined_data$clue_area)

bus_stop_data <- data.frame(
  name = bus_stop_joined_data$clue_area, 
  val = bus_stop_joined_data$total_stops
)
bus_stop_joined_data <- bus_stop_data %>%
  group_by(`name`) %>%
  summarise(bus_stop_count = sum(val, na.rm = TRUE))


# ---------------------- cleaning data for liquor_fig5 ----------------------
liquor_data <- read.csv("../Data/relation/Metropolitan-as-at-31-December-20202.csv", header=FALSE)

liquor_data$V9_lower <- tolower(liquor_data$V9)
suburbs_lga_mel_lower <- tolower(suburbs_lga_mel)
liquor_filtered_data <- liquor_data[liquor_data$V9_lower %in% suburbs_lga_mel_lower,]

lookup <- setNames(suburbs_lga_mel, suburbs_lga_mel_lower)
liquor_filtered_data$V9 <- lookup[liquor_filtered_data$V9_lower]
liquor_filtered_data$V9_lower <- NULL

liquor_filtered_data <- liquor_filtered_data %>%
  group_by(V9) %>%
  summarise(TOTAL_LICES = n())
# ---------------------- cleaning data for population_fig7 ----------------------

population_fig71_data <- read.csv("../Data/relation/city-of-melbourne-population-forecasts-by-small-area-2020-2040.csv")

# 对"Geography"列中的特定值进行替换
population_fig71_data$Geography <- gsub("Melbourne \\(CBD\\)", "Melbourne", population_fig71_data$Geography)
population_fig71_data$Geography <- gsub("West Melbourne \\(Residential\\)", "West Melbourne", population_fig71_data$Geography)
population_fig71_data$Geography <- gsub("South Yarra \\(inc\\. Melbourne Remainder\\)", "South Yarra", population_fig71_data$Geography)
population_fig71_data$Geography <- gsub("West Melbourne \\(Industrial\\)", "West Melbourne", population_fig71_data$Geography)

# 删除包含"Greater Melbourne"和"City of Melbourne"的所有行
population_fig71_data <- population_fig71_data[!population_fig71_data$Geography %in% c("Greater Melbourne", "City of Melbourne"), ]

population_fig71_data_2020 <- population_fig71_data %>% filter(Year == 2021)

population_fig71_result <- population_fig71_data_2020 %>%
  group_by(Geography) %>%
  summarise(total_pouplation = sum(Value))

population_fig71_result <- population_fig71_result %>%
  mutate(label = paste(Geography, total_pouplation, sep=" "))

population_fig71_result <- population_fig71_result %>%
  filter(!is.na(total_pouplation))

population_fig71_result$total_pouplation[is.na(population_fig71_result$total_pouplation)] <- 0

scaling_factor <- 100 / log(max(population_fig71_result$total_pouplation) + 1)

population_fig71_result <- population_fig71_result %>%
  mutate(Scaled_population = log(total_pouplation + 1) * scaling_factor)

population_fig71_data_final <- data.frame(
  individual=population_fig71_result$label,
  group=population_fig71_result$Geography,
  value=population_fig71_result$Scaled_population
)

# -----------------------------------Produce new Data --------------------------

suburb_realtion <- data.frame(Suburb = suburbs_lga_mel)

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

# 将所有数值列的小数点保留到两位
suburb_realtion <- suburb_realtion %>%
  mutate_if(is.numeric, ~round(., 2))
################################################




# Define sidebar layout
mySidebarLayout <- sidebarLayout(
  sidebarPanel(
    selectInput("filter", "Filter by Suburb:", 
                choices = c("Clue.Small.Area", "V9", "clue_area", "Geography"))
  ),
  mainPanel(
    tabsetPanel(
      tabPanel("Relation",
               tabsetPanel(
                 tabPanel("Traffic Related",
                          fluidRow(
                            column(6, plotlyOutput("carpark_fig1")),
                            column(6, plotlyOutput("transport_fig3"))
                          ),
                          fluidRow(
                            column(6, plotlyOutput("bus_stop_fig4")),
                            column(6, plotOutput("population_fig71"))
                          )
                 ),
                 tabPanel("Crime Related", 
                          fluidRow(
                            column(12, plotlyOutput("employment_fig2", height=300))
                          ),
                          fluidRow(
                            column(6, plotOutput("population_fig72")),
                            column(6, plotlyOutput("liquor_fig5"))
                          )
                 )
               )
      )
    )
  )
)

# Define UI using fluidPage and the externally defined sidebar layout
ui <- fluidPage(
  titlePanel("Display Plots"),
  mySidebarLayout
)
server <- function(input, output, session) {
  output$carpark_fig1 <- renderPlotly({
    carpark_fig1 <- plot_ly(
      type = 'bar',
      y = ~suburb_realtion$parking_percentage,
      x = ~suburb_realtion$Suburb,

    ) %>%
      layout(
        title = list(text = "Parking Report", y = 1.5),  # 调整标题的垂直位置
        polar = list(radialaxis = list(visible = T, range = c(0, max_parking_percentage))),
        showlegend = FALSE
      )
  })
  output$employment_fig2 <- renderPlotly({
    employment_fig2 <- plot_ly(
      type = 'bar',
      y = ~suburb_realtion$jobs_percentage,
      x = ~suburb_realtion$Suburb,

    ) %>%
      layout (
        title = list(text = "Employment Report", y = 0.95),  # 调整标题的垂直位置
        polar = list(radialaxis = list(visible = T, range = c(0, max_jobs_percentage))),
        showlegend = FALSE)
    
  })
  output$transport_fig3 <- renderPlotly({
    transport_fig3 <- plot_ly(
      type = 'bar',
      y = ~suburb_realtion$transport_percentage,
      x = ~suburb_realtion$Suburb,

    ) %>% layout(
      title = list(text="Transport Use Land Report",  y = 0.95),
      polar = list(radialaxis = list(visible = T, range = c(0,transport_max_transport_count))),
      showlegend = FALSE)
  })
  output$bus_stop_fig4 <- renderPlotly({
    bus_stop_fig4 <- ggplot(suburb_realtion, aes(x = Suburb, y = bus_stop_count)) +
      geom_segment(aes(xend = Suburb, yend = 0)) +
      geom_point(size = 4, color = "skyblue") +
      coord_flip() +
      theme_bw() +
      xlab("")+labs(title = "Number of Bus Stop Report")
    
    bus_stop_fig4 <- ggplotly(bus_stop_fig4)
  })
  output$liquor_fig5 <- renderPlotly({
    liquor_fig5 <- plot_ly(
      suburb_realtion, x = ~Suburb, y = ~TOTAL_LICES, 
      type = 'bar', text = ~as.character(TOTAL_LICES),
      textposition = 'auto',
      marker = list(color = 'rgb(158,202,225)',
                    line = list(color = 'rgb(8,48,107)'))
    ) %>% layout(title = "Liquor Report",
                 xaxis = list(title = ""),
                 yaxis = list(title = ""))
    

  })
  
  output$population_fig71 <- renderPlot({
    
    suburb_realtion$label <- as.factor(suburb_realtion$label)
    
    empty_bar <- 2
    to_add <- data.frame( matrix(NA, empty_bar*nlevels(suburb_realtion$label), ncol(suburb_realtion)) )
    colnames(to_add) <- colnames(suburb_realtion)
    to_add$label <- rep(levels(suburb_realtion$label), each=empty_bar)
    
    suburb_realtion <- rbind(suburb_realtion, to_add)
    suburb_realtion <- suburb_realtion %>% arrange(label)
    suburb_realtion$id <- seq(1, nrow(suburb_realtion))
    
    label_data <- suburb_realtion
    number_of_bar <- nrow(label_data)
    angle <- 90 - 360 * (label_data$id-0.5) /number_of_bar     
    label_data$hjust <- ifelse( angle < -90, 1, 0)
    label_data$angle <- ifelse(angle < -90, angle+180, angle)
    
    fig7 <- ggplot(suburb_realtion, aes(x=as.factor(id), y=Scaled_population)) +      
      geom_bar(stat="identity", fill=alpha("green", 0.3)) +
      ylim(-100,200) +
      theme_minimal() +
      theme(
        axis.text = element_blank(),
        axis.title = element_blank(),
        panel.grid = element_blank(),
        plot.margin = unit(rep(-1,4), "cm")     
      ) +
      coord_polar(start = 0) + 
      labs(title = "Population Report")+
      geom_text(data=label_data, aes(x=id, y=Scaled_population+10, label=label, hjust=hjust), color="black", fontface="bold",alpha=0.6, size=2.5, angle= label_data$angle, inherit.aes = FALSE ) 
    
    fig7
    
  })
  output$population_fig72 <- renderPlot({
    
    suburb_realtion$label <- as.factor(suburb_realtion$label)
    
    empty_bar <- 1
    to_add <- data.frame( matrix(NA, empty_bar*nlevels(suburb_realtion$label), ncol(suburb_realtion)) )
    colnames(to_add) <- colnames(suburb_realtion)
    to_add$label <- rep(levels(suburb_realtion$label), each=empty_bar)
    
    suburb_realtion <- rbind(suburb_realtion, to_add)
    suburb_realtion <- suburb_realtion %>% arrange(label)
    suburb_realtion$id <- seq(1, nrow(suburb_realtion))
    
    label_data <- suburb_realtion
    number_of_bar <- nrow(label_data)
    angle <- 90 - 360 * (label_data$id-0.5) /number_of_bar     
    label_data$hjust <- ifelse( angle < -90, 1, 0)
    label_data$angle <- ifelse(angle < -90, angle+180, angle)
    
    fig7 <- ggplot(suburb_realtion, aes(x=as.factor(id), y=Scaled_population)) +      
      geom_bar(stat="identity", fill=alpha("green", 0.3)) +
      ylim(-50,200) +
      theme_minimal() +
      theme(
        axis.text = element_blank(),
        axis.title = element_blank(),
        panel.grid = element_blank(),
        plot.margin = unit(rep(-1,4), "cm")     
      ) +
      coord_polar(start = 0) + 
      ggtitle("Populaton report")+
      geom_text(data=label_data, aes(x=id, y=Scaled_population+10, label=label, hjust=hjust), color="black", fontface="bold",alpha=0.6, size=4, angle= label_data$angle, inherit.aes = FALSE ) 
    
    fig7
  })
}

shinyApp(ui, server)
