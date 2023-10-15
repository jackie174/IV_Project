
library(shiny)
library(readxl)
library(dplyr)
library(gdata)
# 读取Excel文件
data_1 <- read_excel("../Data/crime/LGA_Recorded_Offences_Year_Ending_June_2023.xlsx", sheet= "Table 04")

# 筛选满足条件的行
filter_data <- data[grep("Other Transport|public Transport", data_1$`Location Subdivision`, ignore.case = TRUE), ]
filter_data <- filter_data[grep("Melbourne", filter_data$`Local Government Area`, ignore.case = TRUE), ]
#filter_data <- filter_data[grep("2020", filter_data$`Year`, ignore.case = TRUE), ]
selected_data <-
  filter_data[, c('Year', 'Local Government Area', 'Location Subdivision', 'Location Group', 'Offence Count')]

# 更新Location Subdivision列的值
selected_data$`Location Subdivision`[grep("car", selected_data$`Location Group`, ignore.case = TRUE)] <- "Car"

# 对Location Subdivision进行汇总并添加新的列
selected_data <- selected_data %>%
  group_by(Year, `Local Government Area`, `Location Subdivision`) %>%
  mutate(Total_Offence_by_Subdivision = sum(`Offence Count`)) %>%
  ungroup()

# 使用gsub函数删除数字
selected_data$`Location Subdivision` <- gsub("\\d", "", selected_data$`Location Subdivision`)
selected_data$`Location Group` <- gsub("\\d", "", selected_data$`Location Group`)
selected_data <- selected_data %>%
  mutate(label = paste(`Location Group`, `Offence Count`, sep=" "))
# 使用自然对数进行变换，然后乘以一个系数来放大值
scaling_factor <- 100 / log(max(selected_data$`Offence Count`) + 1)
selected_data <- selected_data %>%
  mutate(Scaled_Offence = log(`Offence Count` + 1) * scaling_factor)

#############################################################################################################


########### draw polt 1 ###########################################################################################
library(tidyverse)


##############


# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Old Faithful Geyser Data"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            sliderInput("years",
                        "Number of years:",
                        min = 2014,
                        max = 2023,
                        value = 2020),
            actionButton("sort_btn", "Sort Data")
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("distPlot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  # A reactive data filter
  
  getFilteredYearData <- reactive({
    print(input$years)
    print(selected_data)
    filtered_data <-selected_data %>%
      filter(Year == input$years)
    print(filtered_data)
    filtered_data
  })

  sort_state <- reactiveVal("desc")
  observeEvent(input$sort_btn, {
    if(sort_state() == "desc") {
      sort_state("inc")
    } else {
      sort_state("desc")
    }
  })

  sortedData <- reactive({
    selected_data_1<- getFilteredYearData()
    data <- data.frame(
      individual = selected_data_1$`label`,
      group=selected_data_1$`Location Subdivision`,
      value = selected_data_1$`Scaled_Offence`
    )
    data$group <- as.factor(data$group)
    # Sort the data based on the current value of sort_state
    if(sort_state() == "inc") {
      data <- data %>% arrange(group, desc(value))
      
    } else if(sort_state() == "desc") {
      data <- data %>% arrange(group, value)
      
    }
    

    
    data
  })

    output$distPlot <- renderPlot({
      sort_data <- sortedData()
      # Set a number of 'empty bar' to add at the end of each group
      empty_bar <- 3
      to_add <- data.frame( matrix(NA, empty_bar*nlevels(sort_data$group), ncol(sort_data)) )
      colnames(to_add) <- colnames(sort_data)
      to_add$group <- rep(levels(sort_data$group), each=empty_bar)
      sort_data <- rbind(sort_data, to_add)
      sort_data <- sort_data %>% arrange(group)
      sort_data$id <- seq(1, nrow(sort_data))
      #sort_data$group <- as.character(sort_data$group)
      
      # Get the name and the y position of each label
      label_data <- sort_data
      number_of_bar <- nrow(label_data)
      angle <- 90 - 360 * (label_data$id-0.5) /number_of_bar     # I substract 0.5 because the letter must have the angle of the center of the bars. Not extreme right(1) or extreme left (0)
      label_data$hjust <- ifelse( angle < -90, 1, 0)
      label_data$angle <- ifelse(angle < -90, angle+180, angle)
      
      # prepare a sort_data frame for base lines
      base_data <- sort_data %>% 
        group_by(group) %>% 
        summarize(start=min(id), end=max(id) - empty_bar) %>% 
        rowwise() %>% 
        mutate(title=mean(c(start, end)))
      
      # prepare a sort_data frame for grid (scales)
      grid_data <- base_data
      grid_data$end <- grid_data$end[ c( nrow(grid_data), 1:nrow(grid_data)-1)] + 1
      grid_data$start <- grid_data$start - 1
      grid_data <- grid_data[-1,]
      p <- ggplot(sort_data, aes(x=as.factor(id), y=value, fill=group)) +       # Note that id is a factor. If x is numeric, there is some space between the first bar
        
        geom_bar(aes(x=as.factor(id), y=value, fill=group), stat="identity", alpha=0.5) +
        
        # Add a val=100/75/50/25 lines. I do it at the beginning to make sur barplots are OVER it.
        geom_segment(data=grid_data, aes(x = end, y = 100, xend = start, yend = 100), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
        geom_segment(data=grid_data, aes(x = end, y = 60, xend = start, yend = 60), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
        geom_segment(data=grid_data, aes(x = end, y = 40, xend = start, yend = 40), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
        geom_segment(data=grid_data, aes(x = end, y = 20, xend = start, yend = 20), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
        
        # Add text showing the value of each 100/75/50/25 lines
        annotate("text", x = rep(max(sort_data$id),4), y = c(20, 40, 60, 100), label = c("20", "40", "60", "100") , color="grey", size=3 , angle=0, fontface="bold", hjust=3) +
        
        geom_bar(aes(x=as.factor(id), y=value, fill=group), stat="identity", alpha=0.5) +
        ylim(-100,120) +
        theme_minimal() +
        theme(
          legend.position = "none",
          axis.text = element_blank(),
          axis.title = element_blank(),
          panel.grid = element_blank(),
          plot.margin = unit(rep(-1,4), "cm") 
        ) +
        coord_polar() + 
        geom_text(data=label_data, aes(x=id, y=value+10, label=individual, hjust=hjust), color="red", fontface="bold",alpha=0.6, size=2.5, angle= label_data$angle, inherit.aes = FALSE ) +
        
        # Add base line information
        geom_segment(data=base_data, aes(x = start, y = -5, xend = end, yend = -5), colour = "skyblue", alpha=0.8, size=0.6 , inherit.aes = FALSE )  +
        # ...
        geom_text(data=base_data, aes(x = title, y = -25, label=group), hjust=c(0.8,0.5,0), colour = "blue", alpha=0.8, size=3, fontface="bold", inherit.aes = FALSE)
      p
    })
}
prepare_polar_data <- function(data) {
  # Set a number of 'empty bar' to add at the end of each group
  empty_bar <- 3
  to_add <- data.frame(matrix(NA, empty_bar * nlevels(data$group), ncol(data)))
  colnames(to_add) <- colnames(data)
  to_add$group <- rep(levels(data$group), each = empty_bar)
  data <- rbind(data, to_add)
  data <- data %>% arrange(group)
  data$id <- seq(1, nrow(data))
  
  # Get the name and the y position of each label
  label_data <- data
  number_of_bar <- nrow(label_data)
  angle <- 90 - 360 * (label_data$id - 0.5) / number_of_bar
  label_data$hjust <- ifelse(angle < -90, 1, 0)
  label_data$angle <- ifelse(angle < -90, angle + 180, angle)
  
  # Prepare a data frame for base lines
  base_data <- data %>% 
    group_by(group) %>% 
    summarize(start = min(id), end = max(id) - empty_bar) %>% 
    rowwise() %>% 
    mutate(title = mean(c(start, end)))
  
  # Prepare a data frame for grid (scales)
  grid_data <- base_data
  grid_data$end <- grid_data$end[c(nrow(grid_data), 1:nrow(grid_data) - 1)] + 1
  grid_data$start <- grid_data$start - 1
  grid_data <- grid_data[-1, ]
  
  return(list(data = data, label_data = label_data, base_data = base_data, grid_data = grid_data))
}
# Run the application 
shinyApp(ui = ui, server = server)
