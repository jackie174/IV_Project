
library(shiny)
library(readxl)
library(dplyr)
library(gdata)
# 读取Excel文件
data <- read_excel("../Data/crime/LGA_Recorded_Offences_Year_Ending_June_2023.xlsx", sheet= "Table 04")

# 筛选满足条件的行
filter_data <- data[grep("Other Transport|public Transport", data$`Location Subdivision`, ignore.case = TRUE), ]
filter_data <- filter_data[grep("Melbourne", filter_data$`Local Government Area`, ignore.case = TRUE), ]
selected_data <-
  filter_data[, c('Year', 'Local Government Area', 'Location Subdivision', 'Location Group', 'Offence Count')]

# 对Location Subdivision进行汇总并添加新的列
selected_data <- selected_data %>%
  group_by(Year, `Local Government Area`, `Location Subdivision`) %>%
  mutate(Total_Offence_by_Subdivision = sum(`Offence Count`)) %>%
  ungroup()

# 使用gsub函数删除数字
selected_data$`Location Subdivision` <- gsub("\\d", "", selected_data$`Location Subdivision`)
selected_data$`Location Group` <- gsub("\\d", "", selected_data$`Location Group`)




# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Old Faithful Geyser Data"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            sliderInput("bins",
                        "Number of bins:",
                        min = 1,
                        max = 50,
                        value = 30)
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("distPlot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$distPlot <- renderPlot({
        # generate bins based on input$bins from ui.R
        x    <- faithful[, 2]
        bins <- seq(min(x), max(x), length.out = input$bins + 1)

        # draw the histogram with the specified number of bins
        hist(x, breaks = bins, col = 'darkgray', border = 'white',
             xlab = 'Waiting time to next eruption (in mins)',
             main = 'Histogram of waiting times')
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
