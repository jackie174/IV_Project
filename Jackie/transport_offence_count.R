
# install.packages("shiny")
# install.packages("readxl")

library(shiny)
library(readxl)

ui <- fluidPage(
  titlePanel("Unique Statistics for 'Location Group'"),
  mainPanel(
    tableOutput("dataTable")
  )
)

server <- function(input, output, session) {
  data <- reactive({
    df <- read_excel("../Data/crime/LGA_Recorded_Offences_Year_Ending_June_2023.xlsx")
    subset_df <- df[df$`Location Subdivision` %in% c("Other Transport", "Public Transport"), ]
    subset_df
  })
  
  output$dataTable <- renderTable({
    unique_vals <- unique(data()$`Location Group`)
    data.frame("Unique 'Location Group'" = unique_vals)
  })
}

shinyApp(ui, server)
