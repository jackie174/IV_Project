library('shiny')
library('ggplot2')
library('ggiraph')
library('leaflet')
library('dplyr')
library('tidyr')
library('shinythemes')

block_data <- read.csv('blocks-for-census-of-land-use-and-employment-clue.csv')

block_data <- block_data %>%
  mutate(
    lat = as.numeric(sapply(strsplit(as.character(Geo.Point), ", "), "[", 1)),
    lng = as.numeric(sapply(strsplit(as.character(Geo.Point), ", "), "[", 2))
  )

block_data <- st_as_sf(block_data, coords = c("lng", "lat"), crs = 4326)


##################
# USER INTERFACE #
##################

births_tab <- tabPanel(
  title='Tableau map',
  sidebarLayout(
    sidebarPanel(
        "Hello world!",
    ),
    mainPanel(
      htmlOutput("tableau_viz")

    )
  )
)

hospitals_tab <- tabPanel(
  title='Shiny map',
  sidebarLayout(
    sidebarPanel(
      "Hello world!",
    ), 
    mainPanel(
      leafletOutput('map_block', height = 600)
    )
  )
)

ui <- navbarPage(
  id='mypage', # this is needed to be able to change the selected tab from code
  title='City of Melbourne',
  theme = shinythemes::shinytheme("cosmo"),
  births_tab, 
  hospitals_tab
)

################
# SHINY SERVER #
################

server <- function(input, output, session) {
  
  output$map_block <- renderLeaflet({
    leaflet(block_data) %>%
      addProviderTiles(providers$CartoDB) %>%
      addCircleMarkers(
        fillColor = 'skyblue',
        color = 'white',
        weight = 1,
        opacity = 0.2,
        fillOpacity = 0.6,
        
        label = ~clue_area,
        layerId = ~ clue_area,
        labelOptions = labelOptions(
          direction = 'top',
          textOnly = TRUE,
          noHide = FALSE,
          style = list(
            "font-weight" = "normal",
            "font-size" = "14px",
            "color" = "black",
            padding = "3px 8px"
          )
        )
      )
  })
  
  output$tableau_viz <- renderUI({
    tags$iframe(
      src = "https://public.tableau.com/views/test_16960091823980/Sheet1?:showVizHome=no&:embed=true",
      width = "100%",
      height = "500px"
    )
  })
  
 
}

#############
# Run Shiny #
#############

shinyApp(ui, server)
