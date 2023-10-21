library(sf)
library(leaflet)
library(dplyr)
library(maps)
library(htmltools)
library(htmlwidgets)
library(echarts4r)
library(shiny)
library(shinythemes)
library(RColorBrewer)
library(plotly)
library(ggiraph)
library(shinyWidgets)
library(viridis)
library(hrbrthemes)



#------------------------------  Setup   ----------------------------

#load data
melb_clue <- st_read("../Data/Mel_LGA_Suburbs_GDA94/mel_suburbs_edit.geojson") %>% st_transform(crs = 4326)
crime_data <- read.csv('./crime_clue.csv',header = T)

#pre-processing
melb_clue$center <- sf::st_centroid(melb_clue$geometry)
colnames(melb_clue)[colnames(melb_clue) == "clue_area"] <- "Clue"
colnames(crime_data)[colnames(crime_data) == "Suburb.Town.Name"] <- "Clue"
colnames(crime_data)[colnames(crime_data) == "Offence.Division"] <- "Offence.Type"


crime_data$Offence.Count <- as.numeric(gsub(",", "", crime_data$Offence.Count))
crime_data$Clue <- sub("Flemington", "Kensington", crime_data$Clue)
crime_data$Clue <- sub("Carlton North", "Carlton", crime_data$Clue)
crime_data$Clue <- sub("South Wharf", "Southbank", crime_data$Clue)

crimeClueType <- crime_data %>% group_by(Clue,Offence.Type) %>% summarise(Total_Offences = sum(Offence.Count, na.rm = TRUE))

crimeYearType <- crime_data %>% group_by(Year,Offence.Type) %>% summarise(Total_Offences = sum(Offence.Count, na.rm = TRUE))
crimeType<- crime_data %>% group_by(Offence.Type) %>% summarise(Total_Offences = sum(Offence.Count, na.rm = TRUE))

crimeByClue <- crime_data %>% group_by(Clue) %>%
  summarise(Total_Offences = sum(Offence.Count, na.rm = TRUE),
            content = htmltools::HTML(
              htmltools:::as.character.shiny.tag.list(
                htmlwidgets:::as.tags.htmlwidget(
                  data.frame(x=as.character(Year),
                             y=Offence.Count) %>%
                    group_by(x) %>%
                    summarise(total = sum(y, na.rm = TRUE)) %>%
                    e_charts(x,width=450,height = 300) %>%
                    e_bar(total,legend=FALSE,color = 'green') %>%
                    e_tooltip(
                      trigger = "axis",
                      formatter = htmlwidgets::JS(
                        "function(params) {
                       var year = params[0].name;
                      var value = params[0].value[1];
                      return 'Year: ' + year + '<br/>Offence Count: ' + value;}"           )
                      
                    )%>%
                    e_x_axis(axisLabel = list(interval = 0, rotate = 60,
                                              fontSize=8,
                                              lineHeight=40),
                             name="Year") %>%
                    e_y_axis(name="Offence Count") %>%
                    e_title("Offence Count Per Year")
                  
                ))))





# crime_grouped <- crime_data %>% group_by(Suburb.Town.Name,Offence.Division,Year) %>% summarise(Total_Offence_Count = sum(Offence.Count, na.rm = TRUE))

data <- merge(crimeByClue, melb_clue, by.x = "Clue", by.y = "Clue", all = FALSE)

#------------------------------  Map  ---------------------------
data$Color <- factor(ifelse(data$Total_Offences<5000,"<5000",
                            ifelse(data$Total_Offences<20000,"5000~20000",
                                   ifelse(data$Total_Offences<50000,"20000~50000",">50000"))),
                     levels=c("<5000","5000~20000","20000~50000",">50000"))
cpol <- colorFactor("Greens",na.color = 'grey',ordered=TRUE,domain=data$Color)

html <- leaflet(data) %>%
  addTiles()%>%
  addProviderTiles(providers$CartoDB.DarkMatter)%>%
  addPolygons(data=melb_clue,
              group = data$Clue,
              color = 'gray',
              opacity = 0.5,
              dashArray = 5,
              #fillColor =~color_palette(Total_Offence_Count),
              fillColor = cpol(data$Color),
              fillOpacity = 0.6,
              weight = 4,
              stroke = T,
              popup = data$content,
              popupOptions = popupOptions(minWidth = 450, maxHeight = 300),
              label = ~lapply(paste("<b>CLUE Area: </b>", data$Clue,"<br>",
                                    "<b>Total Offences:</b>", data$Total_Offences),
                              HTML),
              
              highlightOptions = highlightOptions(
                weight = 5,
                color = "white",
                dashArray = "",
                fillOpacity = 1,
                bringToFront = TRUE)
  )%>%
  onRender("function(el,x) {
  this.on('popupopen', function() {HTMLWidgets.staticRender();})
  }") %>%
  addLegend(
    position = "bottomright",
    pal = cpol,
    values = ~Color,
    labels= ~Clue,
    title = "Total Offences"
  ) %>%
  # addLayersControl(
  #   overlayGroups = ~Clue,
  #   options = layersControlOptions(collapsed = FALSE)  
  # ) %>%
  tagList(
    htmlwidgets::getDependency("echarts4r", "echarts4r")
  )



##################
# USER INTERFACE #
##################

#------------------------------  Tab  ---------------------------
map_tab <- tabPanel(
  title='Map',
  h2('Melbourne Crime Map'),
  h5("Total Offence count in City of Melbourne from 2014-2023"),
  uiOutput('plot_map')
)

stat_tab <- tabPanel(
  'Stat',
  h2('Melbourne Crime Stat'),
  sidebarLayout(
    sidebarPanel(
      span(h6("This is the total offences in the City of Melbourne"),style="color:white"),
      br(),
      span(h6("Users could see the specific offences by crime types."),style="color:white"),
      br(),
      
      
      # sliderInput("yearInput", "Select Year", 
      #        min = min(crime_data$Year), 
      #        max = max(crime_data$Year), 
      #        value = min(crime_data$Year), 
      #        step = 1
      #        ),
      
      selectInput(
        "clueInput", "Select CLUE Area:", 
        choices = unique(crime_data$Clue),
        selected = "Carlton"
      ),
      pickerInput(
        "crimeType", "Crime Type Selection:",
        choices = unique(crime_data$Offence.Type),
        selected = unique(crime_data$Offence.Type),
        multiple = TRUE
      )
      
    ),
    
    mainPanel(
      plotlyOutput("plot1"),
      plotlyOutput("plot2")
    )
  )
  
)


#------------------------------  UI  ---------------------------

ui <- navbarPage(
  title='Home',
  theme = shinythemes::shinytheme("cyborg"),
  navbarMenu( #dropdown mean
    "Crime",
    map_tab,
    stat_tab
    
  )
  
)

################
# SHINY SERVER #
################

server <- function(input, output, session) {
  
  #------------------------------  Map  ---------------------------
  
  # Crime Map
  output$plot_map <- renderUI({
    html[[1]]$sizingPolicy$defaultHeight <- 600  
    html %>%
      browsable()
  })
  
  observeEvent(input$map_shape_click, {
    click_event <- input$map_shape_click
    selected_clue <- click_event$id
    data_filtered <- data[data$Clue == selected_clue, ]
    
    # bar for selected CLUE
    output$barplot <- renderPlot({
      ggplot(data_filtered, aes(x = Clue, y = Total_Offences)) +
        geom_bar(stat = "identity") +
        ggtitle(paste("Bar Plot for CLUE area: ", selected_clue))
    })
  })
  
  
  #------------------------------  Filter  ---------------------------
  clue_filter <- reactive({
    filtered_data <- crime_data %>%
      filter(Clue %in% input$clueInput, Offence.Type %in% input$crimeType) %>%
      group_by(Year, Offence.Type) %>%
      summarise(Total_Offences = sum(Offence.Count, na.rm = TRUE))
    return(filtered_data)
  })
  
  bar_filter <- reactive({
    filtered_data <- crime_data %>%
      filter(Clue %in% input$clueInput, Offence.Type %in% input$crimeType) %>%
      group_by(Clue, Offence.Type) %>%
      summarise(Total_Offences = round(mean(Offence.Count, na.rm = TRUE)))
    return(filtered_data)
  })
  
  line_filter <- reactive({
    filtered_data <- crime_data %>%
      filter(Offence.Type %in% input$crimeType) %>%
      group_by(Offence.Type) %>%
      summarise(Total_Offences = round(mean(Offence.Count, na.rm = TRUE)))
    return(filtered_data)
  })
  
  #------------------------------  Plot1  ---------------------------
  #plot1
  output$plot1 <- renderPlotly({
    filtered_data <- clue_filter()
    ggplotly(
      ggplot(filtered_data, aes(x=Year, y=Total_Offences, fill=Offence.Type)) +
        #geom_point_interactive(aes(tooltip = Total_Offences, data_id = continent), size = 2)+
        geom_area(alpha=0.6 , linewidth=.5, colour="white") +
        scale_fill_viridis(discrete = T) +
        theme_ipsum() +
        ggtitle("Crime Types in seleced clue area")+
        theme_minimal() +
        theme(
          panel.background = element_rect(fill = "black"), 
          plot.background = element_rect(fill = "black"),  
          axis.text = element_text(color = "white"),  
          axis.title = element_text(color = "white"), 
          legend.text = element_text(color = "white"), 
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          plot.title = element_text(color = "white")
        )+
        ggtitle("Total Offence Count in selected CLUE area"))
    
    
    
  })
  
  
  
  
  #------------------------------  Plot2  ---------------------------
  
  output$plot2 <- renderPlotly({
    bar_data <- bar_filter()
    line_data <- line_filter()
    
    p <- ggplot() +
      geom_bar(data = bar_data, aes(x = Offence.Type, y = Total_Offences), fill = "lightgreen", alpha = 0.5, linewidth = 0.5, color = "white", stat = "identity", position = "dodge", width = 0.7) +
      geom_line(data = line_data, aes(x = Offence.Type, y = Total_Offences, color = "Mean Offences in City of Melbourne", group = 1, alpha = 0.5) ) +
      geom_point(data = line_data, aes(x = Offence.Type, y = Total_Offences), color = "white", shape = 19, size = 3) + 
      #geom_text(data = bar_data, aes(x = Offence.Type, y = Total_Offences, label = Total_Offences), color = "white", size = 4, position = position_dodge(width = 0.7), vjust = 5) + 
      #geom_text(data = line_data, aes(x = Offence.Type, y = Total_Offences, label = Total_Offences), color = "white", size = 4, vjust = -1, textposition = "top center") + 
      scale_color_manual(values = c("white"), name = "Mean Offences in City of Melbourne") +
      theme_minimal() +
      theme(
        panel.background = element_rect(fill = "black"),
        plot.background = element_rect(fill = "black"),
        axis.text = element_text(color = "white"),
        axis.title = element_text(color = "white"),
        legend.text = element_text(color = "white"),
        legend.background = element_rect(fill = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title = element_text(color = "white")
      ) +
      ggtitle("Mean Offence Count in selected Clue VS City of Melbourne")
    
    ggplotly(p)
  })
  
}

#############
# RUN SHINY #
#############

shinyApp(ui, server)

