library(sf)
library(leaflet)
library(dplyr)
library(maps)
library(htmltools)
library(htmlwidgets)
library(echarts4r)
library(shiny)

#load data
# melb_clue <- st_read("./mel_suburbs_edit.geojson") %>% st_transform(crs = 4326)
# crime_data <- read.csv('./crime_clue.csv',header = T)
#load data
melb_clue <- st_read("../Data/Mel_LGA_Suburbs_GDA94/mel_suburbs_edit.geojson") %>% st_transform(crs = 4326)
crime_data <- read.csv('./crime_clue.csv',header = T)

#pre-processing
melb_clue$center <- sf::st_centroid(melb_clue$geometry)

crime_data$Offence.Count <- as.numeric(gsub(",", "", crime_data$Offence.Count))
crime_data$Suburb.Town.Name <- sub("Flemington", "Kensington", crime_data$Suburb.Town.Name)
crime_data$Suburb.Town.Name <- sub("Carlton North", "Carlton", crime_data$Suburb.Town.Name)
crime_data$Suburb.Town.Name <- sub("South Wharf", "Southbank", crime_data$Suburb.Town.Name)

# crimeByYear <- crime_data %>% group_by(Year) %>% summarise(Total_Offence_Count = sum(Offence.Count, na.rm = TRUE))
crimeByClue <- crime_data %>% group_by(Suburb.Town.Name) %>% 
  summarise(Total_Offence_Count = sum(Offence.Count, na.rm = TRUE),
            content = htmltools::HTML(
              htmltools:::as.character.shiny.tag.list(
                htmlwidgets:::as.tags.htmlwidget(
                 data.frame(x=names(table(Offence.Division)),
                                       y=as.numeric(table(Offence.Division))) %>%
                              e_charts(x,width=350,height = 300) %>%
                              e_bar(y,legend=FALSE) %>%
                              e_tooltip()%>%
                              e_x_axis(axisLabel = list(interval = 0, rotate = 60,
                                                        fontSize=8,
                                                        lineHeight=40))
                                       ))))
# crime_grouped <- crime_data %>% group_by(Suburb.Town.Name,Offence.Division,Year) %>% summarise(Total_Offence_Count = sum(Offence.Count, na.rm = TRUE))

data <- merge(crimeByClue, melb_clue, by.x = "Suburb.Town.Name", by.y = "clue_area", all = FALSE)
# data <- merge(crime_grouped, melb_clue, by.x = "Suburb.Town.Name", by.y = "clue_area", all = FALSE)
data <- data %>% 
  rename(
    clue_area = Suburb.Town.Name,
  )

# color_palette <- colorNumeric(
#   palette = "YlOrRd",  # 选择颜色梯度
#   domain = data$Total_Offence_Count  # 根据犯罪数量范围确定颜色
# )

# total_offences <- crimeByClue$Total_Offence_Count[match(melb_clue,crimeByClue$Suburb.Town.Name)]
cpol <- colorNumeric("Greens",na.color = NA,data$Total_Offence_Count)






map <- leaflet(data) %>%
  addTiles()%>%
  addProviderTiles(providers$CartoDB.DarkMatter)%>%
  #setView(lng = as.numeric(st_coordinates(center_point)[, 'X']), 
  #        lat = as.numeric(st_coordinates(center_point)[, 'Y']), zoom = 14)%>%
  addPolygons(data=melb_clue,
              group = data$clue_area,
              color = 'gray',
              opacity = 1,
              dashArray = 6,
              #fillColor =~color_palette(Total_Offence_Count),
              fillColor = cpol(data$Total_Offence_Count),
              fillOpacity = 0.7,
              weight = 4,
              stroke = T,
              popup = data$content,
              popupOptions = popupOptions(minWidth = 350, maxHeight = 300),
              label = ~lapply(paste("<b>Clue: </b>", data$clue_area,"<br>",
                            "<b>Total Accidents:</b>", data$Total_Offence_Count),
                            HTML),
              
              highlightOptions = highlightOptions(
                weight = 5,
                color = "red",
                dashArray = "",
                fillOpacity = 0.7,
                bringToFront = TRUE)
  )%>%
  onRender("function(el,x) {
  this.on('popupopen', function() {HTMLWidgets.staticRender();})
  }") %>%
  addLegend(
    position = "bottomright",
    pal = cpol,
    values = ~Total_Offence_Count,
    labels= ~clue_area,
    title = "Total Accidents"
  ) %>%
  addLayersControl(
    overlayGroups = ~clue_area,
    options = layersControlOptions(collapsed = FALSE)  # 控制面板默认展开
  ) %>%
  tagList(
    htmlwidgets::getDependency("echarts4r", "echarts4r")
  )

#map[[1]]$sizingPolicy$defaultHeight <- 700  #调整显示高度
map %>%
  browsable()


 