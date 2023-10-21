library(sf)
library(leaflet)
library(dplyr)
library(maps)
library(htmltools)
library(htmlwidgets)
library(echarts4r)
library(shiny)
library(ggplot2)
library(viridis)
library(hrbrthemes)



#load data
melb_clue <- st_read("../Data/Mel_LGA_Suburbs_GDA94/mel_suburbs_edit.geojson") %>% st_transform(crs = 4326)
crime_data <- read.csv('./crime_clue.csv',header = T)

#pre-processing
melb_clue$center <- sf::st_centroid(melb_clue$geometry)
colnames(crime_data)[colnames(crime_data) == "Suburb.Town.Name"] <- "Clue"
colnames(crime_data)[colnames(crime_data) == "Offence.Division"] <- "Offence.Type"
#colnames(crime_data)[colnames(crime_data) == "Offence.Division"] <- "Offence.Count"

crime_data$Offence.Count <- as.numeric(gsub(",", "", crime_data$Offence.Count))
crime_data$Clue <- sub("Flemington", "Kensington", crime_data$Clue)
crime_data$Clue <- sub("Carlton North", "Carlton", crime_data$Clue)
crime_data$Clue <- sub("South Wharf", "Southbank", crime_data$Clue)
#group by
crimeByClue <- crime_data %>% group_by(Clue,Year) %>% 
  summarise(Total_Offences = sum(Offence.Count, na.rm = TRUE))
crimeMean <- crimeByClue %>% group_by(Clue) %>% 
  summarise(Total_Offences = round(mean(Total_Offences, na.rm = TRUE)))
crimeClueType <- crime_data %>% group_by(Clue,Offence.Type) %>% summarise(Total_Offences = sum(Offence.Count, na.rm = TRUE))
crimeYearType <- crime_data %>% group_by(Year,Offence.Type) %>% summarise(Total_Offences = sum(Offence.Count, na.rm = TRUE))

#plot1 https://r-graph-gallery.com/136-stacked-area-chart.html
ggplot(crimeByClue, aes(x=Year, y=Total_Offences, fill=Clue)) + 
  geom_area()


#plot1-2
ggplot(crimeByClue, aes(x=Year, y=Total_Offences, fill=Clue)) + 
  geom_area(alpha=0.6 , linewidth=.5, colour="white") +
  scale_fill_viridis(discrete = T) +
  theme_ipsum() + 
  ggtitle("Total offences in Clues")
#plot1-3
ggplot(crimeYearType, aes(x=Year, y=Total_Offences, fill=Offence.Type)) + 
  geom_area(alpha=0.6 , linewidth=.5, colour="white") +
  scale_fill_viridis(discrete = T) +
  theme_ipsum() + 
  ggtitle("Total offences in Clues")

#bar plot2 https://r4ds.had.co.nz/data-visualisation.html
ggplot(data = crime_data, mapping = aes(x = Clue, fill = Offence.Type)) + 
  geom_bar(alpha = 1/5, position = "identity") + 
  ggtitle("Offence Types in Clues") +
  theme_classic()

#input: Clue+Year

