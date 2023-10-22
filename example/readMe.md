---
title: "User Guide"
output: html_document
---

<center><font color=skyblue size=72>User Guide</font> </center>


### 1. How to run

##### runapp 'a3.r'

### 2. How to use

#### 2.1 Interface Overview:
##### The interface is structured into five main pages, namely:
###### - **Home**
###### - **Traffic**
###### - **Crime**
###### - **Relation**
###### - **About**

#### You can navigate between these pages using the tabs in the top navigation bar.

<font color=white  size=5>2.2 Home Page</font>

###### The Home page serves as a gateway to an overview of Traffic and Crime data. It includes:
###### - **Population Maps**: Helps identify demographic patterns.
###### - **Line Chart**: Displays traffic volumes in different Melbourne suburbs during peak hours. The thickness of the line indicates the number of offenses in each suburb.
###### - **Table**: Compares traffic volume data and crime counts for Melbourne and Victoria. When a region is selected on the map, suburb-specific data appears.
###### - **Radar Chart**: Shows the prevalence of different crime types in a selected suburb.
###### - **Sankey Chart**: Presents crime data associated with different public transportation modes.
<font color=white size=5>2.3 Traffic Page</font>

##### Dive deep into Melbourne's traffic patterns with:
###### - **Interactive Map**: Color-coded streets show traffic volume. Hovering reveals tooltips, and clicking activates a dynamic side-panel.
###### - **Suburbs Traffic Overview**: An interactive bubble chart displays insights into traffic patterns.
###### - **Reset Button**: Clears selections and returns the page to its initial state.
<font color=white size=5>2.4 Crime Page</font>

##### Understand crime patterns and analysis with:
###### - **Map Tab**: Hovering over polygons reveals offense data. Clicking on a polygon displays a bar chart with annual offense counts.
###### - **Analysis Tab**: Utilize filters to select specific suburbs and offenses. Charts provide detailed analysis and comparisons.
<font color=white  size=5>2.5 Relation Page</font>

##### Explore the relationship between various factors and Traffic/Crime with:
###### - **Traffic Related**: Views factors potentially influencing traffic, such as population, number of bus stops, land use for transport, and off-street parking quantity.
###### - **Crime Related**: Displays factors potentially influencing crime, including job count and liquor store count.
###### - **Relation**: Shows the potential connection between various factors and traffic/crime. Users can customize the pairing in line charts.

<font color=white  size=5>2.6 About Page</font>

##### Provides guidance on how to use the interface and references for data sources.



### **Data Source** page: 
##### Traffic volume data
###### https://discover.data.vic.gov.au/dataset/traffic-volume
##### Crime data
###### https://www.crimestatistics.vic.gov.au/crime-statistics/latest-victorian-crime-data/download-data
##### Population data
###### https://discover.data.vic.gov.au/dataset/city-of-melbourne-population-forecasts-by-small-area-2021-2041
##### Liquor store data
###### https://discover.data.vic.gov.au/dataset/victorian-liquor-licences-by-location
##### Parking spots data
###### https://discover.data.vic.gov.au/dataset/off-street-car-parks-with-capacity-and-type
##### Transportation land use data
###### https://data.melbourne.vic.gov.au/explore/dataset/blocks-for-census-of-land-use-and-employment-clue/information/ 
##### Bus stop data
###### https://data.melbourne.vic.gov.au/api/explore/v2.1/catalog/datasets/bus-stops/exports/csv?lang=en&timezone=Australia%2FSydney&use_labels=true&delimiter=%2C 



