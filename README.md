# Tableau in Shiny App

Welcome to our application! This app combines the power of Tableau with the flexibility of Shiny to provide an interactive data analysis experience.  
You can find our app at [GitHub Project Link](https://github.com/jackie174/IV_Project.git).

<div style="display: grid; grid-template-columns: 1fr 1fr; gap: 10px;"> <img src="https://github.com/user-attachments/assets/116ec23a-e8af-461e-8d3a-c371bf8dd391" width="400px"> <img src="https://github.com/user-attachments/assets/5e801751-756a-43ff-92e0-4dba50f0d3ac" width="400px"> <img src="https://github.com/user-attachments/assets/2bc9c6ef-d909-4155-b6e0-0b5f16363714" width="400px"> <img src="https://github.com/user-attachments/assets/1b83bd77-f02d-49ff-96f6-107e36dd2429" width="400px"> </div>


## Features

- **Homepage**: Built with Tableau, our homepage offers a visual overview of the data and insights.
- **Interactive Interface**: With Shiny, you can seamlessly interact with our Tableau visualizations.

## User Guide

### Before Running:

Ensure all required packages are installed. If not, install them with the following commands:

```r
required_packages <- c(
  "RColorBrewer", "dplyr", "echarts4r", "ggiraph", "ggplot2", "hrbrthemes", 
  "htmltools", "htmlwidgets", "jsonlite", "leaflet", "lubridate", "mapboxapi", 
  "maps", "plotly", "readxl", "scales", "sf", "shiny", "shinyWidgets", "shinyjs", 
  "shinythemes", "tidyr", "tidyverse", "viridis"
)

# Check and install missing packages
new_packages <- required_packages[!(required_packages %in% installed.packages()[,"Package"])]
if(length(new_packages) > 0) {
  install.packages(new_packages, dependencies = TRUE)
}

# Load all required packages
lapply(required_packages, library, character.only = TRUE)
```

### How to Run

1. Navigate to the example folder `./IV_Project-final_testing/example`.
2. Run the `a3.R` script to launch the Shiny app (`./IV_Project-final_testing/example/a3.R`).

> Note: You can find the Tableau file in the folder `./IV_Project-final_testing/Jingyi Xiao/Tableau`.

### How to Use

#### 2.1 Interface Overview:

The interface is structured into five main pages:

- Home
- Traffic
- Crime
- Relation
- About

You can navigate between these pages using the tabs in the top navigation bar.

#### 2.2 Home Page

The Home page provides an overview of traffic and crime data, which includes:

- **Population Maps**: Helps identify demographic patterns.
- **Line Chart**: Displays traffic volumes in different Melbourne suburbs during peak hours. The thickness of the line indicates the number of offences in each suburb.
- **Table**: Compares traffic volume data and crime counts for Melbourne and Victoria. When a region is selected on the map, data for that specific suburb appears.
- **Radar Chart**: Shows the prevalence of different crime types in a selected suburb.
- **Sankey Chart**: Presents crime data associated with different public transportation modes.

#### 2.3 Traffic Page

Dive deep into Melbourne's traffic patterns with:

- **Interactive Map**: Color-coded streets show traffic volume. Hovering reveals tooltips, and clicking activates a dynamic side panel.
- **Suburbs Traffic Overview**: An interactive bubble chart that displays insights into traffic patterns.
- **Reset Button**: Clears selections and returns the page to its initial state.

#### 2.4 Crime Page

Understand crime patterns and analysis with:

- **Map Tab**: Hovering over polygons reveals offence data. Clicking on a polygon displays a bar chart with annual offence counts.
- **Analysis Tab**: Utilize filters to select specific suburbs and offences. Charts provide detailed analysis and comparisons.

#### 2.5 Relation Page

Explore the relationship between various factors and traffic/crime, including:

- **Traffic Factor Analysis**: View factors potentially influencing traffic, such as population, number of bus stops, transport land use, and off-street parking.
- **Crime Factor Analysis**: Displays factors potentially influencing crime, such as job count and liquor store count.
- **Correlation**: Shows the potential connection between various factors and traffic/crime. Users can customize the pairing in line charts.

#### 2.6 About Page

Provides guidance on how to use the interface and references for data sources.

### Data Source Page

You can find data in the Data folder (`./IV_Project-final_testing/Data`).

- Traffic volume data: [Victoria Traffic Volume Data](https://discover.data.vic.gov.au/dataset/traffic-volume)
- Crime data: [Victoria Latest Crime Statistics](https://www.crimestatistics.vic.gov.au/crime-statistics/latest-victorian-crime-data/download-data)
- Population data: [Melbourne Population Forecast](https://discover.data.vic.gov.au/dataset/city-of-melbourne-population-forecasts-by-small-area-2021-2041)
- Liquor store data: [Victorian Liquor Licenses](https://discover.data.vic.gov.au/dataset/victorian-liquor-licences-by-location)
- Parking spots data: [Off-Street Car Parks Data](https://discover.data.vic.gov.au/dataset/off-street-car-parks-with-capacity-and-type)
- Transportation land use data: [Melbourne Transportation Land Use Data](https://data.melbourne.vic.gov.au/explore/dataset/blocks-for-census-of-land-use-and-employment-clue/information/)
- Bus stop data: [Melbourne Bus Stops Data](https://data.melbourne.vic.gov.au/api/explore/v2.1/catalog/datasets/bus-stops/exports/csv?lang=en&timezone=Australia%2FSydney&use_labels=true&delimiter=%2C)

> **Note**: Crime data is complete from 2014 to 2023, but traffic data is incomplete, with only data from 2020. Therefore, comparisons and correlation analysis between crime and traffic data will only use data from 2020.

---

This README provides a comprehensive overview of your project, including installation steps, how to run the app, and data source references.
