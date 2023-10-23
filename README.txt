Tableau in Shiny App

Welcome to our application! This app combines the power of Tableau with the flexibility of Shiny to provide an interactive experience.
You can find Our App in https://github.com/jackie174/IV_Project.git
Features:

Homepage: Built with Tableau, our homepage offers a visual overview of our data and insights.
Interactive Interface: With Shiny, you can seamlessly interact with our Tableau visualizations.

User Guide

How to run
Step 1: Navigate to the example folder. (./IV_Project/example)
Step 2: Execute the a3.R script to launch the Shiny app. (./IV_Project/example/a3.R)

How to use
2.1 Interface Overview:
The interface is structured into five main pages, namely:

Home
Traffic
Crime
Relation
About
You can navigate between these pages using the tabs in the top navigation bar.
2.2 Home Page
The Home page is a gateway to an overview of Traffic and Crime data. It includes:

Population Maps: Helps identify demographic patterns.
Line Chart: Displays traffic volumes in different Melbourne suburbs during peak hours. The thickness of the line indicates the number of offences in each suburb.
Table: Compares traffic volume data and crime counts for Melbourne and Victoria. When a region is selected on the map, suburb-specific data appears.
Radar Chart: Shows the prevalence of different crime types in a selected suburb.
Sankey Chart: Presents crime data associated with different public transportation modes.
2.3 Traffic Page
Dive deep into Melbourne's traffic patterns with:

Interactive Map: Color-coded streets show traffic volume. Hovering reveals tooltips, and clicking activates a dynamic side panel.
Suburbs Traffic Overview: An interactive bubble chart displays insights into traffic patterns.
Reset Button: Clears selections and returns the page to its initial state.
2.4 Crime Page
Understand crime patterns and analysis with:

Map Tab: Hovering over polygons reveals offence data. Clicking on a polygon displays a bar chart with annual offence counts.
Analysis Tab: Utilize filters to select specific suburbs and offences. Charts provide detailed analysis and comparisons.
2.5 Relation Page
Explore the relationship between various factors and Traffic/Crime with:

Traffic Factor Analysis: Views factors potentially influencing traffic, such as population, number of bus stops, land use for transport, and off-street parking quantity.
Crime Factor Analysis: Displays factors potentially influencing crime, including job count and liquor store count.
Correlation: This shows the potential connection between various factors and traffic/crime. Users can customize the pairing in line charts.
2.6 About Page
Provides guidance on how to use the interface and references for data sources.

Data Source page:
You can find data in the Data folder. (./IV_Project/Data)
Traffic volume data: https://discover.data.vic.gov.au/dataset/traffic-volume
Crime data: https://www.crimestatistics.vic.gov.au/crime-statistics/latest-victorian-crime-data/download-data
Population data: https://discover.data.vic.gov.au/dataset/city-of-melbourne-population-forecasts-by-small-area-2021-2041
Liquor store data: https://discover.data.vic.gov.au/dataset/victorian-liquor-licences-by-location
Parking spots data: https://discover.data.vic.gov.au/dataset/off-street-car-parks-with-capacity-and-type
Transportation land use data: https://data.melbourne.vic.gov.au/explore/dataset/blocks-for-census-of-land-use-and-employment-clue/information/
Bus stop data: https://data.melbourne.vic.gov.au/api/explore/v2.1/catalog/datasets/bus-stops/exports/csv?lang=en&timezone=Australia%2FSydney&use_labels=true&delimiter=%2C

Note: The crime data from 2014 to 2023 is complete, but traffic data is incomplete and we only have data from 2020. Therefore, the comparison and correlation analysis between crime and traffic data will only use the data from 2020.

