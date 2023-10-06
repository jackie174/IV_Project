library(sf)

# set current working directory to where this app.R file stores
current_directory <- dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(current_directory)

# read in victoria shape file
vic_sf <- st_read("Data/VIC_localities_GDA94/vic_localities.shp")
# define a vector of the suburb names just for City of Melbourne
sub_lga_mel <- c('Carlton', 'Carlton North', 'Docklands', 'East Melbourne',
                 'Flemington', 'Kensington', 'Melbourne', 'North Melbourne',
                 'Parkville', 'Port Melbourne', 'Southbank', 'South Wharf',
                 'South Yarra', 'West Melbourne')

# filter to lga melbourne only
lga_mel_sf <- subset(vic_sf, LOC_NAME %in% sub_lga_mel)

# remove extra columns
lga_mel_sf <- lga_mel_sf[, c(-1, -2, -3, -5, -6)]

plot(lga_mel_sf)

# define the output file name and location
output_shapefile <- "Data/Mel_LGA_Suburbs_GDA94/melbourne_suburbs.shp"

st_write(lga_mel_sf, output_shapefile)

cat("Shapefile saved to:", output_shapefile, "\n")
