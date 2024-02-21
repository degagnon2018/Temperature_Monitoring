library(gulf.data)
library(dplyr)
library(ggplot2)
library(maps)
# Install leaflet package if you haven't already
install.packages("leaflet")

# Load leaflet
library(leaflet)

# Clear all objects from the environment
rm(list = ls())

# Load the data from the RDA file created in build.rda.R
load("temperature.rda") 


# Select distinct rows based on specified columns
distinct_data <- x %>%
  distinct(site, `site.depth.m`, `surface.bottom`, `log.depth.m`, latitude, longitude, year, .keep_all = FALSE)
distinct_data$longitude <- distinct_data$longitude*-1

# View the aggregated data
excel(distinct_data)


leaflet(data = distinct_data) %>%
  addTiles() %>%
  addCircleMarkers(~longitude, ~latitude, radius = 3,  # Smaller radius for smaller points
                   popup = ~as.character(site), color = "red", fillOpacity = 0.8)

