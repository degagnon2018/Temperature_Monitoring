library(gulf.data)
library(dplyr)
library(ggplot2)
library(maps)
library(leaflet)

# Clear all objects from the environment
rm(list = ls())

# Load the data from the RDA file created in build.rda.R
load("temperature.rda") 

# position for sites for each year
sites_year_ll <- x %>%
  group_by(site, year) %>%
  summarise(
    latitude  = median(latitude,  na.rm = TRUE),
    longitude = median(longitude, na.rm = TRUE),
    n = n(),                       # how many records contributed
    .groups = "drop"
  )

# most common position for each site
mode_coord <- function(x) {
  tab <- sort(table(x), decreasing = TRUE)
  as.numeric(names(tab)[1])
}

sites_year_most_common <- x %>%
  group_by(site) %>%
  summarise(
    latitude  = mode_coord(latitude),
    longitude = mode_coord(longitude),
    n = n(),
    .groups = "drop"
  )
