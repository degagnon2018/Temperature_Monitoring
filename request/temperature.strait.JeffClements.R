library(gulf.data)
#library(RODBC)
library(dplyr)
#library(sf)
#library(stringr)
#library(tidyr)
library(readr)

# Clear all objects from the environment
rm(list = ls())

# Load the data from the RDA file created in build.rda.R
load("temperature.rda") 

#x <- read_csv("C:/Users/gagnondj/Documents/Temperature/all_data.csv")

x <- x %>%
  select(site, recorder, site.depth.m, surface.bottom, log.depth.m,
         latitude, longitude, year, month, day, date, time, temperature,
         unique.id, serial.number, buoy.id)

# convert longitude to negative 
x$longitude <- x$longitude*-1

# filter out NA in positions
x <- x %>%
  filter(!is.na(latitude) & !is.na(longitude))

# take out dates in the time columns
x <- x %>%
  mutate(time = sub(".*\\s", "", time))

# take out "0:00:00" from date
x <- x %>%
  mutate(date = sub("\\s0:00:00$", "", date))

# Assuming your dataframe is named `x`
daily_avg_temp <- x %>%
  group_by(site, site.depth.m, surface.bottom, log.depth.m, latitude, longitude, year, month, day) %>%
  summarize(average_temperature = round(mean(temperature, na.rm = TRUE), 2), .groups = "drop") 

# View the resulting dataframe
print(daily_avg_temp)

# Write to CSV files
write.csv(inside_points_first_half, "", row.names = FALSE)


