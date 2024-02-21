library(gulf.data)
library(RODBC)
library(dplyr)
library(sf)
library(stringr)
library(tidyr)
library(readr)

# Clear all objects from the environment
rm(list = ls())

# Load the data from the RDA file created in build.rda.R
load("temperature.rda") 

#x <- read_csv("C:/Users/gagnondj/Documents/Temperature/all_data.csv")

x <- x %>%
  rename(site = 'Site (text)', recorder = Recorder, site.depth.m = 'Site Depth(m)', surface.bottom = 'Surface-Bottom', log.depth.m = 'Log Depth(m)',
         latitude = Latitude, longitude = Longitude, year = Year, month = Month, day = Day, date = Date, time = Time, temperature = Temperature,
         unique.id = Unique_id, serial.number = Serial_number, buoy.id = Buoy_Id)


# Define the polygon coordinates, polygon was done quickly to get wanted sites, but should create a better polygon to be more accurate
polygon_coords <- rbind(
  c(-64.806761511438, 47.124142513389),
  c(-63.99703716228, 47.078300246927),
  c(-64.09179213931, 46.656209894706),
  c(-63.514648188314, 46.259961808948),
  c(-62.53264206274, 46.259961808948),
  c(-62.007182644669, 46.458085851827),
  c(-61.082703318483, 46.466699940648),
  c(-61.547864114807, 45.398552926866),
  c(-64.789533333796, 46.035995499607),
  c(-65.116868708988, 46.923246648152),
  c(-64.806761511438, 47.124142513389)  # Closing the polygon by repeating the first point
)

# Create the polygon using the previous coordinates
polygon <- st_polygon(list(polygon_coords))
polygon_sf <- st_sfc(polygon, crs = 4326)

#check if the polygon geometry is valid
if (!st_is_valid(polygon_sf)) {
  warning("The polygon geometry is not valid.")
  # Optional: Try to fix the polygon
  polygon_sf <- st_make_valid(polygon_sf)
}

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


# data frame x is converted into a spatial data frame x_sf
x_sf <- st_as_sf(x, coords = c("longitude", "latitude"), crs = 4326)

# Find points inside the polygon
inside_points <- x_sf[st_within(x_sf, polygon_sf, sparse = FALSE), ]

# Extract the coordinates from geometry
coords <- st_coordinates(inside_points)

# Add the latitude and longitude as new columns
inside_points$latitude <- coords[, 'Y']
inside_points$longitude <- coords[, 'X']

inside_points <- inside_points %>%
  mutate(
    # Check if the time column contains only the date part and replace with "00:00:00" if so
    time = ifelse(time == "1899-12-30", "00:00:00", 
                  # Otherwise, remove the "1899-12-30 " part
                  sub("1899-12-30 ", "", time))
  ) %>%
  select(site, site.depth.m, surface.bottom, log.depth.m, latitude, longitude, year, month, day, date, time, temperature) %>%
  st_drop_geometry()

# Split the dataframe based on the first letter of site names
inside_points_first_half <- inside_points %>%
  filter(str_to_upper(substr(site, 1, 1)) %in% LETTERS[1:13])

inside_points_second_half <- inside_points %>%
  filter(str_to_upper(substr(site, 1, 1)) %in% LETTERS[14:26])

# # Split the dataframe based on sites
# inside_points_first_half <- inside_points %>% filter(site %in% first_half_sites)
# inside_points_second_half <- inside_points %>% filter(site %in% second_half_sites)

# Write to CSV files
write.csv(inside_points_first_half, "NS.temperature.JClements.1.csv", row.names = FALSE)
write.csv(inside_points_second_half, "NS.temperature.JClements.2.csv", row.names = FALSE)

# -------- checking what is in the polygon

unique_combinations <- unique(inside_points[,c("site", "year")])
excel(unique_combinations)

#-----------------------------------------

# Find points outside the polygon to make sure good site are all in polygon
outside_points <- x_sf[!st_within(x_sf, polygon_sf, sparse = FALSE), ]

# Get unique combinations of 'site' and 'year' for outside points
unique_combinations_outside <- unique(outside_points[,c("site", "year")])

excel(unique_combinations_outside)



#---------------------------------------------
#plot points to see if in or outside of polygon

library(ggplot2)
# Create the polygon
polygon <- st_polygon(list(polygon_coords))
polygon_sf <- st_sfc(polygon, crs = 4326)

long <- -64.785
lat <- 47.11917

# Create a data frame for the position with the given coordinates
position_df <- data.frame(longitude = long, latitude = lat)


# Convert the data frame to an sf object
position_sf <- st_as_sf(position_df, coords = c("longitude", "latitude"), crs = 4326)

# Plot using ggplot2
ggplot() +
  geom_sf(data = polygon_sf, fill = "blue", color = "blue", size = 0.5, alpha = 0.5) +
  geom_sf(data = position_sf, color = "red", size = 3) +
  theme_minimal()
