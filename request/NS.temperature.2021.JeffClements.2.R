
library(RODBC)
library(dplyr)
library(stringr)
library(openxlsx)

# Establish connection to Access database
channel <- odbcConnectAccess2007("C:/Users/Gagnondj/Documents/Database/Temperature_be.mdb")

x <- sqlFetch(channel, "All Data")

# Close the RODBC connection
odbcClose(channel)

# Nova scotia
z <- x %>%
  rename(site = 'Site (text)', surface.bottom = 'Surface-Bottom') %>%
  filter(str_detect(site, "N\\.S\\.$") & (surface.bottom == "B")) #only keep site in Nova Scotia and only bottom temperatures
  
z <- unique(z[,c("Year", "site")])

# Northumberland
 
strait <- x %>%
rename(site = 'Site (text)', surface.bottom = 'Surface-Bottom') %>%
filter(!is.na(Latitude) & !is.na(Longitude) &  (surface.bottom == "B"))

# Define bounding boxes specifically for the Northumberland Strait
# Adjust these values based on actual geographic data
bounding_boxes <- list(
  box1 = list(min_lat = 45.9, max_lat = 46.2, max_long = 64.2, min_long = 63.8),
  box2 = list(min_lat = 46.0, max_lat = 46.3, max_long = 64.0, min_long = 63.6),
  box3 = list(min_lat = 46.1, max_lat = 46.4, max_long = 63.9, min_long = 63.5)
)

# Function to check if a row is within any of the bounding boxes
is_within_boxes <- function(lat, long, boxes) {
  if (is.na(lat) || is.na(long)) {
    return(FALSE)
  }
  for (box in boxes) {
    if (lat >= box$min_lat && lat <= box$max_lat &&
        long >= box$min_long && long <= box$max_long) {
      return(TRUE)
    }
  }
  return(FALSE)
}

# Filter the dataframe
df_subset <- strait %>%
  rowwise() %>%
  filter(is_within_boxes(Latitude, Longitude, bounding_boxes))

Strait.list <- unique(df_subset[,c("Year", "site")])





  
  # Define bounding boxes specifically for the Northumberland Strait
  # These values are hypothetical and should be adjusted based on actual geographic data
  bounding_boxes <- list(
    box1 = list(min_lat = 45.9, max_lat = 46.2, min_long = -64.2, max_long = -63.8),
    box2 = list(min_lat = 46.0, max_lat = 46.3, min_long = -64.0, max_long = -63.6),
    box3 = list(min_lat = 46.1, max_lat = 46.4, min_long = -63.9, max_long = -63.5)
  )

# Function to check if a row is within any of the bounding boxes
is_within_boxes <- function(lat, long, boxes) {
  for (box in boxes) {
    if (lat >= box$min_lat && lat <= box$max_lat &&
        long >= box$min_long && long <= box$max_long) {
      return(TRUE)
    }
  }
  return(FALSE)
}

# Filter the dataframe to include only sites within the Northumberland Strait bounding boxes
df_subset <- strait %>%
  rowwise() %>%
  filter(is_within_boxes(Latitude, Longitude, bounding_boxes))
