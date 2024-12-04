library(marmap)

# Define your coordinates
lon <- -63.5734
lat <- 45.9355

# Get bathymetric data (around the coordinates with a buffer)
bathy_data <- getNOAA.bathy(lon1 = lon - 1, lon2 = lon + 1, lat1 = lat - 1, lat2 = lat + 1, resolution = 1)

depth <- get.depth(bathy_data, x = lon, y = lat, locator = FALSE)
print(depth)
