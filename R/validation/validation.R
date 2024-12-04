library(RODBC)
library(dplyr)
library(gulf.data)

# Establish connection to Access database
channel <- odbcConnectAccess2007("C:/Users/Gagnondj/Documents/Database/Temperature_be.mdb")

x <- sqlFetch(channel, "All Data")

# x <- x %>%
#   rename(site = 'Site (text)', recorder = Recorder, site.depth.m = 'Site Depth(m)', surface.bottom = 'Surface-Bottom', log.depth.m = 'Log Depth(m)',
#          latitude = Latitude, longitude = Longitude, year = Year, month = Month, day = Day, date = Date, time = Time, temperature = Temperature, 
#          unique.id = Unique_id, serial.number = Serial_number, buoy.id = Buoy_Id) %>%
#   filter(!is.na(temperature)) # Filtering out NA temperatures

# Close the RODBC connection
odbcClose(channel)

# Convert Time to character if it's not already
x$Time <- as.character(x$Time)

y <- x %>%
  filter(x$Year %in% 2021:2022) %>%
  mutate(Time = format(as.POSIXct(Time, format = "%H:%M:%S"), "%H:%M:%S")) #format time field

## ---- Corrections ----- 
# adjust site "Anse Bleue 3, N.B.", year 2013. Add .8 degrees to temperatures
x <- x %>%
  mutate(temperature = ifelse(site == "Anse Bleue 3, N.B." & year == 2013, temperature + 0.8, temperature))

# Save the data to an RDA file
save(x, file = "temperature.rda")