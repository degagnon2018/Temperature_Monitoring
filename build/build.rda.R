library(RODBC)
library(dplyr)

# Establish connection to Access database
channel <- odbcConnectAccess2007("C:/Users/Gagnondj/Documents/Database/Temperature_be.mdb")

x <- sqlFetch(channel, "All Data")

x <- x %>%
  rename(site = 'Site (text)', recorder = Recorder, site.depth.m = 'Site Depth(m)', surface.bottom = 'Surface-Bottom', log.depth.m = 'Log Depth(m)',
         latitude = Latitude, longitude = Longitude, year = Year, month = Month, day = Day, date = Date, time = Time, temperature = Temperature, 
         unique.id = Unique_id, serial.number = Serial_number, buoy.id = Buoy_Id)

# Close the RODBC connection
odbcClose(channel)

# Save the data to an RDA file
save(x, file = "temperature.rda")
