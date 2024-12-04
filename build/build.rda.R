library(RODBC)
library(dplyr)
library(gulf.data)

# Establish connection to Access database
channel <- odbcConnectAccess2007("C:/Users/Gagnondj/Documents/Database/Temperature_be.mdb")

x <- sqlFetch(channel, "All Data")

x <- x %>%
  rename(site = 'Site (text)', recorder = Recorder, site.depth.m = 'Site Depth(m)', 
         surface.bottom = 'Surface-Bottom', log.depth.m = 'Log Depth(m)',
         latitude = Latitude, longitude = Longitude, year = Year, month = Month, 
         day = Day, date = Date, time = Time, temperature = Temperature, 
         unique.id = Unique_id, serial.number = Serial_number, buoy.id = Buoy_Id) %>%
  filter(!is.na(temperature)) %>% # Filtering out NA temperatures
  mutate(  time = format(as.POSIXct(time, format = "%H:%M:%S"), "%H:%M:%S") #format time field
)

# Close the RODBC connection
odbcClose(channel)

## ---- Corrections ----- 
# adjust site "Anse Bleue 3, N.B.", year 2013. Add .8 degrees to temperatures
x <- x %>%
  mutate(temperature = ifelse(site == "Anse Bleue 3, N.B." & year == 2013, temperature + 0.8, temperature))

# format time
x <- x %>%
  mutate(  time = format(as.POSIXct(time, format = "%H:%M:%S"), "%H:%M:%S") #format time field
  )

# Save the data to an RDA file
save(x, file = "temperature.rda")

