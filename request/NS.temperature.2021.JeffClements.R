#this code imports Temperature data from Access DB and creates tables to be used in R.

library(RODBC)
library(gulf.data)
library(dplyr)
library(stringr)

# Clear all objects from the environment
rm(list = ls())

min.year <- 1996
max.year <- 2021

## Establish connection to Access database
channel <- odbcConnectAccess2007("C:/Users/Gagnondj/Documents/Database/Temperature_be.mdb")

All.temp <- sqlFetch(channel, "All Data")

#view tables in db
subset(sqlTables(channel), TABLE_TYPE == "TABLE") 

# 
# # SQL query to fetch records for years of interest
# query <- paste0("SELECT * FROM [All Data] WHERE Year BETWEEN ", min(years), " AND ", max(years))
# 
# 
# # Fetch data using sqlQuery
# temperature <- sqlQuery(channel, query)

odbcClose(channel)

#processing temperatures
temperature <- All.temp %>%
  select( #keep only needed fields and rename them
    site = `Site (text)`,
    site.depth.meters = `Site Depth(m)`,
    surface.bottom = `Surface-Bottom`,
    log.depth.meters = `Log Depth(m)`,
    latitude = Latitude,
    longitude = Longitude,
    year = Year,
    month = Month,
    day = Day,
    date = Date,
    time = Time,
    temperature = Temperature,
  ) %>%
  mutate(site = str_trim(site)) %>% # trim for extra spaces
  filter(year >= min.year & year <= max.year) %>% # filter for years between min.year and max.year
  filter(str_detect(site, "N\\.S\\.$"))  %>% #only keep site in Nova Scotia
  mutate(
    time = format(as.POSIXct(time, format = "%H:%M:%S"), "%H:%M:%S") #format time field
  )

write.csv(temperature, file = paste0("request/NS.temperature.N.S.csv"), row.names = FALSE)
