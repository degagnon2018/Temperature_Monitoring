#this code imports Temperature data from Access DB and creates tables to be used in R.

library(RODBC)
library(gulf.data)
library(dplyr)
library(stringr)

years <- 2021

## Establish connection to Access database
channel <- odbcConnectAccess2007("C:/Users/Gagnondj/Documents/Database/Temperature_be.mdb")

#view tables in db
subset(sqlTables(channel), TABLE_TYPE == "TABLE") 

# SQL query to fetch records for years of interest
query <- paste0("SELECT * FROM [All Data] WHERE Year = ", years)

# Fetch data using sqlQuery
temperature <- sqlQuery(channel, query)

#processing temperatures
temperature <- temperature %>%
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
  filter(str_detect(site, "N\\.S\\.$"))  %>% #only keep site in Nova Scotia
  mutate(
    time = format(as.POSIXct(time, format = "%H:%M:%S"), "%H:%M:%S") #format time field
  )

odbcClose(channel)
