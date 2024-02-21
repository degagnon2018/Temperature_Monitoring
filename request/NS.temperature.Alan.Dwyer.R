
library(RODBC)
library(dplyr)
library(stringr)
library(openxlsx)

# Establish connection to Access database
channel <- odbcConnectAccess2007("C:/Users/Gagnondj/Documents/Database/Temperature_be.mdb")

# SQL query to fetch records for specific site/year combinations
query <- "SELECT * FROM [All Data] WHERE 
          (Year = 2020 AND `Site (text)` LIKE '%Caribou, N.S.%') OR 
          (Year = 2021 AND `Site (text)` LIKE '%Arisaig, N.S.%') OR 
          (Year = 2021 AND `Site (text)` LIKE '%Wallace, N.S.%') OR 
          (Year = 2021 AND `Site (text)` LIKE '%River John, N.S.%')"

# Fetch data using sqlQuery
temperature <- sqlQuery(channel, query)

# Close the RODBC connection
odbcClose(channel)

# Processing temperatures
temperature <- temperature %>%
  select(
    site = `Site (text)`,
    year = Year,
    month = Month,
    day = Day,
    temperature = Temperature
  ) %>%
  mutate(
    site = str_trim(site), # trim for extra spaces
    date = as.Date(paste(year, month, day, sep = "-")) # combine year, month, day into a date
  ) %>%
  group_by(site, year, date) %>%
  summarise(daily_avg_temp = mean(temperature, na.rm = TRUE), .groups = 'drop')

# Split the data by site/year and write to Excel files
split_data <- split(temperature, list(temperature$site, temperature$year))

for (data_name in names(split_data)) {
  # Check if the subset contains data
  if (nrow(split_data[[data_name]]) > 0) {
    file_name <- paste0("temperature_data_", gsub("[ ,]", "_", data_name), ".xlsx")
    write.xlsx(split_data[[data_name]], file_name, row.names = FALSE)
  }
}
