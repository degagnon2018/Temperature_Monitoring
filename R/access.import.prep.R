################################################################################################################
## code binds all files together for import into Access database; in the future Should code to eliminate NA, ###
## done by hand in 2024                                                                                       ##
################################################################################################################

library(dplyr)
library(purrr)
library(readxl)

setwd("C:/Users/gagnondj/Documents/GitHub/Temperature_Monitoring")

# Define the path to your folder containing the CSV files
folder_path <- "data/Raw/LL to add"

# Get the list of CSV and XLSX files in the folder
csv_files <- list.files(path = folder_path, pattern = "\\.csv$", full.names = TRUE)
xlsx_files <- list.files(path = folder_path, pattern = "\\.xlsx$", full.names = TRUE)

file_list <- c(csv_files, xlsx_files)

file_list

# Function to read files and ensure all columns are present and correct types
read_and_complete <- function(file) {
  if (grepl("\\.csv$", file)) {
    df <- read.csv(file, sep = ",", stringsAsFactors = FALSE)
  } else if (grepl("\\.xlsx$", file)) {
    df <- read_excel(file)
  }
  
  required_columns <- c("recorder", "serial.number", "site", "site.depth.m", "surface.bottom", 
                        "log.depth.m", "latitude", "longitude", "year", "month", "day", 
                        "date", "time", "temperature", "salinity.psu", "conduct.ms.cm", 
                        "sound.velocity.m.sec", "buoy.id")
  
  # Log missing columns (for debugging)
  missing_columns <- setdiff(required_columns, names(df))
  if (length(missing_columns) > 0) {
    message("File ", file, " is missing columns: ", paste(missing_columns, collapse = ", "))
  }
  
  # Add missing columns with "" values
  df[missing_columns] <- ""
  
  # Ensure these field are all character type
  df <- df %>%
    mutate(across(c("serial.number", "date", "time", "latitude", "longitude"), as.character))
  
  return(df)
}

# Read and bind all files into a single dataframe
combined_df <- file_list %>%
  map_dfr(~ read_and_complete(.)) # applies the read_and_complete function to each element of the 
                                  # list (file_list) and combines the results into a single data frame

# Clean up the site column
combined_df <- combined_df %>%
  mutate(site = gsub(", NB", ", N.B.", site),
         site = gsub(", NS", ", N.S.", site),
         site = gsub(", PEI", ", P.E.I.", site))

# Replace NA with empty strings
combined_df[is.na(combined_df)] <- ""

# Move buoy.id to the last column
combined_df <- combined_df %>%
  select(-buoy.id, buoy.id)

# Validation - ensure expected columns are present
expected_columns <- c("recorder", "serial.number", "site", "site.depth.m", "surface.bottom", 
                      "log.depth.m", "latitude", "longitude", "year", "month", "day", 
                      "date", "time", "temperature", "salinity.psu", "conduct.ms.cm", 
                      "sound.velocity.m.sec", "buoy.id")

# validation - Check if all expected columns are present and in the correct order
if (!identical(names(combined_df), expected_columns)) {
  stop("Column names or order do not match the expected format.")
}

# Extract the unique years from the dataframe
unique_years <- unique(combined_df$year)

# Determine the range of years
year_range <- range(unique_years, na.rm = TRUE)

# Construct the filename with the year range
filename <- paste0("data/raw/Access_import_minilogs_LL_add", year_range[1], "_", year_range[2], ".csv")

# Write the combined dataframe to the specified file
write.csv(combined_df, file = filename, row.names = FALSE)


# validation - Check if there are any duplicate records
has_duplicates <- any(duplicated(combined_df))

if (has_duplicates) {
  # Extract and display duplicate records
  duplicates <- combined_df[duplicated(combined_df) | duplicated(combined_df, fromLast = TRUE), ]
  print("Duplicate records found:")
  excel(duplicates)
} else {
  print("No duplicate records found.")
}

# validation - check number of records per probe
count_per_group <- combined_df %>%
  group_by(site, year, serial.number) %>%
  summarise(record_count = n())

excel(count_per_group)

#### append Access database ----
# 
# # Path to your Access database
# db_path <- "C:/Users/gagnondj/Desktop/Temperature_be.mdb"
# 
# # Establish connection to the Access database
# channel <- odbcConnectAccess2007(db_path)
# 
# # Check existing tables (optional, for verification)
# existing_tables <- sqlTables(channel, schema = "dbo", tableType = "TABLE")
# print(existing_tables$TABLE_NAME)
# 
# # Optional: View the structure of the dataframe
# print(str(combined_df))
# 
# # Append data to an existing table in the database
# # Assuming the table in Access is named 'TemperatureData' and has compatible structure
# sqlSave(channel, combined_df, tablename = "TemperatureData", append = TRUE, rownames = FALSE, colnames = FALSE, fast = TRUE)
# 
# # Close the database connection
# odbcClose(channe
