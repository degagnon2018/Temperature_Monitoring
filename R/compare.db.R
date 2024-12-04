library(RODBC)
library(gulf.data)
library(dplyr)
library(purrr)

years <- 2023

## Establish connection -----------------------------------------------------------------------
#channel <- odbcConnectAccess2007("W:\\Lobster\\Database\\SS_be.mdb")
#channel <- odbcConnectAccess2007("W:/Lobster/Database/Temperature_be.mdb")
channel <- odbcConnectAccess2007("C:/Users/gagnondj/Desktop/Temperature_be.mdb")

channel2 <- odbcConnectAccess2007("C:/Users/Gagnondj/Documents/Database/Temperature_be.mdb")

#view tables in db
subset(sqlTables(channel), TABLE_TYPE == "TABLE") 

#load tables into dataframes
temperature <- sqlFetch(channel, "All Data")
temperature2 <- sqlFetch(channel2, "All Data")

## Close and remove channel
odbcClose(channel)
odbcClose(channel2)

# compare temperature ------------------------------------------------------------------------------

# compare temperature from C: with W:
temperature <- temperature %>%
  filter(Year %in% years) %>%
  arrange('Site (text)', Year, Month, Day, Time)
temperature2 <- temperature2 %>%
  filter(Year %in% years) %>%
  arrange('Site (text)', Year, Month, Day, Time)

# quick check for differences
all.equal(temperature, temperature2)
identical(temperature, temperature2)

# unmatched rows ------------------------------------------------------------------------------
# Create a combined ID column in each dataframe
temperature <- temperature %>%
  mutate(combined_id = paste(`Site (text)`, Year, Month, Day, Time, sep = "_"))

temperature2 <- temperature2 %>%
  mutate(combined_id = paste(`Site (text)`, Year, Month, Day, Time, sep = "_"))

## checks what rows are in one and not the other (temperature vs temperature2)
# Rows in temperature that are not in temperature2
unmatched_in_temperature <- anti_join(temperature, temperature2, by = "combined_id")
  
# Rows in temperature2 that are not in temperature
unmatched_in_temperature2 <- anti_join(temperature2, temperature, by = "combined_id")

# Write unmatched rows to console
if (nrow(unmatched_in_temperature) > 0) {
  print("Unmatched rows in temperature:")
  print(unmatched_in_temperature)
} else {
  print("No unmatched rows in temperature.")
}

excel(unmatched_in_temperature)
excel(unmatched_in_temperature2)

if (nrow(unmatched_in_temperature2) > 0) {
  print("Unmatched rows in temperature2:")
  print(unmatched_in_temperature2)
} else {
  print("No unmatched rows in temperature2.")
}

# row counts for each ----------------------------------------------
## if not equal, compare and print row counts for temperature and temperature2 directly
if (nrow(temperature) == nrow(temperature2)) {
  print("temperature and temperature2 have the same number of rows.")
} else {
  print(sprintf("Rows in temperature: %d, Rows in temperature2: %d", nrow(temperature), nrow(temperature2)))
  print(sprintf("Difference in rows: %d", abs(nrow(temperature) - nrow(temperature2))))
  
  # Check and print unmatched rows
  if (nrow(unmatched_in_temperature) > 0) {
    print("Unmatched rows in temperature:")
    print(unmatched_in_temperature)
  } else {
    print("No unmatched rows in temperature.")
  }
  
  if (nrow(unmatched_in_temperature2) > 0) {
    print("Unmatched rows in temperature2:")
    print(unmatched_in_temperature2)
  } else {
    print("No unmatched rows in temperature2.")
  }
}


# Find common IDs
common_ids <- intersect(temperature$combined_id, temperature2$combined_id)

# Subset dataframes to only include rows with common IDs
temperature_common <- temperature %>% filter(combined_id %in% common_ids) %>% arrange(combined_id)
temperature2_common <- temperature2 %>% filter(combined_id %in% common_ids) %>% arrange(combined_id)

# Assuming the structure of both dataframes is the same and they are now ordered, you can compare them directly
differences <- map2_df(temperature_common, temperature2_common, ~if_else(.x == .y, TRUE, FALSE))

# Identify rows and columns with differences
diff_loc <- which(!differences, arr.ind = TRUE)

# Specify the output file path
output_file_path <- "differences_report.txt"

# Open a connection to the file
file_conn <- file(output_file_path, open = "wt")

# Assuming the rest of the setup is the same as before

if (length(diff_loc) > 0) {
  unique_rows <- unique(diff_loc[, "row"])
  
  for (row in unique_rows) {
    differing_cols <- which(!differences[row, ])
    differing_col_names <- names(differences)[differing_cols]
    
    # Prepare the message
    message <- paste("Differences in row", row, "ID:", temperature_common[row, "combined_id"])
    # Write and print the message
    writeLines(message, file_conn)
    print(message)
    
    for (col_name in differing_col_names) {
      temp_val <- temperature_common[row, col_name]
      temp2_val <- temperature2_common[row, col_name]
      # Prepare the column difference message
      message <- paste(col_name, "temperature:", temp_val, "temperature2:", temp2_val)
      # Write and print the column difference message
      writeLines(message, file_conn)
      print(message)
    }
  }
} else {
  message <- "No differences found in the matching rows."
  writeLines(message, file_conn)
  print(message)
}

# Close the file connection
close(file_conn)


