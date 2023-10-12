library(gulf.data)
library(readxl)
library(openxlsx)
library(lubridate)

year = 2022
folder_path <- "C:/Users/Gagnondj/Documents/GitHub/Temperature_monitoring/data/Raw/processed/"

#get metadata file
setwd("C:/Users/Gagnondj/Documents/GitHub/Temperature_monitoring/data/Raw/")
y <- read.csv("Temp.lookup_R.csv")
y <- y[y$year == year,]

#Change some of the variable names in y
str <- names(y)
str[str == "ï..site"] <- "lookup.sites"
names(y) <- str

#keep only site name without province
y$site <- y$lookup.sites
y$lookup.sites <- sub(",.*", "", y$lookup.sites)

#Set working directory for files to be bind together into one file
#setwd("C:/Users/Gagnondj/Documents/GitHub/Temperature_monitoring/data/Raw/Minilog/")
setwd(paste0("C:/Users/Gagnondj/Documents/GitHub/Temperature_monitoring/data/Raw/to_process/"))

# list of the names of the excel files in the working directory
files = list.files()
files

# Filter CSV files
csv_files <- files[grep("\\.csv$", files)]

#For Star-Oddi .csv files

if (length(csv_files) > 0) {
  # Read CSV files and extract site, SN, and date/time information
  csv_data <- lapply(csv_files, function(file) {
    
    #temp
    #file <- "North Cape PEI 2022 DS S11191.csv"
    
    # Extract site and SN from file name
    
    # Remove the file extension
    file_name <- sub("\\.csv$", "", file)  
    
    # (Removes unwanted words in file name, year, serial number and buoy code)
    site_name <- sub("( collecteur| RM| LL| UPM| NS| NB| PEI | Ledge | Fairway)", "", sub("^([A-Za-z ]+).*", "\\1", file_name))
    
    #remove trailing space
    site_name <- sub("\\s*$", "", site_name)
    
    #extracts the last word of the file name (serial number)
    serial.number <- sub("^.* ([A-Za-z0-9]+)$", "\\1", file_name) 
    
    #site <- gsub(paste0("[", serial.number, "]"), "", site_name)
    
    # Read the CSV file
    df <- read.csv(file)
    
    # Find the column containing date and time information
    date_time_col <- grep("(?i)date.*time", names(df), value = TRUE)
    
    if (length(date_time_col) > 0) {
      # Extract date and time components
      df$date <- as.Date(df[[date_time_col]], origin = "1899-12-30")
      df$time <- format(as.POSIXct(df[[date_time_col]] * 86400, origin = "1970-01-01"), "%I:%M:%S %p")
      
      # Remove the original date/time column
      df <- df[, -which(names(df) == date_time_col)]
    }
    
    #make all variable names lower case
    names(df) <- tolower(names(df))
    
    #Add Star-Oddi to the recorder type variable
    df$recorder <- paste0("Star-Oddi DST CT")
    df$serial.number <- serial.number
    df$site <- site_name
    
    #extract the year, month and day from the date
    df$year <- year(df$date)
    df$month  <- month(df$date)
    df$day <- day(df$date)

    #Change some of the variable names
    str <- names(df)
    str[str == "temp.â.c."] <- "temperature"
    names(df) <- str
    
    #merge the temperature dataframe with the lookup file data
    z <- merge(x=df,y=y, by.x=c("year", "serial.number", "site"), by.y=c("year", "serial.number", "lookup.sites"), all.x=TRUE)
    
    #keep only wanted variables and in the order wanted
    vars <- c("recorder",	"serial.number",	"site.y",	"site.depth.m",	"surface.bottom",	"log.depth.m",	"latitude",	"longitude",	"year",	"month",	"day",	"date",	"time",	"temperature")
    z <- z[vars]
    
    #remove the x variable that appeared in the dataframe
    #df <- df[, -which(names(df) == "x")]
    
    # Write the dataframe to a file in the folder
    folder_path <- "C:/Users/Gagnondj/Documents/GitHub/Temperature_monitoring/data/Raw/processed/"
    file_path <- file.path(folder_path, paste0(file_name, "_processed.csv"))  # Change the extension to .csv
    write.csv(z, file = file_path, row.names = FALSE)
    
    return(df)
  })
  
  # Concatenate CSV data frames into a single data frame, this will be usefull for importing into Access
  combined_csv_data <- do.call(rbind, csv_data)
  
  # Print the combined data frame
  #print(combined_csv_data)
} else {
  print("No CSV files found in the folder.")
}

---------------------------------------------------------------------------------------------------------------------

# # Filter XLSX files
# xlsx_files <- files[grep("\\.xlsx$", files)]
# 
# if (length(xlsx_files) > 0) {
#   # Read XLSX files and extract site and SN information
#   xlsx_data <- lapply(xlsx_files, function(file) {
#     # Extract site and SN from file name
#     file_name <- sub("\\.xlsx$", "", file)  # Remove the file extension
#     site <- sub("( collecteur| RM| LL| UPM)", "", sub("^([A-Za-z ]+).*", "\\1", file_name))
#     site <- trimws(site)  # Trim leading and trailing whitespace
#     serial.number <- sub("^.* ([A-Za-z0-9]+)$", "\\1", file_name)
#     
#     # Read the XLSX file
#     df <- openxlsx::read.xlsx(file)
#     
#     # Find the column containing date and time information
#     date_time_col <- grep("(?i)date.*time", names(df), value = TRUE)
#     
#     if (length(date_time_col) > 0) {
#       # Extract date and time components
#       df$date <- as.Date(df[[date_time_col]], origin = "1899-12-30")
#       df$time <- format(as.POSIXct(df[[date_time_col]] * 86400, origin = "1970-01-01"), "%I:%M:%S %p")
#       
#       # Remove the original date/time column
#       df <- df[, -which(names(df) == date_time_col)]
#     }
#     
#     # Add site and SN as new columns
#     df$site <- site
#     df$serial.number <- serial.number
#     
#     #change field names
#     str <- names(df)
#     str[str == "Temp(°C)"] <- "temperature"
#     str[str == "Salinity.psu."] <- "salinity.psu"
#     str[str == "Conduct.mS.cm."] <- "conduct.ms.cm"
#     str[str == "Sound.Velocity.m.sec."] <- "sound.velocity.m.sec."
#     names(df) <- str
#     
#     # Write the dataframe to a file in the folder
#     folder_path <- "C:/Users/Gagnondj/Documents/GitHub/Temperature_monitoring/data/Raw/processed/"
#     file_path <- file.path(folder_path, paste0(file_name, ".csv"))  # Change the extension to .csv
#     write.csv(df, file = file_path, row.names = FALSE)
#     
#     return(df)
#   })
#   
#   # Concatenate XLSX data frames into a single data frame, this will be usefull for importing into Access
#   combined_xlsx_data <- do.call(rbind, xlsx_data)
#   
#   # Print the combined data frame
#   #print(combined_xlsx_data)
# } else {
#   print("No XLSX files found in the folder.")
# }


# # Merge the combined CSV and XLSX data frames
# df <- combined_xlsx_data

# #create a column containing only the site without the province to match the 2 dataframes
# y$lookup.sites <- sub(",.*", "", y$site)

#extract the year, month and day from the date
df$year <- year(df$date)
df$month  <- month(df$date)
df$day <- day(df$date)

#merge the temperature dataframe with the lookup file data
z <- merge(x=df,y=y, by.x=c("year", "serial.number", "site"), by.y=c("year", "serial.number", "lookup.sites"), all.x=TRUE)
z$id <- paste(as.character(z$year), as.character(z$serial.number), as.character(z$site.y))

#z would be the file to add to the DB

#now would need to create a file for each probe
a <- split(z, f=z$id)

# Define the variables to keep
var <- c("serial.number", "site.y", "site.depth.m", "surface.bottom", 
         "log.depth.m", "latitude", "longitude", "year", "month", "day", "date", "time",  "temperature", "buoy.id")

# Iterate over the list of dataframes and write to CSV
for (i in seq_along(a)) {
  # Get the name for the current section
  site_name <- names(a)[i]
  
  # Select the desired variables for the current section
  selected_data <- a[[i]][var]
  
  # Create the file path for the CSV file
  file_path <- file.path(folder_path, paste0(site_name, ".csv"))
  
  # Write the current section to a CSV file
  write.csv(selected_data, file = file_path, row.names = FALSE)
}

#need to fix file names that output

# # Iterate over the list of dataframes and write to CSV
# for (i in seq_along(a)) {
#   # Get the name for the current section
#   site_name <- names(a)[i]
#   
#   # Select the desired variables for the current section
#   selected_data <- a[[i]][var]
#   
#   # Create the file path for the CSV file
#   file_name <- paste0(a[[i]]$site, "_", a[[i]]$year, "_", a[[i]]$serial.number, ".csv")
#   file_path <- file.path(folder_path, file_name)
#   
#   # Write the current section to a CSV file
#   write.csv(selected_data, file = file_path, row.names = FALSE)
# }
