#######################################################################################################
## Code to process raw  star-Oddi or Starmon temperature recorder files                             ###
## Code will process files In the "to_process" folder, there should not be any folders in this      ###
## folder, just the files to be processed.                                                          ###                 ###
#######################################################################################################

library(gulf.data)
library(openxlsx)
library(lubridate)
library(dplyr)
library(readxl)

# set year of files to be processed
    year = 2023

## get metadata file
    y <- read.csv("data/Raw/Temp.lookup_R.csv")
    y <- y[y$year == year & !is.na(y$year),]
    
    # convert serial.number to lower case
    y$serial.number <- tolower(y$serial.number)
    
    # create a lookup.site column
    y$lookup.sites <- y$site
    y$lookup.sites <- sub(",.*", "", y$lookup.sites)

#Set working directory for files to be bind together into one file
setwd("C:/Users/Gagnondj/Documents/GitHub/Temperature_monitoring/data/Raw/to_process/")

# list of the names of the files in the working directory
files = list.files()
files

# file names in the working directory are validated to insure the year is in it's name
if (length(files) > 0) {
  if (all(grepl(year, files))) {
    print("All files have the year in their name")
  } else {
    stop("Not all files have the year in their name")
  }
}

if (length(files) > 0) {
  data <- lapply(files, function(file) {
    
    #temp
    #file <- "Indian Rocks PEI 2023 N8 S11248.xlsx"
    
    # # Remove the file extension
    file_name <- sub("\\.(csv|xlsx)$", "", file)  
    
    # Define a vector of excluded words
    excluded_words <- c("NS", "PEI", "NB", "collecteur", "collecteurs", "UPM", "RM", "Ledge", "Bottom", "Fairway", "LL")
    
    # Extract the site_name
    parts <- strsplit(file, "\\s+")
    site_name <- unlist(parts)[!unlist(parts) %in% excluded_words]
    site_name <- paste(site_name[1:(which(site_name %in% year) - 1)], collapse = " ")
    
    # Trim any leading or trailing spaces
    site_name <- trimws(site_name)
    
    # Print the site_name
    print(site_name)
    
    #extracts the last word of the file name (serial number)
    serial.number <- sub("^.* ([A-Za-z0-9]+)$", "\\1", file_name)
    
    #convert serial.number to all lower caps
    serial.number <- tolower(serial.number)
    
    # Read the file where the serial number starts with either S or T (Star Oddi DST CT or Starmon Mini)
    if (grepl("^[st]", serial.number, ignore.case = TRUE)) {
    
      if (grepl("\\.csv$", file)) {
        # Read CSV file
        df <- read.csv(file)
        print(file)
      } else if (grepl("\\.xlsx$", file)) {
        # Read Excel file using readxl package
        df <- read_xlsx(file) 
        print(file)
      }    
      
      #make all variable names lower case
      names(df) <- tolower(names(df))
      
      # For files with serial numbers starting with 'S' (for Star Oddi)
      if (grepl("^s", serial.number, ignore.case = TRUE)) {  
    
        # Find the column containing date and time information (star oddi)
        date_time_col <- grep("(?i)date.*time", names(df), value = TRUE)
    
        # # Loop through each datetime and adjust if the seconds are 59
        # for (i in 1:nrow(df)) {
        #   if (as.integer(format(df$datetime[i], "%S")) == 59) {
        #     # Add one second to adjust the time
        #     df$datetime[i] <- df$datetime[i] + 1
        #   }
        # }

        # Create date and time columns
        df <- df %>%
          mutate(
            date = format(`date & time`, "%Y-%m-%d"),
            time = format(`date & time`, "%I:%M:%S %p")
          )
        
        #Change some of the variable names
        str <- names(df)
        str[str == "temp.â.c." | str == "temp(°c)"] <- "temperature"
        str[str == "salinity.psu." | str == "salinity(psu)"] <- "salinity.psu"
        str[str == "conduct.ms.cm." | str == "conduct(ms/cm)"] <- "conduct.ms.cm"
        str[str == "sound.velocity.m.sec." | str == "sound velocity(m/sec)"] <- "sound.velocity.m.sec"
        names(df) <- str
        
        #populate recorder variable
        df$recorder <- paste0("Star-Oddi DST CT")
        
        #setting variables to include in output later in code
        vars <- c("recorder",	"serial.number",	"site",	"site.depth.m",	"surface.bottom",	"log.depth.m",	"latitude",	
                  "longitude",	"year",	"month",	"day",	"date",	"time",	"temperature", "salinity.psu", "conduct.ms.cm", 
                  "sound.velocity.m.sec", "buoy.id")
        
        
      } else if (grepl("^t", serial.number, ignore.case = TRUE)) { # files with SN starting with 'T' (for Starmon mini)
        
        # format date
        df$date <- as.Date(df$date)
        
        #Change some of the variable names
        str <- names(df)
        str[str == "temp.â.c." | str == "temp(°c)"] <- "temperature"
        names(df) <- str
        
        #populate recorder variable
        df$recorder <- "Starmon Mini"
        
        #setting variables to include in output later in code
        vars <- c("recorder",	"serial.number",	"site",	"site.depth.m",	"surface.bottom",	"log.depth.m",	"latitude",	
                  "longitude",	"year",	"month",	"day",	"date",	"time",	"temperature", "buoy.id")
      }
    
    # add serial number and site name to appropriate variables
    df$serial.number <- serial.number
    df$site <- site_name
    
    # extract the year, month and day from the date
     df$year <- year(df$date)
     df$month  <- month(df$date)
     df$day <- day(df$date)

     # only keep data for the year being processed, some files may have multi-year data.
     df <- df[df$year == year, ]

    #merge the temperature dataframe with the lookup file data
    z <- merge(x=df,y=y, by.x=c("year", "serial.number", "site"), by.y=c("year", "serial.number", "lookup.sites"), all.x=TRUE)

    #keep only wanted variables and in the order wanted
    z$site <- z$site.y
    z <- z[vars]
    
    #check if match was successful for z merge
    if (all(is.na(z$site))) {
      # If the 'z$site' is NA in the merged data frame, it means there were no matches
      cat(paste0(unique(df$site), " - ******* No matches found, check lookup file.\n"))
    } else {
      # If 'z$site' has values, it means there were matches
      cat(paste0(unique(df$site), " Matches found.\n"))
    }
    
    # Combine date and time into a datetime object for operations (correct sorting)
    z$datetime_for_operations <- as.POSIXct(paste(z$date, z$time), format="%Y-%m-%d %I:%M:%S %p")
    
    # Arrange chronological
    z <- arrange(z, datetime_for_operations)
    
    # drop datetime_for_operations column after sort
    z <- z %>%
      select(-datetime_for_operations) 
    
    # Write the dataframe to a file in the folder
    folder_path <- "C:/Users/Gagnondj/Documents/GitHub/Temperature_monitoring/data/Raw/processed/"
    file_path <- file.path(folder_path, paste0(file_name, "_processed.csv"))  # Change the extension to .csv
    write.csv(z, file = file_path, row.names = FALSE)
    
    return(df)
    }
  })
  
} else {
  print("No files found in the folder.")
}

