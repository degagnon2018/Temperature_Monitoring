library(gulf.data)
library(openxlsx)

#this code only deals with Star Oddi or Starmon temperature recorders

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
setwd(paste0("C:/Users/Gagnondj/Documents/GitHub/Temperature_monitoring/data/Raw/to process 2/"))

# list of the names of the files in the working directory
files = list.files()
files


if (length(files) > 0) {
  # Read CSV files and extract site, SN, and date/time information
  data <- lapply(files, function(file) {
    
    
    #temp
   #file <- "Cape Egmont collecteurs 2022  S11837.xlsx"
    
    # # Remove the file extension
    file_name <- sub("\\.(csv|xlsx)$", "", file)  
    
    # Define a vector of excluded words
    excluded_words <- c("NS", "PEI", "NB", "collecteur", "collecteurs", "UPM", "RM", "Ledge", "Bottom", "Fairway", "LL")
    
    # Extract the site_name
    parts <- strsplit(file, "\\s+")
    site_name <- unlist(parts)[!unlist(parts) %in% excluded_words]
    site_name <- paste(site_name[1:(which(site_name == "2022") - 1)], collapse = " ")
    
    # Trim any leading or trailing spaces
    site_name <- trimws(site_name)
    
    # Print the site_name
    print(site_name)
    
    #extracts the last word of the file name (serial number)
    serial.number <- sub("^.* ([A-Za-z0-9]+)$", "\\1", file_name) 
    
    #site <- gsub(paste0("[", serial.number, "]"), "", site_name)
    
    # Read the CSV file where the serial number starts with either S or T (Star Oddi DST CT or Starmon Mini)
    if (grepl("^[ST]", serial.number)) {
    
      if (grepl("\\.csv$", file)) {
        # Read CSV file
        df <- read.csv(file)
        print(file)
      } else if (grepl("\\.xlsx$", file)) {
        # Read Excel file using readxl package
        df <- read.xlsx(file)
        print(file)
        }    
      
    # Find the column containing date and time information (star oddi)
    date_time_col <- grep("(?i)date.*time", names(df), value = TRUE)
    
     if (length(date_time_col) > 0) {
    #   # Extract date and time components
       df$date <- as.Date(df[[date_time_col]], origin = "1899-12-30")
       #df$time <- format(as.POSIXct(df[[date_time_col]] * 86400, origin = "1970-01-01", tz = "Halifax"), "%I:%M:%S %p")
       #rounding to closest second to get data to show at every 30 minutes exactly
       df$time <- format(round(as.POSIXct(df[[date_time_col]] * 86400, origin = "1970-01-01"), units = "secs"), "%I:%M:%S %p")
    #   
    #   # Remove the original date/time column
    #   df <- df[, -which(names(df) == date_time_col)]
     }
    # 
    #make all variable names lower case
    names(df) <- tolower(names(df))
    
    #add serial number and site name to appropriate variables
    df$serial.number <- serial.number
    df$site <- site_name
    
    #extract the year, month and day from the date
     df$year <- year(df$date)
     df$month  <- month(df$date)
     df$day <- day(df$date)

    #add recorder type to recorder variable
    if (grepl("^S", serial.number)) {
      
      #Change some of the variable names
      str <- names(df)
      str[str == "temp.â.c." | str == "temp(°c)"] <- "temperature"
      str[str == "salinity.psu." | str == "salinity(psu)"] <- "salinity.psu"
      str[str == "conduct.ms.cm." | str == "conduct(ms/cm)"] <- "conduct.ms.cm"
      str[str == "sound.velocity.m.sec." | str == "sound.velocity(m/sec)"] <- "sound.velocity.m.sec"
      names(df) <- str
      
      #populate recorder variable
      df$recorder <- paste0("Star-Oddi DST CT")
      
      #setting variables to include in output later in code
      vars <- c("recorder",	"serial.number",	"site",	"site.depth.m",	"surface.bottom",	"log.depth.m",	"latitude",	
                "longitude",	"year",	"month",	"day",	"date",	"time",	"temperature", "salinity.psu", "conduct.ms.cm", "sound.velocity.m.sec")
      
    } else if (grepl("^T", serial.number)) {
      
      #Change some of the variable names
      str <- names(df)
      str[str == "temp.â.c." | str == "temp(°c)"] <- "temperature"
      names(df) <- str
      
      #populate recorder variable
      df$recorder <- "Starmon Mini"
      
      #setting variables to include in output later in code
      vars <- c("recorder",	"serial.number",	"site",	"site.depth.m",	"surface.bottom",	"log.depth.m",	"latitude",	
                "longitude",	"year",	"month",	"day",	"date",	"time",	"temperature")
    
      } else {
      # Handle the case when the serial number doesn't start with 'S' or 'T'
      # You can choose to set it to a different value or leave it as is.
      # For example:
      # df$recorder <- "Some Other Value"
    }
    
    
     
    #merge the temperature dataframe with the lookup file data
    z <- merge(x=df,y=y, by.x=c("year", "serial.number", "site"), by.y=c("year", "serial.number", "lookup.sites"), all.x=TRUE)
    
    #keep only wanted variables and in the order wanted
    z$site <- z$site.y
    z <- z[vars]
    
    #check if match was successful for z merge
    if (is.na(z$site)) {
      # If the 'z$site' is NA in the merged data frame, it means there were no matches
      cat(paste0(unique(df$site), " - No matches found, check lookup file.\n"))
    } else {
      # If 'z$site' has values, it means there were matches
      cat(paste0(unique(df$site), "Matches found.\n"))
    }
    
    #sort by date, time
    z <- z[order(z$date, z$time),]
    
    #remove the x variable that appeared in the dataframe
    #df <- df[, -which(names(df) == "x")]
    
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

