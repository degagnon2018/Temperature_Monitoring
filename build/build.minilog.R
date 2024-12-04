library(gulf.data)
library(lubridate)
library(dplyr)

year_to_process = 2022
write.path <- "C:/Users/Gagnondj/Documents/GitHub/Temperature_monitoring/data/Raw/processed/"
setwd("C:/Users/Gagnondj/Documents/GitHub/Temperature_monitoring/")

#get metadata file
y <- read.csv("data/Raw/Temp.lookup_R.csv")

# Subset to only include rows for year = year and where "year" is not NA
y <- y[y$year == year_to_process & !is.na(y$year),]

# #Set working directory for files to be bind together into one file
setwd("C:/Users/Gagnondj/Documents/GitHub/Temperature_monitoring/data/Raw/to_process/")

#only keep the csv files
lst <- list.files(full.names = FALSE, pattern = "\\.csv$", all.files = FALSE)

#only keep the files for minilogs, which are those with serial numbers of 6 digits
lst <- lst[grepl("\\d{6}\\.csv$", lst)]
print(lst)

# process minilog files using the read.minilog function
x <- read.minilog(lst)

# Create new columns in the dataframe with header information
x$site <- x$header.study.description

## Extract the serial number from source.device
x$serial.number <- gsub("[^0-9]", "", x$header.source.device)

#extract recorder type from source.device
x$recorder <- substr(x$header.source.device, 0, nchar(x$header.source.device)-7)

# Function to check if a date is in "m/d/yyyy" format
is_mdyyyy_format <- function(date_str) {
  grepl("^\\d{1,2}/\\d{1,2}/\\d{4}$", date_str)
}

# Convert the date column conditionally
x <- x %>%
  mutate(date = if_else(is_mdyyyy_format(date), 
                        format(as.Date(date, format = "%m/%d/%Y"), "%Y-%m-%d"), 
                        date))

#convert x$date into a date format 
x$date <- as.Date(x$date, format = "%Y-%m-%d")

x$year  <- year(x$date)
x$month <- month(x$date)
x$day <- day(x$date)

## deal with a site with a different date format causing issues
x <- x %>%
  mutate(date = case_when(
    site == "Malpeque PEI bottom JF 2022" ~ as.Date(date, format = "%m/%d/%Y"),
    TRUE ~ as.Date(date, format = "%Y-%m-%d")
  ))



# Subset to only include rows for year = year and where "year" is not NA
x <- subset(x, year == year_to_process)

z <- merge(x, y, by = "serial.number", all.x=TRUE)
  
  #Change some of the variable names
z <- z %>%
  rename_with(~ case_when(
    . == "ï..site" ~ "site",
    . == "site.y" ~ "site",
    . == "year.y" ~ "year",
    TRUE ~ .
  ))
  
# convert z$time to datetime object:
z$time <- as.POSIXct(z$time, format = "%H:%M:%S")  # adjust the input format as necessary
  
# format time as %I:%M:%S %p
z$time <- format(z$time, "%I:%M:%S %p")
  
vars <- c("recorder", "serial.number", "site", "site.depth.m", "surface.bottom", "log.depth.m", "latitude", 
            "longitude", "year", "month", "day", "date", "time", "temperature", "buoy.id") 
z <- z[vars]
  
#z = by(x, by = "serial.number")
sn <- unique(z$serial.number)

#finding data that did not match with lookup file and print info
non.matching <- unique(z[is.na(z$site),]$serial.number)
non.matching <- unique(x[x$serial.number %in% non.matching,]$header.study.description)
  
if (!is.null(non.matching) && length(non.matching) > 0) {
  print(paste0(non.matching, " - no match with lookup file"))
} 

## Create a file for each site
  
# Combine date and time into a datetime object for operations (correct sorting)
z$datetime_for_operations <- as.POSIXct(paste(z$date, z$time), format="%Y-%m-%d %I:%M:%S %p")
  
# Arrange chronological
z <- arrange(z, datetime_for_operations)
  
# Create a file for each serial number with sorted data
for (i in 1:length(sn)){
  z_subset <- filter(z, serial.number == sn[i]) %>%
     arrange(datetime_for_operations) %>% # sort by date and time
     select(-datetime_for_operations) # take out column used for sorting
  file.name <- paste(z_subset$site[1], z_subset$buoy.id[1], z_subset$surface.bottom[1], z_subset$year[1], z_subset$serial.number[1], "_processed.csv", sep="_")
  write.csv(z_subset, file.path(write.path, file.name), row.names = FALSE)
  print(paste("File written:", file.name))
}

head(z)
