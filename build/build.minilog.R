#read.minilog() doesn't run on over R 4.0

library(gulf.data)
library(lubridate)
year = 2022
write.path <- "C:/Users/Gagnondj/Documents/GitHub/Temperature_monitoring/data/Raw/processed/"

#get metadata file
y <- read.csv("data/Raw/Temp.lookup_R.csv")

# Subset to only include rows for year = year and where "year" is not NA
y <- y[y$year == year & !is.na(y$year),]

# #Set working directory for files to be bind together into one file
setwd("C:/Users/Gagnondj/Documents/GitHub/Temperature_monitoring/data/Raw/to_process/")

#only keep the csv files
lst <- list.files(full.names = FALSE, pattern = "\\.csv$", all.files = FALSE)

#only keep the files for minilogs, which are those with serial numbers of 6 digits
lst <- lst[grepl("\\d{6}\\.csv$", lst)]
print(lst)

  x <- read.minilog("Cape Georges NS 2022 VU2 358420.csv")
  
  # Extract the serial number from x$header.source.device
  sn <- x$header.source.device
  
  # Use regular expressions to match the last sequence of digits
  x$serial.number <- gsub("[^0-9]", "", sn)
  
  #convert x$date into a date format 
  x$date <- as.Date(x$date, format = "%Y-%m-%d")
  
  x$month <- month(x$date)
  x$day <- day(x$date)
  
  #extract recorder type from header.source.device
  rec <- x$header.source.device
  x$recorder <- substr(rec, 0, nchar(rec)-7)
  
  z <- merge(x, y, by = "serial.number", all.x=TRUE)
  
  #Change some of the variable names
  str <- names(z)
  str[str == "ï..site"] <- "site"
  names(z) <- str
  
  vars <- c("recorder", "serial.number", "site", "site.depth.m", "surface.bottom", "log.depth.m", "latitude", 
            "longitude", "year", "month", "day", "date", "time", "temperature", "buoy.id") 
  z <- z[vars]
  
  #z = by(x, by = "serial.number")
  sn <- unique(z$serial.number)

  #finding data that did not match with lookup file and print info
  non.matching <- unique(z[is.na(z$site),]$serial.number)
  non.matching <- unique(x[x$serial.number == non.matching,]$header.study.description)
  
  if (!is.null(non.matching) && length(non.matching) > 0) {
    print(paste0(non.matching, " - no match with lookup file"))
  } 

#Create a file for each site
  for (i in 1:length(sn)){
    z.subset <- subset(z, z$serial.number == sn[i])
    #excel(z, file = as.character(unique(z$site)))
    file.name <- unique(paste(z.subset$site, z.subset$buoy.id, z.subset$surface.bottom, z.subset$year, z.subset$serial.number, "_processed.csv"))
    write.csv(z.subset, paste(write.path, file.name), row.names = FALSE)
    print(unique(file.name))
      }
  
