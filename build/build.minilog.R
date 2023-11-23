library(gulf.data)
library(lubridate)
year = 2022

#get metadata file
setwd("C:/Users/Gagnondj/Documents/GitHub/Temperature_monitoring/data/Raw/")
y <- read.csv("Temp.lookup_R.csv")
y <- y[y$year == year,]

#Set working directory for files to be bind together into one file
setwd(paste0("C:/Users/Gagnondj/Documents/GitHub/Temperature_monitoring/data/Raw/to process 2/"))

#only keep the csv files
lst <- list.files(full.names = TRUE, pattern = "\\.csv$", all.files = FALSE)

#only keep the files for minilogs, which are those with serial numbers of 6 digits
lst <- lst[grepl("\\d{6}\\.csv$", lst)]
print(lst)

  x <- read.minilog(lst)
  
  #keeps last 6 character of header.Source Device to be used for serial number
  sn <- x$header.source.device
  x$serial.number <- substr(sn,(nchar(sn)+1)-6,nchar(sn))
  
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
    write.csv(z.subset, paste("C:/Users/Gagnondj/Documents/GitHub/Temperature_monitoring/data/Raw/processed//", file.name), row.names = FALSE)
    print(unique(file.name))
      }
  
