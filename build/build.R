library(gulf.data)
year = 2021

#get metadata file
setwd("C:/Users/Gagnondj/Documents/GitHub/Temperature_monitoring/data/Raw/")
y <- read.csv("119_info_2019_R.csv")
y <- y[y$year == 2021,]

#Set working directory for files to be bind together into one file
#setwd("C:/Users/Gagnondj/Documents/GitHub/Temperature_monitoring/data/Raw/Minilog/")
setwd(paste0("C:/Users/Gagnondj/Documents/GitHub/Temperature_monitoring/data/Raw/to_process/"))

# list of the names of the excel files in the working directory
lst = list.files(full.names = TRUE)

# [5/20/2022 10:02 AM] Surette, Tobie
# y = by(x, by = "file")
# 
# [5/20/2022 10:03 AM] Surette, Tobie
# y[[1]]
# 


#  lst[i] <- gsub("./", "", lst[i])
  
  x <- read.minilog(lst)
  
  #"C:/Users/Gagnondj/Documents/GitHub/Temperature_monitoring/data/Raw/to_process/Cascumpeque PEI JV 2021 354093.csv"
  #keeps last 6 character of header.Source Device to be used for serial number
  sn <- x$header.source.device
  x$serial.number <- substr(sn,(nchar(sn)+1)-6,nchar(sn))
  
  x$month <- month(x$date)
  x$day <- day(x$date)
  
  #extract recorder type from header.source.device
  rec <- x$header.source.device
  x$recorder <- substr(rec, 0, nchar(rec)-7)
  
  x <- merge(x, y, by = "serial.number")
  
  vars <- c("recorder", "serial.number", "site", "site.depth.m", "surface.bottom", "log.depth.m", "latitude", 
            "longitude", "year", "month", "day", "date", "time", "temperature", "buoy.id") 
  x <- x[vars]
  
  #z = by(x, by = "serial.number")
  sn <- unique(x$serial.number)

#Create a file for each site
  for (i in 1:length(sn)){
    z <- subset(x, x$serial.number == sn[i])
    #excel(z, file = as.character(unique(z$site)))
    file.name <- unique(paste(z$site, z$buoy.id, z$surface.bottom, z$year, z$serial.number, "F.csv"))
    write.csv(z, paste("C:/Users/Gagnondj/Documents/GitHub/Temperature_monitoring/data/2021/", file.name), row.names = FALSE)
  }
  
excel(x)
