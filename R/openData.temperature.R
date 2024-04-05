library(gulf.data)
library(dplyr)
library(readr)

# Load the data from the RDA file created in build.rda.R
load("temperature.rda") 

x <- x %>%
  select(site, site_depth__profondeur_site = site.depth.m, log_position__position_sonde = surface.bottom, log_depth__profondeur_sonde = log.depth.m,
         latitude, longitude, year__annee = year, month__mois = month, day__jour = day, temperature, time)

# select year - 1998-2021 - need to fix Pinette 1997 (missing time) and Pugwash, N.S. (1) 1995 (no log depth)
years <- 2021
x <- x %>%
  filter(x$year__annee %in% years)

# convert longitude to negative 
x$longitude <- x$longitude*-1

# filter out NA in positions
x <- x %>%
  filter(!is.na(latitude) & !is.na(longitude))

# take out dates in the time columns
x <- x %>%
  mutate(time = sub(".*\\s", "", time))

# Assuming your dataframe is named `x`
daily_avg_temp <- x %>%
  group_by(site, site_depth__profondeur_site, log_position__position_sonde, log_depth__profondeur_sonde,
           latitude, longitude, year__annee, month__mois, day__jour) %>%
  summarize(daily_average_temperature__temperature_moyenne_journaliere = round(mean(temperature, na.rm = TRUE), 2), .groups = "drop") 

# View the resulting dataframe
print(daily_avg_temp)

excel(daily_avg_temp)

# Extract the last year from the years vector
last_year <- max(years)

# Write to CSV files
write.csv(daily_avg_temp, file = paste0("request/open.data.temperature.", last_year, ".csv"), row.names = FALSE)

# Validation ----

library(ggplot2)
library(dplyr)
library(lubridate)

# create a dataframe for bottom probes and one for surface probes
daily_avg_temp_b <- daily_avg_temp[daily_avg_temp$log_position__position_sonde == "B",]
daily_avg_temp_s <- daily_avg_temp[daily_avg_temp$log_position__position_sonde == "S",]

# Convert year, month, and day to Date format
daily_avg_temp_b$date <- as.Date(with(daily_avg_temp_b, paste(year__annee, month__mois, day__jour, sep="-")), "%Y-%m-%d")
daily_avg_temp_s$date <- as.Date(with(daily_avg_temp_s, paste(year__annee, month__mois, day__jour, sep="-")), "%Y-%m-%d")

# Create a 'day of year' variable
daily_avg_temp_b$day_of_year <- yday(daily_avg_temp_b$date)
daily_avg_temp_s$day_of_year <- yday(daily_avg_temp_s$date)

# Open a PDF device for b
pdf("daily_avg_temp_b_plots.pdf", width = 11, height = 8.5)

# Loop over each site and create a plot for the bottom probes (B)
for (site in unique(daily_avg_temp_b$site)) {
  site_data <- daily_avg_temp_b %>% filter(site == !!site)
  
  # Create the plot for each site
  gg <- ggplot(site_data, aes(x=day_of_year, y=daily_average_temperature__temperature_moyenne_journaliere, group=year__annee, color=factor(year__annee))) +
    geom_line() +
    labs(title=paste("Site:", site), x="Day of Year", y="Daily Average Temperature (°C)", color="Year") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) # Rotate the x-axis labels for readability
  
  # Print the plot to the PDF device
  print(gg)
}

# Close the PDF device
dev.off()

# Open a PDF device for s
pdf("daily_avg_temp_s_plots.pdf", width = 11, height = 8.5)

# Loop over each site and create a plot for the bottom probes (B)
for (site in unique(daily_avg_temp_s$site)) {
  site_data <- daily_avg_temp_s %>% filter(site == !!site)
  
  # Create the plot for each site
  gg <- ggplot(site_data, aes(x=day_of_year, y=daily_average_temperature__temperature_moyenne_journaliere, group=year__annee, color=factor(year__annee))) +
    geom_line() +
    labs(title=paste("Site:", site), x="Day of Year", y="Daily Average Temperature (°C)", color="Year") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) # Rotate the x-axis labels for readability
  
  # Print the plot to the PDF device
  print(gg)
}


# Close the PDF device
dev.off()
