library(gulf.data)
library(dplyr)
library(readr)
library(ggplot2)

# Load the data from the RDA file created in build.rda.R
load("temperature.rda") 

x <- x %>%
  select(site, site_depth__profondeur_site = site.depth.m, log_position__position_sonde = surface.bottom, log_depth__profondeur_sonde = log.depth.m,
         latitude, longitude, year__annee = year, month__mois = month, day__jour = day, temperature, time)

years <- 2020:2024
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

# summarize the data
daily_avg_temp <- x %>%
  group_by(site, site_depth__profondeur_site, log_position__position_sonde, log_depth__profondeur_sonde,
           latitude, longitude, year__annee, month__mois, day__jour) %>%
  summarize(daily_average_temperature__temperature_moyenne_journaliere = round(mean(temperature, na.rm = TRUE), 2), .groups = "drop") 

# View the resulting dataframe
#print(daily_avg_temp)

#excel(daily_avg_temp)

# Extract the last year from the years vector
last_year <- max(years)

# Write to CSV files
write.csv(daily_avg_temp, file = paste0("request/open.data.temperature.", last_year, ".csv"), row.names = FALSE)

# --------------- Validation ------------------------------------------------------------------

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
    scale_y_continuous(breaks = seq(floor(min(site_data$daily_average_temperature__temperature_moyenne_journaliere)), 
                                    ceiling(max(site_data$daily_average_temperature__temperature_moyenne_journaliere)), 
                                    by = 1)) +
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

# __________________________________________________________________
# scatter plot

# Open a PDF device for bottom probes (B)
pdf("daily_avg_temp_b_scatter_plots.pdf", width = 11, height = 8.5)

# Loop over each site and create a scatter plot for the bottom probes (B)
for (site in unique(daily_avg_temp_b$site)) {
  site_data <- daily_avg_temp_b %>% filter(site == !!site)
  
  # Create the scatter plot for each site
  gg <- ggplot(site_data, aes(x=day_of_year, y=daily_average_temperature__temperature_moyenne_journaliere, color=factor(year__annee))) +
    geom_point() +
    labs(title=paste("Site:", site), x="Day of Year", y="Daily Average Temperature (°C)", color="Year") +
    scale_y_continuous(breaks = seq(floor(min(site_data$daily_average_temperature__temperature_moyenne_journaliere, na.rm = TRUE)), 
                                    ceiling(max(site_data$daily_average_temperature__temperature_moyenne_journaliere, na.rm = TRUE)), 
                                    by = 1)) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) # Rotate the x-axis labels for readability
  
  # Print the plot to the PDF device
  print(gg)
}

# Close the PDF device
dev.off()
# __________________________________________________________________
# Scatter plots for all temperatures (not daily averages)

# Create a 'day of year' variable
x$date <- as.Date(with(x, paste(year__annee, month__mois, day__jour, sep = "-")), "%Y-%m-%d")
x$day_of_year <- yday(x$date)

# Open a PDF device for bottom probes (B)
pdf("temp_scatter_plots_b.pdf", width = 11, height = 8.5)

# Loop over each site and create a scatter plot for the bottom probes (B)
for (site in unique(x$site[x$log_position__position_sonde == "B"])) {
  site_data <- x %>% filter(site == !!site & log_position__position_sonde == "B")
  
  # Create the scatter plot for each site
  gg <- ggplot(site_data, aes(x = day_of_year, y = temperature, color = factor(year__annee))) +
    geom_point() +
    labs(title = paste("Site:", site), x = "Day of Year", y = "Temperature (°C)", color = "Year") +
    scale_y_continuous(breaks = seq(floor(min(site_data$temperature, na.rm = TRUE)), 
                                    ceiling(max(site_data$temperature, na.rm = TRUE)), 
                                    by = 1)) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) # Rotate the x-axis labels for readability
  
  # Print the plot to the PDF device
  print(gg)
}

# Close the PDF device
dev.off()

#-------------------------------------------------------------



## Map each site

library(sf)

# Remove duplicates
unique_data <- daily_avg_temp %>%
  distinct(site, year__annee, log_position__position_sonde, .keep_all = TRUE)

# Convert to spatial object
unique_data_sf <- st_as_sf(unique_data, coords = c("longitude", "latitude"), crs = 4326)

ggplot(data = unique_data_sf) +
  geom_sf(aes(color = as.factor(year__annee), shape = log_position__position_sonde)) +
  labs(title = "Unique Site/Year/Log Position on Map",
       color = "Year",
       shape = "Log Position") +
  theme_minimal()
