library(gulf.data)
library(dplyr)
library(readr)
library(ggplot2)

# Load the data from the RDA file created in build.rda.R
load("temperature.rda") 

x <- x %>%
  select(site, site_depth__profondeur_site = site.depth.m, log_position__position_sonde = surface.bottom, log_depth__profondeur_sonde = log.depth.m,
         latitude, longitude, year__annee = year, month__mois = month, day__jour = day, temperature, time)

# Filter for specific sites and year 2013
specific_sites <- c("Anse Bleue 1, N.B.", "Anse Bleue 2, N.B.", "Anse Bleue 3, N.B.")
x <- x %>%
  filter(site %in% specific_sites & year__annee == 2016)

# Convert longitude to negative 
x$longitude <- x$longitude * -1

# Filter out NA in positions
x <- x %>%
  filter(!is.na(latitude) & !is.na(longitude))

# Take out dates in the time columns
x <- x %>%
  mutate(time = sub(".*\\s", "", time))

# Summarize the data
daily_avg_temp <- x %>%
  group_by(site, site_depth__profondeur_site, log_position__position_sonde, log_depth__profondeur_sonde,
           latitude, longitude, year__annee, month__mois, day__jour) %>%
  summarize(daily_average_temperature__temperature_moyenne_journaliere = round(mean(temperature, na.rm = TRUE), 2), .groups = "drop") 

# View the resulting dataframe
print(daily_avg_temp)

excel(daily_avg_temp)

# Write to CSV file
write.csv(daily_avg_temp, file = "request/open.data.temperature.2013.csv", row.names = FALSE)

# Validation ----

library(lubridate)

# Convert year, month, and day to Date format
daily_avg_temp$date <- as.Date(with(daily_avg_temp, paste(year__annee, month__mois, day__jour, sep="-")), "%Y-%m-%d")

# Create a 'day of year' variable
daily_avg_temp$day_of_year <- yday(daily_avg_temp$date)

# Open a PDF device for plots
pdf("daily_avg_temp_plots.pdf", width = 11, height = 8.5)

# Create a combined plot for all sites
gg <- ggplot(daily_avg_temp, aes(x=day_of_year, y=daily_average_temperature__temperature_moyenne_journaliere, group=interaction(site, year__annee), color=interaction(site, year__annee))) +
  geom_line() +
  labs(title="Daily Average Temperature for Selected Sites in 2013", x="Day of Year", y="Daily Average Temperature (°C)", color="Site/Year") +
  scale_y_continuous(breaks = seq(floor(min(daily_avg_temp$daily_average_temperature__temperature_moyenne_journaliere)), 
                                  ceiling(max(daily_avg_temp$daily_average_temperature__temperature_moyenne_journaliere)), 
                                  by = 1)) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) # Rotate the x-axis labels for readability

# Print the plot to the PDF device
print(gg)

# Close the PDF device
dev.off()

# Map each site

library(sf)

# Remove duplicates
unique_data <- daily_avg_temp %>%
  distinct(site, year__annee, .keep_all = TRUE)

# Convert to spatial object
unique_data_sf <- st_as_sf(unique_data, coords = c("longitude", "latitude"), crs = 4326)

ggplot(data = unique_data_sf) +
  geom_sf(aes(color = as.factor(year__annee))) +
  labs(title = "Unique Site/Year on Map",
       color = "Year") +
  theme_minimal()
