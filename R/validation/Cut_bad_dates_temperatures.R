library(readr)
library(gulf.data)
library(tidyr)
library(changepoint)
library(ggplot2)
library(dplyr)
library(purrr)
library(lubridate)

# ──────────────────────────────────────────────────────────────────────────────
# 1) Read & bind all CSV files into one data frame
# ──────────────────────────────────────────────────────────────────────────────
folder_path     <- "C:/Users/Gagnondj/Documents/GitHub/Temperature_Monitoring/data/Raw/processed"
csv_files       <- list.files(path = folder_path, pattern = "\\.csv$", full.names = TRUE)

## Specify column types
my_col_types <- cols(
  recorder        = col_character(),
  serial.number   = col_character(),
  site            = col_character(),
  site.depth.m    = col_double(),
  surface.bottom  = col_character(),
  log.depth.m     = col_double(),
  latitude        = col_double(),
  longitude       = col_double(),
  year            = col_integer(),
  month           = col_integer(),
  day             = col_integer(),
  date            = col_date(),
  time            = col_time(),
  temperature     = col_double(),
  salinity.psu    = col_double(),
  conduct.ms.cm   = col_double(),
  sound.velocity.m.sec = col_double(),
  buoy.id         = col_character()
)

# Read all files and tag source filename
temperature_data <- lapply(csv_files, function(f) {
  message("Reading file: ", basename(f))
  
  # Read, treating only empty strings as NA (so "NA" stays literal)
  df <- read_csv(
    f, 
    col_types = my_col_types,
    na          = c(""),      # only "" → NA
    show_col_types = TRUE     # keep column spec messages
  )
  
  # Tag every row with its source filename
  df %>% mutate(source_file = basename(f))
}) %>% 
  bind_rows()

# ──────────────────────────────────────────────────────────────────────────────
# 2) Parse a POSIX datetime column from `date` + `time`
# ──────────────────────────────────────────────────────────────────────────────
temperature_data <- temperature_data %>%
  mutate(
    datetime = as.POSIXct(
      paste(`date`, `time`),
      format = "%Y-%m-%d %H:%M:%S",
      tz     = "UTC"
    )
  )

# ──────────────────────────────────────────────────────────────────────────────
# 3) Tag each row as “pre-deployment”, “in-water”, or “out-of-water”
# ──────────────────────────────────────────────────────────────────────────────

# Rule in use: OUT starts 2 days before the first sustained low run

# 3.1 Daily means for salinity and conductivity, plus daily_low flag
daily_flags <- temperature_data %>%
  mutate(sample_date = as.Date(datetime)) %>%
  group_by(site, serial.number, sample_date, buoy.id) %>%
  summarize(
    salinity.psu  = mean(salinity.psu,  na.rm = TRUE),
    conduct.ms.cm = mean(conduct.ms.cm, na.rm = TRUE),
    .groups = "drop_last"
  ) %>%
  mutate(daily_low = salinity.psu < 6 | conduct.ms.cm < 10) %>%
  arrange(site, serial.number, sample_date)

# 3.2) First in-water day
# 3.2 First in-water day per probe (start the day AFTER the first non-low day if pre-deployment records exist)
daily_flags <- daily_flags %>%
  group_by(site, serial.number) %>%
  mutate(
    # earliest day that is NOT low
    first_nonlow_day = {
      d <- sample_date[!daily_low]
      if (length(d)) min(d) else as.Date(NA)
    },
    # did we record anything before that non-low day? → pre-deployment present
    has_predeployment = ifelse(is.na(first_nonlow_day), FALSE, any(sample_date < first_nonlow_day)),
    # rule: if pre-deployment exists, flip to in-water the NEXT day; else use the non-low day itself
    first_in_day = ifelse(
      is.na(first_nonlow_day),
      as.Date(NA),
      ifelse(has_predeployment, first_nonlow_day + 1L, first_nonlow_day)
    )
  ) %>%
  ungroup() %>%
  select(-first_nonlow_day, -has_predeployment)

# 3.3) Daily runs of low and candidate out-of-water start days
daily_runs <- daily_flags %>%
  arrange(site, serial.number, sample_date) %>%
  group_by(site, serial.number) %>%
  mutate(run_id = with(rle(daily_low), rep(seq_along(values), lengths))) %>%
  group_by(site, serial.number, run_id) %>%
  summarize(
    buoy.id   = first(buoy.id),
    low_val   = first(daily_low),
    run_start = min(sample_date),
    run_end   = max(sample_date),
    run_len   = as.integer(run_end - run_start + 1L),
    first_in_day = first(first_in_day),
    .groups = "drop"
  ) %>%
  # candidates: low runs for ≥2 days, after deployment, if CCG buoy (not DFO/COLLECTEUR), 
  # confine start to Nov–Jan
  mutate(
    is_ccg = !is.na(buoy.id) & buoy.id != "" &
      !toupper(buoy.id) %in% c("DFO","COLLECTEUR")
  ) %>%
  filter(
    low_val,
    run_len >= 2,
    !is.na(first_in_day),
    run_start > first_in_day,
    (!is_ccg) | month(run_start) %in% c(11,12,1)   # vectorized
  ) %>%
  group_by(site, serial.number) %>%
  summarize(first_out_start = min(run_start), .groups = "drop") # earliest qualifying run

# 3.4) Apply “−2 days” rule and assign per-day state
daily_states <- daily_flags %>%
  left_join(daily_runs, by = c("site","serial.number")) %>%
  mutate(
    # Special rule: out-of-water begins two days before the low run starts
    first_out_rule_day = ifelse(is.na(first_out_start), NA, as.Date(first_out_start) - 2L),
    # do not allow OUT before or on first_in_day
    first_out_rule_day = ifelse(!is.na(first_in_day) & !is.na(first_out_rule_day) &
                                  first_out_rule_day <= first_in_day, as.Date(NA), first_out_rule_day),
    state = case_when(
      is.na(first_in_day) ~ "pre-deployment",
      sample_date < first_in_day  ~ "pre-deployment",
      is.na(first_out_rule_day) & sample_date >= first_in_day ~ "in-water",
      sample_date >= first_in_day & sample_date < first_out_rule_day  ~ "in-water",
      sample_date >= first_out_rule_day                       ~ "out-of-water"
    )
  ) %>%
  select(site, serial.number, sample_date, first_in_day, first_out_rule_day, state)

# 3.5) Join day-level state back to original rows in data
temperature_data <- temperature_data %>%
  mutate(sample_date = as.Date(datetime)) %>%
  left_join(daily_states, by = c("site","serial.number","sample_date")) %>%
  select(-sample_date)  # keep state and cutoff days

# 3.6) Store event dates for plotting (no recomputation later)
events_summary <- daily_states %>%
  group_by(site, serial.number) %>%
  summarize(
    first_in  = first(na.omit(first_in_day))  %||% as.Date(NA),
    first_out = first(na.omit(first_out_rule_day)) %||% as.Date(NA),
    .groups = "drop"
  )

  
# ──────────────────────────────────────────────────────────────────────────────
# 4) Summarize date ranges per site, serial.number, state
# ──────────────────────────────────────────────────────────────────────────────
summary_tbl <- temperature_data %>%
  group_by(site, serial.number, state) %>%
  summarise(
    count            = n(),
    start_datetime   = min(datetime, na.rm = TRUE),
    end_datetime     = max(datetime, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(site, serial.number, state)

# Print the summary table with full detail
print(summary_tbl, n = Inf)

# excel(summary_tbl)

# ──────────────────────────────────────────────────────────────────────────────
# 5) plots with vertical line for validation
# ──────────────────────────────────────────────────────────────────────────────

# set where to save plots
plot_dir <- "C:/Users/Gagnondj/Documents/GitHub/Temperature_Monitoring/figures"
if (!dir.exists(plot_dir)) dir.create(plot_dir, recursive = TRUE)

pdf(file.path(plot_dir, "daily_with_transitions_labeled.pdf"), width = 10, height = 5)

# 5.1) Build daily_data
daily_data <- temperature_data %>%
  mutate(sample_date = as.Date(datetime)) %>%
  group_by(site, serial.number, sample_date) %>%
  summarize(
    temperature    = mean(temperature,    na.rm = TRUE),
    salinity.psu   = mean(salinity.psu,   na.rm = TRUE),
    conduct.ms.cm  = mean(conduct.ms.cm,  na.rm = TRUE),
    sound_velocity = mean(sound.velocity.m.sec, na.rm = TRUE),
    # grab the sample_date's state (assumes it's uniform within each sample_date)
    state          = first(state),
    .groups = "drop"
  ) %>%
  mutate(sound_scaled = sound_velocity / 20)

# 5.2) Build one plot per probe
probes <- daily_data %>% distinct(site, serial.number)

for (i in seq_len(nrow(probes))) {
  s  <- probes$site[i]
  sn <- probes$serial.number[i]
  
  df <- daily_data %>%
    filter(site == s, serial.number == sn)
  ev <- events_summary %>%
    filter(site == s, serial.number == sn)
  
  # make absolutely sure these are Date
  in_date  <- as.Date(ev$first_in)
  out_date <- as.Date(ev$first_out)
  
  # determine a nice y-position for the labels
  y_max <- max(
    df$temperature,
    df$salinity.psu,
    df$conduct.ms.cm,
    df$sound_scaled,
    na.rm = TRUE
  )
  
  p <- ggplot(df, aes(x = sample_date)) +
    geom_line(aes(y = temperature,  color = "Temperature")) +
    geom_line(aes(y = salinity.psu, color = "Salinity")) +
    geom_line(aes(y = conduct.ms.cm,color = "Conductivity")) +
    geom_line(aes(y = sound_scaled, color = "Sound Velocity ÷20")) +
    
    # Use scale_x_date so Date transforms work
    scale_x_date(date_labels = "%Y-%m-%d") +
    
    # only draw the in-water line + label if we have a Date
    { if (!is.na(in_date)) 
      list(
        geom_vline(xintercept = in_date,
                   linetype = "dashed",
                   color = "steelblue",
                   linewidth = 0.8),
        annotate("text",
                 x     = in_date,
                 y     = y_max,
                 label = format(in_date, "%Y-%m-%d"),
                 color = "steelblue",
                 vjust = -0.5,
                 size  = 3)
      )
      else NULL
    } +
    
    # only draw the out-of-water line + label if we have a Date
    { if (!is.na(out_date)) 
      list(
        geom_vline(xintercept = out_date,
                   linetype = "dashed",
                   color = "firebrick",
                   linewidth = 0.8),
        annotate("text",
                 x     = out_date,
                 y     = y_max,
                 label = format(out_date, "%Y-%m-%d"),
                 color = "firebrick",
                 vjust = -0.5,
                 size  = 3)
      )
      else NULL
    } +
    
    scale_color_manual(values = c(
      "Temperature"         = "firebrick",
      "Salinity"            = "dodgerblue",
      "Conductivity"        = "darkgreen",
      "Sound Velocity ÷20"  = "purple"
    )) +
    labs(
      title = sprintf("Daily Averages — Site: %s  Probe: %s", s, sn),
      x     = "Date",
      y     = "Value",
      color = "Variable"
    ) +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5))
  
  print(p)
}

dev.off()


# ──────────────────────────────────────────────────────────────────────────────
# 6) clean data - take out pre-deployment and out-of-water data
#  - check plots before doing this to determine exception sites - code can't
# process all sites yet
# ──────────────────────────────────────────────────────────────────────────────

# Define exception sites

# 2024 exceptions
exceptions <- c(
  "Anse Bleue 3, NB",
  "Caribou, NS",
  "Indian Rocks, PEI",
  "Mabou Harbour, NS",
  "North Cape, PEI"
)

# if no exceptions
# exceptions <- character(0)  # no exceptions this year

clean_data <- temperature_data %>%
  # Add day and flag full‐in‐water days
  mutate(sample_date = as.Date(datetime)) %>%
  group_by(site, serial.number, sample_date) %>%
  mutate(
    all_in_water = all(state == "in-water")
  ) %>%
  ungroup() %>%
  
  # Keep rows if either
  #    • site is in exceptions (keep everything)
  #    • OR it’s a good (all_in_water) day
  filter(
    site %in% exceptions |
      all_in_water
  ) %>%
  
  # Drop helper columns
  select(-sample_date, -all_in_water)

# ──────────────────────────────────────────────────────────────────────────────
# 7) output csv files per site, year, serial.number
# ──────────────────────────────────────────────────────────────────────────────

# Make sure the output folder exists
out_dir <- "C:/Users/Gagnondj/Documents/GitHub/Temperature_Monitoring/data/Cleaned_By_Probe"
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

# Build a table of all site/year/serial.number combos in the cleaned data
exports <- clean_data %>%
  mutate(year = year(datetime)) %>%
  distinct(site, year, serial.number)

# Loop and write one CSV per probe-year
for (i in seq_len(nrow(exports))) {
  s  <- exports$site[i]
  y  <- exports$year[i]
  sn <- exports$serial.number[i]
  
  df_out <- clean_data %>%
    filter(
      site == s,
      serial.number == sn,
      year(datetime) == y
    ) %>%
    select(-source_file, -datetime, -state, -first_in_day, -first_out_rule_day) #drop unwanted columns
    
  
  # sanitize names for filenames
  safe_site <- gsub("[^A-Za-z0-9]", "_", s)
  safe_sn   <- gsub("[^A-Za-z0-9]", "_", sn)
  
  file_name <- sprintf("%s_%d_%s_cleaned.csv", safe_site, y, safe_sn)
  file_path <- file.path(out_dir, file_name)
  
  write_csv(df_out, file_path)
}


# -------------------------------------------------------- cleaned data plots


# ──────────────────────────────────────────────────────────────────────────────
# A) Re-create daily_data with the original state tags
# ──────────────────────────────────────────────────────────────────────────────
daily_data <- temperature_data %>%
  mutate(sample_date = as.Date(datetime)) %>%
  group_by(site, serial.number, sample_date) %>%
  summarize(
    temperature          = mean(temperature,          na.rm = TRUE),
    salinity.psu         = mean(salinity.psu,         na.rm = TRUE),
    conduct.ms.cm        = mean(conduct.ms.cm,        na.rm = TRUE),
    sound.velocity.m.sec = mean(sound.velocity.m.sec, na.rm = TRUE),
    # pull the day's state (should be uniform)
    state                = first(state),
    .groups = "drop"
  ) %>%
  mutate(sound_scaled = sound.velocity.m.sec / 20)

# ──────────────────────────────────────────────────────────────────────────────
# B) Build daily_clean from the already‐filtered clean_data
# ──────────────────────────────────────────────────────────────────────────────
daily_clean <- clean_data %>%
  mutate(sample_date = as.Date(datetime)) %>%
  group_by(site, serial.number, sample_date) %>%
  summarize(
    temperature          = mean(temperature,          na.rm = TRUE),
    salinity.psu         = mean(salinity.psu,         na.rm = TRUE),
    conduct.ms.cm        = mean(conduct.ms.cm,        na.rm = TRUE),
    sound.velocity.m.sec = mean(sound.velocity.m.sec, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(sound_scaled = sound.velocity.m.sec / 20)

# ──────────────────────────────────────────────────────────────────────────────
# C) Extract the per-day state lookup
# ──────────────────────────────────────────────────────────────────────────────
daily_state <- daily_data %>%
  select(site, serial.number, sample_date, state)

# ──────────────────────────────────────────────────────────────────────────────
# D) Join state onto the cleaned daily series
# ──────────────────────────────────────────────────────────────────────────────
daily_with_state <- daily_clean %>%
  left_join(daily_state, by = c("site", "serial.number", "sample_date"))

# ──────────────────────────────────────────────────────────────────────────────
# E) Recompute first_in / first_out from the cleaned daily_with_state
# ──────────────────────────────────────────────────────────────────────────────
events_clean <- daily_with_state %>%
  group_by(site, serial.number) %>%
  summarize(
    first_in  = if (any(state == "in-water",    na.rm = TRUE))
      min(sample_date[state == "in-water"],    na.rm = TRUE)
    else as.Date(NA),
    first_out = if (any(state == "out-of-water", na.rm = TRUE))
      min(sample_date[state == "out-of-water"], na.rm = TRUE)
    else as.Date(NA),
    .groups = "drop"
  )

# Now daily_with_state and events_clean are ready for plotting


# 4) Plot one panel per probe with the new event dates
probes <- daily_with_state %>% distinct(site, serial.number)

pdf(file.path(plot_dir, "cleaned_daily_plots_optionB.pdf"), width = 10, height = 5) # set where to save plots

for (i in seq_len(nrow(probes))) {
  s  <- probes$site[i]
  sn <- probes$serial.number[i]
  
  df <- daily_with_state %>%
    filter(site == s, serial.number == sn)
  ev <- events_clean %>%
    filter(site == s, serial.number == sn)
  
  # Top of y-axis for labels
  y_max <- max(
    df$temperature,
    df$salinity.psu,
    df$conduct.ms.cm,
    df$sound_scaled,
    na.rm = TRUE
  )
  
  p <- ggplot(df, aes(x = sample_date)) +
    geom_line(aes(y = temperature,   color = "Temperature")) +
    geom_line(aes(y = salinity.psu,  color = "Salinity")) +
    geom_line(aes(y = conduct.ms.cm, color = "Conductivity")) +
    geom_line(aes(y = sound_scaled,  color = "Sound Velocity ÷20")) +
    scale_x_date(date_labels = "%Y-%m-%d") +
    
    # Deployment line & label
    { if (!is.na(ev$first_in))
      list(
        geom_vline(xintercept = ev$first_in,  linetype = "dashed",
                   color = "steelblue", linewidth = 0.8),
        annotate("text",
                 x     = ev$first_in,
                 y     = y_max,
                 label = format(ev$first_in, "%Y-%m-%d"),
                 color = "steelblue",
                 vjust = -0.5,
                 size  = 3)
      )
      else NULL
    } +
    # Retrieval line & label
    { if (!is.na(ev$first_out))
      list(
        geom_vline(xintercept = ev$first_out, linetype = "dashed",
                   color = "firebrick", linewidth = 0.8),
        annotate("text",
                 x     = ev$first_out,
                 y     = y_max,
                 label = format(ev$first_out, "%Y-%m-%d"),
                 color = "firebrick",
                 vjust = -0.5,
                 size  = 3)
      )
      else NULL
    } +
    
    scale_color_manual(values = c(
      "Temperature"        = "firebrick",
      "Salinity"           = "dodgerblue",
      "Conductivity"       = "darkgreen",
      "Sound Velocity ÷20" = "purple"
    )) +
    labs(
      title = sprintf("Cleaned Daily Averages — Site: %s  Probe: %s", s, sn),
      x     = "Date",
      y     = "Value",
      color = "Variable"
    ) +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5))
  
  print(p)
}
dev.off()
