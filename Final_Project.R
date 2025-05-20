#Getting the shooting data 
install.packages(c("httr", "jsonlite"))

# Load libraries
library(httr)
library(jsonlite)

dir_path <- "final_project"
local_file <- file.path(dir_path, "shooting_data.json")

# Check if the directory exists, create it if not
if (!dir.exists(dir_path)) {
  dir.create(dir_path)
}

# Check if the file exists in the directory
if (!file.exists(local_file)) {
  message("Downloading data from NYC Open Data...")
  
  # Download from API (limit for testing; adjust or remove limit as needed)
  url <- "https://data.cityofnewyork.us/resource/5ucz-vwe8.json"
  response <- GET(url)
  
  # Save raw JSON to the specified directory
  write(content(response, "text"), local_file)
} else {
  message("Using previously downloaded data.")
}

# Load the data from the local file
shooting_data_2025 <- fromJSON(local_file)

#Getting shooting data from 2022-2024
dir_path <- "final_project"
local_file <- file.path(dir_path, "shooting_data_historic.json")

# Check if the directory exists, create it if not
if (!dir.exists(dir_path)) {
  dir.create(dir_path)
}

# Check if the file exists in the directory
if (!file.exists(local_file)) {
  message("Downloading data from NYC Open Data...")
  
  # Download from the new API endpoint
  url <- "https://data.cityofnewyork.us/resource/833y-fsy8.json"  # Updated URL
  response <- GET(url)
  
  # Check if the request was successful
  if (status_code(response) == 200) {
    # Save raw JSON to the specified directory
    write(content(response, "text"), local_file)
  } else {
    stop("Failed to download data, status code: ", status_code(response))
  }
} else {
  message("Using previously downloaded data.")
}

# Load the data from the local file
shooting_data_historic <- fromJSON(local_file)

#join the data
library(dplyr)
shooting_data_2025$statistical_murder_flag <- as.character(shooting_data_2025$statistical_murder_flag)
shooting_data_historic$statistical_murder_flag <- as.character(shooting_data_historic$statistical_murder_flag)

full_shooting <- bind_rows(shooting_data_2025, shooting_data_historic)
#filter out to 2022


# Filter out rows with 'occur_day' before 1/1/2022
full_shooting_2022_2025 <- full_shooting |>
  filter(occur_date >="2021-12-31T00:00:00.000")

nrow(full_shooting_2022_2025)

#Get the 311 data 
# Set directory and local file path
dir_path <- "final_project"
local_file <- file.path(dir_path, "noise_311_data.json")  # Updated filename

# Check if the directory exists, create it if not
if (!dir.exists(dir_path)) {
  dir.create(dir_path)
}

# Check if the file already exists
if (!file.exists(local_file)) {
  message("Downloading 311 Noise Complaint data from NYC Open Data...")
  
  # Download from the 311 noise complaint dataset
  url <- "https://data.cityofnewyork.us/resource/p5f6-bkga.json"  # New dataset URL
  response <- GET(url)
  
  # Check for successful response
  if (status_code(response) == 200) {
    # Save JSON to file
    write(content(response, "text"), local_file)
  } else {
    stop("Failed to download data. Status code: ", status_code(response))
  }
} else {
  message("Using previously downloaded 311 data.")
}

# Load the data
noise_311_data <- fromJSON(local_file)

#filter to 2022
noise_311_data_2022_2025 <- noise_311_data|>
  filter(created_date >="2021-12-31T00:00:00.000")
nrow(noise_311_data_2022_2025)

#Exploration of shooting data:
#location description
loc_description <- full_shooting_2022_2025|>
  filter(location_desc != "(null)")|>
  count(location_desc)

loc_description

library(ggplot2)
ggplot(loc_description, aes(x = location_desc, y = n)) +
  geom_bar(stat = "identity", fill = "deepskyblue1" ) +
  labs(title = "Incident Locations", x = 'Location Description', y = "Number of Incidents ") +
  theme_minimal()+
  theme(
    panel.grid.minor = element_blank()
  )+
  coord_flip()

#borough 
borough <- full_shooting_2022_2025|>
  count(boro)|>
  rename( 'Incidents' = n)

ggplot(borough, aes(x = boro, y = Incidents, fill = Incidents)) +
  geom_bar(stat = "identity") +
  labs(title = "Shooting Reports by Borough", x = 'Borough', y = "Number of Incidents ") +
  scale_fill_gradient(low = "lightblue", high = "darkblue") +
  theme_minimal()+
  theme(
    panel.grid.minor = element_blank()
  )+
  coord_flip()

#noise complaint type 
loc_description_noise <- noise_311_data_2022_2025|>
  count(location_type)|>
  rename( Complaints = n)

loc_description

library(ggplot2)
ggplot(loc_description_noise, aes(x = location_type, y = Complaints)) +
  geom_bar(stat = "identity", fill = "orange" ) +
  labs(title = "Location of Complaints", x = 'Location Description', y = "Number of Complaints ") +
  theme_minimal()+
  theme(
    panel.grid.minor = element_blank()
  )+
  coord_flip()

#noise complain borough
borough_noise <- noise_311_data_2022_2025|>
  filter(borough != "Unspecified")|>
  count(borough)|>
  rename( 'Complaints' = n)

ggplot(borough_noise, aes(x = borough, y = Complaints, fill = Complaints)) +
  geom_bar(stat = "identity") +
  labs(title = "Noise Complaints by Borough", x = 'Borough', y = "Number of Complaints") +
  scale_fill_gradient(low = "orange", high = "darkorange4") +
  theme_minimal()+
  theme(
    panel.grid.minor = element_blank()
  )+
  coord_flip()


borough_noise <- noise_311_data_2022_2025|>
  filter(borough != "Unspecified", borough == "QUEENS")|>
  count(descriptor)

#time of day for shooting
library(lubridate)
time_hour <- full_shooting_2022_2025 |>
  mutate(Hour = hour(hms(occur_time)))

hourly_counts <- time_hour |>
  group_by(Hour)|>
  summarise(incident_count = n())

ggplot(hourly_counts, aes(x = Hour, y = incident_count)) +
  geom_point(color = "steelblue", size = 3, alpha = 0.7) +
  geom_smooth(method = "loess", se = FALSE, color = "deepskyblue", linewidth = 1.2) +
  scale_x_continuous(
    breaks = seq(0, 23, by = 2),
    limits = c(0, 23)
  ) +
  labs(
    x = "Hour of Day (24hrs)",
    y = "Number of Incidents",
    title = "Shooting Incidents by Hour"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 12),
    panel.grid.major = element_line(color = "grey90"),
    panel.grid.minor = element_blank()
  )

#noise complaints by hour 
library(tidyverse)
library(lubridate)

# Step 1: Extract hour from Created Date
library(stringr)
time_hour_noise <- noise_311_data_2022_2025 |>
  mutate(
    created_date_clean = str_replace(created_date, "T", " "),
    created_date_parsed = ymd_hms(created_date_clean),
    Hour = hour(created_date_parsed)
  ) |>
  filter(!is.na(Hour))

hourly_counts <- time_hour_noise |>
  group_by(Hour)|>
  summarise(incident_count = n())


# Step 3: Plot the distribution
ggplot(hourly_counts_noise, aes(x = Hour, y = incident_count)) +
  geom_point(color = "steelblue", size = 3, alpha = 0.7) +
  geom_smooth(method = "loess", se = FALSE, color = "deepskyblue", linewidth = 1.2) +
  scale_x_continuous(
    breaks = seq(0, 23, by = 2),
    limits = c(0, 23)
  ) +
  labs(
    x = "Hour of Day (24hrs)",
    y = "Number of Incidents",
    title = "Shooting Incidents by Hour"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 12),
    panel.grid.major = element_line(color = "grey90"),
    panel.grid.minor = element_blank()
  )
