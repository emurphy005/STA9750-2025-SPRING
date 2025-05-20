library(readr)
library(ggplot2)
library(tidyverse)
library(dplyr)

nyc_crime <- read_csv(file.choose())
glimpse(nyc_crime)
nyc_crime
#Missing Data
missing_percentage <- colSums(is.na(nyc_crime)) / nrow(nyc_crime) * 100
missing_percentage<- 
available_percentage <- 100 - missing_percentage 


missing_df <- data.frame(
  column_name = names(missing_percentage),
  available_percentage = available_percentage
)

ggplot(missing_df, aes(x = reorder(column_name, available_percentage), y = available_percentage)) +
  geom_bar(stat = "identity", fill = "green") +  # Green for available data
  labs(title = "Data Quality Assetment - NYC Shooting",
       x = "Column", y = "Available Percentage (%)") +
  theme_minimal() +
  coord_flip() +  # Flip to make column names more readable
  scale_y_continuous(labels = scales::percent_format(scale = 1))
  

#null in location
loc_description <- nyc_crime|>
  filter(LOCATION_DESC != "(null)")|>
  count(LOCATION_DESC)
  
loc_description

ggplot(loc_description, aes(x = LOCATION_DESC, y = n)) +
  geom_bar(stat = "identity", fill = "plum" ) +
  labs(title = "Incident Locations", x = 'Location', y = "# of Incidents ") +
  theme_minimal()+
  theme(
    panel.grid.minor = element_blank()
  )+
  coord_flip()

colors()

ggplot(nyc_crime, aes(x))

#time of day explorations 
library(lubridate)
time_hour <- nyc_crime |>
  mutate(HOUR = hour(hms(OCCUR_TIME)))

hourly_counts <- time_hour |>
  group_by(HOUR)|>
  summarise(incident_count = n())

ggplot(hourly_counts, aes(x = HOUR, y = incident_count)) +
  geom_point(color = "steelblue") +
  geom_smooth(method = "loess", se = FALSE, color = "skyblue") +
  labs(x = "Hour of Day (24hrs)", y = "# of Incidents", title = "Incidents by Hour of Day") +
  theme_minimal()





