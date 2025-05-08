#task 1 - US County Shapefile 
if (!require("utils")) install.packages("utils", dependencies = TRUE)

# Set directory and file info
dir_path <- "data/mp04"
file_name <- "cb_2023_us_county_5m.zip"
file_url <- paste0("https://www2.census.gov/geo/tiger/GENZ2023/shp/", file_name)
file_path <- file.path(dir_path, file_name)

# Create directory if it doesn't exist
if (!dir.exists(dir_path)) {
  dir.create(dir_path, recursive = TRUE)
  cat("Created directory:", dir_path, "\n")
}

#Download file if it doesn't already exist
if (!file.exists(file_path)) {
  download.file(file_url, destfile = file_path, mode = "wb")
  cat("Downloaded shapefile to:", file_path, "\n")
} else {
  cat("Shapefile already exists at:", file_path, "\n")
}

#Unzip the shapefile if not already unzipped
unzipped_files <- list.files(dir_path, pattern = "cb_2023_us_county_5m.*\\.shp$", full.names = TRUE)
if (length(unzipped_files) == 0) {
  unzip(file_path, exdir = dir_path)
  cat("Unzipped shapefile into:", dir_path, "\n")
} else {
  cat("Shapefile already unzipped in:", dir_path, "\n")
}

#Task 2 - Acquire 2024 use Presidential Election Results 
library(httr2)
library(rvest)
library(readr)
library(stringr)
library(dplyr)

# Create directory to store raw HTMLs
dir.create("data/election_pages", recursive = TRUE, showWarnings = FALSE)

# Helper function: generate Wikipedia slug
get_wikipedia_slug <- function(state_name) {
  slug <- str_replace_all(state_name, " ", "_")
  paste0("2024_United_States_presidential_election_in_", slug)
}

# Function to fetch and clean county-level election data for one state
get_state_election_results <- function(state_name) {
  slug <- get_wikipedia_slug(state_name)
  page_url <- paste0("https://en.wikipedia.org/wiki/", slug)
  file_path <- file.path("data/election_pages", paste0(slug, ".html"))
  
  # Download and cache HTML
  if (!file.exists(file_path)) {
    resp <- request(page_url) |> req_perform()
    writeBin(resp_body_raw(resp), file_path)
    message("Downloaded and saved: ", state_name)
  } else {
    message("Using cached: ", state_name)
  }
  
  # Read page and parse tables
  page <- read_html(file_path)
  tables <- page |> html_elements("table") |> html_table(fill = TRUE)
  
  # Identify the county-level results table
  target_table <- NULL
  for (tbl in tables) {
    col_names <- tolower(names(tbl))
    if (any(str_detect(col_names, "county|parish|borough|city and borough"))) {
      target_table <- tbl
      break
    }
  }
  
  if (is.null(target_table)) {
    warning("No county-level results table found for: ", state_name)
    return(NULL)
  }
  
  # Clean column names and add state
  names(target_table) <- make.names(names(target_table), unique = TRUE)
  
  clean_table <- target_table |>
    rename_with(~str_replace_all(tolower(.x), "\\s+", "_")) |>
    mutate(state = state_name)
  
  return(clean_table)
}

#get results for all 50 states 
states <- state.name  

# Fetch and store results
all_results <- lapply(states, function(st) {
  tryCatch(
    get_state_election_results(st),
    error = function(e) {
      message("Failed to get data for ", st, ": ", e$message)
      return(NULL)
    }
  )
})

combined_results_2024 <- bind_rows(all_results)
combined_results_2024

#Task 3 Acquire 2020 US Presidential Election Results
# Create directory to store raw HTMLs
dir.create("data/election_pages_2020", recursive = TRUE, showWarnings = FALSE)

# Wikipedia slug generator for 2020 pages
get_2020_slug <- function(state_name) {
  slug <- str_replace_all(state_name, " ", "_")
  paste0("2020_United_States_presidential_election_in_", slug)
}

# Function to fetch and clean county-level election data for one state (2020)
get_state_election_results_2020 <- function(state_name) {
  slug <- get_2020_slug(state_name)
  page_url <- paste0("https://en.wikipedia.org/wiki/", slug)
  file_path <- file.path("data/election_pages_2020", paste0(slug, ".html"))
  
  # Download and cache HTML
  if (!file.exists(file_path)) {
    resp <- request(page_url) |> req_perform()
    writeBin(resp_body_raw(resp), file_path)
    message("Downloaded and saved: ", state_name)
  } else {
    message("Using cached: ", state_name)
  }
  
  # Read page and parse tables
  page <- read_html(file_path)
  tables <- page |> html_elements("table") |> html_table(fill = TRUE)
  
  # Identify the county-level results table
  target_table <- NULL
  for (tbl in tables) {
    col_names <- tolower(names(tbl))
    if (any(str_detect(col_names, "county|parish|borough|city and borough"))) {
      target_table <- tbl
      break
    }
  }
  
  if (is.null(target_table)) {
    warning("No county-level results table found for: ", state_name)
    return(NULL)
  }
  
  # Clean column names and add state
  names(target_table) <- make.names(names(target_table), unique = TRUE)
  
  clean_table <- target_table |>
    rename_with(~str_replace_all(tolower(.x), "\\s+", "_")) |>
    mutate(state = state_name)
  
  return(clean_table)
}
#get all 50 states 
states <- state.name

# Fetch and store results
all_results_2020 <- lapply(states, function(st) {
  tryCatch(
    get_state_election_results_2020(st),
    error = function(e) {
      message("Failed to get data for ", st, ": ", e$message)
      return(NULL)
    }
  )
})

# Combine into one data frame
combined_results_2020 <- bind_rows(all_results_2020)

#Task 4 Join the Tables & shape file 
Joined_election_results <- combined_results_2020 |>
  inner_join(combined_results_2024, by = c( "county", "state"))

#checking columns 
names(Joined_election_results)

#shapefile 
shapefile_path <- list.files("data/mp04", pattern = "\\.shp$", full.names = TRUE)
# Read the shapefile
county_sf <- st_read(shapefile_path[1])

#combine joined data with shape file:
full_combined <- county_sf |>
  left_join(Joined_election_results, by = c("NAME" = "county", "STATE_NAME" = "state"))

#Task 4 - Exploratory Questions 
#Which county or counties cast the most votes for Trump (in absolute terms) in 2024?
most_trump_2024 <- combined_results_2024 |>
  filter(donald.trumprepublican == max(donald.trumprepublican, na.rm = TRUE))|>
  select(county, donald.trumprepublican, state)
#Renville in North Dakota (933, 82.2% of the vote)

#Which county or counties cast the most votes for Biden (as a fraction of total votes cast) in 2020?
library(dplyr)
library(tidyr)
combined_results_2020_joe <- combined_results_2020 |>
  select(county, joe.bidendemocratic, state, total.votes.cast)|>
  drop_na(total.votes.cast)

#convert to numbers for math
combined_results_2020_joe$total.votes.cast[combined_results_2020_joe$total.votes.cast == "NA"] <- NA
combined_results_2020_joe$joe.bidendemocratic[combined_results_2020_joe$joe.bidendemocratic == "NA"] <- NA
combined_results_2020_joe$total.votes.cast <- as.numeric(combined_results_2020_joe$total.votes.cast)
combined_results_2020_joe$joe.bidendemocratic <- as.numeric(combined_results_2020_joe$joe.bidendemocratic)

#find the fraction and sort
combined_results_2020_joe <- combined_results_2020_joe |>
  mutate(
    biden_vote_fraction = joe.bidendemocratic /total.votes.cast
  )|>
  arrange(desc(biden_vote_fraction))
#Kalawao County in Hawaii 

#Which county or counties had the largest shift towards Trump (in absolute terms) in 2024?
trump_shift <- Joined_election_results|>
  select(county, donald.trumprepublican.x,donald.trumprepublican.y, state)|>
  drop_na(donald.trumprepublican.x)|>
  filter(donald.trumprepublican.x != "NA", donald.trumprepublican.y != "NA")
  

#check to see if numeric for math
is.numeric(trump_shift$donald.trumprepublican.x)
is.numeric(trump_shift$donald.trumprepublican.y)

#convert to numeric 
trump_shift$donald.trumprepublican.x[trump_shift$donald.trumprepublican.x == "NA"] <- NA
trump_shift$donald.trumprepublican.y[trump_shift$donald.trumprepublican.y == "NA"] <- NA

trump_shift$donald.trumprepublican.x <- gsub(",", "", trump_shift$donald.trumprepublican.x)
trump_shift$donald.trumprepublican.x <- as.numeric(trump_shift$donald.trumprepublican.x)

trump_shift$donald.trumprepublican.y <- gsub(",", "", trump_shift$donald.trumprepublican.y)
trump_shift$donald.trumprepublican.y <- as.numeric(trump_shift$donald.trumprepublican.y)

#find the shift
trump_shift <- trump_shift |>
  mutate(
    change = donald.trumprepublican.y - donald.trumprepublican.x
  )|>
  drop_na(county)
biggest_shift<- trump_shift|>
  arrange(desc(change))

#costilla county in colorado -> shifter 109 votes to DT in 2024

#Which state had the largest shift towards Harris (or smallest shift towards Trump) in 2024?
harris_swing <- trump_shift|>
  group_by(state)|>
  summarise(harris_change = sum(change, na.rm = TRUE))|>
  filter(harris_change == min(harris_change, na.rm = TRUE))
#Texas had the biggest swing towards Harris in 2024

#What is the largest county, by area, in this data set
library(sf)

area_find <-full_combined
#find the area of each countt
area_find$areafull <- st_area(area_find$geometry)

largest_county <- area_find |>
  select(NAME, STATE_NAME, areafull)|>
  arrange(desc(areafull))
#biggest county area wise is Yukon-Koyuku in Alaska 380504049918 [m^2]

#Which county has the highest voter density (voters per unit of area) in 2020
total_voters_2020 <- area_find|>
  select(NAME, total.x , STATE_NAME, areafull)|>
  filter(total.x != "NA")

is.numeric(total_voters_2020$total.x)

total_voters_2020$total.x[total_voters_2020$total.x == "NA"] <- NA
total_voters_2020$total.x <- as.numeric(total_voters_2020$total.x)

#convert to KM
total_voters_2020$area_km2 <- total_voters_2020$areafull / 1e6
  
total_voters_2020 <- total_voters_2020 |>
  mutate(
    voter_density = total.x / area_km2
  )|>
  arrange(desc(voter_density))
#Harmon Oklahoma 0.6685674 voters per square mile 

#Which county had the largest increase in voter turnout in 2024?
cleaned_joined_election <- Joined_election_results|>
  select(county, total.x ,total.y, state)|>
  filter( total.x != "NA", total.y != "NA")

#change to numeric
cleaned_joined_election$total.x[cleaned_joined_election$total.x == "NA"] <- NA
cleaned_joined_election$total.x<- as.numeric(cleaned_joined_election$total.x)

cleaned_joined_election$total.y[cleaned_joined_election$total.y == "NA"] <- NA
cleaned_joined_election$total.y<- as.numeric(cleaned_joined_election$total.y)

#find increase in voter turnout 
cleaned_joined_election <- cleaned_joined_election |>
  mutate(
    voter_turn_out = total.y - total.x
  )|>
  filter(county != 'NA')|>
  arrange(desc(voter_turn_out))
#loving county Texas had the biggest increase in voter turn out 

#Task 7 
#merge right data
joined_for_figure <- inner_join(county_sf, trump_shift, by = c( "NAME" = "county"))
library(tidyr)
joined_for_figure <- joined_for_figure |>
  drop_na(change)

#reposition Alaska & Hawaii
library(sf)

merged <- st_transform(joined_for_figure, crs = 2163)

alaska <- merged[merged$STATEFP == "02", ]
hawaii <- merged[merged$STATEFP == "15", ]
lower48 <- merged[!(merged$STATEFP %in% c("02", "15")), ]


alaska <- st_transform(alaska, crs = 2163)
lower48 <- st_transform(lower48, crs = 2163)

if (is.na(st_crs(hawaii))) {
  st_crs(hawaii) <- 2163  # Assign the same CRS as the rest
}

merged <- bind_rows(lower48, alaska, hawaii) |>
  st_transform(crs = 4326)


#centroids
centroids <- st_centroid(merged)
max_shift <- max(abs(merged$change), na.rm = TRUE)
centroids <- centroids %>%
  mutate(
    coords = st_coordinates(geometry),
    x = coords[, 1],
    y = coords[, 2],
    xend = x + (change / max_shift) * 0.5,
    yend = y
  )

#plot
library(ggplot2)
ggplot() +
  geom_sf(data = merged, fill = "gray90", color = "white", size = 0.1) +
  geom_segment(
    data = centroids,
    aes(x = x, y = y, xend = xend, yend = yend, color = change),
    arrow = arrow(length = unit(0.08, "inches")),
    size = 0.3
  ) +
  scale_color_gradient2(
    low = "blue", mid = "violet", high = "red",
    midpoint = 0, name = "Rightward Shift (#of votes"
  ) +
  theme_minimal() +
  labs(title = "County-Level 2024 Presidential Shift (Reproduced from NYT)") +
  theme(legend.position = "bottom") +
   coord_sf()


  
