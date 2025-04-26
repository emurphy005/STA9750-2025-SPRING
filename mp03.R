#Task 1 - song characteristics data set 
load_songs <- function() {
  # Set up the directory and file paths
  dir_path <- "data/mp03"
  file_name <- "data.csv"
  file_path <- file.path(dir_path, file_name)
  url <- "https://raw.githubusercontent.com/gabminamedez/spotify-data/refs/heads/master/data.csv"
  
  # Create the directory if it doesn't exist
  if (!dir.exists(dir_path)) {
    dir.create(dir_path, recursive = TRUE)
  }
  
  # Download the file if it's not already present
  if (!file.exists(file_path)) {
    download.file(url, destfile = file_path, mode = "wb")
  }
  
  # Read the CSV file into a data frame
  df <- read.csv(file_path, stringsAsFactors = FALSE)
  
  # Return the data frame
  return(df)
}

songs <- load_songs()


#spliting artist into multiple lines 
library(tidyr)
library(stringr)
library(dplyr)
clean_artist_string <- function(x){
  x |>
    str_replace_all("\\['", "") |>
    str_replace_all("'\\]", "") |>
    str_replace_all("[ ]?'", "") |>
    str_replace_all("[ ]*,[ ]*", ",") 
}

full_song <- songs |> 
  separate_longer_delim(artists, ",") |>
  mutate(artist = clean_artist_string(artists)) |>
  select(-artists)

full_song

#Task 2 - Playlists 
load_playlists <- function(local_dir = "spotify_data", 
                           base_url = "https://raw.githubusercontent.com/DevinOgrady/spotify_million_playlist_dataset/main/data1/",
                           file_slices = seq(0, 49000, by = 1000)) { #reducing number of files downloaded to be processed for performance reasons per instructor permission
  # Load required package
  if (!require(jsonlite)) install.packages("jsonlite", dependencies = TRUE)
  library(jsonlite)
  
  # Create local directory if it doesn't exist
  if (!dir.exists(local_dir)) {
    dir.create(local_dir)
  }
  
  playlist_list <- list()
  
  for (start in file_slices) {
    end <- start + 999
    file_name <- sprintf("mpd.slice.%d-%d.json", start, end)
    file_path <- file.path(local_dir, file_name)
    file_url <- paste0(base_url, file_name)
    
    # Download only if not already downloaded
    if (!file.exists(file_path)) {
      message("Downloading ", file_name)
      tryCatch({
        download.file(file_url, destfile = file_path, mode = "wb", quiet = TRUE)
      }, error = function(e) {
        message("Failed to download: ", file_name)
      })
    }
    
    if (file.exists(file_path)) {
      message("Reading ", file_name)
      data <- fromJSON(file_path, simplifyVector = FALSE)
      
      # ✅ Append list of playlists
      playlist_list <- c(playlist_list, data$playlists)
    }
  }
  
  return(playlist_list)
}

all_playlists <- load_playlists()
length(all_playlists)


#Task 3 - Rectangle the playlist data
library(purrr)
library(data.table)
strip_spotify_prefix <- function(x){
  str_match(x, ".*:.*:(.*)")[,2]
}
playlist_rows <- vector("list", length(all_playlists))

for (i in seq_along(all_playlists)) {
  playlist <- all_playlists[[i]]
  tracks <- playlist$tracks
  
  if (length(tracks) > 0) {
    track_rows <- lapply(tracks, function(track) {
      list(
        playlist_name = playlist$name,
        playlist_id = playlist$pid,
        playlist_position = track$pos,
        playlist_followers = playlist$num_followers,
        artist_name = track$artist_name,
        artist_id = strip_spotify_prefix(track$artist_uri),
        track_name = track$track_name,
        track_id = strip_spotify_prefix(track$track_uri),
        album_name = track$album_name,
        album_id = strip_spotify_prefix(track$album_uri),
        duration = track$duration_ms
      )
    })
    playlist_rows[[i]] <- track_rows
  }
}

# Flatten the nested list and convert to data.table then tibble
flat_list <- do.call(c, playlist_rows)
tidy_playlists <- as_tibble(rbindlist(flat_list, fill = TRUE))

tidy_playlists 

#Task 4 - Initial Exploration
#Number of distinct tracks 
n_disctict_tracks <- tidy_playlists |>
  summarise(distinct_tracks = n_distinct(track_id))
n_disctict_tracks #206451 tracks 

#Number of distinct artists 
n_disctict_artists <- tidy_playlists |>
  summarise(distinct_artists = n_distinct(artist_id))
n_disctict_artists #42327 artists 

#what are the five most popular tracks?
top_tracks <- tidy_playlists |>
  group_by(track_id, track_name, artist_name) |>
  summarise(apperances = n(), .groups = "drop") |>
  arrange(desc(apperances))|>
  slice_head(n = 5)
top_tracks
# Humble - Kendrick Lamar 589 appearances
# Once Dance - Drake 562 appearances 
# Closer - The Chainsmokers 543 appearances
# Broccoli (feat. Lil Yachty) - DRAM 525 appearances 
# Congratulations - Post Malone 511 appearances 

colnames(tidy_playlists)
colnames(full_song)

#most popular track in the playlist data that does not have a corresponding entry in the song characteristics data
missing_tracks <- tidy_playlists |>
  anti_join(full_song, by = c("track_id" = "id"))

most_popular_missing <- missing_tracks %>%
  group_by(track_id, track_name, artist_name) %>%
  summarise(appearances = n(), .groups = "drop") %>%
  arrange(desc(appearances)) %>%
  slice_head(n = 1)

most_popular_missing  #One dance by Drake 

#Song with most Danceability 
dance_track <- full_song |>
  arrange(desc(danceability))|>
  slice_head(n = 1)
dance_track # Funky Cold Medina by Tone-Loc 
#appearances
most_danceable <- "5YIF6HSOtHN9HdcE5IPzMe"
song_count <- tidy_playlists |>
  filter(track_id == most_danceable) |>
  summarise(appearances = n())
song_count # 5 appearances 

#playlist with the longest average track length 
longest_ave_playlist <- tidy_playlists |>
  group_by(playlist_id, playlist_name) |>
  summarise(avg_duration = mean(duration, na.rm = TRUE), .groups = "drop") |>
  arrange(desc(avg_duration)) |>
  slice_head(n = 1)

longest_ave_playlist # sleep duration 1053798

#most popular playlist 
most_popular_playlist <- tidy_playlists |>
  distinct(playlist_id, playlist_name, playlist_followers) |>
  arrange(desc(playlist_followers))|>
  slice_head(n=1)

most_popular_playlist #TOP POP 15842 followers

#inner joining the playlists 
joined_data <- tidy_playlists |>
  inner_join(full_song, by = c("track_id" = "id"))

joined_data

#Task 5 - Visually Identifying Characteristics of Popular songs 
#identify popular songs and playlist appearances 
Popular_track <- joined_data |>
  count(track_id, artist_name, track_name, popularity, name = "playlist_count")|>
  arrange(desc(playlist_count))


popular_track_score <-joined_data |>
  count(track_id, artist_name, track_name, popularity, name = "playlist_count")|>
  arrange(desc(popularity))


Popular_track <- Popular_track |>
  mutate(popularity_tier = cut(
    popularity,
    breaks = c(0, 40, 75, 100),
    labels = c("Low", "Medium", "High"),
    include.lowest = TRUE
  ))

#correlation
cor(Popular_track$playlist_count, Popular_track$popularity, use = "complete.obs")
#.390 not much correlation

#popular song has to have a score over 75 - Champions by Kanye West had most playlist counts and is under 75 threshold at 68. 
#While goosebumps by Travis Scott had the highest popularity score (92) and only appeared 410 in the playlists. 
#plot
library(ggplot2)

ggplot(Popular_track, aes(x = playlist_count, y = popularity, color = popularity_tier)) +
  geom_point(alpha = 0.6, size = 2) +
  scale_color_manual(
    values = c("Low" = "palegreen", "Medium" = "green", "High" = "green4")
  ) +
  labs(
    title = "Playlist Appearances vs. Popularity",
    subtitle = "Colored by Popularity Tier",
    x = "Number of Playlist Appearances",
    y = "Popularity Score",
    color = "Popularity Tier",
    caption = "Spotify Million Playlist Dataset"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", size = 18),
    plot.subtitle = element_text(size = 13, color = "gray40"),
    legend.position = "top"
  )

#year were the most popular songs were released 
library(lubridate)
popular_song_year <- joined_data |>
  filter(popularity >= 75)|>
  mutate(release_date = parse_date_time(release_date, orders = c("mdy", "dmy", "ymd", "mdy HMS"))) |>
  mutate(year_released = year(release_date))


top_years <- popular_song_year |>
  filter(!is.na(year_released)) |>
  group_by(year_released)|>
  summarise(count = n())|>
  arrange(desc(count))
top_years
top_5_years <- head(top_years, 5)
#plot
ggplot(top_5_years, aes(x = factor(year_released), y = count, fill = factor(year_released))) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = c("2011" ="darkseagreen1", "2014" = "palegreen3", "2015" = "limegreen", "2016" = "green3", "2017" = "green4")) +
  labs(
    title = "Top 5 Years Most Popular Songs Were Released",
    x = "Year",
    y = "Number of Popular Songs",
    fill = "Year"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "none"
  )

#dancaebility peak
##convert years to same format 
danceability_year <- joined_data |>
  mutate(release_date = parse_date_time(release_date, orders = c("mdy", "dmy", "ymd", "mdy HMS")))|>
  filter(!is.na(release_date)) |>
  mutate(year_released = year(release_date))
danceability_year
#peak
danceability_peak <- danceability_year |>
  filter(
    !is.na(danceability), 
    !is.na(year_released),
    year_released <= 2025) |>
  group_by(year_released) |>
  summarise(avg_danceability = mean(danceability)) |>
  arrange(desc(avg_danceability))
danceability_peak

#plot - peaked in 2017 @ .717
ggplot(danceability_peak, aes(x = year_released, y = avg_danceability)) +
  geom_line(color = "springgreen4", size = 1) +
  geom_point(color = "springgreen3", size = 2) +
  labs(
    title = "Average Danceability by Year",
    x = "Year Released",
    y = "Average Danceability"
  ) +
  theme_minimal()

#decade represented the most 
playlist_decades <- joined_data |>
  mutate(release_date = parse_date_time(release_date, orders = c("mdy", "dmy", "ymd", "mdy HMS")))|>
  filter(!is.na(release_date)) |>
  mutate(year_released = year(release_date))

playlist_decades <- playlist_decades |>
  mutate(decade = (year_released %/% 10) * 10) %>%  
  count(decade, name = "num_appearances") %>%
  arrange(desc(num_appearances))

playlist_decades <-head(playlist_decades, 6) # highest 2010
library(scales)
ggplot(playlist_decades, aes(x = factor(decade), y = num_appearances)) +
  geom_bar(stat = "identity", fill = "#66c2a5") +
  scale_y_continuous(labels = comma) +
  labs(
    title = "Most Represented Decades on Playlists",
    x = "Decade",
    y = "Number of Appearances"
  ) +
  theme_minimal()

#key counts
key_counts <- joined_data |>
  filter(!is.na(key)) |>
  group_by(key) |>
  summarise(count = n()) |>
  ungroup()

ggplot(key_counts, aes(x = factor(key), y = count)) +
  geom_bar(stat = "identity", fill = "#8da0cb") +
  coord_polar(start = 0) +
  labs(
    title = "Frequency of Musical Keys",
    x = "Key",
    y = "Count"
  ) +
  theme_minimal()

#track lengths 
track_length <- joined_data |>
  mutate(track_length_seconds = duration_ms / 1000) |>
  mutate(track_length_category = case_when(
    track_length_seconds < 180 ~ "Short (< 3 min)",       # Less than 3 minutes
    track_length_seconds >= 180 & track_length_seconds <= 300 ~ "Medium (3-5 min)",  # Between 3-5 minutes
    track_length_seconds > 300 ~ "Long (> 5 min)"          # More than 5 minutes
  )) |>
  group_by(track_length_category) |>
  summarise(count = n()) |>
  arrange(desc(count))
track_length
#3-5 mins is the post popular track length 

#plot
ggplot(track_length, aes(x = track_length_category, y = count, fill = track_length_category)) +
  geom_bar(stat = "identity", color = "grey27") +
  scale_y_continuous(labels = comma) +
  labs(
    title = "Track Length Distribution in User Playlists",
    x = "Track Length",
    y = "Number of Tracks"
  ) +
  scale_fill_manual(values = c("Short (< 3 min)" = "#66c2a5", 
                               "Medium (3-5 min)" = "mediumspringgreen", 
                               "Long (> 5 min)" = "#8da0cb")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 

#track popularity vs track length
ggplot(joined_data, aes(x = duration_ms / 1000, y = popularity)) +
  geom_point(alpha = 0.5, color = "seagreen4") +
  labs(title = "Track Popularity vs. Track Length",
       x = "Track Length (seconds)",
       y = "Track Popularity") +
  theme_minimal()

#average popularity vs track length
track_popularity_vs_length <- joined_data |>
  mutate(track_length_seconds = duration_ms / 1000) |>
  mutate(track_length_category = case_when(
    track_length_seconds < 180 ~ "Short (< 3 min)",       # Less than 3 minutes
    track_length_seconds >= 180 & track_length_seconds <= 300 ~ "Medium (3-5 min)",  # Between 3-5 minutes
    track_length_seconds > 300 ~ "Long (> 5 min)"          # More than 5 minutes
  )) |>
  group_by(track_length_category) |>
  summarise(
    avg_popularity = mean(popularity, na.rm = TRUE),    # Average popularity for each category
    avg_length = mean(track_length_seconds, na.rm = TRUE) # Average length for each category
  ) |>
  arrange(desc(avg_popularity))

track_popularity_vs_length
#plot
ggplot(track_popularity_vs_length, aes(x = track_length_category, y = avg_popularity, fill = track_length_category)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  geom_text(aes(label = round(avg_popularity, 1)), vjust = -0.5, color = "black") +
  labs(title = "Average Track Popularity by Length Category",
       x = "Track Length Category",
       y = "Average Popularity") +
  theme_minimal() +
  scale_fill_manual(values = c("Short (< 3 min)" = "darkolivegreen2", 
                               "Medium (3-5 min)" = "darkolivegreen3", 
                               "Long (> 5 min)" = "darkolivegreen4")) 

#popularity by artist
popularity_by_artist <- joined_data |>
  group_by(artist_name) |>
  summarise(avg_popularity = mean(popularity, na.rm = TRUE)) |>
  arrange(desc(avg_popularity))
popularity_by_artist
top_ten <-head(popularity_by_artist, 10)
top_ten


# plot
ggplot(top_ten, aes(x = reorder(artist_name, avg_popularity), y = avg_popularity, fill = avg_popularity)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  coord_flip() +  # Flip the axes for better readability
  labs(title = "Average Track Popularity by Artist",
       x = "Artist Name",
       y = "Average Popularity") +
  theme_minimal() +
  scale_fill_gradient(low = "lightgreen", high = "darkgreen")

#Task 6 
anchor_song_one<- "Closer" #by the chainsmokers
anchor_song_two <-'YOUTH' #troye sivan 

anchor_songs_1 <- joined_data |>
  filter(track_name %in% c("Closer"))|>
  select(artist_name, track_name, duration, year, acousticness, key, tempo)|>
  distinct(track_name, .keep_all = TRUE)

anchor_songs_1

#finding similar songs 
similar_songs <-joined_data |>
  filter(track_name %in% c(anchor_song_one, anchor_song_two)) |>
  pull(playlist_id)
  
songs_on_same_playlists <- joined_data %>%
  filter(playlist_id %in% similar_songs)

songs_on_same_playlists %>%
  select(track_name, artist_name, year, tempo, key, popularity, acousticness, danceability) %>%
  filter(year %in% c("2015", "2016"))|>
  distinct()|>
  print(n=200)

#list of 20
songs_on_same_playlists_20 <- songs_on_same_playlists |>
  select(track_name, artist_name, year, tempo, key, popularity, acousticness, danceability) |>
  filter(between(tempo, 85, 110), danceability >= 0.6, year %in% c('2015', '2016'), artist_name %in% c("The Chainsmokers", "Troye Sivan", "Fifth Harmony", "Kygo", 'Zac Brown Band', 'Flo Rida', 'Matoma', 'The Weeknd', "ALex Newell",
                                                                                  "Little Mix", "Adele", "Timeflies", "Selena Gomez", "Twenty One Pilots", "Chris Brown","BØRNS", "KYLE"  ))|>
  distinct()|>
  print(n = 100)

library(DT)
list_of_20 <-datatable(songs_on_same_playlists_20,options=list(searching=FALSE, 
                                                               scrollY = "400px",
                                                               scrollyX = TRUE,
                                                               paging = FALSE,
                                                               fixedHeader = TRUE,
                                                               info=FALSE))
list_of_20

#Task 7 - Ultimate Playlist 
top_songs <- songs_on_same_playlists |>
  select(track_name, artist_name, year, tempo, popularity, acousticness, danceability) |>
  filter(between(tempo, 85, 110), danceability >= 0.6, acousticness <=.42, year %in% c('2015', '2016'), artist_name %in% c("The Chainsmokers", "Troye Sivan", "Fifth Harmony", "Kygo", 'Flo Rida', 'Matoma', 'The Weeknd', "ALex Newell",
                                                                                                       "Little Mix", "Adele", "Timeflies", "Selena Gomez", "Twenty One Pilots","BØRNS", "KYLE"  ))|>
  distinct()

top_songs
top_song_table <-datatable(top_songs,options=list(searching=FALSE, 
                                                  scrollY = "400px",
                                                  scrollyX = TRUE,
                                                  paging = FALSE,
                                                  fixedHeader = TRUE,
                                                  info=FALSE))

top_song_table
  
  