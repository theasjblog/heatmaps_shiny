library(dplyr)
library(jsonlite)
library(parallel)


list_files <- list.files(
  '/Users/adrianstevejoseph/Library/GoldenCheetah/Adrian/activities', 
  full.names = TRUE
)

num_cores <- detectCores() - 1  # Leave one core free
cl <- makeCluster(num_cores)

clusterExport(cl, varlist = c("list_files"), envir = environment())
clusterEvalQ(cl, library(jsonlite))  # Load needed packages on workers


all_activities <- parLapply(cl, list_files, function(d){
  a <- jsonlite::fromJSON(d)
  df <- data.frame(
    filepath = d,
    timestamp = as.POSIXct(tools::file_path_sans_ext(basename(d)),
                           format = "%Y_%m_%d_%H_%M_%S",
                           tz = "UTC"),
    sport = toupper(trimws(ifelse(is.null(a$RIDE$TAGS$Sport), NA, a$RIDE$TAGS$Sport), 'both')),
    subsport = toupper(trimws(ifelse(is.null(a$RIDE$TAGS$SubSport), NA, a$RIDE$TAGS$SubSport), 'both')),
    hasGPS = !is.null(a$RIDE$SAMPLES$LAT)
  )
  df
}) |>
  dplyr::bind_rows()

parallel::stopCluster(cl)

# to do: edit these raw files to add bike as sport
missing_sport_bike <- c(
  which(all_activities$sport == ""),
  which(is.na(all_activities$sport))
)
wrong_subsport <- c(
  which(all_activities$filepath == "/Users/adrianstevejoseph/Library/GoldenCheetah/Adrian/activities/2025_07_17_19_31_14.json")
)
# should be turbo_trainer

all_activities$sport[missing_sport_bike] <- 'BIKE'
all_activities$subsport[wrong_subsport] <- 'TURBO_TRAINER'


rides <- all_activities |>
  filter(hasGPS) |>
  filter(as.Date(timestamp) >= as.Date("2025-01-01")) |>
  filter(sport != 'TRANSITION') |>
  filter(sport == 'BIKE') |>
  filter(subsport == "OUTDOOR_RIDE")




#########
# OPTION 1
# Load required libraries
library(jsonlite)
library(dplyr)
library(leaflet)
library(leaflet.extras)

# Function to extract lat/lon from a single activity JSON file
extract_gps <- function(filepath) {
  activity <- tryCatch(fromJSON(filepath), error = function(e) NULL)
  
  if (is.null(activity)) return(NULL)
  
  lat <- activity$RIDE$SAMPLES$LAT
  lon <- activity$RIDE$SAMPLES$LON
  
  if (is.null(lat) || is.null(lon) || length(lat) != length(lon)) return(NULL)
  
  sport <- toupper(trimws(ifelse(is.null(activity$RIDE$TAGS$Sport), NA, activity$RIDE$TAGS$Sport), 'both'))
  
  data.frame(lat = lat, lon = lon,
             sport = sport)
}


# Extract GPS data from each file
gps_data_list <- lapply(rides$filepath, extract_gps)

# Combine all GPS points into a single data frame
gps_points <- bind_rows(gps_data_list)

# Optional: clean invalid coordinates
gps_points <- gps_points |>
  filter(!is.na(lat), !is.na(lon)) |>
  filter(lat >= -90, lat <= 90, lon >= -180, lon <= 180) |>
  filter(!(lat == 0 & lon == 0))

# Round GPS coordinates to reduce jitter
gps_rounded <- gps_points |>
  mutate(
    lat = round(lat, 4),
    lon = round(lon, 4)
  )

# Count occurrences of each rounded point
gps_weighted <- gps_rounded |>
  count(lat, lon, name = "weight")



leaflet() %>%
  addTiles() %>%
  
  # Tracks layer (purple)
  {
    map <- .
    for (track in tracks) {
      if (nrow(track) < 2) next
      map <- map %>% addPolylines(
        lng = track$lon,
        lat = track$lat,
        color = "purple",
        weight = 2,
        opacity = 0.6,
        group = "Tracks"
      )
    }
    map
  } %>%
  
  # Heatmap layer
  addHeatmap(
    data = gps_weighted,
    lng = ~lon,
    lat = ~lat,
    intensity = ~weight,
    blur = 20,
    radius = 15,
    max = max(gps_weighted$weight, na.rm = TRUE) / 10,
    group = "Heatmap"
  ) %>%
  
  # Layers control UI
  addLayersControl(
    overlayGroups = c("Tracks", "Heatmap"),
    options = layersControlOptions(collapsed = FALSE)
  )

# IDEAS
# Add toggles/layer control for activity type
# Color by activity type








library(jsonlite)
library(dplyr)
library(leaflet)
library(leaflet.extras)
library(magrittr)
library(tidyr)

# Function to extract lat/lon from a single activity JSON file
extract_gps <- function(filepath) {
  activity <- tryCatch(fromJSON(filepath), error = function(e) NULL)
  if (is.null(activity)) return(NULL)
  
  lat <- activity$RIDE$SAMPLES$LAT
  lon <- activity$RIDE$SAMPLES$LON
  if (is.null(lat) || is.null(lon) || length(lat) != length(lon)) return(NULL)
  
  df <- data.frame(lat = lat, lon = lon)
  df <- df |>
    filter(!is.na(lat), !is.na(lon)) |>
    filter(lat >= -90, lat <= 90, lon >= -180, lon <= 180) |>
    filter(!(lat == 0 & lon == 0))
}


# Extract all tracks
tracks <- lapply(rides$filepath, extract_gps)
tracks <- tracks[!sapply(tracks, is.null)]  # remove NULL entries

# Combine all GPS points from all tracks into one data frame
gps_points <- do.call(rbind, tracks)

# Optional: clean invalid coordinates
gps_points <- gps_points |>
  filter(!is.na(lat), !is.na(lon)) |>
  filter(lat >= -90, lat <= 90, lon >= -180, lon <= 180) |>
  filter(!(lat == 0 & lon == 0))


# Round coordinates to reduce GPS jitter
gps_rounded <- gps_points %>%
  mutate(
    lat = round(lat, 4),
    lon = round(lon, 4)
  )

# Count occurrences of each rounded point (weight for heatmap)
gps_weighted <- gps_rounded %>%
  count(lat, lon, name = "weight")

# Build the leaflet map
leaflet() %>%
  addTiles() %>%
  
  # Add all tracks as purple polylines, grouped "Tracks"
  {
    map <- .
    for (track in tracks) {
      if (nrow(track) < 2) next
      map <- map %>%
        addPolylines(
          lng = track$lon,
          lat = track$lat,
          color = "purple",
          weight = 2,
          opacity = 0.6,
          group = "Tracks"
        )
    }
    map
  } %>%
  
  # Add heatmap layer, grouped "Heatmap"
  addHeatmap(
    data = gps_weighted,
    lng = ~lon,
    lat = ~lat,
    intensity = ~weight,
    blur = 20,
    radius = 15,
    max = max(gps_weighted$weight, na.rm = TRUE) / 3,
    group = "Heatmap"
  ) %>%
  
  # Add layer control to toggle visibility
  addLayersControl(
    overlayGroups = c("Tracks", "Heatmap"),
    options = layersControlOptions(collapsed = FALSE)
  )














library(jsonlite)
library(dplyr)
library(leaflet)
library(leaflet.extras)
library(magrittr)
library(tidyr)
library(RColorBrewer)

# Function to extract lat/lon and activity type from a single activity JSON file
extract_gps_with_type <- function(filepath) {
  activity <- tryCatch(fromJSON(filepath), error = function(e) NULL)
  if (is.null(activity)) return(NULL)
  
  lat <- activity$RIDE$SAMPLES$LAT
  lon <- activity$RIDE$SAMPLES$LON
  sport <- toupper(trimws(ifelse(is.null(activity$RIDE$TAGS$Sport), NA, activity$RIDE$TAGS$Sport), 'both'))
  if (is.null(lat) || is.null(lon) || length(lat) != length(lon)) return(NULL)
  
  df <- data.frame(lat = lat, lon = lon, filepath = filepath)
  df <- df |>
    filter(!is.na(lat), !is.na(lon)) |>
    filter(lat >= -90, lat <= 90, lon >= -180, lon <= 180) |>
    filter(!(lat == 0 & lon == 0))
  df
}


# Extract data
all_points <- lapply(rides$filepath, extract_gps_with_type) |>
  dplyr::bind_rows() |>
  dplyr::left_join(
    all_activities |>
      select(all_of(c(
        'filepath', 'sport'
      ))),
    by = 'filepath'
  ) |>
  mutate(
    sport = ifelse(sport == "WALKING", 'HIKING', sport)
  )

all_tracks <- split(all_points, all_points$filepath)

# Dynamically assign colors to each sport
unique_sports <- unique(all_points$sport)
n_sports <- length(unique_sports)

# Use RColorBrewer if <= 12 sports, else rainbow
if (n_sports <= 8) {
  palette <- brewer.pal(n_sports, "Dark2")
} else {
  palette <- rainbow(n_sports)
}
activity_colors <- setNames(palette, unique_sports)


# Create map
m <- leaflet() %>%
  addTiles()

# Add tracks by activity type
for (fp in names(all_tracks)) {
  track <- all_tracks[[fp]]
  sport <- unique(track$sport)
  if (is.na(sport) || length(sport) == 0) sport <- "OTHER"
  color <- activity_colors[[sport]]
  if (is.null(color)) color <- "black"
  
  m <- m %>%
    addPolylines(
      data = track,
      lng = ~lon, lat = ~lat,
      color = color,
      weight = 2,
      opacity = 0.7,
      group = sport
    )
}

# Create rounded GPS points for heatmap
gps_rounded <- all_points %>%
  mutate(lat = round(lat, 4), lon = round(lon, 4)) %>%
  count(lat, lon, name = "weight")

# Add heatmap on top
m <- m %>%
  addHeatmap(
    data = gps_rounded,
    lng = ~lon,
    lat = ~lat,
    intensity = ~weight,
    blur = 20,
    radius = 15,
    max = max(gps_rounded$weight, na.rm = TRUE) / 10,
    group = "Heatmap"
  )

# Add layer control
m <- m %>%
  addLayersControl(
    overlayGroups = c(names(activity_colors), "Heatmap"),
    options = layersControlOptions(collapsed = FALSE)
  )

# Show map
m

a <- read.csv(file.choose())
