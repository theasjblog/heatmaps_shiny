library(dplyr)
library(jsonlite)
library(magrittr)
library(tidyr)
library(arrow)
library(googleCloudStorageR)

# Authenticate (this will open a browser for login)
gcs_auth(json_file = file.choose())


# Function to extract lat/lon and activity type from a single activity JSON file
extract_gps_with_type <- function(filepath) {
  activity <- tryCatch(fromJSON(filepath), error = function(e) NULL)
  if (is.null(activity)) return(NULL)
  
  time_stamp <- as.POSIXct(
    tools::file_path_sans_ext(basename(filepath)),
    format = "%Y_%m_%d_%H_%M_%S",
    tz = "UTC"
  )
  
  lat <- activity$RIDE$SAMPLES$LAT
  lon <- activity$RIDE$SAMPLES$LON
  sport <- toupper(trimws(ifelse(is.null(activity$RIDE$TAGS$Sport), NA, activity$RIDE$TAGS$Sport), 'both'))
  if (is.null(lat) || is.null(lon) || length(lat) != length(lon)) return(NULL)
  
  df <- data.frame(lat = lat, lon = lon, filepath = filepath, 
                   time_stamp = time_stamp)
  df <- df |>
    filter(!is.na(lat), !is.na(lon)) |>
    filter(lat >= -90, lat <= 90, lon >= -180, lon <= 180) |>
    filter(!(lat == 0 & lon == 0))
  df
}

download.file('https://storage.googleapis.com/blogs_josa/sport/parquets/heathmap_all_points.parquet',
              destfile = file.path('data', 'heathmap_all_points.parquet'))
old_data <- open_dataset(file.path('data', 'heathmap_all_points.parquet')) |>
  collect()

# Set your bucket name
gcs_global_bucket("blogs_josa")

# List files in a specific folder
objects <- gcs_list_objects(prefix = "sport/parquets/activities")

# View file names
file_names <- objects$name
google_files_names <- file_names[file_names != "sport/parquets/activities/"]


list_files <- list.files(
  '/Users/adrianstevejoseph/Library/GoldenCheetah/Adrian/activities', 
  full.names = TRUE
)

files_to_copy <- setdiff(list_files, paste0('/Users/adrianstevejoseph/Library/GoldenCheetah/Adrian/activities/',
                                            basename(google_files_names)))
if(dir.exists('to_google')){
  unlink('to_google', force = TRUE, recursive = TRUE)
}
dir.create('to_google')
for(i in files_to_copy){
  file.copy(from = i,
            to = file.path('to_google', basename(i)))
}



if(length(files_to_copy)>0){
  all_activities <- lapply(files_to_copy, function(d){
    a <- jsonlite::fromJSON(d)
    df <- data.frame(
      filepath = d,
      time_stamp = as.POSIXct(tools::file_path_sans_ext(basename(d)),
                              format = "%Y_%m_%d_%H_%M_%S",
                              tz = "UTC"),
      sport = toupper(trimws(ifelse(is.null(a$RIDE$TAGS$Sport), NA, a$RIDE$TAGS$Sport), 'both')),
      subsport = toupper(trimws(ifelse(is.null(a$RIDE$TAGS$SubSport), NA, a$RIDE$TAGS$SubSport), 'both')),
      hasGPS = !is.null(a$RIDE$SAMPLES$LAT)
    )
    df
  }) |>
    dplyr::bind_rows()
  
  all_activities_gps <- all_activities |>
    filter(hasGPS)
  if(nrow(all_activities_gps)>0){
    # Extract data
    all_points <- lapply(all_activities_gps$filepath, extract_gps_with_type) |>
      dplyr::bind_rows() |>
      dplyr::left_join(
        all_activities |>
          select(all_of(c(
            'filepath', 'sport'
          ))),
        by = 'filepath'
      ) |>
      dplyr::bind_rows(old_data) |>
      distinct()
    arrow::write_parquet(old_data, file.path('data', 'heathmap_all_points.parquet'))
  }
  
}



