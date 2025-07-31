get_colors <- function(unique_sports){
  
  # Dynamically assign colors to each sport
  n_sports <- length(unique_sports)
  
  if(n_sports <= 4){
    palette <- c("red", "green", "blue", "grey")
  }
  
  # Use RColorBrewer if <= 12 sports, else rainbow
  if (n_sports > 4 & n_sports <= 8) {
    palette <- brewer.pal(n_sports, "Dark2")
  }
  
  if (n_sports > 8 & n_sports <= 12) {
    palette <- brewer.pal(n_sports, "Set3")
  }
  
  if(n_sports > 12){
    palette <- rainbow(n_sports)
  }
  
  
  activity_colors <- setNames(palette, unique_sports)
  activity_colors <- activity_colors[seq(1, n_sports)]
  
  activity_colors
}

get_heatmap <- function(requested_points){
  all_tracks <- split(requested_points, requested_points$filepath)
  
  activity_colors <- get_colors(unique(requested_points$sport))
  
  
  
  # Create map
  m <- leaflet() %>%
    addTiles()
  
  # Add tracks by activity type
  withProgress(message = "Processing tracks...", value = 0, {
    total_tracks <- length(all_tracks)
    k <- 0
  for (fp in names(all_tracks)) {
    k <- k + 1
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
        opacity = 1,
        group = sport
      )
    incProgress(1 / total_tracks, detail = paste("Track", k, "out of", total_tracks))
    }
  })
  
  # Create rounded GPS points for heatmap
  gps_rounded <- requested_points %>%
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
      options = layersControlOptions(collapsed = TRUE)
    )
  m
}  




