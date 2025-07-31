server <- function(input, output, session) {
  
  rV <- reactiveValues(
    requested_tracks = NULL
  )
  
  output$select_sport_ui <- renderUI({
    sports <- ds |>
      select(sport) |>
      distinct() %>%
      collect() |>
      pull(sport)
    
    selectInput("sport_selector", "Choose sports", 
                choices = sports, 
                multiple = TRUE)
  })
  
  output$selete_dates_ui <- renderUI({
    timestamp_range <- ds |>
      select(time_stamp) |>
      summarize(
        min_time = min(time_stamp, na.rm = TRUE),
        max_time = max(time_stamp, na.rm = TRUE)
      ) |>
      collect()
    
    shiny::dateRangeInput('date_selector', 'Select a date range',
                          start = ymd(paste0(year(Sys.Date()), "-01-01")),
                          end = timestamp_range$max_time[1],
                          min = timestamp_range$min_time[1],
                          max = timestamp_range$max_time[1])
  })
  
  
  observeEvent(list(input$sport_selector, input$date_selector), {
    req(input$sport_selector)
    req(input$date_selector)
    
    requested_tracks <- ds |>
      filter(sport %in% input$sport_selector) |>
      filter(time_stamp >= input$date_selector[1]) |>
      filter(time_stamp <= input$date_selector[2]) |>
      select(filepath) |>
      distinct() |>
      collect() |>
      pull(filepath)
    
    output$n_tracks <- renderText({
      paste("You selected ", length(requested_tracks), " tracks to be plotted.")
    })
    
    
  })
  
  
  observeEvent(input$do_heathmap, {
    req(input$sport_selector)
    req(input$date_selector)
    
    requested_tracks <- ds |>
      filter(sport %in% input$sport_selector) |>
      filter(time_stamp >= input$date_selector[1]) |>
      filter(time_stamp <= input$date_selector[2]) |>
      collect()
    
    if (nrow(requested_tracks) == 0) {
      validate("Nothing to plot")
      rV$requested_tracks <- NULL
    } else {
      rV$requested_tracks <- requested_tracks
    }
  })
  
  
  # Leaflet map
  output$heathmap <- renderLeaflet({
    req(rV$requested_tracks)
    get_heatmap(rV$requested_tracks)
    
  })
}

