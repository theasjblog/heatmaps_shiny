
lapply(list.files('R', full.names = TRUE), source)

download.file('https://storage.googleapis.com/blogs_josa/sport/parquets/heathmap_all_points.parquet',
              destfile = file.path('data', 'heathmap_all_points.parquet'))

ds <<- arrow::open_dataset(file.path('data', 'heathmap_all_points.parquet'))

# Run the application 
shinyApp(ui = ui, server = server, enableBookmarking = 'url')
