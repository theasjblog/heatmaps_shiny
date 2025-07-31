library(shiny)
library(arrow)
library(dplyr)
library(leaflet)
library(leaflet.extras)
library(magrittr)
library(tidyr)
library(RColorBrewer)
library(htmlwidgets)
library(shinyjs)
library(lubridate)

ui <- fluidPage(
  titlePanel("Activities Heathmap"),
  shinyjs::useShinyjs(),
  #bookmarkButton('bk_btn'),
  # Selector at the top
  uiOutput('select_sport_ui'),
  uiOutput('selete_dates_ui'),
  textOutput('n_tracks'),
  actionButton('do_heathmap', 'Compute heathmap'),
  leafletOutput('heathmap')
)

