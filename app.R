library(shiny)
library(tidyverse)
library(tigris)
library(leaflet)

df <- read.csv("data.csv")
# Get the data for NJ
nj_counties <- counties("NJ", year = 2020)
# Merge data from the county with the main data set

ui <- fluidPage(

  # Application title
  titlePanel("New Jersey Policing"),
  fluidRow(
      leafletOutput("nj_map")
  )
)

server <- function(input, output) {

  # Draft the first map

  output$nj_map <- renderLeaflet({
    my_map <- nj_counties %>%
      leaflet() %>%
      addTiles() %>%
      addMarkers(label = nj_counties$NAME)
  })
}

# Run the application
shinyApp(ui = ui, server = server)