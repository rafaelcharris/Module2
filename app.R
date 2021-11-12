#1. Load Packages

library(shiny)
library(tidyverse)
library(tigris)
library(leaflet)
library(janitor)
#2.  Load Ddata
##2.1 Load General Data (In class)
df <- read.csv(".//data//data.csv") %>%
  clean_names() %>%
  separate(town_county, sep = ",", c("city", "county_name")) %>% 
  mutate(city = tolower(as.character(coverage_city)),
         county = tolower(county))

##2.2 Load Geographical Data 
nj_counties <- counties("NJ",
                        cb = TRUE, # This is so the borders looks better
                        year = 2020)

##2.3 Load Municipality data

nj_mun <- read.csv(".//data//MunicipalData.csv", skip = 1) %>%
  clean_names() %>%
  rename("city" = "municipality",
         "population" = "x2020_census_population") %>%
  mutate(city = tolower(city),
         county = tolower(county))

# 4. Merge data
# 4.1 Match the population of the city with the full data set
df_city_pop <- inner_join(df, nj_mun, by = c("county", "city"))

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("New Jersey Policing"),
  
  fluidRow(
    leafletOutput("nj_map")
  ),
  # Show a plot of the generated distribution
  fluidRow(
  )
)


# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$nj_map <- renderLeaflet({
    
    nj_counties %>%
      leaflet() %>%
      addTiles() %>%
      addPolygons(fillColor = "grey", 
                  color = "black", #Line Color
                  weight = 1.5, # Line thickness
                  label = nj_counties$NAME, # Add labels when hovering over the territory
                  smoothFactor = 0.2
                  )  
    })
}

# Run the application 
shinyApp(ui = ui, server = server)

