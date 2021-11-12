library(tigris)
library(leaflet)
library(tidyverse)

df <- read.csv("data.csv")

# Get the data for NJ
nj_counties <- counties("NJ", year = 2020)
# Merge data from the county with the main data set

df_map <- df %>%
    left_join(nj_counties, by = c("county" = "NAME"))

# Draft the first map

nj_counties %>%
  leaflet() %>%
  addTiles() %>%
  addPolygons(fillColor = "grey", 
              color = "black", #Line Color
              weight = 1.5, # Line thickness
              label = nj_counties$NAME, # Add labels when hovering over the territory
              smoothFactor = 0.2
  )  %>%
  addLegend(
    colors = number.to.colors(df_map$pctblack_adjpop, colors = c("red", "blue"), num = 100),
    values = df_map$pctblack_adjpop)