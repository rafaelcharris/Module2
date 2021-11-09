library(tigris)
library(leaflet)
library(tidyverse)

df <- read.csv("data.csv")

# Get the data for NJ
nj_counties <- counties("NJ", year = 2020)
# Merge data from the county with the main data set

df_map <- df %>%
    left_join(nj_counties, by = c("county" = "NAME")) %>%
    mutate(lat = as.numeric(INTPTLAT),
    long = as.numeric(INTPTLON))

# Draft the first map

nj_counties %>%
leaflet() %>%
addTiles() %>%
addPolygons(lng = long, lat = lat)
