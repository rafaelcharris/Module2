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
df <- head(df, -1)
df <- df %>% drop_na()

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

# 5. Load county-level data
url = 'https://raw.githubusercontent.com/rafaelcharris/Module2/master/data/county_level_df.csv'
df_cl = read.csv(url)

## 5.1 Data for a bar chart showing incidents from 2012 to 2016

df1 <- df[c('county', 'total_incidents_2012', 'total_incidents_2013', 'total_incidents_2014','total_incidents_2015','total_incidents_2016')]
df1 <-  df1 %>%
  group_by(county) %>%
  summarise(across(everything(), mean))
colnames(df1) <- c('county', 2012:2016)

df1.long = melt(df1,id.vars = 'county', variable.name = "time", value.name = "incidents")
df1.long$county = tolower(df1.long$county)

## 5.2 Data for a bar chart showing percentage of ethnicity officers and subjects

df_ofc <- df_cl %>%
  dplyr::select(county, contains("per_police")) %>%
  pivot_longer(cols = matches("per_police"),
               names_to = "per_officers")

names(df_ofc)[3] <- 'per_ofc'

df_sub <- df_cl %>%
  dplyr::select(county, contains('per_subject')) %>%
  pivot_longer(cols = matches("per_subject"),
               names_to = "per_subjects")

df_sub <- df_sub[!grepl("injured", df_sub$per_subjects),]
names(df_sub)[3] <- 'per_sub'

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

  sidebarLayout(
  sidebarPanel(uiOutput("countyOutput")),

  mainPanel(plotOutput('inc_bar'),
            br(),
            plotOutput('eth_ofc_bar'),
            br(),
            plotOutput('eth_sub_bar'),
            br(),
            plotOutput("radar_forces"))
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
  filtered <- reactive(df1.long %>% filter(county %in% input$countyInput))
  filtered_1 <- reactive(df_ofc %>% filter(county %in% input$countyInput))
  filtered_2 <- reactive(df_sub %>% filter(county %in% input$countyInput))
  
  output$inc_bar <- renderPlot({
    if(is.null(input$countyInput)){
      return (NULL)
    }
    if (length(input$countyInput) == 0) return ()
    
    ggplot(filtered()) +
      geom_bar(mapping = aes(x = time,y=incidents), fill='blue', stat = 'identity')})

  
  output$eth_ofc_bar <- renderPlot({
  if(is.null(input$countyInput)){
    return (NULL)
    }
  if (length(input$countyInput) == 0) return ()

  ggplot() +
    geom_bar(mapping = aes(x = filtered_1()$per_officers,y=filtered_1()$per_ofc), fill='blue', stat = 'identity')})
  
  output$eth_sub_bar <- renderPlot({
    if(is.null(input$countyInput)){
      return (NULL)
    }
    if (length(input$countyInput) == 0) return ()
    
    ggplot(filtered_2()) +
      geom_bar(mapping = aes(x = per_subjects,y=per_sub), fill='red', stat = 'identity')})
  
  output$countyOutput <- renderUI({
    selectInput('countyInput', 'County',
                choices = sort(unique(df_ofc$county)),
                selected = 'Atlantic')
  
}

# Run the application 
shinyApp(ui = ui, server = server)

