#1. Load Packages
library(shiny)
library(tidyverse)
library(tigris)
library(leaflet)
library(janitor)
library(plotly)
library(stringr)
library(reshape2)
library(sf)
library(fmsb)
library(shinyWidgets)
library(data.table)
library(scales)

# Define palette
cbp1 <- c("#999999", "#E69F00", "#56B4E9", "#009E73",
          "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

#2.  Load Data
##2.1 Load General Data (In class)
df <- read.csv(".//appdata//main_df.csv") %>%
  filter(county != "") # last row should not be here because is the whole state

df_long <- df%>% 
  select(county, 11:15, 17:21, 23:27) %>%
  pivot_longer(cols = !county,
               values_to = "count",
               names_to = c("event","year"),
               names_pattern = "total_(.*)_(.*)") %>%
  mutate(county = paste0(" ",str_to_title(county), " County"))

##2.2 Load Geographical Data 
nj_counties<- counties("NJ",
                       cb = TRUE, # This is so the borders looks better
                       year = 2020) %>%
  mutate(name = tolower(NAME))

##2.3 Load Municipality data
nj_mun <- read.csv(".//appdata//municipality.csv")

#2.4 Load data at the county level 
df_cl <- read.csv(".//data//county_level_df.csv")
#2.4.1 Load data at the county level with extended data
df_cl_ext <- read.csv(".//appdata//county_level_df_ext.csv")

# 2.5 For radar plot

df_forces <- df_cl %>%
  select("county", "pct_complaince_hold_cl","pct_hands_fists_cl","pct_pepper_spray_cl","pct_baton_cl","pct_leg_strikes_cl","pct_take_down_cl","pct_deadly_force_cl") %>%
  pivot_longer(cols = matches("_"),
               names_to = "forces_type",
               values_to = "percentage",
               names_pattern = "pct_(.*)_cl"
  ) %>%
  mutate(county = paste0(" ",str_to_title(county), " County"))


# 3. Merge data
# 3.1 Match the population of the city with the full data set
df_map <- nj_counties %>%
  left_join(df_cl, by = c("name" = "county"))

#4 map data in the long format
barplot_df <- df_cl_ext %>%
  mutate(per_police_unreported_cl = 100 -
           (per_police_asian_cl + per_police_hisp_cl + per_police_black_cl + per_police_white_cl)) %>%
  select(county, contains("per_police_"), contains("per_subject_"),contains("per_citizen_")) %>%
  pivot_longer(cols = matches("per_police|per_subject|per_citizen"),
               names_to = c("Group", "Ethnicity"),
               names_pattern = "per_(.*)_(.*)_cl",
               values_to = "percentage") %>%
  mutate(
    #`Police Force` = as.factor(1),
    county_name = paste0(" ",str_to_title(county), " County"),
    percentage = round(percentage, 1),
    Ethnicity = ifelse(Ethnicity == "hisp", "Hispanic",str_to_title(Ethnicity)),
    Group = factor(Group, levels = c("police", "subject", "citizen")))

# Define UI for application that draws a histogram
ui <- fluidPage(
  # Application title
  titlePanel("New Jersey Policing"),
  sidebarLayout(
    sidebarPanel(uiOutput("countyOutput"),
                 uiOutput("mapDataOutput")),
    
    mainPanel(
      fluidRow(
        leafletOutput("nj_map"))
      ,
      fluidRow(
        # Show a plot of the barplot
        tabsetPanel(id = "tabset",
                    tabPanel("Bar Plot", plotlyOutput("barplot")),
                    tabPanel("Time trends",
                             plotlyOutput("lineplot")),
                    tabPanel("Radar Plot", 
                             plotOutput("radarplot")))
      )))
)


# Define server logic required to draw a histogram
server <- function(input, output) {
  
  # A. Define Reactive variables
  # Barplot
  reactive_barplot <- reactive({
    barplot_df %>% 
      filter(county_name %in% input$countyInput)

  })
  
  reactive_radar <- reactive(df_forces %>% 
                               filter(county %in% input$countyInput))
  
  selected <- reactive({
    df_map %>%
      filter(county_name %in% input$countyInput) 
  })
  # Line plot reactive data set
  reactive_lineplot <- reactive({
    df_long %>%
      filter(county %in% input$countyInput, event %in% input$mapdataInput) %>%
      group_by(year)%>%
      mutate(total_counts= sum(count, na.rm = TRUE))
  })
  
  #B Define outputs
  #B.1 Map Output
  output$nj_map <- renderLeaflet({
    
    if(is.null(input$countyInput)){
      PU <- paste0("<b>",df_map$NAME,"</b>", "<br>Population: ", 
                   format(df_map$county_pop,
                          big.mark=".",
                          small.interval=3,
                          decimal.mark = ","), " inhabitants"
      ) 
      return (df_map %>%
                leaflet() %>%
                addTiles() %>%
                addPolygons(
                  fillOpacity = 0.5,
                  color = "black", #Line Color
                  weight = 1.5, # Line thickness
                  label = df_map$NAME, # Add labels when hovering over the territory
                  smoothFactor = 0.2,
                  popup = PU))
    }
    #The color for the variable to show
    pal <- colorNumeric(
      palette = "Blues",
      domain = df_map$ratio_arr_pop_cl)
    #The information in the popup
    PU <- paste0("<b>",selected()$NAME,"</b>", "<br>Population: ", 
                 format(selected()$county_pop,
                        big.mark=".",
                        small.interval=3,
                        decimal.mark = ","), " inhabitants",
                 "<br>Est. Crime Rate: ", round(selected()$ratio_rows_pop_cl*100,1),"%"
    ) 
    
    selected()  %>%
      st_transform(crs = "+init=epsg:4326") %>%
      leaflet() %>%
      addTiles() %>%
      addPolygons(
        fillColor = ~pal(ratio_inc_pop_cl),
        fillOpacity = 0.5,
        color = "black", #Line Color
        weight = 1.5, # Line thickness
        label = selected()$NAME, # Add labels when hovering over the territory
        smoothFactor = 0.2,
        popup = PU) %>%
      addLegend("bottomright", pal = pal, values = ~df_map$ratio_inc_pop_cl,
                title = "Est. Crime Rate",
                #labFormat = labelFormat(suffix = "%"),
                opacity =0.6)
  })
  #B.2 Barplot output
  output$barplot <- renderPlotly({
    
    if(is.null(input$countyInput)){
      return (NULL)
    }
    p <- ggplot(reactive_barplot(),
                aes(x = Group,
                    y = percentage,
                    fill = Ethnicity)) +
      stat_summary(geom = "bar",
                   fun = "mean",
                   position = "fill") +
      scale_x_discrete(labels = c("Police", "Subjects", "General\nPopulation")) +
      labs(y = "Percentage") + 
      scale_y_continuous(labels = function(x)paste0(x*100, "%")) +
      theme_minimal() +
      theme() +
      coord_cartesian(ylim = c(0, 1)) +
      scale_fill_manual(values=cbp1)
    
    ggplotly(p, tooltip = c("fill","y"))
  })
  #A.3 
  output$lineplot <- renderPlotly({
    if(is.null(input$countyInput)){
      return (NULL)
    }
    
    p1 <- reactive_lineplot() %>%
      ggplot(aes(x = year,
                 y = total_counts)) +
      geom_line(group = 1, size = 1.2) +
      labs(y = "Number of reports", x = "Year") + 
      theme_minimal()
    
    ggplotly(p1)
  })
  # A Radar plot
  output$radarplot <- renderPlot({
    req(input$countyInput)
    if (length(input$countyInput) > 1 ){
      return (NULL)
    } else {
    data <- as.data.frame(reactive_radar()$percentage)
    data <-  data.table::transpose(data)
    colnames(data) <- reactive_radar()$forces_type
    df <- rbind(rep(95,7), rep(0,7), data)
    radarchart(df, pcol = rgb(0.2, 0.5, 0.5, 0.9), 
               pfcol = rgb(0.2, 0.5, 0.5, 0.4), 
               plwd = 4, plty = 1, cglcol = 'grey', 
               cglty = 1, cglwd = 0.8, vlcex = 0.8)
    }
    # TODO: add title to radar plot 
    #p2 <- reactive_radar() %>%
    #  group_by(county) %>%
    #  pivot_wider(
    #    values_from = percentage, 
    #    names_from = forces_type) %>%
    #  mutate_at(vars(-county), rescale) %>%
    #  ggradar()
    #ggplotly(p2)
  })
  # Define User input
  output$countyOutput <- renderUI({
    pickerInput(
      inputId = "countyInput",
      label = "Select/deselect the county(ies) you want to explore",
      choices = sort(unique(df_map$county_name)),
      selected = sort(unique(df_map$county_name)),
      options = list(
        `actions-box` = TRUE,
        size = 10,
        `selected-text-format` = "count > 3"
      ),
      multiple = TRUE
    )
  })
  output$mapDataOutput <- renderUI({
    selectInput('mapdataInput', "Plot Time Trend",
                choices = sort(unique(df_long$event)),
                selected = "arrests")
  })
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)

