library(shiny)
library(tidyverse)

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("New Jersey Policing"),
  
  fluidRow(
    column(6,
           
    ),
    column(6,
    )
  ),
  # Show a plot of the generated distribution
  fluidRow(
  )
)


# Define server logic required to draw a histogram
server <- function(input, output) {

}

# Run the application 
shinyApp(ui = ui, server = server)
