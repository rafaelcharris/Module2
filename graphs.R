library(fmsb)
library(leaflet)
library(tidyverse)
library(shiny)
library(ggplot2)
library(dplyr)
library(reshape2)
library(ECharts2Shiny)
library(data.table)

#Load the original dataset
df = read.csv("/Users/mac/Documents/1. Chapman/2. MSBCE/3. Fall 2021/CS 614 - Interactive Data Analysis/Modules/Module2/UOF_BY_DEPARTMENTS.csv")
df <- head(df, -1)
df <- df %>% drop_na()

#Load county-level dataset
url = 'https://raw.githubusercontent.com/rafaelcharris/Module2/master/data/county_level_df.csv'
df_cl = read.csv(url)

#Create dataset for a bar chart of incidents from 2012 to 2016
df1 <- df[c('county', 'total_incidents_2012', 'total_incidents_2013', 'total_incidents_2014','total_incidents_2015','total_incidents_2016')]
df1 <-  df1 %>%
  group_by(county) %>%
  summarise(across(everything(), mean))
colnames(df1) <- c('county', 2012:2016)

df1.long = melt(df1,id.vars = 'county', variable.name = "time", value.name = "incidents")
df1.long$county = tolower(df1.long$county)

#Create a dataset for a bar chart of ethnicity of officers and subjects
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

#Create a dataset for a radar chart of total forces per population in 2016
df_forces <- df_cl[, c("county", "pct_complaince_hold_cl","pct_hands_fists_cl","pct_pepper_spray_cl","pct_baton_cl","pct_leg_strikes_cl","pct_take_down_cl","pct_deadly_force_cl")]

colnames(df_forces) <- c('county', 'c_hold', 'h_fists', 'p_spray', 'batton_', 'l_strikes', 't_down', 'd_force')

df_forces <- df_forces %>%
  pivot_longer(cols = matches("_"),
               names_to = "forces_type")
names(df_forces)[3] <- 'per_forces'

#Create a dataset for a radar chart of total threats per population in 2016
df_threats <- df_cl[, c("county", "pct_resisted_police_cl","pct_physical_attack_cl","pct_knife_attack_cl","pct_blunt_attack_cl","pct_gun_threat_cl","pct_gun_fire_cl","pct_car_attack_cl")]
colnames(df_threats) <- c('county', 'resisted_', 'physical_', 'knife_', 'blunt_', 'gun_t', 'gun_f', 'car_')

df_threats <- df_threats %>%
  pivot_longer(cols = matches("_"),
               names_to = "threats_type")
names(df_threats)[3] <- 'per_threats'

#Create a dataset for a summary table
df_table <- df_cl %>%
  dplyr::select(county, c("ratio_rows_pop_cl","ratio_inc_pop_cl","ratio_arr_pop_cl","per_officers_injured_cl","per_subjects_injured_cl"))

df_table <- df_table %>%
  pivot_longer(cols = matches("_"),
               names_to = "summary")
names(df_table)[3] <- 'ratios'

ui <- fluidPage(
  titlePanel("Module 2"),
  
  sidebarLayout(
    sidebarPanel(uiOutput("countyOutput")),
    
    mainPanel(dataTableOutput("tableOutput"),
              br(),
              plotOutput('inc_bar'),
              br(),
              plotOutput('eth_ofc_bar'),
              br(),
              plotOutput('eth_sub_bar'),
              br(),
              plotOutput("radar_forces"),
              br(),
              plotOutput('radar_threats'))
  )
)

server <- function(input, output){
  
  filtered <- reactive(df1.long %>% filter(county %in% input$countyInput))
  filtered_1 <- reactive(df_ofc %>% filter(county %in% input$countyInput))
  filtered_2 <- reactive(df_sub %>% filter(county %in% input$countyInput))
  filtered_3 <- reactive(df_forces %>% filter(county %in% input$countyInput))
  filtered_4 <- reactive(df_threats %>% filter(county %in% input$countyInput))
  filtered_5 <- reactive(df_table %>% filter(county %in% input$countyInput))
  
  output$tableOutput <- renderDataTable({
    filtered_5()
  })

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
  
  output$radar_forces <- renderPlot({
    # df <- filtered_3() %>% dplyr::select(c(2:3)),
    data <- as.data.frame(filtered_3()$per_forces)
    data <-  transpose(data)
    colnames(data) <- filtered_3()$forces_type
    # max_forces = max(df$per_forces),
    # min_forces = min(df$per_forces),
    df <- rbind(rep(95,7), rep(0,7), data)
    radarchart(df, pcol = rgb(0.2, 0.5, 0.5, 0.9), pfcol = rgb(0.2, 0.5, 0.5, 0.4), plwd = 4, plty = 1, cglcol = 'grey', cglty = 1, cglwd = 0.8, vlcex = 0.8)
  })
  
  output$radar_threats <- renderPlot({
    # df <- filtered_3() %>% dplyr::select(c(2:3)),
    data <- as.data.frame(filtered_4()$per_threats)
    data <-  transpose(data)
    colnames(data) <- filtered_4()$threats_type
    # max_forces = max(df$per_forces),
    # min_forces = min(df$per_forces),
    df <- rbind(rep(95,7), rep(0,7), data)
    radarchart(df, pcol = rgb(0.2, 0.5, 0.5, 0.9), pfcol = rgb(0.2, 0.5, 0.5, 0.4), plwd = 4, plty = 1, cglcol = 'grey', cglty = 1, cglwd = 0.8, vlcex = 0.8)
  })

  output$countyOutput <- renderUI({
    selectInput('countyInput', 'County',
                choices = sort(unique(df_ofc$county)),
                selected = 'Atlantic')
  })
}
shinyApp(ui, server)



# df_plot1 <- df %>% 
#   select(-contains("_2012_2016")) %>%
#   pivot_longer(cols = matches("total_incident|total_arrests"),
#                values_to = "num_incidents",
#                names_to = c("event", "year"),
#                names_pattern = "total_(.*)_(.*)") %>%
#   select(county, coverage_city, year, event, num_incidents) %>%
#   group_by(county,year, event) %>%
#   summarise(total_event = sum(num_incidents, na.rm = TRUE),
#          mean_event  = mean(num_incidents, na.rm = TRUE))
# 
# 
# 
# ggplot(df_plot1, aes(x = year)) +
#   scale_y_continuous(name = 'incidents', limits = c(0,max(df_plot1[df_plot1$event == "incidents","mean_event"])),
#                      sec.axis = sec_axis(name = 'arrests', trans = ~.*10)) +
#   geom_bar(aes(y=mean_event, fill = event, group=event), stat = 'identity',position = position_dodge())
# 
# 

# round(max(df_plot1[df_plot1$event == "arrests","mean_event"])/max(df_plot1[df_plot1$event == "incidents","mean_event"])

# df2 <- df[c('county', 'total_arrests_2012', 'total_arrests_2013', 'total_arrests_2014', 'total_arrests_2015', 'total_arrests_2016')]

# 
# df2 <-  df2 %>%
#   group_by(county) %>%
#   summarise(across(everything(), mean))
# colnames(df2) <- c('county', 2012:2016)
# 
# df2.long = melt(df2,id.vars = 'county', variable.name = "time", value.name = "arrests")
# 
# df3.long <- merge(df1.long, df2.long, by=c('county', 'time'))
# View(df3.long)
# 
# df_al <-  df3.long %>%
#   filter(df3.long$county == 'Atlantic')
# 
# ggplot(df_al, aes(x=time, y = incidents))+
#   geom_line(color='blue') +
#   geom_bar(aes(x=time, y=arrests), stat = 'identity') +
#   scale_y_continuous(sec.axis = sec_axis(~.+10, name = 'arrests'))

# df_mon <- df_forces %>%
#   filter(county == "monmouth") %>%
#   dplyr::select(c(2:3))
# 
# data <- as.data.frame(df_mon$per_forces)
# data <-  transpose(data)
# colnames(data) <- df_mon$forces_type
# 
# max_forces = max(df_mon$per_forces)
# min_forces = min(df_mon$per_forces)
# 
# df <- rbind(rep(max_forces,7), rep(min_forces,7), data)