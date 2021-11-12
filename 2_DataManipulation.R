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

# 5. Data Manipulation
df_cl <- df_city_pop %>% 
  group_by(county) %>%
  mutate(population = as.numeric(population),
         county_pop = sum(population)) %>%
  ungroup() %>%
  mutate(perc_total_pop = population/county_pop,
         white_police_weight = white_pct_officers*perc_total_pop,
         asian_police_weight = asian_pacific_islander_pct_officers*perc_total_pop,
         hisp_police_weight = hispanic_pct_officers*perc_total_pop,
         black_police_weight = black_pct_officers*perc_total_pop,
         
         white_subject_weight = white_pct_subjects*perc_total_pop,
         asian_subject_weight = asian_pacific_islander_pct_subjects*perc_total_pop,
         hisp_subject_weight = hispanic_pct_subjects*perc_total_pop,
         black_subject_weight = black_pct_subjects*perc_total_pop,
         
         pct_officers_injured_weight =  pct_officers_injured*perc_total_pop,
         pct_subjects_injured_weight =  pct_subjects_injured*perc_total_pop,
         
         pct_complaince_hold_weight    = pct_complaince_hold*perc_total_pop,                     
         pct_hands_fists_weight    = pct_hands_fists*perc_total_pop,
         pct_pepper_spray_weight    = pct_pepper_spray*perc_total_pop,                         
         pct_baton_weight    = pct_baton*perc_total_pop,                
         pct_leg_strikes_weight    = pct_leg_strikes*perc_total_pop,                          
         pct_take_down_weight    = pct_take_down*perc_total_pop,                             
         pct_deadly_force_weight    = pct_deadly_force*perc_total_pop,

         pct_resisted_police_weight  = pct_resisted_police*perc_total_pop,                  
         pct_physical_attack_weight  = pct_physical_attack*perc_total_pop,        
         pct_knife_attack_weight  = pct_knife_attack*perc_total_pop,    
         pct_blunt_attack_weight  = pct_blunt_attack*perc_total_pop,           
         pct_gun_threat_weight  = pct_gun_threat*perc_total_pop,        
         pct_gun_fire_weight  = pct_gun_fire*perc_total_pop,          
         pct_car_attack_weight  = pct_car_attack*perc_total_pop,
         
  ) %>%
  group_by(county) %>%
  mutate( per_police_white_cl = sum(white_police_weight, na.rm = TRUE),
          per_police_asian_cl = sum(asian_police_weight, na.rm = TRUE),
          per_police_hisp_cl = sum(hisp_police_weight , na.rm = TRUE),
          per_police_black_cl = sum(black_police_weight, na.rm = TRUE),
          per_subject_white_cl = sum(white_subject_weight, na.rm = TRUE),
          per_subject_asian_cl = sum(asian_subject_weight, na.rm = TRUE),
          per_subject_hisp_cl = sum(hisp_subject_weight, na.rm = TRUE),
          per_subject_black_cl = sum(black_subject_weight, na.rm = TRUE),
          per_officers_injured_cl = sum(pct_officers_injured_weight, na.rm = TRUE),
          per_subjects_injured_cl = sum(pct_subjects_injured_weight, na.rm = TRUE),
          pct_complaince_hold_cl    = sum(pct_complaince_hold_weight, na.rm = TRUE),                     
          pct_hands_fists_cl    = sum(pct_hands_fists_weight,na.rm = TRUE),
          pct_pepper_spray_cl    = sum(pct_pepper_spray_weight, na.rm = TRUE),                         
          pct_baton_cl    = sum(pct_baton_weight, na.rm = TRUE),                
          pct_leg_strikes_cl    = sum(pct_leg_strikes_weight, na.rm = TRUE),                          
          pct_take_down_cl    = sum(pct_take_down_weight, na.rm = TRUE),                             
          pct_deadly_force_cl    = sum(pct_deadly_force_weight, na.rm = TRUE),
          pct_resisted_police_cl  = sum(pct_resisted_police_weight, na.rm = TRUE),                  
          pct_physical_attack_cl  = sum(pct_physical_attack_weight, na.rm = TRUE),        
          pct_knife_attack_cl  = sum(pct_knife_attack_weight, na.rm = TRUE),    
          pct_blunt_attack_cl  = sum(pct_blunt_attack_weight, na.rm = TRUE),           
          pct_gun_threat_cl  = sum(pct_gun_threat_weight, na.rm = TRUE),        
          pct_gun_fire_cl  = sum(pct_gun_fire_weight, na.rm = TRUE),          
          pct_car_attack_cl  = sum(pct_car_attack_weight, na.rm = TRUE),
          total_rows_cl = sum(total_rows_2016, na.rm = TRUE),
          ratio_rows_pop_cl = total_rows_cl/county_pop,
          total_incidents_cl = sum(total_arrests_2016, na.rm = TRUE),
          ratio_inc_pop_cl = total_incidents_cl/county_pop,
          total_arrests_cl = sum(total_arrests_2016, na.rm = TRUE),
          ratio_arr_pop_cl = total_arrests_cl/county_pop
          ) %>%
  select(
    county_name,
    county_pop,
    per_police_white_cl
    ,per_police_asian_cl
    ,per_police_hisp_cl
    ,per_police_black_cl
    ,per_subject_white_cl
    ,per_subject_asian_cl
    ,per_subject_hisp_cl
    ,per_subject_black_cl
    ,per_officers_injured_cl
    ,per_subjects_injured_cl
    ,pct_complaince_hold_cl
    ,pct_hands_fists_cl
    ,pct_pepper_spray_cl
    ,pct_baton_cl
    ,pct_leg_strikes_cl
    ,pct_take_down_cl
    ,pct_deadly_force_cl
    ,pct_resisted_police_cl
    ,pct_physical_attack_cl
    ,pct_knife_attack_cl
    ,pct_blunt_attack_cl
    ,pct_gun_threat_cl
    ,pct_gun_fire_cl
    ,pct_car_attack_cl
    ,total_rows_cl
    ,ratio_rows_pop_cl
    ,total_incidents_cl
    ,ratio_inc_pop_cl
    ,total_arrests_cl
    ,ratio_arr_pop_cl
  ) %>%
  distinct() 

# Save the data set
df_cl %>%
  write.csv("county_level_df.csv")



df_cl %>% 
  select(county,
         per_police_white_cl
         ,per_police_asian_cl
         ,per_police_hisp_cl
         ,per_police_black_cl
         ,per_subject_white_cl
         ,per_subject_asian_cl
         ,per_subject_hisp_cl
         ,per_subject_black_cl
         ,per_officers_injured_cl,
         per_subjects_injured_cl) %>%
  View()



try %>%
  group_by(county) %>%
  summarise(per = sum(perc_total_pop)) %>%
  arrange(desc(per))

