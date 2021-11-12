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
df_city_pop %>% 
  group_by(county) %>%
  mutate(population = as.numeric(population),
    county_pop = sum(population)) %>%
  ungroup() %>%
  mutate(perc_total_pop = population/county_pop,
         white_police_weight = white_pct_officers*perc_total_pop,
         asian_police_weight = asian_pacific_islander_pct_officers*perc_total_pop,
         hisp_police_weight = hispanic_pct_officers*perc_total_pop,
         black_police_weight = black_pct_officers*perc_total_pop,
         
         white_subject_weight = white_pct_subject*perc_total_pop,
         asian_subject_weight = asian_pacific_islander_pct_subject*perc_total_pop,
         hisp_subject_weight = hispanic_pct_subject*perc_total_pop,
         black_subject_weight = black_pct_subject*perc_total_pop,
         
         pct_officers_injured_weight =  pct_officers_injured*perc_total_pop,
         pct_subjects_injured_weight =  pct_subjects_injured*perc_total_pop) %>%
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
         per_subjects_injured_cl = sum(pct_subjects_injured_weight, na.rm = TRUE)) %>%
  
  write.csv()



try %>%
  group_by(county) %>%
  summarise(per = sum(perc_total_pop)) %>%
  arrange(desc(per))

