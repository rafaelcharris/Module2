library(tidyverse)
library(tigris)
library(leaflet)
library(Hmisc)

df <- read.csv("data.csv")
# Explore variables
# 1. Explore type of forces
describe(df)


# Find the geographical information of the counties


# First plots

df_long <- df %>%
 pivot_longer(
    cols = contains("_pct_subjects"),
    values_to = "percentage_subjects",
    names_to = "eth_subject",
    names_pattern = "(.*)_pct_subjects"
  ) %>%
  pivot_longer(
    cols = matches("pct(.*)_adjpop"),
    values_to = "percentage_population",
    names_to = "eth_population",
    names_pattern = "pct(.*)_adjpop")

ggplot(aes(x = percentage_population,
y = percentage_subjects)) +
geom_point()

df  %>%
  pivot_longer(
    cols = matches("pct(.*)_adjpop"),
    values_to = "percentage_",
    names_to = "variable",
    names_pattern = "pct(.*)_adjpop") %>%
    View()


df_long %>%
group_by(county, eth_population) %>%
summarise(mean = mean(percentage_population, na.rm = TRUE)) %>%
View()

