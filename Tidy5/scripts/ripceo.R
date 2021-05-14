#Kevin Candray 
# TidyTuesday Week 18
# CEO Departures 

# loading the data 
departures <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-04-27/departures.csv')
View(departures)

#loading the libs 
library(tidyverse)
library(here)
library(scales)

ripceo <- departures %>% 
  ggplot(aes(fyear))
