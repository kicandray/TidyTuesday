#Kevin Candray 
# 5/4/2021
# water access points tidy tuesday, Week 19

###########################################
# reading in the data
water <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-05-04/water.csv')

View(water)

#install libs
library(tidyverse)
library(here)
library(lubridate)
library(countrycode)
library(ggthemes)
###########################################
ttwater<- water %>% 
  mutate(report_date = mdy(report_date)) %>% 
  rename(lat = lat_deg, 
         lon = lon_deg, 
         country = country_name) %>% 
  separate(water_tech, c("water_tech","brand"), 
           sep = "-",
           fill = "right") %>% 
  mutate(install_year = ifelse(install_year > 2021,
                               NA_real_, install_year)) %>%  
  filter(!country %in% c("Peru", "Dominican Republic","Timor-Leste"),
         !is.na(country)) %>% 
  filter(between(lat,-37, 35),
         between(lon, -40,60))
###########################################

###############################################
ttwater %>% 
  count(install_year) %>%
  filter(install_year >= 1950 & install_year <= 2020) %>% 
  ggplot(aes(install_year, n))+
  geom_col(color = "#95FF9F", 
           fill = "#3A413B")+
  labs(x = "Year Water Access Point was Installed",
       y = "# of Installed Water Access Points",
       title = "Water Access Points in Africa Over the Past Century",
       caption = "Created by Kevin Candray | TidyTuesday")+
  theme(axis.text = element_text(size=12),
        axis.title = element_text(size=16),
        axis.text.y = element_text(size = 10, color = "#000000"),
        axis.text.x = element_text(size = 9, color ="#000000" ),
        panel.background = element_rect(fill ="#95FFE5"),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        plot.background = element_rect(fill = "#95FFE5"))+
  ggsave(here("Tidy6","output","installAfrica.png"),
         width = 9, height = 9)
  

##################MAPS######################################
countries <- unique(ttwater$country)
unique(ttwater$install_year)

africamapdata<- map_data("world") %>% 
  as_tibble() %>% 
  mutate(continent = countrycode(region, "country.name", "continent")) %>% 
  filter(continent == "Africa")

ttwater %>%
  sample_n(20000) %>%
  ggplot(aes(lon, lat))+
  geom_polygon(aes(long, lat, group = group),
               fill ="white",
               color = "black",
               data = africamapdata,
               size = .25)+
  geom_point(size = .05, alpha = .35)+
  theme_map()+
  labs(title = "Water Sources in Africa from WPDX Data Source")+
  ggsave(here("Tidy6","output","WAPAfrica.png"),
         width = 9, height = 9)
