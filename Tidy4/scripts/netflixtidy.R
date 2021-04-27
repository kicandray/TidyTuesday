# Kevin Candray 
# Netflix Tidy Tuedsday
# 3rd submission 
# april 20 2021
#########################################
# loading the data 
netflix_titles <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-04-20/netflix_titles.csv')
# look at the head of the data to see what we are messing with 
head(netflix_titles)
# viewing
View(netflix_titles)
# graph overall production of movies and tv shows
#########################################
# loading the libs
library(tidyverse)
library(lubridate)
library(ggridges)
library(stringr)
#########################################
# looking to see what was i analyzing 
netflix_country <- netflix_titles %>%
  filter(country %in% c("United States","France","Germany","Japan","South Korea", "Italy", "Mexico"))
netflix<- netflix_country %>%
  select(country, release_year)
netflix_graph <- netflix %>%
  group_by(release_year) %>%
  count(country) %>%
  ungroup()
netflix_graph <- netflix_graph %>%
  filter(release_year >= 2010 & release_year <= 2020)
netflix_graph$release_year <- as.integer(netflix_graph$release_year)
netflix_graph$country <- as.factor(netflix_graph$country, levels = c("United States","France","Germany","Japan","South Korea", "Italy", "Mexico"))
#########################################
#finally decided what I wanted to do, look at Movie ratings over the years on Netflix in the U.S.
#####################################################
#data wrangle
net_data<- netflix_titles %>%
  filter(type %in% c("Movie")) %>% #only get movies
  filter(country %in% c("United States")) %>% # only get U.S
  mutate(mdy(date_added)) %>%
  mutate(year(mdy(date_added))) %>%
  mutate(duration=str_replace_all(duration, pattern = "\\D", replacement = ""))%>% 
  select(-show_id, -director,-cast,-date_added,-listed_in,-description,-title) %>% 
  rename("year_added"= "year(mdy(date_added))")
##################################################################
net_data %>% 
  ggplot(aes(x=year_added, y= rating, fill=country))+
  coord_flip()+
  geom_violin(scale = "area",
              color = "#E50914",
              fill = "#E50914")+
  theme_bw()+
  theme(legend.position = "none",
        axis.text = element_text(size=12),
        axis.title = element_text(size=16),
        axis.text.y = element_text(size = 10, color = "#000000"),
        axis.text.x = element_text(size = 9, color ="#000000" ),
        panel.grid.major = element_blank(),
        plot.caption = element_text(hjust = 0.5),
        plot.title = element_text(hjust = 0.5),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill ="#000000" ),
        plot.background = element_rect(fill = "#767e8a"))+
  labs(x = "Movie Rating",
       y = "Year Released",
       title = "Netflix Movie Ratings in the U.S. From 2018-2020",
       caption = "Created by Kevin Candray | TidyTuesday")+
  ggsave()
                  