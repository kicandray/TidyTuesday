#Kevin Candray 
# TidyTuesday Week 18
# CEO Departures 

# loading the data 
departures <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-04-27/departures.csv')
View(departures)

#loading the libs 
library(tidyverse)
library(here)
library(lubridate)
library(tidytext)
library(flair)
library(wordcloud2)
library(DescTools)
library(webshot)
library("htmlwidgets")
ripceo <- departures %>% 
  ggplot(aes(fyear))

departures_words<- departures %>% 
  unnest_tokens(word, notes) %>% 
  anti_join(stop_words, by = "word") %>% 
  count(coname, fyear, departure_code, word)

departurewords<- departures_words %>% 
  count(word) %>% 
  arrange(desc(n)) %>% 
  filter(n >=7) %>% 
  slice(1:100)

departure.cloud<- wordcloud2(departurewords, shape = 'star', size = 0.25, minSize = 5, rotateRatio = 0, shuffle = FALSE)
saveWidget(departure.cloud, "dep.html", selfcontained = F)
webshot("dep.html", "dep_1.png", delay = 5, vwidth = 480, vheight = 480)
