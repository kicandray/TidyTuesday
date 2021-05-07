# Kevin Candray 
# Super Bowl Ads, Tidy Tuesday 
# 5/6/21
####################################
library(here)
library(tidyverse)
library(tidytext)
library(scales)
####################################
youtube <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-03-02/youtube.csv') %>% 
  select(-favorite_count)

View(youtube)
####################################
brands <- youtube %>% 
  count(brand, sort = TRUE) %>% 
  head(20) %>% 
  mutate(brand = fct_reorder(brand, n)) %>% 
  ggplot(aes(n, brand))+
  geom_col()

unique(youtube$brand)
# only 10 different brands
# what do the 10 brands like to use in their ads such as the catgories listed as columns

categories<- youtube %>%
  gather(category, value, funny:use_sex)
# gather by the categories as columns 

# make a graph representing the categories and seeing what each brand uses for all of their ads

categories %>% 
  group_by(brand, category) %>% 
  summarize(pct = mean(value)) %>% 
  ungroup() %>%
  mutate(brand = reorder_within(brand, pct, category)) %>% 
  ggplot(aes(pct, brand))+
  geom_col()+
  scale_x_continuous(labels = percent)+
  scale_y_reordered()+
  facet_wrap(~ category, scales = "free_y")
  
