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
  select(-favorite_count) # removing favorite because there isnt much there with the final graph produced

View(youtube)
####################################
brands <- youtube %>% 
  count(brand, sort = TRUE) %>% 
  head(20) %>% 
  mutate(brand = fct_reorder(brand, n)) %>% 
  ggplot(aes(n, brand))+
  geom_col()
######################################
unique(youtube$brand)
# only 10 different brands
# what do the 10 brands like to use in their ads such as the catgories listed as columns

categories<- youtube %>%
  gather(category, value, funny:use_sex) %>%  # making a vector with the catgories
  mutate(category = str_to_title(str_replace_all(category, "_"," "))) # editing the column name
# gather by the categories as columns 

# make a graph representing the categories and seeing what each brand uses for all of their ads

categories %>% 
  group_by(brand, category) %>% # group by brand and category 
  summarize(pct = mean(value)) %>% #do some analysis on the average value of ads that are in a category
  ungroup() %>% #ungroup 
  mutate(brand = reorder_within(brand, pct, category)) %>% # change column 
  ggplot(aes(pct, brand))+ # plot x and y 
  geom_col(fill = "#DA3F3F", # type of graph 
           color = "#BBD7C0")+
  scale_x_continuous(labels = percent)+ # make the scale percent
  scale_y_reordered()+ #reorder the y from high to low
  facet_wrap(~ category, scales = "free_y")+ # facet wrap based on Category 
  theme_bw()+ # change theme
  theme(axis.text = element_text(size=12), # editing style and color
        axis.title = element_text(size=16),
        axis.text.y = element_text(size = 10, color = "#000000"), # changing the style 
        axis.text.x = element_text(size = 9, color ="#000000" ),
        panel.background = element_rect(fill ="#53A361"),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        plot.background = element_rect(fill = "#53A361"))+
  labs(x = "Percent of Brand Ads Category Shown During the SuperBowl", #adding labels
       y = "Brand", # more labels
       title = "Brand Trends in Superbowl Ads",
       caption = "Created by Kevin Candray | TidyTuesday")+
  ggsave(here("Tidy3","output","supercat.png"), # save
         width = 9, height = 9)
  
