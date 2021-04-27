#Kevin Candray 
#2021-02-25
#TidyTuesday Week 5: Number 1
#load the libs
library(tidyverse)
library(scales)
library(here)
################################################################
theme_set(theme_bw()) #set the theme 
############################################################
employed <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-02-23/employed.csv')
earn <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-02-23/earn.csv')
#############################################################
#looking at the data i see that there is a TOTAL, meaning that race and gender were taking into account 
employed<- employed %>% 
  mutate(dimension = case_when(
    race_gender =="TOTAL"~"Total",
    race_gender %in% c("Men","Women")~"Gender",
    TRUE ~ "Race"
  ))
#########################################################################
employed %>% 
  filter(dimension=="Gender") %>% #filter only Gender
  filter(!is.na(employ_n)) %>% #remove NA
  mutate(industry = fct_lump(industry, 8, w=employ_n), #on the industry column, seperate into 8 diferent industries with emplotment num,ber 
         industry = fct_reorder(industry, employ_n,sum)) %>% #reorder it on sum function 
  ggplot(aes(year,employ_n,fill=race_gender))+ #plotting 
  geom_col()+ #chose column 
  theme(axis.title = element_text(size = 10, color = "#ffffff"), #changing size and color 
        axis.text.x = element_text(size = 12, color = "#ffffff"),
        axis.text.y = element_text(size = 12, color = "#ffffff"),
        panel.background = element_rect(fill = "#c4cecf"),
        plot.background = element_rect(fill = "#043f40"))+
  facet_wrap(~industry, scales = "free_y")+ #facet wrap on industries
  scale_y_continuous(labels = comma)+
  labs(y="# employed in industry", #labels
       x="Year")+
  ggsave(here("Tidy 1","Output","employed.png"), #save
         width = 9, height = 9)
#########################################################