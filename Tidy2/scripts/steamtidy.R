# Kevin Candray 
#Tidy Tuesday Week 12 data 
#Video Games and Sliced
#March 20 2021
###############################################
#loading the data
games <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-03-16/games.csv')
###############################################
View(games)
###############################################
library(ggplot2)
library(tidyverse)
library(dplyr)
library(here)
################################################
games$month <- as.factor(games$month) #change month from character to factor so we can rename levels
games$month <- recode_factor(games$month, #rename month levels to numbers
                             January = "1",
                             February = "2",
                             March = "3",
                             April = "4",
                             May = "5",
                             June = "6",
                             July = "7",
                             August = "8",
                             September = "9",
                             October = "10",
                             November = "11",
                             December = "12")
games$month <- as.numeric(games$month) 

games %>%
  select("gamename", "year", "month", "avg") %>%
  filter(complete.cases(.),
         year == 2020,
         gamename == "Persona 4 Golden" | gamename == "Fall Guys: Ultimate Knockout" | gamename == "Phasmophobia" |
           gamename == "Left 4 Dead 2" | gamename == "VRChat") %>%
  ggplot(aes(x = month,
             y = avg,
             color = gamename)) +
  geom_line(size = 1.5) +
  theme_minimal()+
  scale_x_continuous(expand = c(0, 0),
                     breaks = seq(1, 12, 1),
                     labels = c("Jan.", "Feb.", "Mar.", "Apr.", "May", "Jun.", "Jul.", "Aug.", "Sept.", "Oct.", "Nov.", "Dec.")) + #rename months
  scale_y_continuous(expand = c(0, 0)) +
  scale_color_manual(values = c("Fall Guys: Ultimate Knockout" = "#b434eb",
                                "Phasmophobia" = "#34e8eb",
                                "Persona 4 Golden" = "#e8eb34",
                                "Left 4 Dead 2"= "#ff0000",
                                "VRChat" = "#3d997f")) +
  labs(x = " ",
       y = "Average number of simultaneous players",
       color = " ",
       title = "Average Simultaneous Players of my Steam Video Game Library in 2020\n")+
  ggsave(here("Tidy2", "output","steamtidy.png"), width = 9, height = 8)

