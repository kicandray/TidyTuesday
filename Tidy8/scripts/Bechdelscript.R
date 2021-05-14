# Kevin Candray 
# Tidy Tuesday Week 8 Assignment 
# 14 May 2021
######################################
raw_bechdel <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-03-09/raw_bechdel.csv')
movies <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-03-09/movies.csv')

view(movies)

movies %>% 
  count(year, binary) %>% 
  ggplot(aes(year, n, fill = binary))+
  geom_col()+
  labs(x = "Year",
       y = "# amount of movies",
       title = "Amount of movies that fail or pass the Bechdel Test",
       caption = "Created by Kevin Candray | TidyTuesday")+
  ggsave(here("Tidy8","Output","movies8.png"),
         width = 9, height = 9)
