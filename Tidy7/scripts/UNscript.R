# Kevin Candray 
# Tidy Tuesday 7 
#14 May 2021

###################################################################
unvotes <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-03-23/unvotes.csv')
roll_calls <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-03-23/roll_calls.csv')
issues <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-03-23/issues.csv')

by_country<- unvotes %>% 
  group_by(country) %>% 
  summarize(n_votes = n(), 
            n_yes = sum(vote == "yes"), 
            pct_yes = n_yes / n_votes) %>% 
  filter(n_votes >= 100) %>% 
  arrange(desc(pct_yes))

by_country %>% 
  slice(1:20)%>% 
  mutate(country = fct_reorder(country, pct_yes)) %>% 
  ggplot(aes(pct_yes, country))+
  geom_point(aes(size = n_votes))+
  scale_x_continuous(labels = percent)+
  labs(x = "% of yes votes in UN", 
       title = "What Countries Votes Yes the Lease Amount of Times")+
  ggsave
