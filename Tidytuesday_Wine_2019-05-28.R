wine_ratings <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-05-28/winemag-data-130k-v2.csv") %>%
      select(- X1)
glimpse(wine_ratings) 

country1 <- wine_ratings %>% count(country) %>% filter(n > 50) %>% select(country)
countryvect <- country1[[1, ]]

wine_ratings1 <- wine_ratings %>% group_by(country) %>% filter(country %in% countryvect) %>% summarise(avg_point = mean(points))
wine_ratings2 <- na.omit(wine_ratings1)

wine_ratings2 %>% ggplot(aes(reorder(country, avg_point), avg_point)) + 
      geom_point() +
      coord_flip()
wine_ratings %>% group_by(country) %>% 
