wine_ratings <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-05-28/winemag-data-130k-v2.csv") %>%
      select(- X1)
glimpse(wine_ratings) 

country1 <- wine_ratings %>% count(country) %>% filter(n > 50) %>% select(country)
countryvect <- country1[[1, ]]

wine_ratings1 <- wine_ratings %>% group_by(country) %>% filter(country %in% countryvect) %>% summarise(avg_point = mean(points))
wine_ratings2 <- na.omit(wine_ratings1)

## country by there average score point
wine_ratings2 %>% ggplot(aes(reorder(country, avg_point), avg_point)) + 
      geom_point() +
      coord_flip() + 
      geom_hline(yintercept = c(86, 88, 90)) +
      scale_y_continuous("Mean of points", expand = c(0,0.3)) + 
      scale_x_discrete("Country") + 
      labs(title = "Average score per country") + 
      theme_economist()

## country by points boxplot
wine_ratings %>% group_by(country) %>% filter(country %in% countryvect) %>% sample_n(size = 30) %>% ggplot(aes(reorder(country, points), points)) +
      geom_boxplot(alpha = 0.2) +
      geom_point(col = "blue", position = "jitter", alpha = 0.3) +
      theme(axis.text.x = element_text(angle = 90))

##country by points errorbar
wine_ratings %>% group_by(country) %>% filter(country %in% countryvect) %>% sample_n(size = 30) %>% ggplot(aes(reorder(country, points), points)) +
      stat_summary(fun.data = mean_sdl, fun.args = list(mult = 1), geom = "errorbar") +
      stat_summary(fun.y = mean, geom = "point") +
      theme_economist() +
      theme(axis.text.x = element_text(angle = 90))

## price by points + case_when
max(wine_ratings$price, na.rm = TRUE)
min(wine_ratings$price, na.rm = TRUE)

wine_price <- wine_ratings %>% filter(!is.na(price)) %>% mutate(economic_rate = case_when(price <= 25 ~ "normal",
                                                                            price <= 100 ~ "high", 
                                                                            price <= 1000 ~ "very high",
                                                                            price <= 3300 ~ "no limit"))
wine_price_omit <- na.omit(wine_price)

wine_price %>% ggplot(aes(points, price)) +
      geom_point() +
      geom_smooth(method = "lm", se = FALSE)


top_producer <- wine_price %>% count(province) %>% arrange(desc(n)) %>% head(20) %>% select(-2)
province_top_p <- top_producer[[1, ]]                                                  
wine_ratings %>% filter(province %in% province_top_p) %>% 
      ggplot(aes(province, price)) +
      geom_boxplot() + 
      scale_y_log10() + 
      theme(axis.text.x = element_text(angle = 90))

library(ggridges)

wine_ratings %>% filter(province %in% province_top_p) %>% 
      ggplot(aes(price, province)) +
      geom_density_ridges(alpha = 0.5, fill = "red") +
      geom_point(alpha = 0.2, shape = "|", position = position_nudge(y = -0.5))
      xlim(0, 100)

wine_price %>% filter(country %in% c("France", "Italy", "US", "Germany")) %>% 
      ggplot(aes(reorder(country, points), points)) + 
      geom_violin(alpha = 0.7, fill = NA) + 
      geom_boxplot(alpha = 0.05)
      
