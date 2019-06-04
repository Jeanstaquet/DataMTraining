wine_ratings <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-05-28/winemag-data-130k-v2.csv") %>%
      select(-X1)
wine_ratings_tbl <- as_tibble(wine_ratings)
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

## wine score per taster with 500+ ratings

taster1 <- wine_ratings %>% count(taster_name) %>% filter(!is.na(taster_name), n > 515)
taster_final <- taster1[[1]]
wine_ratings %>% filter(taster_name %in% taster_final) %>% summarise(avg_point = mean(points))

wine_ratings %>% filter(taster_name %in% taster_final, country %in% c("France", "Germany", "Us", "Italy")) %>% ggplot(aes(points, taster_name, fill = country)) + 
      geom_density_ridges(alpha = 0.7, color = NA) +
      scale_y_discrete("Name of the best tasters", expand = c(0,0))
      geom_vline(xintercept = 88.6) +
      theme_excel()

## Text analysis wine ratings
library(tidytext)
library(tidyverse)
      
tidy_wine_description <- wine_ratings %>% unnest_tokens(word, description)

tidy_wine_description2 <- tidy_wine_description %>% anti_join(stop_words2)

top10_words <- tidy_wine_description2 %>% filter(points > 95, country == "US") %>% count(word) %>% arrange(desc(n)) %>% top_n(15, n) %>% mutate(word2 = fct_reorder(word, n)) 

top10_words %>% ggplot(aes(word2, n)) + 
                  geom_col() +
                  coord_flip() + 
                  labs(title = "Review Word Count")


#### Add some stop words
custom_words <- tribble(
      ~word, ~lexicon,
      "wine", "CUSTOM",
      "flavors", "CUSTOM")

stop_words2 <- stop_words %>% bind_rows(custom_words)

min10_words <- tidy_wine_description2 %>% filter(points < 85, country == "US") %>% count(word) %>% arrange(desc(n)) %>% top_n(15, n) %>% mutate(word2 = fct_reorder(word, n))

min10_words %>% ggplot(aes(word2, n)) + 
      geom_col() +
      coord_flip() + 
      labs(title = "Review Word Count")
