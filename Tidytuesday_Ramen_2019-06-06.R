ramen_ratings <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-06-04/ramen_ratings.csv")

##explore the text of the dataset : variety
library(tidyverse)
library(tidytext)
str(ramen_ratings)
summary(ramen_ratings)

ramen_ratings %>% count(variety) %>% arrange(desc(n))

ramen_ratings %>% unnest_tokens(word, variety) %>% count(word) %>% arrange(desc(n)) %>% top_n(15) %>% 
      ggplot(aes(fct_reorder(word, n), n)) +
      geom_col() +
      theme(axis.text.x = element_text(angle = 45))

stop_word_ramen <- tribble(
                        ~word, ~lexicon,
                        "noodle", "CUSTOM",
                        "noodles", "CUSTOM")

tidy_ramen <- ramen_ratings %>% unnest_tokens(word, variety) %>% anti_join(stop_word_ramen)

tidy_ramen %>% filter(stars <= 1) %>% count(word) %>% arrange(desc(n)) %>% top_n(15) %>%
      ggplot(aes(fct_reorder(word, n), n )) + 
      geom_col() +
      coord_flip()
      theme(axis.text.x = element_text(angle = 45))
