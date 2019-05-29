## Data manipulation training
data(master)
head(master)
glimpse(master)
names(master)

summary(master)
master$age <- as.factor(master$age)
master$generation <- as.factor(master$generation)
master$sex <- as.factor(master$sex)


master <- master[, - 8]
gen <- master$generation
gen2 <- unique(gen)

unique(master$year)

master %>% select(generation, age) %>% unique()


avg1 <- master %>% group_by(country) %>% 
        filter(generation == "Generation X", year == 2010) %>% 
        summarise(avg = mean(suicides_no), med = median(suicides_no)) %>% 
        arrange(avg) %>%
        head(20)

avg1 %>% ggplot(aes( x = reorder(country, avg), y = avg)) +
         geom_col() + 
         coord_flip() + 
         scale_x_discrete("Country") + 
         scale_y_continuous("Average suicide in pop") + 
         labs(title = "Average suicide per country in 2010") + 
         theme_economist() + 
         theme(axis.text.y = element_text(size = 7))

master %>% filter(country == "France", age == "75+ years") %>% 
      ggplot(aes(x = year, y = suicides_no, color = sex)) + 
      geom_line() + 
      facet_grid(. ~sex)

country <- sample(master$country, 5) 

master%>% group_by(country) %>% filter(year == 2010, suicides_no > 0, age == "75+ years") %>% arrange(desc(`gdp_per_capita ($)`)) %>% 
      ggplot(aes(`gdp_per_capita ($)`, `suicides/100k pop`)) +
      geom_point() + 
      stat_smooth(model = "lm")
