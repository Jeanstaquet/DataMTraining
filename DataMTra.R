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

master %>% group_by(country) %>% filter(year == 2010, suicides_no > 0, age == "75+ years") %>% arrange(desc(`gdp_per_capita ($)`)) %>% 
      ggplot(aes(`gdp_per_capita ($)`, `suicides/100k pop`)) +
      geom_point() + 
      stat_smooth(model = "lm")

master %>% group_by(country) %>%
      filter(year == 2010, age == "75+ years") %>%
      ggplot(aes(x =`suicides/100k pop`, y = population, fill = sex)) + 
      geom_violin()



small_trains <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-02-26/small_trains.csv") 
small_trains %>% glimpse %>% head %>% summary
small_trains[, c(3, 4, 5, 12)] <- map(small_trains[, c(3, 4, 5, 12)], as.factor)

summary(small_trains)
small_trains$month[small_trains$month == 1] <- "Jan"
small_trains$month[small_trains$month == 2] <- "Feb"
small_trains$month[small_trains$month == 3] <- "Mar"
small_trains$month[small_trains$month == 4] <- "Ap"
small_trains$month[small_trains$month == 5] <- "Mei"
small_trains$month[small_trains$month == 6] <- "Jun"
small_trains$month[small_trains$month == 7] <- "Jul"
small_trains$month[small_trains$month == 8] <- "Aug"
small_trains$month[small_trains$month == 9] <- "Sep"
small_trains$month[small_trains$month == 10] <- "Oct"
small_trains$month[small_trains$month == 11] <- "Nov"
small_trains$month[small_trains$month == 12] <- "Dec"

head(small_trains)
small_trains$delay_cause
summary(small_trains$delay_cause)

## plot of the différent delay_cause
retard_causes <- small_trains %>% group_by(delay_cause) %>% 
                 summarise(n_obs = n()) %>%
                 mutate(percent = round(n_obs/sum(n_obs) * 100))
poucentage_cause <- retard_causes$percent
names(poucentage_cause) <- retard_causes$delay_cause

waffle::waffle(poucentage_cause, rows = 5)

small_trains %>% ggplot(aes(x = 1, fill = delay_cause)) + 
      geom_bar(color = "white") +
      coord_polar(theta = "y") + 
      labs(title = "Causes de retards des trains") 

## Est ce qu'il y a plus de retard pour les longs trajets ?
glimpse(small_trains)
ret_tot <- small_trains %>% mutate(retard_total = avg_delay_all_departing + avg_delay_all_arriving)

ret_tot %>% ggplot(aes(journey_time_avg, retard_total)) +
      geom_point() + 
      geom_smooth(method = "lm")

cor(ret_tot$retard_total, ret_tot$journey_time_avg) 
hist(ret_tot$journey_time_avg)

small_trains %>% group_by(service) %>% summarise(avg_journey = mean(journey_time_avg),
                                                 avg_delay_all_departing = mean(avg_delay_all_departing),
                                                 avg_delay_all_arriving = mean(avg_delay_all_arriving))

small_trains %>% group_by(month) %>% 
      summarise(avg_journey = mean(journey_time_avg),
                                               avg_delay_all_departing = mean(avg_delay_all_departing),
                                               avg_delay_all_arriving = mean(avg_delay_all_arriving)) %>% 
      arrange(desc(avg_delay_all_departing)) %>%
            ggplot(aes(avg_delay_all_departing, avg_delay_all_arriving)) +
            geom_point() 
small_trains %>% group_by(departure_station) %>% 
                 summarise(avg = mean(avg_delay_all_departing)) %>% 
                 arrange(avg)
small_trains %>% group_by(departure_station) %>% 
                 summarise(avg = mean(avg_delay_all_departing)) %>% 
                 arrange(desc(avg)) %>%
                 head(10) %>%
                  ggplot(aes(reorder(departure_station, avg), avg)) + 
                        geom_point() + 
                        coord_flip() + 
                        labs(title = "Les 10 pires stations de retards au départ")

