data("Vocab")

Vocab
glimpse(vocab)
vocab <- as.tbl(Vocab)
head(vocab1)
dim(Vocab)

vocab1 <- vocab %>% count(education) %>% mutate(education_levels = ifelse(education <= 12, "poor_education", "high_education")) 

vocab1 %>% ggplot(aes(education, n, color = education_levels)) + 
      geom_point()

(vocab2 <- vocab1 %>% mutate(prop = n/sum(n)) %>% arrange(desc(prop)))
sum(vocab2$prop)
            
vocab2  %>% filter(education_levels == "poor_education") %>% count(education_levels)

Vocab %>% group_by(sex) %>% sample_n(size = 150) %>% count(sex)
