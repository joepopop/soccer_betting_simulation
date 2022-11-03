# load packages and required objects ----

library(tidyverse)
load("data/processed/initial_split.rda")

# analyze data ----

# observe correlations between quantitative variables
football_train %>% 
  mutate(attendance = ifelse(is.na(attendance), mean(attendance, na.rm = T), attendance)) %>%
  select_if(is.numeric) %>% 
  cor() %>% 
  corrplot::corrplot()

# generate plots to analyze qualitative variables
football_train %>% 
  ggplot(aes(result, mean_score_home)) +
  geom_violin() +
  facet_wrap(~season)

football_train %>% 
  ggplot(aes(result, mean_points_home)) +
  geom_violin()

football_train %>% 
  mutate(day = factor(day, levels = c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun"))) %>% 
  ggplot(aes(day, fill = result)) +
  geom_bar(position = "fill") 


football_train %>% 
  ggplot(aes(result, attendance)) +
  geom_boxplot() 

football_train %>% 
  ggplot(aes(result, attendance)) +
  geom_boxplot() +
  facet_wrap(~season)

football_train %>% 
  ggplot(aes(result, b365h)) +
  geom_violin()

football_train %>% 
  ggplot(aes(result, b365a)) +
  geom_violin()

football_train %>% 
  ggplot(aes(result, b365d)) +
  geom_violin()
