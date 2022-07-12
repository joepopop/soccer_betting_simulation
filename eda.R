library(tidyverse)

# load required objects ----
load("data/initial_split.rda")

football_train %>% 
  mutate(attendance = ifelse(is.na(attendance), mean(attendance, na.rm = T), attendance)) %>%
  select_if(is.numeric) %>% 
  cor() %>% 
  corrplot::corrplot()


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
# draws and away wins happen when there are few attendees!

football_train %>% 
  ggplot(aes(result, attendance)) +
  geom_boxplot() +
  facet_wrap(~season)

# will include season to account for attendance difference in 2021

football_train %>% 
  ggplot(aes(result, b365h)) +
  geom_violin()

football_train %>% 
  ggplot(aes(result, b365a)) +
  geom_violin()

football_train %>% 
  ggplot(aes(result, b365d)) +
  geom_violin()
