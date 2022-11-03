# load packages
library(tidyverse)
library(lubridate)

# add cumulative profit column with arbitrage strategy 
arbitrage <- read_rds("data/processed/full_data.rds") %>% 
  filter(ymd(date) > "2019-7-1") %>% 
  mutate(
    bet = if_else((1/max_h + 1/max_d + 1/max_a) < 1, 1, 0),
    profit = case_when(
      result == "H" ~ max_h*bet - bet*3,
      result == "D" ~ max_d*bet - bet*3,
      result == "A" ~ max_a*bet - bet*3
    ),
    cum_profit = cumsum(replace_na(profit, 0))
    )

# generate plot of cumulative profit over time
arbitrage %>% 
  ggplot(aes(date, cum_profit)) +
  geom_point(size = 0.1, alpha = 0.75, color = "#c99800") +
  labs(
    title = "Cumulative profit from betting $1 on all outcomes ($3 total) for every match with arbitrage opportunity",
    x = "Date",
    y = "Cumulative profit ($)"
  ) +
  theme_minimal() +
  theme(
    text = element_text(family = "Optima")
  )