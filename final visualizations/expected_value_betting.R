# load packages
library(tidyverse)
library(lubridate)
load("data/processed/initial_split.rda")

# add cumulative profit column with ev strategy 
ev <- football_test %>% 
  filter(ymd(date) > "2019-7-1") %>% 
  mutate(
    prob_h = 1/avg_h,
    prob_d = 1/avg_d,
    prob_a = 1/avg_a,
    prob_total = prob_h+prob_d+prob_a,
    prob_h = prob_h-(prob_total-1)*(prob_h/prob_total),
    prob_a = prob_a-(prob_total-1)*(prob_a/prob_total),
    prob_d = prob_d-(prob_total-1)*(prob_d/prob_total),
    prob_total = prob_h+prob_d+prob_a,
    ev_h = 1*(max_h-1)*prob_h + -1*(1-prob_h),
    ev_d = 1*(max_d-1)*prob_d + -1*(1-prob_d),
    ev_a = 1*(max_a-1)*prob_a + -1*(1-prob_a),
    bet = case_when(
      ev_h <= 0 & ev_d <= 0 & ev_a <= 0 ~ "pass",
      pmax(ev_h, ev_d, ev_a) == ev_h ~ "H",
      pmax(ev_h, ev_d, ev_a) == ev_d ~ "D",
      pmax(ev_h, ev_d, ev_a) == ev_a ~ "A",
    ),
    profit = case_when(
      bet == "pass" ~ 0,
      bet == "H" & result == "H" ~ 1*(max_h-1),
      bet == "D" & result == "D" ~ 1*(max_d-1),
      bet == "A" & result == "A" ~ 1*(max_a-1),
      TRUE ~ -1
    ),
    cum_profit = cumsum(replace_na(profit, 0))
  ) %>% 
  filter(bet != "pass")


# generate plot of cumulative profit over time
ev %>% 
  ggplot(aes(date, cum_profit)) +
  geom_point(size = 0.1, alpha = 0.75, color = "darkgreen") +
  labs(
    title = "Positive EV: cumulative profit from $1 bets",
    x = "Date",
    y = "Cumulative profit ($)"
  ) +
  theme_minimal() +
  theme(
    text = element_text(family = "Optima")
  )


mean(pmax(ev$ev_h, ev$ev_d, ev$ev_a))
max(ev$profit)
mean(ev$profit)
min(ev$profit)
