# load packages
library(tidyverse)
library(lubridate)

# add cumulative profit column with arbitrage strategy 
arbitrage <- read_rds("data/processed/full_data.rds") %>% 
  filter(ymd(date) > "2019-7-1") %>% 
# calculate market max odds  
  mutate(
    max_h = pmax(b365h, bwh, iwh, psh, whh, vch, na.rm = T),
    max_d = pmax(b365d, bwd, iwd, psd, whd, vcd, na.rm = T),
    max_a = pmax(b365a, bwa, iwa, psa, wha, vca, na.rm = T),
    bet = if_else((1/max_h + 1/max_d + 1/max_a) < 1, 1, 0),
    profit = if_else(bet == TRUE, 1/(1+max_h/max_d+max_h/max_a)*(max_h-1), 0),
    cum_profit = cumsum(replace_na(profit, 0))
  ) %>% 
  filter(bet == 1)

# generate plot of cumulative profit over time
arbitrage %>% 
  ggplot(aes(date, cum_profit)) +
  geom_point(size = 0.1, alpha = 0.75, color = "#c99800") +
  labs(
    title = "Cumulative profit from betting on all outcomes ($1 total) for every match with arbitrage opportunity",
    x = "Date",
    y = "Cumulative profit ($)"
  ) +
  theme_minimal() +
  theme(
    text = element_text(family = "Optima")
  )

mean(arbitrage$profit)
