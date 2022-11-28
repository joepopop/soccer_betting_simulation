# load packages
library(tidyverse)
library(lubridate)

# add cumulative profit column with arbitrage strategy 
unbiased_arbitrage <- read_rds("data/processed/full_data.rds") %>% 
  filter(ymd(date) > "2019-7-1") %>% 
# calculate market max odds  
  mutate(
    bet = if_else((1/max_h + 1/max_d + 1/max_a) < 1, 1, 0),
    wager_h = 400/(1+max_h/max_d+max_h/max_a),
    wager_d = 400/(1+max_d/max_h+max_d/max_a),
    wager_a = 400/(1+max_a/max_h+max_a/max_d),
    wager_total = wager_h+wager_d+wager_a,
    profit = case_when(
      bet == FALSE ~ 0,
      bet == TRUE & result == "H" ~ wager_h*max_h-400,
      bet == TRUE & result == "D" ~ wager_d*max_d-400,
      bet == TRUE & result == "A" ~ wager_a*max_a-400
    ),
    cum_profit = cumsum(replace_na(profit, 0))
  ) %>% 
  filter(bet != 0)

# save data as csv to calculate compounded results
write_csv(unbiased_arbitrage, "data/raw/unbiased_arbitrage.csv")

# load data
unbiased_arbitrage_compounded <- readxl::read_excel("data/processed/unbiased_arbitrage.xlsm")


# generate plot of cumulative profit over time
unbiased_arbitrage %>% 
  head(50) %>%
  ggplot(aes(date, cum_profit)) +
  geom_point(size = 0.1, alpha = 0.75, color = "#c99800") +
  labs(
    title = "Unbiased arbitrage: cumulative profit from $400 bets",
    x = "Date",
    y = "Cumulative profit ($)"
  ) +
  theme_minimal() +
  theme(
    text = element_text(family = "Optima")
  )

# same graph but compounded version
unbiased_arbitrage_compounded %>% 
  head(50) %>%
  ggplot(aes(date, compounded_cum_profit)) +
  geom_point(size = 0.1, alpha = 0.75, color = "#c99800") +
  labs(
    title = "Unbiased arbitrage (compounded): cumulative profit from $400 bets",
    x = "Date",
    y = "Cumulative profit ($)"
  ) +
  theme_minimal() +
  theme(
    text = element_text(family = "Optima")
  )

max(unbiased_arbitrage_compounded$profit)
mean(unbiased_arbitrage$profit)
min(unbiased_arbitrage$profit)


