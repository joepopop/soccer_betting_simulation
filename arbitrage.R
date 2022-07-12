
arbitrage <- read_rds("data/full_data.rds") %>% 
  # select(-max_h, -max_d, -max_a) %>%
  # mutate(
  #   max_h = pmax(b365h, bwh, iwh, psh, whh, vch, na.rm = T),
  #   max_d = pmax(b365d, bwd, iwd, psd, whd, vcd, na.rm = T),
  #   max_a = pmax(b365a, bwa, iwa, psa, wha, vca, na.rm = T),
  # ) %>% 
  ungroup() %>%
  mutate(
    bet = if_else((1/max_h + 1/max_d + 1/max_a) < 1, 1, 0),
    profit = case_when(
      result == "H" ~ max_h*bet - bet*3,
      result == "D" ~ max_d*bet - bet*3,
      result == "A" ~ max_a*bet - bet*3
    ),
    cum_profit = cumsum(replace_na(profit, 0))
    )

mean(arbitrage$profit, na.rm = T)
min(arbitrage$profit, na.rm = T)
max(arbitrage$cum_profit)
arbitrage %>% 
  filter(bet == 1)
arbitrage %>% 
  filter(ymd(date) > "2019-7-1") %>% 
  filter(bet == 1)



arbitrage %>% 
  skimr::skim_without_charts()

arbitrage %>% 
  filter(date > ymd("2019-7-1")) %>% 
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



