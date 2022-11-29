library(readxl)
library(tidyverse)

read_excel("final documents/results_table.xlsx") %>% 
  arrange(desc(`Number of bets`)) %>%
  arrange(desc(`Estimated profit per season ($)*`)) %>% 
  filter(!str_detect(Method, "\\((A|D)\\)")) %>%
  filter(Method != "Arbitrage" & Method != "Arbitrage (Compounded)") %>% 
  gt::gt() %>% 
  gt::tab_options(table.font.names = "Optima")
