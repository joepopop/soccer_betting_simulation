library(readxl)
library(tidyverse)

read_excel("final visualizations/roi_table.xlsx") %>% 
  select(Method, Profit, `Profit per season`, Matches) %>% 
  arrange(desc(Matches)) %>%
  arrange(desc(`Profit per season`)) %>% 
  rename(`Profit ($)*` = Profit) %>% 
  rename(`Profit per season ($)*` = `Profit per season`) %>% 
  filter(!str_detect(Method, "\\((A|D)\\)")) %>%
  gt::gt() %>% 
  gt::tab_options(table.font.names = "Optima")
