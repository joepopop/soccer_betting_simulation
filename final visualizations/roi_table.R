library(readxl)
library(tidyverse)

read_excel("final visualizations/roi_table.xlsx") %>% 
  filter(ROI > 0) %>% 
  arrange(desc(ROI)) %>% 
  arrange(desc(Matches)) %>%
  mutate(ROI = round(ROI*100, 2)) %>% 
  rename("ROI (%)" = ROI) %>% 
  rename("Cost per match ($)" = "Cost per match") %>% 
  rename("Profit ($)" = Profit) %>% 
  rename("Total Cost ($)" = "Total Cost") %>% 
  gt::gt() %>% 
  gt::tab_options(table.font.names = "Optima")
