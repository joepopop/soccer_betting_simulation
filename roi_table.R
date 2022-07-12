library(readxl)

read_excel("roi_table.xlsx") %>% 
  filter(ROI > 0) %>% 
  arrange(desc(ROI)) %>% 
  arrange(desc(Matches)) %>%
  mutate(ROI = ROI*100) %>% 
  rename("ROI (%)" = ROI) %>% 
  gt::gt() %>% 
  gt::tab_options(table.font.names = "Optima")
