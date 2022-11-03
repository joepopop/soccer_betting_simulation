# load packages ----
library(tidyverse)
library(rvest)
library(readxl)
library(lubridate)

# fixture data ----

# get paths of excel files with fixture data
fixture_files <- dir(path = "data/raw/fixture_data/", pattern = '\\.xls', full.names = T)

# define function to retrieve table from excel files
get_table <- function(file){
  file %>% 
    read_html(encoding = "UTF-8") %>% 
    html_table() 
}

# gather tables from excel files into a dataframe
fixture_data <- fixture_files %>%
  map(get_table) %>%    
  reduce(bind_rows)  

# process fixture data
fixture_data <- fixture_data %>%  
  janitor::clean_names() %>% 
  filter(!grepl('Relegation', round)) %>% 
  mutate_all(na_if,"") %>% 
  filter(!((is.na(wk)) & (is.na(score)))) %>% 
  filter(!grepl('Postponed', notes)) %>%
  filter(!grepl('awarded', notes)) %>%
  filter(!grepl('Cancelled', notes)) %>% 
  separate(score, into = c("score_home", "score_away")) %>% 
  rename(xg_home = x_g_7) %>% 
  rename(xg_away = x_g_9) %>% 
  mutate(
    score_home = as.numeric(score_home),
    score_away = as.numeric(score_away),
    attendance = as.numeric(gsub("\\,", "", attendance)),
    result = case_when(
      score_home > score_away ~ "H",
      score_home < score_away ~ "A",
      score_home == score_away ~ "D"
    ),
    points_home = case_when(
      result == "H" ~ 3,
      result == "A" ~ 0,
      result == "D" ~ 1
    ),
    points_away = case_when(
      result == "A" ~ 3,
      result == "H" ~ 0,
      result == "D" ~ 1
    )
  ) %>% 
  relocate(points_home, .before = xg_home) %>% 
  relocate(points_away, .after = xg_away) %>% 
  relocate(result, .before = home) %>% 
  select(-(contains("x_g")), -notes, -round) %>% 
  mutate(
    season = year(date),
    date = ymd(date)
    ) %>% 
  arrange(date) %>% 
  group_by(season, home) %>% 
  mutate(
    lag_points_home = lag(points_home, n = 1),
    lag_score_home = lag(score_home, n = 1),
    mean_points_home = zoo::rollapplyr(lag_points_home, 4, mean, na.rm = TRUE, by = 1, partial = TRUE, fill = NA),
    mean_score_home = zoo::rollapplyr(lag_score_home, 4, mean, na.rm = TRUE, by = 1, partial = TRUE, fill = NA)
  ) %>% 
  ungroup() %>% 
  group_by(season, away) %>% 
  mutate(
    lag_points_away = lag(points_away, n = 1),
    lag_score_away = lag(score_away, n = 1),
    mean_points_away = zoo::rollapplyr(lag_points_away, 4, mean, na.rm = TRUE, by = 1, partial = TRUE, fill = NA),
    mean_score_away = zoo::rollapplyr(lag_score_away, 4, mean, na.rm = TRUE, by = 1, partial = TRUE, fill = NA)
  ) %>% 
  ungroup() %>% 
  select(-points_home, -xg_home, -score_home, -points_away, -xg_away, -score_away, -starts_with("lag")) %>% 
  drop_na(mean_points_home, mean_score_home, mean_points_away, mean_score_away)
  
# odds_data ----

# get paths of csv files with odds data
odds_files <- dir(path = "data/raw/odds_data/", pattern = '\\.csv', full.names = T)

# gather tables from csv files into dataframe
odds_data <- odds_files %>%
  map(read_csv) %>%  
  reduce(bind_rows)        

# process odds data
odds_data <- odds_data %>% 
  janitor::clean_names() %>% view()
  rename(home = home_team) %>%
  mutate(
    date = dmy(date)
  ) %>% 
  select(date, home, b365h, b365d, b365a, bwh, bwd, bwa, iwh, iwd, iwa, psh, psd, psa, whh, whd, wha, vch, vcd, vca, max_h, max_d, max_a, avg_h, avg_d, avg_a) %>% 
  mutate(
    home = case_when(
      home == "Ein Frankfurt" ~ "Eint Frankfurt",
      home == "FC Koln" ~ "Köln",
      home == "Hertha" ~ "Hertha BSC",
      home == "Bielefeld" ~ "Arminia",
      home == "Mainz" ~ "Mainz 05",
      home == "M'gladbach" ~ "M'Gladbach",
      home == "Fortuna Dusseldorf" ~ "Düsseldorf",
      home == "Paderborn" ~ "Paderborn 07",
      home == "Hannover" ~ "Hannover 96",
      home == "Nurnberg" ~ "Nürnberg",
      home == "Hamburg" ~ "Hamburger SV",
      home == "Greuther Furth" ~ "Greuther Fürth",
      home == "Sheffield United" ~ "Sheffield Utd",
      home == "Leeds" ~ "Leeds United",
      home == "Man United" ~ "Manchester Utd",
      home == "Newcastle" ~ "Newcastle Utd",
      home == "Leicester" ~ "Leicester City",
      home == "Man City" ~ "Manchester City",
      home == "Norwich" ~ "Norwich City",
      home == "Cardiff" ~ "Cardiff City",
      home == "Stoke" ~ "Stoke City",
      home == "Swansea" ~ "Swansea City",
      home == "Nimes" ~ "Nîmes",
      home == "St Etienne" ~ "Saint-Étienne",
      home == "Paris SG" ~ "Paris S-G",
      home == "Clermont" ~ "Clermont Foot",
      home == "Verona" ~ "Hellas Verona",
      home == "Spal" ~ "SPAL",
      home == "Cadiz" ~ "Cádiz",
      home == "Alaves" ~ "Alavés",
      home == "Celta" ~ "Celta Vigo",
      home == "Sociedad" ~ "Real Sociedad",
      home == "Ath Madrid" ~ "Atlético Madrid",
      home == "Ath Bilbao" ~ "Athletic Club",
      home == "Leganes" ~ "Leganés",
      home == "Espanol" ~ "Espanyol",
      home == "Vallecano" ~ "Rayo Vallecano",
      home == "La Coruna" ~ "La Coruña",
      home == "Malaga" ~ "Málaga",
      TRUE ~ home)
  )

# combine data ----

# full join fixture and odds data 
  full_data <- full_join(fixture_data, odds_data) %>% 
  select(-contains("365c"), -contains("avg")) %>% 
  drop_na(wk, b365h, b365d, b365a, bwh, bwd, bwa, iwh, iwd, iwa, psh, psd, psa, whh, whd, wha, vch, vcd, vca) %>% 
  mutate(season = as.character(season))

# save data ----
write_rds(full_data, "data/processed/full_data.rds")
write_rds(fixture_data, "data/processed/fixture_data.rds")
write_rds(odds_data, "data/processed/odds_data.rds")

