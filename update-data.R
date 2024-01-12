library(rvest)
library(dplyr)
library(stringr)
library(lubridate)
library(readr)

path = 'https://www.partidosdelaroja.com/1970/01/partidos-clase-a.html'
web = read_html(path)

tables = html_table(web)

games = tables[2] %>% as.data.frame()

columns = c("num_game", "date", "city", "team_home", "goals_home", "team_away", "goals_away", "competition")

names(games) = columns

games_ok = games %>% 
          slice(3:nrow(games)) %>%
          mutate(across(c(goals_home, goals_away), ~gsub("[(].*", "", .x))) %>% 
          mutate(across(c(num_game, goals_home, goals_away), ~as.numeric(.x))) %>% 
          mutate(date = dmy(date)) %>% 
          filter(!is.na(goals_home))

write_csv(games_ok, "chile_games.csv")
