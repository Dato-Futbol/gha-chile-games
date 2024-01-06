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
          mutate(chile_game_goal_dif = ifelse(team_home == "Chile", goals_home - goals_away, goals_away - goals_home),
                 chile_cum_goal_dif = cumsum(chile_game_goal_dif),
                 chile_cum_goal_dif_sin_amistosos = cumsum(ifelse(competition == "Partido amistoso", 0, chile_game_goal_dif)),
                 date = dmy(date),
                 competition2 = case_when(str_starts(competition, "Copa del Mundo") ~ "Copa del Mundo",
                                          str_starts(competition, "Clasificatorias") ~ "Clasificatorias",
                                          str_starts(competition, "Copa América") ~ "Copa América",
                                          str_starts(competition, "Copa Confederaciones") ~ "Copa Confederaciones",
                                          str_starts(competition, "Campeonato Sudamericano") ~ "Campeonato Sudamericano",
                                          str_starts(competition, "Juegos Olímpicos") ~ "Juegos Olímpicos",
                                          T ~ "Amistoso u otros"),
                 amistoso = ifelse(competition == "Partido amistoso", "Sí", "No")) %>% 
          filter(!is.na(goals_home))

write_rds("chile_games.rds")
