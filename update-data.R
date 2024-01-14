library(rvest)
library(dplyr)
library(stringr)
library(lubridate)
library(readr)
library(janitor)


# chile games
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


# chile dts
path_dt = "https://www.partidosdelaroja.com/1970/01/entrenadores.html"
web_dt = read_html(path_dt)
tables_dt = html_table(web_dt)

data_dt = tables_dt[2] %>% as.data.frame()
names(data_dt) = data_dt[1, ]

numeric_columns = c("pj", "pg", "pe", "pp", "gf", "gc", "dif")

dt_stats = data_dt %>%
          clean_names() %>% 
          rename("dt" = "entrenador") %>% 
          filter(!dt %in% c("Entrenador", "Total", "Sin entrenador")) %>% 
          mutate(across(all_of(numeric_columns), ~as.numeric(.x))) %>% 
          mutate(desde = dmy(desde),
                 hasta = dmy(hasta)) 

write_csv(dt_stats, "chile_dts_stats.csv")
