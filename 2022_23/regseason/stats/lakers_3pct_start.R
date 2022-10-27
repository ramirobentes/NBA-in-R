library(tidyverse)
library(hoopR)
library(janitor)
library(future)

seasons <- 1985:2022 %>%
  enframe(name = NULL) %>%
  mutate(season_id = paste(value, str_sub(value + 1, -2, -1), sep = "-")) %>%
  pull(season_id)

plan(multicore)
team_logs_multi <- map_df(seasons, ~ nba_leaguegamelog(season = ., player_or_team = "T") %>%
                            pluck("LeagueGameLog")) %>%
  clean_names()

team_logs_multi %>%
  mutate(across(starts_with("fg"), as.numeric),
         season = paste(str_sub(season_id, -4, -1), str_pad(as.numeric(str_sub(season_id, -2, -1)) + 1, pad = 0, width = 2), sep = "-")) %>%
  arrange(game_date) %>%
  group_by(season, team_abbreviation) %>%
  mutate(number_games = row_number(),
         across(c(fg3m, fg3a), cumsum)) %>%
  ungroup() %>%
  select(season, team_abbreviation, number_games, starts_with("fg3")) %>%
  mutate(fg3_pct = fg3m / fg3a) %>%
  filter(number_games == 4,
         fg3a >= 80) %>%
  arrange(fg3_pct)
