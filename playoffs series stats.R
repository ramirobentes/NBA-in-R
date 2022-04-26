library(tidyverse)
library(hoopR)
library(janitor)
library(future)

seasons <- year_to_season(c(1946:2021))
plan(multiprocess)
playoffs_logs <- furrr::future_map_dfr(seasons, ~ nba_leaguegamelog(season = ., 
                                                                    player_or_team = "T",
                                                                    season_type = "Playoffs")) %>%
  pluck("LeagueGameLog") %>%
  clean_names() %>%
  mutate(across(c(min:plus_minus), as.numeric))

playoffs_series <- playoffs_logs %>%
  transmute(season_id, game_id, game_date = as.Date(game_date), slug_team = team_abbreviation, matchup, pts_team = as.numeric(pts)) %>%
  group_by(season_id, game_id) %>%
  mutate(pts_opp = sum(pts_team) - pts_team) %>%
  ungroup() %>%
  mutate(location = ifelse(str_detect(matchup, "\\@"), "away", "home"),
         result = ifelse(pts_team > pts_opp, "W", "L")) %>%
  arrange(game_date) %>%
  mutate(series = map_chr(str_extract_all(matchup, "[A-Z]{3}"), ~ paste(sort(.), collapse = " vs "))) %>%
  group_by(season_id, series, slug_team) %>%
  mutate(game_number = row_number(),
         wins = cumsum(result == "W")) %>%
  ungroup() %>%
  arrange(game_date, series)

playoffs_series %>%
  group_by(season_id, series) %>%
  filter(max(wins) == 4) %>%
  mutate(series_win = unique(slug_team[which(wins == max(wins))]),
         number_games = n_distinct(game_id)) %>%
  ungroup() %>%
  filter(game_number == 3,
         wins == 3,
         number_games >= 6) %>%
  mutate(season = as.numeric(str_sub(season_id, -4, -1)),
         season = paste(season, str_sub(season + 1, -2, -1), sep = "-")) %>%
  select(season, series, lead_3_0 = slug_team, series_win, number_games)

playoffs_series %>%
  group_by(season_id, series, slug_team) %>%
  filter(max(wins) == 4) %>%
  filter(sum(game_number == 4 & wins == 4) > 0) %>%
  summarise(pts_diff = sum(pts_team - pts_opp)) %>%
  ungroup() %>%
  arrange(pts_diff) %>%
  mutate(season = as.numeric(str_sub(season_id, -4, -1)),
         season = paste(season, str_sub(season + 1, -2, -1), sep = "-")) %>%
  select(season, series, winner = slug_team, pts_diff)
