library(tidyverse)
library(hoopR)
library(janitor)

team_logs <- map_df(year_to_season(1946:2023), 
                    function(x){
                      nba_leaguegamelog(season = x, player_or_team = "T") %>%
                        pluck("LeagueGameLog")
                    }) %>%
  clean_names()

team_logs %>%
  mutate(plus_minus = as.numeric(plus_minus)) %>%
  group_by(season_id, team_id) %>%
  mutate(streak = cumsum(plus_minus >= 10),
         game_number = row_number()) %>%
  ungroup() %>%
  filter(plus_minus < 10) %>%
  group_by(season_id, team_id, team_abbreviation, team_name, streak) %>%
  summarise(games = n(),
            start_game = min(game_number),
            end_game = max(game_number)) %>%
  ungroup() %>%
  filter(start_game == 1) %>%         # for streaks to open season
  arrange(-games) %>%
  select(-c(start_game, end_game, streak)) 
