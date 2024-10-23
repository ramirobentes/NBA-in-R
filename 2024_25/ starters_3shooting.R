library(tidyverse)
library(hoopR)
library(janitor)

player_logs <- map_df(hoopR::year_to_season(2009:2024),
                      ~ nba_leaguegamelog(season = ., player_or_team = "P") %>%
                        pluck("LeagueGameLog")) %>%
  clean_names() %>%
  mutate(across(c(player_id, team_id), as.numeric))


lineups <- map_df(2010:2025, 
                  ~ read_csv(glue::glue("https://github.com/ramirobentes/nba_pbp_data/raw/main/lineup-final{.}/data.csv")))

player_logs %>%
  select(game_id, team_abb = team_abbreviation, player_id, player_name, fg3m, fg3a) %>%
  mutate(across(c(fg3m, fg3a), as.integer)) %>%
  semi_join(lineups %>%
              filter(period == 1, stint == 1) %>%
              separate_rows(lineup_team, sep = ", ") %>%
              mutate(player_id = as.integer(word(lineup_team, 1)))) %>%
  group_by(game_id, team_abb) %>%
  summarise(over_50pct = sum(fg3m / fg3a >= 0.5, na.rm = TRUE),
            over_2fgm = sum(fg3m >= 3, na.rm = TRUE)) %>%
  ungroup() %>%
  filter(over_50pct == 5, 
         over_2fgm == 5)
