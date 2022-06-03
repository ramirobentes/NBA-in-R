library(tidyverse)
library(hoopR)
library(janitor)
library(future)

seasons <- year_to_season(1979:2021)
plan(multisession)
regseason_logs <- furrr::future_map_dfr(seasons, ~ nba_leaguegamelog(season = ., 
                                                                     player_or_team = "P",
                                                                     season_type = "Regular Season")) %>%
  pluck("LeagueGameLog")

plan(multisession)
playoffs_logs <- furrr::future_map_dfr(seasons, ~ nba_leaguegamelog(season = ., 
                                                                    player_or_team = "P",
                                                                    season_type = "Playoffs")) %>%
  pluck("LeagueGameLog")

players3 <- bind_rows(regseason_logs, playoffs_logs) %>%
  clean_names() %>%
  mutate(season = as.numeric(str_sub(season_id, -4, -1)) + 1,
         across(c(min:fantasy_pts), as.numeric),
         season_type = fct_rev(ifelse(str_starts(season_id, "4"), "playoffs", "reg_season"))) %>%
  group_by(season, season_type, player_id, player_name) %>%
  summarise(across(c(fg3m, fg3a), ~ sum(., na.rm = TRUE))) %>%
  ungroup() %>%
  mutate(fg3pct = fg3m / fg3a)

players3 %>%
  pivot_wider(names_from = season_type,
              values_from = starts_with("fg3")) %>%
  filter(fg3a_reg_season >= 250,
         fg3a_playoffs >= 80) %>%
  mutate(diff = fg3pct_playoffs - fg3pct_reg_season) %>%
  arrange(-diff)
