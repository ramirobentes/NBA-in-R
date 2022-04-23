library(tidyverse)
library(hoopR)
library(janitor)
library(lubridate)
library(slider)

seasons_combs <- expand_grid(season = paste(2007:2021, str_pad(08:22, pad = 0, width = 2), sep = "-"),
                             season_type = c("Regular Season", "Playoffs"))
kd_logs <- map2_df(seasons_combs$season, seasons_combs$season_type, ~ nba_playergamelog(player_id = "201142", 
                                                                                        season = .x,
                                                                                        season_type = .y) %>%
                     pluck("PlayerGameLog")) %>%
  clean_names()
                     

kd_logs %>%
  mutate(game_date = mdy(game_date),
         across(c(min:plus_minus), as.numeric)) %>%
  arrange(game_date) %>%
  group_by(season_id) %>%
  mutate(across(c(fgm:fg3a), ~ slide_dbl(., sum, .before = 1), .names = "rolling_{.col}")) %>%
  filter(row_number() > 1,
         season_id != "22007") %>%
  ungroup() %>%
  select(game_date, matchup, pts, fgm, fga, fg3m, fg3a, starts_with("rolling")) %>%
  mutate(eff_fg = (rolling_fgm + (rolling_fg3m * 0.5)) / rolling_fga) %>%
  arrange(eff_fg)
  
