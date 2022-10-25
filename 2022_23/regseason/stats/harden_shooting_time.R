library(tidyverse)
library(hoopR)
library(janitor)

player_logs <- nba_leaguegamelog(season = "2022-23", player_or_team = "P") %>%
  pluck("LeagueGameLog") %>%
  clean_names()

selected_player_id <- player_logs %>%
  filter(player_name == "James Harden") %>%
  distinct(player_id) %>%
  pull(player_id)

selected_team_id <- player_logs %>%
  filter(team_abbreviation == "PHI") %>%
  distinct(team_id) %>%
  pull(team_id)

# for FGA % by touching time in 2021-22 with Philly and first game of 2022-23
shooting_dash22_phi <- hoopR::nba_playerdashptshots(season = "2021-22", player_id = selected_player_id) # only for 76ers games
shooting_dash23_phi <- hoopR::nba_playerdashptshots(season = "2022-23", player_id = selected_player_id)

shooting_dash23 %>%
  pluck("TouchTimeShooting") %>%
  clean_names() %>%
  select(player = player_name_last_first, touch_time_range, fga_frequency, fgm, fga, fg_pct, fg3m, fg3a, fg3_pct)

# for 3pt FG% by touching time in last 3 seasons

function_shooting <- function(x){
  hoopR::nba_playerdashptshots(season = x, player_id = selected_player_id) %>%
    pluck("TouchTimeShooting") %>%
    mutate(season = x)
}

shooting_dash <- map_df(c("2019-20", "2020-21", "2021-22"), function_shooting)

shooting_dash %>%
  clean_names() %>%
  mutate(across(c(fga_frequency:fg3_pct), as.numeric)) %>%
  select(player = player_name_last_first, touch_time_range, fga_frequency, fgm, fga, fg_pct, fg3m, fg3a, fg3_pct, season) %>%
  group_by(touch_time_range) %>%
  summarise(across(c(fg3m, fg3a), sum)) %>%
  ungroup() %>%
  mutate(fg3_pct = fg3m / fg3a)

