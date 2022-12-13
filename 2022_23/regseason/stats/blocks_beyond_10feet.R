library(tidyverse)
library(hoopR)
library(janitor)

pbp2023 <- read_rds("https://github.com/ramirobentes/NBA-in-R/blob/master/2022_23/regseason/pbp/pbp-poss-rs23/data.rds?raw=true")

shots_season <- nba_shotchartdetail(season = "2022-23",
                                    player_id = 0) %>%
  pluck("Shot_Chart_Detail") %>%
  clean_names()

pbp2023 %>%
  filter(msg_type == 2,
         !is.na(player3)) %>%
  left_join(shots_season %>%
              transmute(across(c(game_id, number_original = game_event_id, shot_distance), as.integer), shot_zone_basic)) %>%
  select(game_id, period, clock, player1, player3, description, number_original, desc_value, shot_zone_basic, shot_distance) %>%
  filter(shot_distance >= 10) %>%
  count(name_player = player3, sort = T, name = "blocked_shots")
