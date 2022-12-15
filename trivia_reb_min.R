library(tidyverse)
library(hoopR)
library(janitor)

seasons <- paste(2000:2022, str_pad(01:23, pad = 0, width = 2), sep = "-")
player_logs_multi <- map_df(seasons, ~ nba_leaguegamelog(season = ., player_or_team = "P") %>%
                              pluck("LeagueGameLog")) %>%
  clean_names() %>%
  mutate(across(c(min:fantasy_pts), as.numeric),
         game_date = as.Date(game_date))

players_info <- hoopR::nba_playerindex() %>%
  pluck("PlayerIndex") %>%
  clean_names()

player_logs_multi %>%
  semi_join(players_info %>%
              separate(height, into = c("feet", "inches"), sep = "-", convert = TRUE) %>%
              mutate(total_inches = feet * 12 + inches) %>%
              filter(total_inches >= 82 & str_detect(position, "C")) %>%
              select(player_id = person_id)) %>%
  filter(min >= 30,
         reb <= 2) %>%
  count(player_id, player_name, sort = T)
