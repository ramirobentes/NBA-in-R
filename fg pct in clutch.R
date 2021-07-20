library(tidyverse)
library(nbastatR)
library(future)

# Player FG% from 3, from 2 and FT in the clutch

year_season <- 2021
slug_team <- "MIA"
name_player <- "Jimmy Butler"

# Get games table to find team game ids
all_games <- game_logs(year_season, result_types = "team")

all_games_id <- all_games %>%
  filter(slugTeam == slug_team) %>%
  distinct(idGame) %>%
  pull(idGame)

# Get play-by-play for every game of the team
plan(multiprocess)
play_logs <- play_by_play_v2(game_ids = all_games_id)



play_edited <- play_logs %>%
  distinct(idGame, numberEvent, .keep_all = TRUE) %>%
  mutate(pts_play_home = case_when(numberEventMessageType == 1 & str_detect(descriptionPlayHome, "3PT") ~ 3,
                                   numberEventMessageType == 1 & !str_detect(descriptionPlayHome, "3PT") ~ 2,
                                   numberEventMessageType == 3 & !str_detect(descriptionPlayHome, "MISS") ~ 1,
                                   TRUE ~ 0),
         pts_play_away = case_when(numberEventMessageType == 1 & str_detect(descriptionPlayVisitor, "3PT") ~ 3,
                                   numberEventMessageType == 1 & !str_detect(descriptionPlayVisitor, "3PT") ~ 2,
                                   numberEventMessageType == 3 & !str_detect(descriptionPlayVisitor, "MISS") ~ 1,
                                   TRUE ~ 0)) %>%
  group_by(idGame) %>%
  mutate(total_pts_home = cumsum(pts_play_home),
         total_pts_away = cumsum(pts_play_away)) %>%
  ungroup() %>%
  left_join(all_games %>%
              filter(slugTeam == slug_team) %>%
              select(idGame, locationGame)) %>%
  mutate(description_team = ifelse(locationGame == "H", descriptionPlayHome, descriptionPlayVisitor))

play_edited %>%
  filter(abs(lag(total_pts_home) - lag(total_pts_away)) <= 5,
         minuteGame >= 43,
         namePlayer1 == name_player) %>%
  filter(numberEventMessageType %in% c(1, 2, 3)) %>%
  mutate(shot_result = case_when(numberEventMessageType == 1 & str_detect(description_team, "3PT") ~ "made_three",
                                 numberEventMessageType == 2 & str_detect(description_team, "3PT") ~ "missed_three",
                                 numberEventMessageType == 1 & !str_detect(description_team, "3PT") ~ "made_two",
                                 numberEventMessageType == 2 & !str_detect(description_team, "3PT") ~ "missed_two",
                                 numberEventMessageType == 3 & !str_detect(description_team, "MISS") ~ "made_ft",
                                 numberEventMessageType == 3 & str_detect(description_team, "MISS") ~ "missed_ft")) %>%
  count(shot_result) %>%
  separate(shot_result, into = c("result", "shot"), sep = "_") %>%
  pivot_wider(names_from = shot,
              values_from = n) %>%
  mutate(fg = three + two)

  
# for comparison: https://www.nba.com/stats/players/clutch-traditional/?sort=GP&dir=-1&Season=2020-21&SeasonType=Regular%20Season&CF=PLAYER_NAME*E*Jimmy%20Butler&PerMode=Totals