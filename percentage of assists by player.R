library(tidyverse)
library(nbastatR)
library(future)

# What percentage of team made field goals (in this case, 3-pt field goals) were assisted by a player?

year_season <- 2021
slug_team <- "PHI"

# Get games table to find team game ids
all_games <- game_logs(year_season, result_types = "team")

all_games_id <- all_games %>%
  filter(slugTeam == slug_team) %>%
  distinct(idGame) %>%
  pull(idGame)

# Get play-by-play for every game of the team
plan(multiprocess)
play_logs <- play_by_play_v2(game_ids = all_games_id)

# Find number of 3s made by team and how many were assisted by player
play_logs %>%
  filter(numberEventMessageType == 1,
         slugTeamPlayer1 == slug_team) %>%
  filter(str_detect(descriptionPlayHome, "3PT") | str_detect(descriptionPlayVisitor, "3PT")) %>%
  count(namePlayer = namePlayer2, name = "assists_on_3s") %>%
  replace_na(list(namePlayer = "Unassisted")) %>%
  mutate(percentage_on_3s = assists_on_3s / sum(assists_on_3s)) %>%
  arrange(desc(percentage_on_3s))
  

