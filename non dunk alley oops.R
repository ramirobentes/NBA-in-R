library(tidyverse)
library(nbastatR)
library(future)

logs_teams <- game_logs(seasons = 2022, result_types = "team")
games <- logs_teams %>%
  mutate(slugTeamHome = ifelse(locationGame == "H", slugTeam, slugOpponent),
         slugTeamAway = ifelse(locationGame == "A", slugTeam, slugOpponent)) %>%
  distinct(idGame, slugTeamHome, slugTeamAway)

plan(multiprocess)
play_logs_all <- play_by_play_v2(game_ids = unique(logs_teams$idGame))

play_logs_all %>%
  left_join(games %>%
              select(idGame, slugTeamHome, slugTeamAway)) %>%
  filter(numberEventMessageType == 1) %>%
  mutate(description = ifelse(slugTeamPlayer1 == slugTeamHome, descriptionPlayHome, descriptionPlayVisitor)) %>%
  filter(str_detect(description, "Alley Oop Layup")) %>%
  count(namePlayer1, sort = T)
