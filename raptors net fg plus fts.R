library(tidyverse)
library(nbastatR)   # devtools::install_github("abresler/nbastatR")
library(future)

# find every game the Raptors have played
logs_teams <- game_logs(seasons = 2022, result_types = "team")
logs_raptors <- logs_teams %>%
  filter(slugTeam == "TOR") %>%
  distinct(slugTeam, slugOpp = slugOpponent, idGame)

# download play-by-pay for raptors games
plan(multiprocess)
play_logs_all <- play_by_play_v2(game_ids = logs_raptors$idGame)

# get field goal attempts and free throw trips (ft 1 of 2 and 1 of 3 + flagrant 1 of 2 and 1 of 3)
play_logs_all %>%
  select(idGame, numberEventMessageType, numberEventActionType, slugTeamPlayer1,
         descriptionPlayHome, descriptionPlayVisitor) %>%
  filter(numberEventMessageType %in% c(1, 2) | (numberEventMessageType == 3 & numberEventActionType %in% c(11, 13, 18, 27))) %>%
  count(idGame, slugTeamPlayer1) %>%
  mutate(team = ifelse(slugTeamPlayer1 == "TOR", "Raptors", "Opponent")) %>%
  select(-slugTeamPlayer1) %>%
  pivot_wider(names_from = team, 
              values_from = n) %>%
  mutate(Net = Raptors - Opponent) %>%
  left_join(logs_raptors %>%
              select(idGame, slugOpp))