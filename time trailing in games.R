library(tidyverse)
library(future)
library(nbastatR)
library(janitor)

team_logs <- game_logs(seasons = c(2022), result_types = "team")
team_analysis <- "MIL"
team_games <- team_logs %>%
  filter(slugTeam == team_analysis) %>%
  distinct(idGame)
plan(multiprocess)
play_logs_all <- play_by_play_v2(game_ids = team_games$idGame)

new_pbp <- play_logs_all %>%
  distinct(idGame, numberEvent, .keep_all = TRUE) %>%   # remove duplicate events
  mutate(secsLeftQuarter = (minuteRemainingQuarter * 60) + secondsRemainingQuarter) %>%                       
  mutate(secsStartQuarter = case_when(                                                                        
    numberPeriod %in% c(1:5) ~ (numberPeriod - 1) * 720,
    TRUE ~ 2880 + (numberPeriod - 5) * 300
  )) %>%
  mutate(secsPassedQuarter = ifelse(numberPeriod %in% c(1:4), 720 - secsLeftQuarter, 300 - secsLeftQuarter),  
         secsPassedGame = secsPassedQuarter + secsStartQuarter) %>%
  arrange(idGame, secsPassedGame) %>%
  mutate(shotPtsHome = case_when(
    numberEventMessageType == 3 & !str_detect(descriptionPlayHome, "MISS") ~ 1,                               
    numberEventMessageType == 1 & str_detect(descriptionPlayHome, "3PT") ~ 3,                                 
    numberEventMessageType == 1 & !str_detect(descriptionPlayHome, "3PT") ~ 2,
    TRUE ~ 0
  )) %>%
  mutate(shotPtsAway = case_when(
    numberEventMessageType == 3 & !str_detect(descriptionPlayVisitor, "MISS") ~ 1,
    numberEventMessageType == 1 & str_detect(descriptionPlayVisitor, "3PT") ~ 3,
    numberEventMessageType == 1 & !str_detect(descriptionPlayVisitor, "3PT") ~ 2,
    TRUE ~ 0
  )) %>%
  group_by(idGame) %>%
  mutate(ptsHome = cumsum(shotPtsHome),
         ptsAway = cumsum(shotPtsAway)) %>%
  ungroup() %>%
  left_join(team_logs %>%
              mutate(slugTeamHome = ifelse(locationGame == "H", slugTeam, slugOpponent),
                     slugTeamAway = ifelse(locationGame == "A", slugTeam, slugOpponent)) %>%
              distinct(idGame, slugTeamHome, slugTeamAway))

new_pbp %>%
  filter(shotPtsHome > 0 | shotPtsAway > 0) %>%
  group_by(idGame) %>%
  mutate(duration = abs(secsPassedGame - lead(secsPassedGame))) %>%
  ungroup() %>%
  mutate(team_margin = ifelse(slugTeamHome == team_analysis, ptsHome - ptsAway, ptsAway - ptsHome)) %>%
  group_by(idGame, losing = team_margin < 0) %>%
  summarise(time_secs = sum(duration, na.rm = TRUE)) %>%
  ungroup() %>%
  filter(losing) %>%
  filter(idGame %in% tail(team_games$idGame, 5))