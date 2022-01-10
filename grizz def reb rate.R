library(tidyverse)
library(nbastatR)

final_poss_pack <- read_csv("https://github.com/ramirobentes/NBA-in-R/releases/download/final-poss-pack-42862e5/data.csv",
                            col_types = c(timeQuarter = "c",
                                          start_poss = "c")) %>%
  mutate(across(starts_with("description"), ~ coalesce(., "")))  # url above might change daily
team_logs <- game_logs(seasons = 2022, result_types = "team")


# put every missed shot and rebound in order
shots_rebs <- final_poss_pack %>%
  filter(numberEventMessageType %in% c(2, 4) | numberEventMessageType == 3 & shotPtsHome + shotPtsAway == 0) %>%
  mutate(slugTeamPlayer1 = case_when(is.na(slugTeamPlayer1) & descriptionPlayHome == "" ~ slugTeamAway,
                                     is.na(slugTeamPlayer1) & descriptionPlayVisitor == "" ~ slugTeamHome,
                                     TRUE ~ slugTeamPlayer1)) %>%
  group_by(idGame, numberPeriod, shot_reb = ifelse(numberEventMessageType == 4, "rebound", "shot")) %>%
  mutate(sequence_num = row_number()) %>%
  ungroup() %>%
  arrange(idGame, numberPeriod, sequence_num) %>%
  mutate(desc_type = case_when(numberEventMessageType == 4 & slugTeamPlayer1 == lag(slugTeamPlayer1) ~ "off reb",
                               numberEventMessageType == 4 & slugTeamPlayer1 != lag(slugTeamPlayer1) ~ "def reb",
                               TRUE ~ "missed shot")) %>%
  filter(!(numberEventMessageType == 3 & numberEventActionType %in% c(11, 13, 14, 16, 18:22, 25, 27:29))) %>%
  filter(!(desc_type == "def reb" & numberEventMessageType == 4 & numberEventActionType == 1)) %>%
  mutate(lineup_team = ifelse(slugTeamPlayer1 == slugTeamHome, lineupHome, lineupAway),
         lineup_opp = ifelse(slugTeamPlayer1 == slugTeamHome, lineupAway, lineupHome)) %>%
  select(idGame, numberPeriod, timeQuarter, numberEventMessageType, numberEventActionType, slugTeam =  slugTeamPlayer1, 
         descriptionPlayHome, descriptionPlayVisitor, desc_type, lineup_team, lineup_opp) %>%
  left_join(team_logs %>%
              distinct(idGame, slugTeam, slugOpponent, dateGame)) %>%
  mutate(slugTeam = case_when(slugTeam == "MEM" & str_detect(lineup_team, "Steven Adams") ~ "MEM with Adams",
                              slugTeam == "MEM" & !str_detect(lineup_team, "Steven Adams") ~ "MEM without Adams",
                              TRUE ~ slugTeam),
         slugOpponent = case_when(slugOpponent == "MEM" & str_detect(lineup_opp, "Steven Adams") ~ "MEM with Adams",
                                  slugOpponent == "MEM" & !str_detect(lineup_opp, "Steven Adams") ~ "MEM without Adams",
                                  TRUE ~ slugOpponent))

shots_rebs %>%
  filter(desc_type == "def reb") %>%
  count(slugTeam, name = "def_reb") %>%
  left_join(shots_rebs %>%
              filter(desc_type == "missed shot") %>%
              count(slugTeam = slugOpponent, name = "def_reb_chances")) %>%
  mutate(def_reb_pct = round(def_reb / def_reb_chances, 4)) %>%
  arrange(-def_reb_pct)