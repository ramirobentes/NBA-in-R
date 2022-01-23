library(tidyverse)
library(lubridate)
library(zoo)
library(nbastatR)   # devtools::install_github("abresler/nbastatR")
library(future)
library(readxl)

team_logs <- game_logs(seasons = 2022, result_types = "team")
source("https://raw.githubusercontent.com/ramirobentes/NBA-in-R/master/data%20add%20pbp.R")

games <- team_logs %>%
  mutate(slugTeamHome = ifelse(locationGame == "H", slugTeam, slugOpponent),
         slugTeamAway = ifelse(locationGame == "A", slugTeam, slugOpponent)) %>%
  distinct(idGame, slugTeamHome, slugTeamAway)

plan(multiprocess)
play_logs_all <- play_by_play_v2(game_ids = unique(games$idGame))

new_pbp <- play_logs_all %>%
  mutate(numberOriginal = numberEvent) %>%
  distinct(idGame, numberEvent, .keep_all = TRUE) %>%   # remove duplicate events
  mutate(secsLeftQuarter = (minuteRemainingQuarter * 60) + secondsRemainingQuarter) %>%                       
  mutate(secsStartQuarter = case_when(                                                                        
    numberPeriod %in% c(1:5) ~ (numberPeriod - 1) * 720,
    TRUE ~ 2880 + (numberPeriod - 5) * 300
  )) %>%
  mutate(secsPassedQuarter = ifelse(numberPeriod %in% c(1:4), 720 - secsLeftQuarter, 300 - secsLeftQuarter),  
         secsPassedGame = secsPassedQuarter + secsStartQuarter) %>%
  arrange(idGame, secsPassedGame) %>%
  filter(numberEventMessageType != 18) %>%     # instant replay
  group_by(idGame) %>%
  mutate(numberEvent = row_number()) %>%  # new numberEvent column with events in the right order
  ungroup() %>%
  select(idGame, numberOriginal, numberEventMessageType, numberEventActionType, namePlayer1, namePlayer2, namePlayer3,                   
         slugTeamPlayer1, slugTeamPlayer2,  slugTeamPlayer3, numberPeriod, timeQuarter, secsPassedGame, 
         descriptionPlayHome, numberEvent, descriptionPlayVisitor, descriptionPlayNeutral) %>%
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
  left_join(games %>%
              select(idGame, slugTeamHome, slugTeamAway)) %>%
  select(idGame, numberOriginal, numberEventMessageType, numberEventActionType, slugTeamHome, slugTeamAway, slugTeamPlayer1,
         slugTeamPlayer2, slugTeamPlayer3, numberPeriod, timeQuarter, secsPassedGame, numberEvent, namePlayer1, namePlayer2, 
         namePlayer3, descriptionPlayHome, descriptionPlayVisitor, ptsHome, ptsAway, shotPtsHome, shotPtsAway,
         descriptionPlayNeutral) %>%
  mutate(marginBeforeHome = ptsHome - ptsAway - shotPtsHome + shotPtsAway,
         marginBeforeAway = ptsAway - ptsHome - shotPtsAway + shotPtsHome,
         timeQuarter = str_pad(timeQuarter, width = 5, pad = 0)) %>%
  mutate(numberEventNew = numberEvent) %>%
  rows_update(event_changes, by = c("idGame", "numberEvent")) %>%
  mutate(numberEvent = numberEventNew) %>%
  select(-numberEventNew) %>%
  arrange(idGame, numberEvent)


subs_made <- new_pbp %>%
  filter(numberEventMessageType == 8) %>%
  mutate(slugTeamLocation = ifelse(slugTeamPlayer1 == slugTeamHome, "Home", "Away")) %>%
  select(idGame, numberPeriod, timeQuarter, secsPassedGame, slugTeamPlayer = slugTeamPlayer1,
         slugTeamLocation, playerOut = namePlayer1, playerIn = namePlayer2) %>%
  pivot_longer(cols = starts_with("player"),
               names_to = "inOut",
               names_prefix = "player",
               values_to = "namePlayer") %>%
  group_by(idGame, numberPeriod, slugTeamPlayer, namePlayer) %>%
  filter(row_number() == 1) %>%
  ungroup()

others_qtr <- new_pbp %>%
  filter(numberEventMessageType != 8) %>%                             
  filter(!(numberEventMessageType == 6 & numberEventActionType %in% c(10, 11, 16, 18, 25))) %>% 
  filter(!(numberEventMessageType == 11 & numberEventActionType %in% c(1, 4))) %>%
  pivot_longer(cols = starts_with("namePlayer"),
               names_to = "playerNumber",
               names_prefix = "namePlayer",
               values_to = "namePlayer") %>%
  mutate(slugTeamPlayer = case_when(playerNumber == 1 ~ slugTeamPlayer1,
                                    playerNumber == 2 ~ slugTeamPlayer2,
                                    playerNumber == 3 ~ slugTeamPlayer3,
                                    TRUE ~ "None")) %>%
  mutate(slugTeamLocation = ifelse(slugTeamPlayer == slugTeamHome, "Home", "Away")) %>%
  filter(!is.na(namePlayer),
         !is.na(slugTeamPlayer)) %>%
  anti_join(subs_made %>%
              select(idGame, numberPeriod, slugTeamPlayer, namePlayer)) %>%    # remove players that were subbed in the quarter
  distinct(idGame, numberPeriod, namePlayer, slugTeamPlayer, slugTeamLocation)

lineups_quarters <- subs_made %>%
  filter(inOut == "Out") %>%
  select(idGame, numberPeriod, slugTeamPlayer, namePlayer, slugTeamLocation) %>%
  bind_rows(others_qtr) %>%
  arrange(idGame, numberPeriod, slugTeamPlayer)

lineups_quarters %>%
  count(idGame, numberPeriod, slugTeamPlayer) %>%
  filter(n != 5)

missing_players_ot <- data_missing_players %>%
  left_join(games %>%
              select(idGame, slugTeamHome, slugTeamAway)) %>%
  mutate(slugTeamLocation = ifelse(slugTeamHome == slugTeamPlayer, "Home", "Away")) %>%
  select(-c(slugTeamHome, slugTeamAway))

lineups_quarters <- lineups_quarters %>%
  bind_rows(missing_players_ot) %>%
  arrange(idGame, numberPeriod, slugTeamPlayer)

lineup_subs <- new_pbp %>%
  filter(numberEventMessageType == 8) %>%
  select(idGame, numberPeriod, timeQuarter, secsPassedGame, slugTeamPlayer = slugTeamPlayer1, playerOut = namePlayer1, 
         playerIn = namePlayer2, numberEvent) %>%
  arrange(idGame, numberEvent) %>%
  group_by(idGame, numberPeriod, slugTeamPlayer) %>%
  mutate(row1 = row_number()) %>%
  ungroup() %>%
  left_join(lineups_quarters %>%
              group_by(idGame, numberPeriod, slugTeamPlayer) %>%
              summarise(lineupBefore = paste(sort(unique(namePlayer)), collapse = ", ")) %>%
              ungroup() %>%
              mutate(row1 = 1)) %>%
  select(-row1)

lineup_subs <- lineup_subs %>%
  mutate(lineupBefore = str_split(lineupBefore, ", ")) %>% 
  arrange(idGame, numberEvent) %>%
  group_by(idGame, numberPeriod, slugTeamPlayer) %>%
  mutate(lineupAfter = accumulate2(playerIn, playerOut, ~setdiff(c(..1, ..2), ..3), .init = lineupBefore[[1]])[-1],
         lineupBefore = coalesce(lineupBefore, lag(lineupAfter))) %>%
  ungroup() %>% 
  mutate_all(~map_chr(., ~paste(.x, collapse = ", "))) %>%
  mutate_at(vars("numberEvent", "numberPeriod", "idGame"), ~ as.integer(.)) %>%
  mutate(secsPassedGame = as.numeric(secsPassedGame)) %>%
  arrange(idGame, numberEvent) %>%
  left_join(lineups_quarters %>%
              distinct(idGame, slugTeamPlayer, slugTeamLocation)) %>%
  filter(!is.na(slugTeamLocation))

lineup_game <- new_pbp %>%
  group_by(idGame, numberPeriod) %>%
  mutate(row1 = row_number()) %>%
  ungroup() %>%
  left_join(lineups_quarters %>%
              group_by(idGame, numberPeriod, slugTeamLocation) %>%
              summarise(lineupBefore = paste(sort(unique(namePlayer)), collapse = ", ")) %>%
              ungroup() %>%
              pivot_wider(names_from = slugTeamLocation,
                          names_prefix = "lineupInitial",
                          values_from = lineupBefore) %>%
              mutate(row1 = 1)) %>%
  select(-row1) %>%
  left_join(lineup_subs %>%
              mutate(lineupBeforeHome = ifelse(slugTeamLocation == "Home", lineupBefore, NA),
                     lineupAfterHome = ifelse(slugTeamLocation == "Home", lineupAfter, NA),
                     lineupBeforeAway = ifelse(slugTeamLocation == "Away", lineupBefore, NA),
                     lineupAfterAway = ifelse(slugTeamLocation == "Away", lineupAfter, NA)) %>%
              select(idGame, numberPeriod, timeQuarter, secsPassedGame, numberEvent, slugTeamPlayer1 = slugTeamPlayer,
                     contains("Home"), contains("Away"))) %>%
  mutate_at(vars(c(lineupBeforeHome, lineupAfterHome)), ~ ifelse(!is.na(lineupInitialHome), lineupInitialHome, .)) %>%
  mutate_at(vars(c(lineupBeforeAway, lineupAfterAway)), ~ ifelse(!is.na(lineupInitialAway), lineupInitialAway, .)) %>%
  select(-starts_with("lineupInitial"))

lineup_game <- lineup_game %>%
  group_by(idGame, numberPeriod) %>%
  mutate(lineupHome = na.locf(lineupAfterHome, na.rm = FALSE),
         lineupAway = na.locf(lineupAfterAway, na.rm = FALSE),
         lineupHome = ifelse(is.na(lineupHome), na.locf(lineupBeforeHome, fromLast = TRUE, na.rm = FALSE), lineupHome),
         lineupAway = ifelse(is.na(lineupAway), na.locf(lineupBeforeAway, fromLast = TRUE, na.rm = FALSE), lineupAway),
         lineupHome = str_split(lineupHome, ", "),
         lineupAway = str_split(lineupAway, ", "),
         lineupHome = map_chr(lineupHome, ~ paste(sort(.), collapse = ", ")),
         lineupAway = map_chr(lineupAway, ~ paste(sort(.), collapse = ", "))) %>%
  ungroup() %>%
  select(-c(starts_with("lineupBefore"), starts_with("lineupAfter")))

lineup_game_stats <- lineup_game %>%
  mutate(canSub = case_when(numberEventMessageType == 5 & !numberEventActionType %in% c(1, 2) ~ 1,    # dead ball turnovers
                            numberEventMessageType == 6 & numberEventActionType != 16 ~ 1,            # fouls
                            numberEventMessageType == 11 & numberEventActionType != 4 ~ 1,
                            numberEventMessageType == 7 & numberEventActionType == 5 ~ 1,             # kickballs
                            numberEventMessageType == 4 & numberEventActionType == 0 & !str_detect(str_to_upper(descriptionPlayHome), "OFF:") ~ 1,
                            numberEventMessageType == 4 & numberEventActionType == 0 & !str_detect(str_to_upper(descriptionPlayVisitor), "OFF:") ~ 1,
                            TRUE ~ 0)) %>%
  mutate(secsPassedGame2 = ifelse(timeQuarter == "12:00" &
                                    (str_detect(str_to_lower(descriptionPlayHome), "technical") |
                                       str_detect(str_to_lower(descriptionPlayVisitor), "technical")),
                                  secsPassedGame + 0.005, secsPassedGame)) %>%    # Note 4
  mutate(secsPassedGame2 = ifelse(timeQuarter == "00:00" & numberEventMessageType == 3 & numberEventActionType != 10,
                                  secsPassedGame2 - 0.1,
                                  secsPassedGame2)) %>%
  group_by(idGame, numberPeriod, secsPassedGame) %>%
  mutate(numberNew = ifelse(numberEventMessageType == 3 & numberEventActionType == 12, 
                            paste(numberEvent[numberEventMessageType == 3 & numberEventActionType == 11], collapse = ", "), 
                            as.character(numberEvent)),
         numberNew = ifelse(numberEventMessageType == 3 & numberEventActionType %in% c(14, 15), 
                            paste(numberEvent[numberEventMessageType == 3 & numberEventActionType == 13], collapse = ", "),
                            numberNew)) %>%
  mutate(techs_and1 = sum(numberEventMessageType == 3 & numberEventActionType == 16) > 0 &
           sum(numberEventMessageType == 3 & numberEventActionType == 10) > 0 & 
           sum(numberEventMessageType == 8) > 0) %>%
  mutate(numberNew = ifelse(numberEventMessageType == 3 & numberEventActionType == 10 & techs_and1, 
                            paste(numberEvent[numberEventMessageType == 6 & numberEventActionType == 2 & techs_and1], collapse = ", "), 
                            as.character(numberNew))) %>%
  mutate(numberNew = str_split(numberNew, ", "),
         numberNew = map(numberNew, ~as.numeric(.)),
         numberNew = map2_dbl(numberNew, numberEvent, ~ max(.x[.x <= .y]))) %>%
  ungroup() %>%
  arrange(idGame, numberNew, numberEvent) %>%
  group_by(idGame) %>%
  mutate(newptsHome = cumsum(shotPtsHome),
         newptsAway = cumsum(shotPtsAway)) %>%
  group_by(idGame, numberPeriod, secsPassedGame2) %>%
  mutate(subOpp = cumsum(canSub)) %>%
  group_by(idGame = as.character(idGame), 
           numberPeriod = as.character(numberPeriod), 
           subOpp, 
           secsPassedGame2 = as.character(secsPassedGame2)) %>%
  mutate(hasFouls = sum(numberEventMessageType == 3)) %>%
  mutate(newptsHome = ifelse(hasFouls > 0,
                             newptsHome[row_number() == max(row_number()[numberEventMessageType == 3])],
                             newptsHome),
         newptsAway = ifelse(hasFouls > 0,
                             newptsAway[row_number() == max(row_number()[numberEventMessageType == 3])],
                             newptsAway)) %>%
  ungroup() %>%
  select(-c(secsPassedGame2, numberNew, techs_and1, hasFouls)) %>%
  mutate(across(starts_with("description"), ~ coalesce(., "")))

# Adding possession when fg attempts, ft 1 of 2 and 1 of 3 and turnovers
possession_initial <- lineup_game_stats %>%
  mutate(possession = case_when(numberEventMessageType %in% c(1, 2, 5) ~ 1,
                                numberEventMessageType == 3 & numberEventActionType %in% c(12, 15) ~ 1,
                                TRUE ~ 0),
         team_possession = case_when(is.na(slugTeamPlayer1) & possession == 1 & descriptionPlayHome == "" ~ slugTeamAway,
                                     is.na(slugTeamPlayer1) & possession == 1 & descriptionPlayVisitor == "" ~ slugTeamHome,
                                     TRUE ~ slugTeamPlayer1))

# lane violation when there's no description of turnover (don't shoot last free throw and consider 1st free throw 1 of 1)
lane_description_missing <- possession_initial %>%
  group_by(idGame, secsPassedGame) %>%
  filter(sum(numberEventMessageType == 3 & numberEventActionType == 10) > 0,
         sum(numberEventMessageType == 6 & numberEventActionType == 2) > 0,
         sum(numberEventMessageType == 7 & numberEventActionType == 3) > 0,
         sum(numberEventMessageType == 1) == 0) %>%
  ungroup() %>%
  mutate(possession = ifelse(numberEventMessageType == 3 & numberEventActionType == 10, 1, possession)) %>%
  select(idGame, numberEvent, team_possession, possession)

# adding turnover to opponent of team when the challenger gets the jumpball
jumpball_turnovers <- possession_initial %>%
  group_by(idGame, numberPeriod) %>%
  mutate(prev_poss = zoo::na.locf0(ifelse(possession == 1, team_possession, NA)),
         next_poss = zoo::na.locf0(ifelse(possession == 1, team_possession, NA), fromLast = TRUE)) %>%
  ungroup() %>%
  mutate(slugTeamPlayer1 = case_when(numberEventMessageType == 9 & descriptionPlayHome == "" ~ slugTeamAway,
                                     numberEventMessageType == 9 & descriptionPlayVisitor == "" ~ slugTeamHome,
                                     TRUE ~ slugTeamPlayer1)) %>%
  group_by(idGame, secsPassedGame) %>%
  mutate(team_reb_chall = sum(numberEventMessageType == 9 & numberEventActionType == 7) > 0 &
           sum(numberEventMessageType == 4 & is.na(namePlayer1)) > 0) %>% 
  ungroup() %>%
  filter(numberEventMessageType == 10 & numberEventActionType == 1 & 
           lag(numberEventMessageType) == 9 & lag(numberEventActionType) == 7 &
           slugTeamPlayer3 == lag(slugTeamPlayer1) &
           prev_poss == next_poss &
           lag(team_reb_chall) == FALSE) %>%
  mutate(team_possession = ifelse(slugTeamPlayer3 == slugTeamPlayer1, slugTeamPlayer2, slugTeamPlayer1),
         possession = 1) %>%
  select(idGame, numberEvent, team_possession, possession)

# finding when there are consecutive poss and changing the first one to zero
change_consec <- possession_initial %>%
  rows_update(lane_description_missing, by = c("idGame", "numberEvent")) %>%
  rows_update(jumpball_turnovers, by = c("idGame", "numberEvent")) %>%
  filter(possession == 1 | (numberEventMessageType == 6 & numberEventActionType == 30)) %>%
  group_by(idGame, numberPeriod) %>%
  filter(possession == lead(possession) & team_possession == lead(team_possession)) %>%
  ungroup() %>%
  mutate(possession = 0) %>%
  select(idGame, numberEvent, possession)

# replacing in original data
poss_pack <- possession_initial %>%
  rows_update(lane_description_missing, by = c("idGame", "numberEvent")) %>%
  rows_update(jumpball_turnovers, by = c("idGame", "numberEvent")) %>%
  rows_update(change_consec, by = c("idGame","numberEvent"))

# identifying start of possession
start_possessions <- poss_pack %>%
  mutate(slugTeamPlayer1 = case_when(is.na(slugTeamPlayer1) & descriptionPlayHome == "" ~ slugTeamAway,
                                     is.na(slugTeamPlayer1) & descriptionPlayVisitor == "" ~ slugTeamHome,
                                     TRUE ~ slugTeamPlayer1)) %>% 
  select(idGame, numberPeriod, timeQuarter, numberEventMessageType,  slugTeamPlayer1, 
         descriptionPlayHome, descriptionPlayVisitor, numberEvent) %>%
  filter(numberEventMessageType %in% c(1:5)) %>%
  group_by(idGame, numberPeriod) %>%
  mutate(start_poss = case_when(slugTeamPlayer1 != lag(slugTeamPlayer1) & numberEventMessageType == 4 ~ timeQuarter, 
                                slugTeamPlayer1 != lag(slugTeamPlayer1) & numberEventMessageType != 4 ~ lag(timeQuarter))) %>%
  mutate(start_poss = case_when(is.na(start_poss) & row_number() == 1 & numberPeriod <= 4 ~ "12:00", 
                                is.na(start_poss) & row_number() == 1 & numberPeriod > 4 ~ "05:00",
                                TRUE ~ start_poss)) %>%
  ungroup()

# add column with start of possession to the original table and identify heaves
poss_pack_start <- poss_pack %>%
  left_join(start_possessions %>%
              select(idGame, numberEvent, start_poss)) %>%
  group_by(idGame, numberPeriod) %>%
  mutate(start_poss = ifelse(possession == 1, na.locf0(start_poss), start_poss),
         start_poss = ifelse(numberEventMessageType == 4 & numberEventActionType == 1, na.locf0(start_poss), start_poss),
         start_poss = na.locf0(start_poss, fromLast = TRUE)) %>%
  ungroup() %>%
  mutate(heave = ifelse(numberEventMessageType %in% c(2, 5) & possession == 1 & as.integer(str_sub(start_poss, 4, 5)) <= 2 & str_starts(start_poss, "00:") & (lead(shotPtsHome) + lead(shotPtsAway) == 0), 1, 0),
         possession = ifelse(heave == 1, 0, possession))


# adding extra possessions at end of quarter when team gets the ball with more than 2 secs
last_possessions <- poss_pack_start %>%
  group_by(idGame, numberPeriod) %>%
  filter(cumsum(possession) >= max(cumsum(possession)) & possession == 1) %>%
  ungroup()

last_rebounds <- poss_pack_start %>%
  group_by(idGame, numberPeriod) %>%
  filter(numberEventMessageType == 4 & !(lag(numberEventMessageType) == 3 & lag(numberEventActionType) %in% c(18:20, 27:29))) %>%
  filter(row_number() == max(row_number())) %>%
  ungroup() %>%
  mutate(rebound_team = case_when(is.na(slugTeamPlayer1) & descriptionPlayHome == "" ~ slugTeamAway,
                                  is.na(slugTeamPlayer1) & descriptionPlayVisitor == "" ~ slugTeamHome,
                                  TRUE ~ slugTeamPlayer1)) %>%
  select(idGame, numberPeriod, rebound_team, timeQuarterReb = timeQuarter)

missedft_and1_last <- poss_pack_start %>%
  semi_join(last_possessions %>%
              select(idGame, secsPassedGame)) %>%
  group_by(idGame, secsPassedGame) %>%
  filter(sum(numberEventMessageType == 1) > 0 & sum(numberEventMessageType == 3 & numberEventActionType == 10) > 0 & sum(str_detect(descriptionPlayHome, "MISS") | str_detect(descriptionPlayVisitor, "MISS")) > 0) %>%
  ungroup() %>%
  filter(numberEventMessageType == 1) %>%
  select(idGame, numberEvent)

addit_poss_reb <- last_possessions %>%
  left_join(last_rebounds, by = c("idGame", "numberPeriod")) %>%
  left_join(missedft_and1_last %>%
              mutate(and1_ft = 1)) %>%
  filter(numberEventMessageType == 2 | (numberEventMessageType == 3 & (str_detect(descriptionPlayHome, "MISS") | str_detect(descriptionPlayVisitor, "MISS"))) | and1_ft == 1) %>%
  filter(rebound_team != team_possession,
         as.integer(str_sub(timeQuarterReb, 4, 5)) >= 3) %>%
  transmute(idGame, numberPeriod, start_poss = timeQuarterReb, 
            team_possession = rebound_team, possession)

addit_poss_made <- last_possessions %>%
  filter(numberEventMessageType %in% c(1, 5) | (numberEventMessageType == 3 & !str_detect(descriptionPlayHome, "MISS") & !str_detect(descriptionPlayVisitor, "MISS"))) %>%
  anti_join(missedft_and1_last) %>%
  left_join(team_logs %>%
              distinct(idGame = as.character(idGame), .keep_all = TRUE) %>%
              select(idGame, slugTeam, slugOpponent)) %>%
  mutate(team_possession_next = ifelse(team_possession == slugTeam, slugOpponent, slugTeam)) %>%
  filter(as.integer(str_sub(timeQuarter, 4, 5)) >= 3) %>%
  transmute(idGame, numberPeriod, start_poss = timeQuarter, 
            team_possession = team_possession_next, possession)

additional_possessions <- bind_rows(addit_poss_reb,  addit_poss_made) %>%
  mutate(numberEventMessageType = 0,
         numberEventActionType = 0,
         numberOriginal = 0,
         descriptionPlayNeutral = "Last possession of quarter") %>%
  left_join(poss_pack %>%
              filter(numberEventMessageType == 13) %>%
              select(-c(numberOriginal, numberEventMessageType, numberEventActionType,
                        descriptionPlayNeutral, possession, team_possession))) %>%
  mutate(numberEvent = numberEvent - 0.5)

final_poss_pack <- poss_pack_start %>%
  bind_rows(additional_possessions) %>%
  arrange(idGame, numberEvent) %>%
  select(-c(hasFouls, subOpp, canSub)) %>%
  mutate(across(starts_with("description"), ~ coalesce(., "")))

# changing possession when it ends in free throw (make it end at foul that led to fts)
fouls_possessions <- final_poss_pack %>%
  filter(numberEventMessageType == 3 & possession == 1) %>%
  select(idGame, secsPassedGame, player_foul = namePlayer1, team_possession, numberEvent_ft = numberEvent) %>%
  left_join(final_poss_pack %>%
              filter(numberEventMessageType == 6 & !numberEventActionType %in% c(6, 9, 11, 13, 14, 15, 16, 17)) %>%
              mutate(description = ifelse(slugTeamPlayer1 == slugTeamHome, descriptionPlayHome, descriptionPlayVisitor)) %>%
              select(idGame, secsPassedGame, player_foul = namePlayer2, numberEvent_foul = numberEvent, description)) %>%
  add_count(idGame, secsPassedGame, player_foul, name = "number_plays") %>%
  filter(!(number_plays > 1 & !str_detect(description, " S.FOUL |\\.PN\\)")))

missing_comp <- fouls_possessions %>%
  filter(is.na(numberEvent_foul)) %>%
  left_join(final_poss_pack %>%
              filter(numberEventMessageType == 6 & !numberEventActionType %in% c(6, 9, 11, 13, 14, 15, 16, 17)) %>%
              mutate(description = ifelse(slugTeamPlayer1 == slugTeamHome, descriptionPlayHome, descriptionPlayVisitor)) %>%
              select(idGame, secsPassedGame, numberEvent_foul = numberEvent, description),
            by = c("idGame", "secsPassedGame"),
            suffix = c("", "_new")) %>%
  mutate(numberEvent_foul = numberEvent_foul_new,
         description = description_new) %>%
  select(-c(numberEvent_foul_new, description_new))

fouls_possessions <- fouls_possessions %>%
  rows_update(missing_comp, by = c("idGame", "secsPassedGame", "player_foul", "team_possession", "numberEvent_ft", "number_plays")) %>%
  select(idGame, secsPassedGame, team_possession, numberEvent_ft, numberEvent_foul) %>%
  pivot_longer(cols = starts_with("numberEvent"),
               names_to = "type_play",
               values_to = "numberEvent",
               names_prefix = "numberEvent_") %>%
  mutate(possession_players = ifelse(type_play == "foul", 1, 0)) %>%
  select(-type_play)

final_poss_pack <- final_poss_pack %>%
  mutate(possession_players = possession) %>%
  rows_update(fouls_possessions, by = c("idGame", "numberEvent"))

lineup_stats <- final_poss_pack %>%
  select(idGame, numberEvent, slugTeamHome, slugTeamAway, numberPeriod, timeQuarter, secsPassedGame, 
         newptsHome, newptsAway, lineupHome, lineupAway, possession_players, team_possession) %>%
  mutate(possession_home = ifelse(team_possession == slugTeamHome & possession_players == 1, 1, 0),
         possession_away = ifelse(team_possession == slugTeamAway & possession_players == 1, 1, 0)) %>%
  pivot_longer(cols = starts_with("lineup"),
               names_to = "lineupLocation",
               names_prefix = "lineup",
               values_to = "lineup") %>%
  mutate(ptsTeam = ifelse(lineupLocation == "Home", newptsHome, newptsAway),
         ptsOpp = ifelse(lineupLocation == "Away", newptsHome, newptsAway),
         possTeam = ifelse(lineupLocation == "Home", possession_home, possession_away),
         possOpp = ifelse(lineupLocation == "Away", possession_home, possession_away),
         slugTeam = ifelse(lineupLocation == "Home", slugTeamHome, slugTeamAway),
         slugOpp = ifelse(lineupLocation == "Away", slugTeamHome, slugTeamAway)) %>%
  distinct(idGame, slugTeam, slugOpp, numberPeriod, timeQuarter, secsPassedGame, ptsTeam, ptsOpp,
           possTeam, possOpp, lineup, teamLocation = lineupLocation, numberEvent) %>%
  arrange(idGame, numberEvent) %>%
  group_by(idGame, slugTeam) %>%
  mutate(lineupChange = lineup != lag(lineup),
         lineupChange = coalesce(lineupChange, FALSE)) %>%
  group_by(idGame, slugTeam) %>%
  mutate(lineupStint = cumsum(lineupChange)) %>%
  ungroup() %>%
  arrange(idGame, lineupStint, numberEvent) %>%
  group_by(idGame, slugTeam, lineup, lineupStint, numberPeriod) %>%
  summarise(totalPossTeam = sum(possTeam),
            totalPossOpp = sum(possOpp),
            initialScoreTeam = ptsTeam[row_number() == min(row_number())],
            initialScoreOpp = ptsOpp[row_number() == min(row_number())],
            finalScoreTeam = ptsTeam[row_number() == max(row_number())],
            finalScoreOpp =  ptsOpp[row_number() == max(row_number())],
            initialTime = secsPassedGame[row_number() == min(row_number())],
            finalTime = secsPassedGame[row_number() == max(row_number())]) %>%
  ungroup() %>%
  arrange(idGame, lineupStint) %>%
  group_by(idGame, slugTeam) %>%                              
  mutate(finalTime = ifelse(row_number() == max(row_number()), finalTime, lead(initialTime))) %>%  
  ungroup() %>%
  mutate(across(c(contains("Score")), ~ as.numeric(.), .names = "{col}")) %>%
  mutate(totalScoreTeam = finalScoreTeam - initialScoreTeam,
         totalScoreOpp = finalScoreOpp - initialScoreOpp,
         netScoreTeam = totalScoreTeam - totalScoreOpp,
         totalTime = finalTime - initialTime) %>%
  arrange(idGame, lineupStint)

lineup_stats <- lineup_stats %>%
  left_join(lineup_stats %>%
              filter(lineupStint == 0) %>%
              distinct(idGame, slugTeam, starters = lineup)) %>%
  mutate(across(c(lineup, starters), ~ str_split(., ", "), .names = "{.col}_list")) %>%
  mutate(reserves = map_int(map2(lineup_list, starters_list, setdiff), length)) %>%
  select(-c(contains("list"), starters))


rm(games, event_changes, play_logs_all, new_pbp, subs_made, others_qtr,
   lineups_quarters, data_missing_players, missing_players_ot, lineup_subs, 
   lineup_game, lineup_game_stats, possession_initial, jumpball_turnovers, lane_description_missing,
   change_consec, poss_pack, start_possessions, poss_pack_start, last_possessions, fouls_possessions,
   missing_comp, last_rebounds, missedft_and1_last, addit_poss_reb, addit_poss_made, additional_possessions)
