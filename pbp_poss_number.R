library(tidyverse)
library(hoopR)

pbp2023 <- read_rds("https://github.com/ramirobentes/NBA-in-R/blob/master/2022_23/regseason/pbp/pbp-poss-rs23/data.rds?raw=true")

and_ones <- pbp2023 %>%
  group_by(game_id, secs_passed_game, 
           slug_team_foul = ifelse(msg_type == 6, ifelse(team_home == slug_team, team_away, team_home), slug_team)) %>%
  mutate(and1 = sum(msg_type == 1) > 0 &
           sum(msg_type == 3) > 0 &
           sum(msg_type == 6 & act_type == 2) > 0 &
           (msg_type == 1 | (msg_type == 3 & act_type == 10))) %>%
  ungroup() %>%
  filter(and1) %>%
  select(game_id, period, clock, number_event, msg_type, possession) %>%
  group_by(game_id, period, clock) %>%
  mutate(possession = sum(possession) - possession) %>%
  ungroup()

pbp_changed <- pbp2023 %>%
  rows_update(and_ones, by = c("game_id", "period", "clock", "number_event", "msg_type")) %>%  
  mutate(possession = case_when(poss_home == 1 & possession == 0 & msg_type != 1 ~ 1,
                                poss_away == 1 & possession == 0 & msg_type != 1 ~ 1,
                                possession == 1 & msg_type == 6 ~ 0,
                                TRUE ~ possession),
         slug_team = case_when(poss_home == 1 ~ team_home,
                               poss_away == 1 ~ team_away,
                               TRUE ~ slug_team)) %>%
  group_by(game_id, period, poss_team = ifelse(possession == 1, slug_team, NA)) %>%
  mutate(poss_number = ifelse(possession == 1, cumsum(possession), NA)) %>%
  group_by(game_id, period) %>%
  mutate(across(c(poss_team, poss_number), ~ zoo::na.locf0(., fromLast = TRUE))) %>%
  ungroup() %>%
  mutate(lineup_off = ifelse(poss_team == team_home, lineup_home, lineup_away),
         lineup_def = ifelse(poss_team == team_away, lineup_home, lineup_away)) %>%
  select(game_id, period, clock, number_event, msg_type, act_type, slug_team, poss_team, description, lineup_off, lineup_def, possession,
         poss_number, shot_pts) %>%
  group_by(game_id, period, poss_team, poss_number) %>%
  mutate(pts_poss = ifelse(possession == 1, sum(shot_pts), NA)) %>%
  ungroup() 
