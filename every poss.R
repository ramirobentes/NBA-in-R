library(tidyverse)

pbp_final_gt <- read_rds("https://github.com/ramirobentes/NBA-in-R/blob/master/pbp-final-gt-post/data.rds?raw=true")

and_ones <- pbp_final_gt %>%
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

poss_details <- pbp_final_gt %>%
  rows_update(and_ones, by = c("game_id", "period", "clock", "number_event", "msg_type")) %>%  
  group_by(game_id, period, poss_team = ifelse(possession == 1, slug_team, NA)) %>%
  mutate(poss_number = ifelse(possession == 1, cumsum(possession), NA)) %>%
  group_by(game_id, period) %>%
  mutate(across(c(poss_team, poss_number), ~ zoo::na.locf0(., fromLast = TRUE))) %>%
  group_by(game_date, game_id, period, poss_team, poss_number) %>%
  summarise(start_poss = max(start_poss),
            end_poss = min(clock),
            end_type = msg_type[which(row_number() == max(row_number()))],
            description = description[which(row_number() == max(row_number()))],
            poss_pts = sum(shot_pts),
            team_pts = ifelse(poss_team == team_home, hs, vs)[which(row_number() == max(row_number()))]) %>%
  ungroup() %>%
  filter(!end_type %in% c(0, 13))

poss_details %>%
  group_by(game_date, game_id, period, poss_team) %>%
  summarise(total_poss = n(),
            tov = sum(end_type == 5),
            score = sum(poss_pts > 0),
            pts = sum(poss_pts)) %>%
  ungroup() %>%
  mutate(off_rtg = pts / total_poss * 100)
