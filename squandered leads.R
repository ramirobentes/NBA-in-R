library(tidyverse)
library(hoopR)
library(janitor)

pbp_final_gt <- read_rds("https://github.com/ramirobentes/NBA-in-R/blob/master/pbp-final-gt/data.rds?raw=true")

lead_size <- 10

score_changes <- pbp_final_gt %>%
  mutate(leading_team = case_when(vs > hs ~ team_away,
                                  hs > vs ~ team_home,
                                  TRUE ~ "tie"),
         margin = abs(vs - hs)) %>%
  select(game_id, period, clock, secs_passed_game, number_event, leading_team, margin, shot_pts) %>%
  group_by(game_id) %>%
  mutate(change_lead = cumsum(leading_team != lag(leading_team, default = "tie"))) %>%
  group_by(game_id, change_lead, leading_team) %>%
  summarise(start_time = min(secs_passed_game),
            biggest_lead = max(margin),
            last_size_lead = ifelse(biggest_lead < lead_size, NA, max(secs_passed_game[which(margin >= lead_size & shot_pts > 0)]))) %>%
  group_by(game_id) %>%
  mutate(final_time = lead(start_time)) %>%
  ungroup()

score_changes %>%
  filter(biggest_lead >= lead_size & !is.na(final_time)) %>%
  filter(last_size_lead >= 2160) %>%  # to keep only leads after the start of 4th quarter
  left_join(pbp_final_gt %>%
              group_by(game_id) %>%
              filter(row_number() == max(row_number())) %>%
              ungroup() %>%
              mutate(win = ifelse(hs > vs, team_home, team_away),
                     lose = ifelse(hs > vs, team_away, team_home)) %>%
              select(game_id, win, lose)) %>%
  group_by(leading_team) %>%
  summarise(squandered_leads = n(),
            wins = n_distinct(game_id[which(win == leading_team)]),
            losses = n_distinct(game_id[which(lose == leading_team)])) %>%
  ungroup() %>%
  mutate(pts_lead = lead_size, .after = leading_team) %>%
  arrange(-squandered_leads)
