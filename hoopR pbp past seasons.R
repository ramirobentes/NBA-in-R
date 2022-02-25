library(tidyverse)
library(hoopR)
library(janitor)
library(future)
library(zoo)

player_logs <- nba_leaguegamelog(season = "2020-21", player_or_team = "P") %>%  # change season here
  pluck("LeagueGameLog") %>%
  clean_names() %>%
  mutate(team_location = ifelse(str_detect(matchup, "\\@"), "away", "home"),
         across(c(player_id, team_id), as.numeric))

function_pbp <- function(x){
  nba_data_pbp(x) %>%
    mutate(game_id = x)
}

games <- player_logs %>%
  distinct(game_id) %>%
  pull(game_id)

plan(multicore)
nba_pbp_raw <- map_df(games, function_pbp)

nba_pbp <- nba_pbp_raw %>%
  mutate(across(c(epid, opid), as.numeric)) %>%
  left_join(player_logs %>%
              distinct(pid = player_id, player1 = player_name)) %>%
  left_join(player_logs %>%
              distinct(epid = player_id, player2 = player_name)) %>%
  left_join(player_logs %>%
              distinct(opid = player_id, player3 = player_name)) %>%
  left_join(player_logs %>%
              distinct(tid = team_id, slug_team = team_abbreviation)) %>%
  left_join(player_logs %>%
              distinct(oftid = team_id, off_slug_team = team_abbreviation)) %>%
  select(game_id, period, clock = cl, number_event = evt, msg_type = etype, act_type = mtype, slug_team,
         off_slug_team, player1, player2, player3, description = de, desc_value = opt1,
         opt2, hs, vs, ord, locX, locY) %>%
  mutate(game_id = as.integer(game_id)) %>%
  left_join(player_logs %>%
              distinct(game_id = as.integer(game_id), slug_team = team_abbreviation, team_location) %>%
              pivot_wider(names_from = team_location,
                          values_from = slug_team,
                          names_prefix = "team_")) %>%
  as_tibble()

nba_pbp <- nba_pbp %>%
  mutate(number_original = number_event) %>%
  separate(clock, into = c("min_remain", "sec_remain"), sep = ":", remove = FALSE, convert = TRUE) %>%
  mutate(secs_left_qtr = (min_remain * 60) + sec_remain) %>%                       
  mutate(secs_start_qtr = case_when(                                                                        
    period %in% c(1:5) ~ (period - 1) * 720,
    TRUE ~ 2880 + (period - 5) * 300
  )) %>%
  mutate(secs_passed_qtr = ifelse(period %in% c(1:4), 720 - secs_left_qtr, 300 - secs_left_qtr),  
         secs_passed_game = secs_passed_qtr + secs_start_qtr) %>%
  arrange(game_id, secs_passed_game) %>%
  filter(msg_type != 18) %>%     # instant replay
  group_by(game_id) %>%
  mutate(number_event = row_number()) %>%  # new numberEvent column with events in the right order
  ungroup() %>%
  select(-c(contains("remain"), secs_left_qtr, secs_start_qtr, secs_passed_qtr)) %>%
  arrange(game_id, number_event) %>%
  mutate(shot_pts = desc_value * ifelse(msg_type %in% c(1:3) & !str_detect(description, "Missed"), 1, 0)) %>%
  group_by(game_id) %>%
  mutate(hs = cumsum(coalesce(if_else(slug_team == team_home, shot_pts, 0), 0)),
         vs = cumsum(coalesce(if_else(slug_team == team_away, shot_pts, 0), 0))) %>%
  ungroup()

# Lineups -----------------------------------------------------------------

# players that were subbed in or out in quarter
players_subbed <- nba_pbp %>%
  filter(msg_type == 8) %>%
  select(game_id, period, number_event, player_in = player2, player_out = player1, description) %>%
  pivot_longer(cols = starts_with("player"),
               names_to = "in_out",
               values_to = "player_name",
               names_prefix = "player_") %>%
  arrange(game_id, period, number_event) %>%
  distinct(game_id, period, player_name, .keep_all = TRUE) %>%
  distinct(game_id, period, player_name, in_out) %>%
  mutate(starter = ifelse(in_out == "out", 1, 0))

# find every player that contributed to pbp in quarter and remove those that were subbed. The ones remaining played the entire quarter. Then add the players that were subbed and started (first sub was out)
starters_quarters <- nba_pbp %>%
  filter(!(msg_type == 6 & act_type %in% c(11, 12, 16, 18, 30))) %>%
  filter(!msg_type %in% c(9, 11)) %>% # timeout and ejection
  select(game_id, period, starts_with("player")) %>%
  pivot_longer(cols = starts_with("player")) %>%
  filter(!is.na(value),
         value != 0) %>%
  distinct(game_id, period, player_name = value) %>%
  anti_join(players_subbed) %>%
  bind_rows(players_subbed %>%
              filter(starter == 1)) %>%
  transmute(game_id, period, player_name) %>%
  left_join(player_logs %>%
              distinct(game_id = as.integer(game_id), player_name, slug_team = team_abbreviation))

# see if there are quarters when couldn't find 5 starters for team (player played entire quarter and didn't contribute to pbp)
qtrs_missing <- starters_quarters %>%
  count(game_id, period, slug_team) %>%
  filter(n != 5)

# Find missing starter from period
missing_st_qtr <- function(game_id_miss, period_miss){
  nba_boxscoretraditionalv2(game_id = game_id_miss, start_period = period_miss, end_period = period_miss, range_type = 1) %>%
    pluck("PlayerStats") %>%
    mutate(period = period_miss) %>%
    filter((period_miss > 4 & MIN == "5:00") | (period_miss <= 4 & MIN == "12:00")) %>%
    mutate(across(c(FGM:PLUS_MINUS), as.integer)) %>%
    filter(FGA + FTA + REB + AST + STL + BLK + TO + PF == 0)
}

missing_starters <- map2_df(qtrs_missing$game_id, qtrs_missing$period, missing_st_qtr)

# put together table with starter for every team in every period, adding missing starters
starters_quarters <- starters_quarters %>%
  bind_rows(missing_starters %>%
              clean_names() %>%
              transmute(game_id = as.numeric(game_id), period, player_name, slug_team = team_abbreviation)) %>%
  arrange(game_id, period, slug_team) %>%
  group_by(game_id, period, slug_team) %>%
  summarise(lineup_start = paste(sort(unique(player_name)), collapse = ", ")) %>%
  ungroup() %>%
  left_join(player_logs %>%
              distinct(game_id = as.integer(game_id), slug_team = team_abbreviation, team_location))

# starters_quarters %>% count(str_count(lineup_start, ","))

# find lineup before and after every sub
lineup_subs <- nba_pbp %>%
  filter(msg_type == 8) %>%
  left_join(starters_quarters) %>%
  select(game_id, number_event, period, clock, slug_team, player_out = player1, player_in = player2, 
         team_location, lineup_before = lineup_start) %>%
  group_by(game_id, period, slug_team) %>%
  mutate(lineup_before = ifelse(row_number() == 1, lineup_before, NA)) %>%
  ungroup() %>%
  mutate(lineup_before = str_split(lineup_before, ", ")) %>% 
  arrange(game_id, number_event) %>%
  group_by(game_id, period, slug_team) %>%
  mutate(lineup_after = accumulate2(player_in, player_out, ~setdiff(c(..1, ..2), ..3), .init = lineup_before[[1]])[-1],
         lineup_before = coalesce(lineup_before, lag(lineup_after))) %>%
  ungroup() %>% 
  mutate(across(starts_with("lineup"), ~ map_chr(., ~ paste(.x, collapse = ", "))))

# add lineup to every event of pbp
lineup_game <- nba_pbp %>%
  left_join(starters_quarters %>%
              select(-slug_team) %>%
              pivot_wider(names_from = team_location,
                          values_from = lineup_start,
                          names_prefix = "lineup_start_") %>%
              mutate(msg_type = 12)) %>%
  left_join(lineup_subs %>%
              select(-c(clock, starts_with("player"))) %>%
              pivot_wider(names_from = team_location,
                          values_from = starts_with("lineup"))) %>%
  mutate(across(c(lineup_before_home, lineup_after_home), ~ ifelse(!is.na(lineup_start_home), lineup_start_home, .)),
         across(c(lineup_before_away, lineup_after_away), ~ ifelse(!is.na(lineup_start_away), lineup_start_away, .))) %>%
  group_by(game_id, period) %>%
  mutate(lineup_home = na.locf(lineup_after_home, na.rm = FALSE),
         lineup_away = na.locf(lineup_after_away, na.rm = FALSE),
         lineup_home = coalesce(lineup_home, na.locf(lineup_before_home, fromLast = TRUE, na.rm = FALSE)),
         lineup_away = coalesce(lineup_away, na.locf(lineup_before_away, fromLast = TRUE, na.rm = FALSE))) %>%
  ungroup() %>%
  mutate(lineup_home = map_chr(str_split(lineup_home, ", "), ~ paste(sort(.), collapse = ", ")),
         lineup_away = map_chr(str_split(lineup_away, ", "), ~ paste(sort(.), collapse = ", "))) %>%
  select(-c(starts_with("lineup_start"), starts_with("lineup_before"), starts_with("lineup_after")))


# Possessions -------------------------------------------------------------

poss_initial <- lineup_game %>%
  mutate(possession = case_when(msg_type %in% c(1, 2, 5) ~ 1,
                                msg_type == 3 & act_type %in% c(12, 15) ~ 1,
                                TRUE ~ 0))

# finding lane violations that are not specified
lane_description_missing <- poss_initial %>%
  group_by(game_id, secs_passed_game) %>%
  filter(sum(msg_type == 3 & act_type == 10) > 0,
         sum(msg_type == 6 & act_type == 2) > 0,
         sum(msg_type == 7 & act_type == 3) > 0,
         sum(msg_type == 1) == 0) %>%
  ungroup() %>%
  mutate(possession = ifelse(msg_type == 3 & act_type == 10, 1, possession)) %>%
  select(game_id, number_event, off_slug_team, possession)

# identify turnovers from successfull challenge + jump ball that are not specified
jumpball_turnovers <- poss_initial %>%
  group_by(game_id, period) %>%
  mutate(prev_poss = zoo::na.locf0(ifelse(possession == 1, off_slug_team, NA)),
         next_poss = zoo::na.locf0(ifelse(possession == 1, off_slug_team, NA), fromLast = TRUE)) %>%
  ungroup() %>%
  group_by(game_id, secs_passed_game) %>%
  mutate(team_reb_chall = sum(msg_type == 9) > 0 & sum(msg_type == 4 & is.na(player1)) > 0) %>% 
  ungroup() %>%
  filter(msg_type == 10 & act_type == 1 & 
           lag(msg_type) == 9 &
           slug_team == lag(slug_team) &
           prev_poss == next_poss &
           lag(team_reb_chall) == FALSE) %>%
  mutate(possession = 1) %>%
  transmute(game_id, number_event, off_slug_team = ifelse(slug_team == team_home, team_away, team_home), possession) %>%
  mutate(slug_team = off_slug_team)

# identify and change consecutive possessions
change_consec <- poss_initial %>%
  rows_update(lane_description_missing, by = c("game_id", "number_event")) %>%
  rows_update(jumpball_turnovers, by = c("game_id", "number_event")) %>%
  filter(possession == 1 | (msg_type == 6 & act_type == 30)) %>%
  group_by(game_id, period) %>%
  filter(possession == lead(possession) & off_slug_team == lead(off_slug_team)) %>%
  ungroup() %>%
  mutate(possession = 0) %>%
  select(game_id, number_event, possession)

# replace in data
poss_non_consec <- poss_initial %>%
  rows_update(lane_description_missing, by = c("game_id", "number_event")) %>%
  rows_update(jumpball_turnovers, by = c("game_id", "number_event")) %>%
  rows_update(change_consec, by = c("game_id","number_event"))


# find start of possessions
start_possessions <- poss_non_consec %>%
  filter((possession == 1 & (msg_type %in% c(1, 5, 10) | (msg_type == 3 & shot_pts > 0))) | (msg_type == 4 & act_type == 0 & desc_value == 0) | (msg_type == 3 & act_type == 10) | (msg_type == 6 & act_type == 6)) %>%
  filter(!(msg_type == 3 & act_type == 10 & lag(msg_type) == 6 & lag(act_type) == 6)) %>%
  filter(!(msg_type == 6 & act_type == 6)) %>%
  group_by(game_id, secs_passed_game, slug_team) %>%
  mutate(and1 = sum(msg_type == 1) > 0 & sum(msg_type == 3) > 0) %>%
  ungroup() %>%
  mutate(start_poss = ifelse(and1 & msg_type %in% c(1, 3), NA, clock),
         number_event = ifelse(msg_type == 4, number_event, number_event + 1)) %>%
  ungroup() %>%
  filter(!is.na(start_poss))

# add start of possession column to table
poss_non_consec <- poss_non_consec %>%
  left_join(start_possessions %>%
              select(game_id, number_event, start_poss)) %>%
  group_by(game_id, period) %>%
  mutate(start_poss = ifelse(row_number() == 1, clock, start_poss),
         start_poss = na.locf(start_poss)) %>%
  ungroup()


##### Adding extra possessions

addit_poss <- poss_non_consec %>%
  filter(msg_type %in% c(1:5) & !(msg_type == 3 & act_type %in% c(16, 18:19, 20, 27:29, 25:26)) & !(msg_type == 4 & act_type == 1)) %>%
  group_by(game_id, period) %>%
  filter(row_number() == max(row_number())) %>%
  ungroup() %>%
  filter(clock != "00:00.0" & !(msg_type == 4 & desc_value == 1)) %>%
  transmute(game_id, period, start_poss = clock, possession = 1,
            off_slug_team = ifelse(msg_type == 4 | msg_type == 3 & act_type %in% c(19, 20, 29, 26), 
                                   slug_team, 
                                   ifelse(slug_team == team_home, team_away, team_home)),
            msg_type = 99, act_type = 0, number_original = 0, description = "Last possession of quarter") %>%
  left_join(poss_non_consec %>%
              filter(msg_type == 13) %>%
              select(-c(number_original, msg_type, act_type, start_poss,
                        description, possession, off_slug_team))) %>%
  mutate(number_event = number_event - 0.5,
         slug_team = off_slug_team)


# Adding extra possessions
pbp_poss <- poss_non_consec %>%
  bind_rows(addit_poss) %>%
  arrange(game_id, number_event)

# Editing free throws pts and poss position ------------------------------------------------------------

# connecting free throws to fouls

## TECHNICALS
### find unidentified double technicals (instead of description showing double technical, there's one event for each but no FTs)
unident_double_techs <- lineup_game %>%
  filter(!msg_type %in% c(9, 11)) %>%   # ejection or timeout
  filter((game_id == lead(game_id) & secs_passed_game == lead(secs_passed_game) & msg_type == 6 & act_type == 11 & msg_type == lead(msg_type) & act_type == lead(act_type) & slug_team != lead(slug_team)) | (game_id == lag(game_id) & secs_passed_game == lag(secs_passed_game) & msg_type == 6 & act_type == 11 & msg_type == lag(msg_type) & act_type == lag(act_type) & slug_team != lag(slug_team))) %>%
  transmute(game_id, secs_passed_game, slug_team, number_event, description = str_replace(description, "Technical", "Double Technical"))

techs <- lineup_game %>%
  rows_update(unident_double_techs, by = c("game_id", "secs_passed_game", "slug_team", "number_event")) %>%
  filter(str_detect(description, "Technical|Defense 3 Second") & !str_detect(description, "Double Technical")) %>%
  group_by(game_id, secs_passed_game, msg_type) %>%
  mutate(sequence_num = row_number()) %>%
  ungroup() %>%
  transmute(game_id, secs_passed_game, number_event, msg_type = ifelse(msg_type == 3, "ft", "foul"), sequence_num) %>%
  pivot_wider(names_from = msg_type,
              values_from = number_event,
              names_prefix = "number_event_")

## FLAGRANT - CLEAR PATH
flagrant_clear <- lineup_game %>%
  filter(msg_type == 3 & act_type %in% c(18:20, 25:26, 27:29)) %>%
  select(game_id, secs_passed_game, number_event_ft = number_event, slug_team) %>%
  left_join(lineup_game %>%
              filter(msg_type == 6 & act_type %in% c(9, 14, 15)) %>%
              transmute(game_id, secs_passed_game, number_event_foul = number_event, 
                        slug_team = ifelse(slug_team == team_home, team_away, team_home)))

## REGULAR FOULS
other_fouls <- lineup_game %>%
  filter(msg_type %in% c(3, 6)) %>%
  filter(!str_detect(description, "Technical|Defense 3 Second"),
         !(msg_type == 3 & act_type %in% c(18:20, 25:26, 27:29)),
         !(msg_type == 6 & act_type %in% c(9, 14, 15)))

regular_fouls <- other_fouls %>%
  filter(msg_type == 3) %>%
  select(game_id, secs_passed_game, number_event_ft = number_event, slug_team, player_fouled = player1) %>%
  left_join(other_fouls %>%
              filter(msg_type == 6 & str_detect(description, "FT")) %>%
              transmute(game_id, secs_passed_game, number_event_foul = number_event, player_fouled = player3,
                        slug_team = ifelse(slug_team == team_home, team_away, team_home))) %>%
  left_join(other_fouls %>%
              filter(msg_type == 6 & str_detect(description, "FT")) %>%
              transmute(game_id, secs_passed_game, number_event_foul_y = number_event, 
                        number_event_foul = NA,
                        slug_team = ifelse(slug_team == team_home, team_away, team_home))) %>%
  mutate(number_event_foul = coalesce(number_event_foul, number_event_foul_y)) %>%
  select(-number_event_foul_y)

# putting everything together
fouls_stats <- bind_rows(regular_fouls, flagrant_clear, techs) %>%
  select(game_id, secs_passed_game, number_event_ft, number_event_foul) %>%
  left_join(pbp_poss %>%
              select(game_id, number_event_ft = number_event, slug_team, shot_pts, team_home, team_away, possession)) %>%
  # filter(is.na(shot_pts))  # test to see if there's nothing missing
  group_by(game_id, slug_team, number_event = number_event_foul, team_home, team_away) %>%
  summarise(total_fta = n(),
            total_pts = sum(shot_pts),
            total_poss = sum(possession)) %>%
  ungroup() %>%
  mutate(shot_pts_home = ifelse(slug_team == team_home, total_pts, 0),
         shot_pts_away = ifelse(slug_team == team_away, total_pts, 0),
         poss_home = ifelse(slug_team == team_home, total_poss, 0),
         poss_away = ifelse(slug_team == team_away, total_poss, 0)) %>%
  select(game_id, number_event, total_fta, shot_pts_home:poss_away)

pbp_poss_final <- pbp_poss %>%
  # mutate(possession = ifelse(start_poss == "00:00.0", 0, possession)) %>%   # considering nba.com bug when play has wrong clock at 00:00.0 (correct would be to not have this line)
  left_join(fouls_stats) %>%
  mutate(shot_pts_home = coalesce(shot_pts_home, ifelse(msg_type == 1 & slug_team == team_home, shot_pts, 0)),
         shot_pts_away = coalesce(shot_pts_away, ifelse(msg_type == 1 & slug_team == team_away, shot_pts, 0)),
         poss_home = coalesce(poss_home, ifelse(msg_type != 3 & possession == 1 & slug_team == team_home, possession, 0)),
         poss_away = coalesce(poss_away, ifelse(msg_type != 3 & possession == 1 & slug_team == team_away, possession, 0))) %>%
  group_by(game_id, period) %>%
  mutate(secs_played = lead(secs_passed_game) - secs_passed_game,
         secs_played = coalesce(secs_played, 0)) %>%
  ungroup() %>%
  left_join(player_logs %>%
              distinct(game_id = as.numeric(game_id), game_date = as.Date(game_date)))


# Add garbage time --------------------------------------------------------

pbp_final_gt <- pbp_poss_final %>%
  left_join(starters_quarters %>%
              filter(period == 1) %>%
              select(-c(period, slug_team)) %>%
              pivot_wider(names_from = team_location,
                          values_from = lineup_start,
                          names_prefix = "lineup_start_")) %>%
  mutate(across(c(contains("lineup")), ~ str_split(., ", "), .names = "{.col}_list")) %>%
  mutate(total_starters_home = map_int(map2(lineup_home_list, lineup_start_home_list, intersect), length),
         total_starters_away = map_int(map2(lineup_away_list, lineup_start_away_list, intersect), length)) %>%
  select(-contains("list")) %>%
  mutate(margin_before = case_when(shot_pts > 0 & slug_team == team_home ~ abs(hs - shot_pts - vs),
                                   shot_pts > 0 & slug_team == team_away ~ abs(vs - shot_pts - hs),
                                   TRUE ~ abs(hs - vs))) %>%
  mutate(garbage_time = case_when(
    # score differential >= 25 for minutes 12-9:
    secs_passed_game >= 2160 & secs_passed_game < 2340 & margin_before >= 25 & total_starters_home + total_starters_away <= 2 & period == 4 ~ 1,
    # score differential >= 20 for minutes 9-6:
    secs_passed_game >= 2340 & secs_passed_game < 2520 & margin_before >= 20 & total_starters_home + total_starters_away <= 2 & period == 4 ~ 1,
    # score differential >= 10 for minutes 6 and under:
    secs_passed_game >= 2520 & margin_before >= 10 & total_starters_home + total_starters_away <= 2 & period == 4 ~ 1,
    TRUE ~ 0)) %>%
  group_by(game_id) %>%
  mutate(max_nongarbage = max(number_event[which(garbage_time == 0)])) %>%
  ungroup() %>%
  mutate(garbage_time = ifelse(garbage_time == 1 & number_event < max_nongarbage, 0, garbage_time)) %>%
  select(-c(starts_with("lineup_start_"), max_nongarbage, opt2, ord, locX, locY))


# Stats -----------------------------------------------------------

lineup_stats <- pbp_final_gt %>%
  group_by(game_id, slug_team) %>%
  mutate(stint_home = ifelse(slug_team == team_home, cumsum(msg_type == 8) + 1, NA),
         stint_away = ifelse(slug_team == team_away, cumsum(msg_type == 8) + 1, NA)) %>%
  group_by(game_id) %>%
  mutate(across(starts_with("stint"), ~ na.locf0(., fromLast = TRUE)),
         across(starts_with("stint"), ~ na.locf(.))) %>%
  ungroup() %>%
  pivot_longer(cols = starts_with("lineup"),
               names_to = "lineup_location",
               values_to = "lineup",
               names_prefix = "lineup_") %>%
  mutate(pts_team = ifelse(lineup_location == "home", shot_pts_home, shot_pts_away),
         pts_opp = ifelse(lineup_location == "away", shot_pts_home, shot_pts_away),
         poss_team = ifelse(lineup_location == "home", poss_home, poss_away),
         poss_opp = ifelse(lineup_location == "away", poss_home, poss_away),
         slug_team = ifelse(lineup_location == "home", team_home, team_away),
         slug_opp = ifelse(lineup_location == "away", team_home, team_away),
         stint = ifelse(lineup_location == "home", stint_home, stint_away)) %>%
  select(game_id, game_date, period, stint, number_event, msg_type, description, lineup, pts_team, pts_opp,
         poss_team, poss_opp, secs_played, slug_team, slug_opp, garbage_time) %>%
  group_by(game_id, game_date, period, stint, slug_team, slug_opp, lineup, garbage_time) %>%
  summarise(across(c(pts_team, pts_opp, poss_team, poss_opp, secs_played), sum)) %>%
  ungroup() %>%
  filter(secs_played + poss_opp + poss_team + pts_opp + pts_team > 0) %>%
  group_by(game_id, slug_team) %>%
  mutate(stint = row_number()) %>%
  ungroup()
