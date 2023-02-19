library(tidyverse)
library(hoopR)
library(janitor)
library(future)

pbp2023 <- read_rds("https://github.com/ramirobentes/NBA-in-R/blob/master/2022_23/regseason/pbp/pbp-poss-rs23/data.rds?raw=true")
lineups2023 <- read_rds("https://github.com/ramirobentes/NBA-in-R/blob/master/2022_23/regseason/pbp/lineup-stats-rs23/data.rds?raw=true")
player_stats <- lineups2023 %>%
  separate_rows(lineup, sep = ", ") %>%
  group_by(player_name = lineup) %>%
  summarise(games = n_distinct(game_id),
            across(c(pts_team:secs_played), sum)) %>%
  ungroup()

# Bam Adebayo -------------------------------------------------------------

# 2. Leads the league in total clutch minutes (140) and has shot 23-for-38 (60.5%) with the score within five points in the last five minutes of the fourth quarter or overtime. That’s the fourth-best mark among 85 players with at least 25 clutch field goal attempts.

pbp2023 %>%
  group_by(game_id) %>%
  mutate(prev_play = lag(secs_passed_game)) %>%
  ungroup() %>%
  mutate(clutch_time = case_when(secs_passed_game >= 2580 & abs(margin_before) <= 5 ~ secs_passed_game - prev_play,
                                 # secs_passed_game >= 2580 & prev_play < 2580 & abs(margin_before) <= 5 ~ secs_passed_game - 2580,
                                 TRUE ~ 0)) %>%
  pivot_longer(cols = starts_with("lineup"),
               names_to = "lineup_location",
               values_to = "lineup",
               names_prefix = "lineup_") %>%
  separate_rows(lineup, sep = ", ") %>%
  group_by(player_name = lineup) %>%
  summarise(clutch_min = sum(clutch_time, na.rm = TRUE) / 60) %>%
  ungroup() %>%
  arrange(-clutch_min)

pbp2023 %>%
  filter(msg_type %in% c(1, 2),
         secs_passed_game >= 2580,
         abs(margin_before) <= 5) %>%
  count(player_name = player1, shot_result = ifelse(msg_type == 1, "made", "missed"), name = "fgm") %>%
  group_by(player_name) %>%
  mutate(fga = sum(fgm)) %>%
  ungroup() %>%
  mutate(fg_pct = fgm / fga) %>%
  filter(shot_result == "made",
         fga >= 25) %>%
  arrange(-fg_pct) %>%
  select(-shot_result)


# Giannis Antetokounmpo ---------------------------------------------------

# 3. Has drawn 9.5 fouls per game, the most for any player in the last 12 seasons.

plan(multicore)
boxscores_misc <- map_df(year_to_season(2011:2022), ~ hoopR::nba_playergamelogs(season = ., measure_type = "Misc", player_id = 0) %>%
                           pluck("PlayerGameLogs")) %>%
  clean_names()

boxscores_misc %>%
  mutate(across(c(min:nba_fantasy_pts_rank), as.numeric)) %>%
  group_by(season = season_year, player_id, player_name) %>%
  summarise(games = n(),
            fouls_drawn = sum(pfd)) %>%
  ungroup() %>%
  mutate(fd_game = fouls_drawn / games) %>%
  arrange(-fd_game)



# Jaylen Brown ------------------------------------------------------------

# 2. Has seen a drop in the percentage of his baskets that have been assisted every season he’s been in the league, from 70.3% as a rookie to 53.1% this season.

pbp_raw_past <- vroom::vroom("https://github.com/ramirobentes/NBA-in-R/releases/download/pbp17to22/data.csv")
pbp_raw_2023 <- vroom::vroom("https://github.com/ramirobentes/NBA-in-R/releases/download/pbp-raw-2023/data.csv")

bind_rows(pbp_raw_past, pbp_raw_2023) %>%
  filter(etype == 1,
         pid == 1627759) %>%
  count(season = str_sub(game_id, 2, 3), type = ifelse(!is.na(epid), "assisted", "unassisted")) %>%
  mutate(season = glue::glue("20{season}-{as.integer(season)+1}")) %>%
  pivot_wider(names_from = type,
              values_from = n) %>%
  mutate(pct_assisted = assisted / (assisted + unassisted))



# Stephen Curry -----------------------------------------------------------

# 3. Has grabbed 8.9% of available rebounds while he’s been on the floor. That’s a career-high rate and ranks third among 48 players 6-3 or shorter who’ve averaged at least 15 minutes per game.

player_info <- hoopR::nba_playerindex(season = "2022-23") %>%
  pluck("PlayerIndex") %>%
  clean_names()

pbp2023 %>%
  filter(msg_type == 4) %>%
  filter(!is.na(player1)) %>%
  count(player_name = player1, name = "total_reb", sort = T) %>%
  left_join(pbp2023 %>%
              filter(msg_type %in% c(2, 3),
                     !(msg_type == 3 & act_type %in% c(11, 13, 14, 16, 18:22, 25:26, 27:29)),
                     shot_pts == 0) %>%
              mutate(lineup = paste(lineup_home, lineup_away, sep = ", ")) %>%
              separate_rows(lineup, sep = ", ") %>%
              count(player_name = lineup, name = "reb_chances")) %>%
  mutate(reb_pct = total_reb / reb_chances) %>%
  arrange(-reb_pct) %>%
  semi_join(player_stats %>%
              filter(secs_played >= 15 * 60 * games,
                     games >= 25) %>%
              distinct(player_name)) %>%
  # filter to less than 6'4 (76 inches)
  semi_join(player_info %>%
              separate(height, into = c("feet", "inches"), sep = "-", convert = TRUE) %>%
              mutate(total_inches = feet * 12 + inches,
                     player_name = paste(player_first_name, player_last_name)) %>%
              filter(total_inches < 76) %>%
              distinct(player_name))


# DeMar DeRozan -----------------------------------------------------------

# 2. Is 7-for-18 (39%) on shots to tie or take the lead in the final minute of the fourth quarter or overtime. Both of those totals – the seven makes and the 18 attempts – lead the league.

pbp2023 %>%
  filter(msg_type %in% c(1, 2),
         margin_before <= 0,
         desc_value >= abs(margin_before),
         period >= 4,
         str_starts(clock, "01:00|00:")) %>%
  # select(game_id, period, clock, msg_type, description, player1, margin_before, desc_value, hs, vs, team_home, team_away) %>%
  count(player_name = player1, shot_result = ifelse(msg_type == 1, "made", "missed"), name = "fg") %>%
  group_by(player_name) %>%
  mutate(fga = sum(fg)) %>%
  ungroup() %>%
  mutate(fg_pct = fg / fga) %>%
  filter(shot_result == "made") %>%
  select(-shot_result) %>%
  arrange(-fg)






# Luka Doncic -------------------------------------------------------------

# 3. Leads the league (for the third straight season) in time of possession at 9.6 minutes per game, what would be the highest mark in the 10 years of tracking data.

plan(multicore)
player_timeposs<- map_df(year_to_season(2013:2022), ~ hoopR::nba_leaguedashptstats(season = ., 
                                                                                   player_or_team = "Player", 
                                                                                   pt_measure_type = "Possessions") %>%
                           pluck("LeagueDashPtStats") %>%
                           mutate(season = .x)) %>%
  clean_names() %>%
  mutate(across(c(gp:pts_per_paint_touch), as.numeric))

player_timeposs %>%
  filter(gp >= 25) %>%
  select(season, player_id, player_name, team_abbreviation, gp, time_of_poss) %>%
  mutate(time_poss_game = time_of_poss / gp) %>%
  arrange(-time_poss_game)





# Kevin Durant ------------------------------------------------------------

# 2. Has shot 57.1% from mid-range, which would be the best mark on at least 200 mid-range attempts in the 27 seasons for which we have shot-location data.

plan(multicore)
shots_season_all <- map_df(year_to_season(1996:2022), ~ nba_shotchartdetail(season = .,
                                                                            player_id = 0) %>%
                             pluck("Shot_Chart_Detail") %>%
                             mutate(season = .x)) %>%
  clean_names()
# for the data until 2021-22: https://github.com/ramirobentes/NBA-in-R/releases/download/shots-season-hist/data.csv

shots_season_all %>%
  filter(shot_zone_basic == "Mid-Range") %>%
  count(season, player_id, player_name, event_type, name = "fg") %>%
  group_by(season, player_id) %>%
  mutate(fga = sum(fg),
         fg_pct = fg / fga) %>%
  ungroup() %>%
  filter(event_type == "Made Shot",
         fga >= 200) %>%
  select(-event_type) %>%
  arrange(-fg_pct)



# Anthony Edwards ---------------------------------------------------------

# 2. Has attempted 84 2-pointers from 18 feet or deeper, fourth most in the league. His 30-for-84 (35.7%) on those long 2s ranks 21st among 26 players who’ve attempted at least 50.
  
shots_season <- nba_shotchartdetail(season = "2022-23",
                                    player_id = 0) %>%
  pluck("Shot_Chart_Detail") %>%
  clean_names()

shots_season %>%
  mutate(shot_distance = as.numeric(shot_distance)) %>%
  filter(shot_distance >= 18,
         shot_type == "2PT Field Goal") %>%
  count(player_id, player_name, event_type, name = "fg") %>%
  group_by(player_id) %>%
  mutate(fga = sum(fg),
         fg_pct = fg / fga) %>%
  ungroup() %>%
  filter(event_type == "Made Shot",
         fga >= 50) %>%
  select(-event_type) %>%
  arrange(fg_pct)
  

# Joel Embiid -------------------------------------------------------------

# 3. Has shot 38-for-85 (44.2%) on catch-and-shoot 3-pointers, but just 12-for-54 (22.2%) on pull-up 3-pointers. That’s the fourth biggest differential among 95 players who’ve attempted at least 50 of each. Has taken only 15.3% of his total shots from 3-point range, the lowest rate of his career.
  
shooting_dash <- hoopR::nba_playerdashptshots(season = "2022-23", player_id = 0) %>%
  pluck("GeneralShooting") %>%
  clean_names() %>%
  mutate(across(c(fga_frequency:fg3_pct), as.numeric))
  
shooting_dash %>%
  filter(shot_type %in% c("Catch and Shoot", "Pull Ups")) %>%
  mutate(player_name = str_split(player_name_last_first, ", "),
         player_name = map(player_name, rev),
         player_name =  map_chr(player_name, ~ paste(., collapse = " "))) %>%
  select(player_id, player_name, shot_type, fg3m, fg3a, fg3_pct) %>%
  pivot_wider(names_from = shot_type,
              values_from = contains("fg"),
              values_fill = 0) %>%
  clean_names() %>%
  filter(fg3a_pull_ups >= 50,
         fg3a_catch_and_shoot >= 50) %>%
  mutate(diff_pct = fg3_pct_catch_and_shoot - fg3_pct_pull_ups) %>%
  arrange(-diff_pct)
  
  
  

# De’Aaron Fox ------------------------------------------------------------

# 1. Leads the league in clutch usage rate (44.3%) total clutch points (148). His 57-for-97 (58.8%) shooting on clutch shots is the best mark (by a wide margin) among 29 players with at least 50 clutch field goal attempts.

  
players_clutch <- hoopR::nba_leaguedashplayerclutch(season = "2022-23", measure_type = "Advanced") %>%
  pluck("LeagueDashPlayerClutch") %>%
  clean_names()

players_clutch %>%
  mutate(across(c(gp:usg_pct), as.numeric)) %>%
  filter(min * gp >= 25) %>%
  select(player_id, player_name, team_abbreviation, usg_pct) %>%
  arrange(-usg_pct)
  
pbp2023 %>%
  filter(secs_passed_game >= 2580,
         abs(margin_before) <= 5) %>%
  group_by(player_name = player1) %>%
  summarise(total_pts = sum(shot_pts)) %>%
  ungroup() %>%
  arrange(-total_pts)


# Paul George -------------------------------------------------------------

# 3. Ranks fifth with 3.5 deflections per game, but has averaged just 1.5 steals, down from 2.2 last season. That’s the third biggest drop among 313 players who’ve played at least 25 games in each of the last two seasons.
  

players_hustle <- hoopR::nba_leaguehustlestatsplayer(season = "2022-23") %>%
  pluck("HustleStatsPlayer") %>%
  clean_names() %>%
  mutate(across(c(g:pct_box_outs_reb), as.numeric))
  
players_hustle %>%
  filter(g >= 25) %>%
  select(player_id, player_name, team_abbreviation, g, deflections) %>%
  mutate(deflections_game = deflections / g) %>%
  arrange(-deflections_game)  


# Shai Gilgeous-Alexander -------------------------------------------------

# 1. Leads the league (for the third straight season) with 24.2 drives per game, the second most for any player in the 10 seasons of tracking data, trailing only his own mark of 25.2 drives per game two seasons ago.
  
plan(multicore)
players_drives <- map_df(year_to_season(2013:2022), ~ nba_leaguedashptstats(season = ., 
                                                                            player_or_team = "Player", 
                                                                            pt_measure_type = "Drives") %>%
                           pluck("LeagueDashPtStats") %>%
                           clean_names() %>%
                           mutate(season = .x))  
  
players_drives %>%
  mutate(across(c(gp:drive_pf_pct), as.numeric)) %>%
  select(season, player_id, player_name, team_abbreviation, gp, drives) %>%
  mutate(drives_game = drives / gp) %>%
  arrange(-drives_game)
  


# Tyrese Haliburton -------------------------------------------------------

  
# 3. One of three players – Curry and Irving are the others – who have shot 37% or better on at least 100 pull-up 3-pointers in each of the last three seasons.
  
plan(multicore)
shooting_dash_multi <- map_df(year_to_season(2020:2022), ~ hoopR::nba_playerdashptshots(season = ., player_id = 0) %>%
                                pluck("GeneralShooting") %>%
                                mutate(season = .x)) %>%
  clean_names() %>%
  mutate(across(c(fga_frequency:fg3_pct), as.numeric))
  
  
shooting_dash_multi %>%
  filter(shot_type == "Pull Ups") %>%
  mutate(player_name = str_split(player_name_last_first, ", "),
         player_name = map(player_name, rev),
         player_name =  map_chr(player_name, ~ paste(., collapse = " "))) %>%
  select(season, player_id, player_name, shot_type, fg3m, fg3a, fg3_pct) %>%
  filter(fg3a >= 100) %>%
  group_by(player_id) %>%
  filter(sum(fg3_pct >= 0.37) == 3) %>%
  ungroup()
  


# Jrue Holiday ------------------------------------------------------------

# 1. Has taken 39.5% of his shots, the highest rate of his career, from 3-point range. One of six players – Doncic, Haliburton Mitchell are three of the others – who’ve made at least 100 3-pointers with less than half of them being assisted.

pbp2023 %>%
  filter(msg_type == 1,
         shot_pts == 3) %>%
  count(player_name = player1, type = ifelse(!is.na(player2), "assisted", "unassisted"), name = "fg") %>%
  group_by(player_name) %>%
  mutate(fgm = sum(fg),
         assisted_pct = fg / fgm) %>%
  filter(fgm >= 100,
         type == "assisted",
         assisted_pct <= 0.5)
  

# Kyrie Irving ------------------------------------------------------------

# 3. Has shot 22-for-23 (96%) on clutch free throws, the second-best mark (behind that of Lillard) among 38 players who’ve attempted at least 20.
  
pbp2023 %>%
  filter(secs_passed_game >= 2580,
         abs(margin_before) <= 5) %>%
  filter(msg_type == 3) %>%
  count(player_name = player1, shot_pts, name = "ft") %>%
  group_by(player_name) %>%
  mutate(fta = sum(ft),
         ft_pct = ft / fta) %>%
  ungroup() %>%
  filter(fta >= 20,
         shot_pts == 1) %>%
  select(-shot_pts) %>%
  arrange(-ft_pct)
  
  
  
  

# Jaren Jackson Jr. -------------------------------------------------------

# 1. Leads the league with 3.3 blocks per game, which would be the second-highest average for qualified players in the last 10 seasons. Opponents have shot 44.8% at the rim when he’s been there, the best mark (by a wide margin) among 56 players who’ve defended at least 200 shots at the rim.
  

players_def <- nba_leaguedashptstats(season = "2022-23", 
                      player_or_team = "Player", 
                      pt_measure_type = "Defense") %>%
  pluck("LeagueDashPtStats") %>%
  clean_names()

players_def %>%
  mutate(across(c(contains("fg")), as.numeric)) %>%
  select(player_id, player_name, team_abbreviation, gp, def_rim_fgm, def_rim_fga, def_rim_fg_pct) %>%
  filter(def_rim_fga >= 200) %>%
  arrange(def_rim_fg_pct)


# LeBron James ------------------------------------------------------------

# 2. Leads the league with 6.4 fast-break points per game, the highest mark of his career.

plan(multicore)
lebron_misc <- map_df(year_to_season(2003:2022), ~ hoopR::nba_playergamelogs(season = ., measure_type = "Misc", player_id = 2544) %>%
         pluck("PlayerGameLogs")) %>%
  clean_names()

lebron_misc %>%
  mutate(across(c(min:nba_fantasy_pts_rank), as.numeric)) %>%
  group_by(season = season_year) %>%
  summarise(games = n(),
            pts_fb = sum(pts_fb)) %>%
  ungroup() %>%
  mutate(pts_fb_game = pts_fb / games) %>%
  arrange(-pts_fb_game)



# Nikola Jokic ------------------------------------------------------------

# 3. Leads the league in cumulative plus-minus by a wide margin, with the Nuggets having outscored their opponents by 531 points with him on the floor. Denver has been 25.3 points per 100 possessions better with him on the floor (plus-14.3) than its been with him off the floor (minus-11.0). That would be the second biggest on-off differential (minimum 1,000 minutes on the floor) in the 17 seasons for which we have on-off data, topped only by Draymond Green’s differential of 26.1 per 100 possessions in 2015-16.

lineups2023 %>%
  separate_rows(lineup, sep = ", ") %>%
  group_by(player_name = lineup) %>%
  summarise(across(c(pts_team:secs_played), sum)) %>%
  ungroup() %>%
  mutate(plus_minus = pts_team - pts_opp) %>%
  arrange(-plus_minus)




# Damian Lillard ----------------------------------------------------------

# 2. Has a true shooting percentage of 64.8%, up from 55.0% last season. That’s the biggest jump among 224 players with at least 200 field goal attempts in each of the last two seasons. Leads the league with six games of scoring 30 or more points on a true shooting percentage of 80% or better, with no other player having more than four.

plan(multicore)
player_logs_multi <- map_df(year_to_season(2021:2022), ~ hoopR::nba_playergamelogs(season = ., player_id = .) %>%
                              pluck("PlayerGameLogs")) %>%
  clean_names()

player_logs_multi %>%
  mutate(across(c(min:plus_minus_rank), as.numeric)) %>%
  group_by(season = season_year, player_id, player_name) %>%
  summarise(across(c(pts, fga, fta), sum)) %>%
  group_by(player_id) %>%
  filter(sum(fga >= 200) == 2) %>%
  ungroup() %>%
  mutate(ts_pct = pts / (2 * (fga + 0.44 * fta))) %>%
  select(season, player_id, player_name, fga, ts_pct) %>%
  pivot_wider(names_from = season,
              values_from = c(fga, ts_pct)) %>%
  clean_names() %>%
  mutate(ts_diff = ts_pct_2022_23 - ts_pct_2021_22) %>%
  arrange(-ts_diff)
  

# Lauri Markkanen ---------------------------------------------------------

# 2. Has scored 0.490 points per touch, most among 363 players with at least 500 touches. One of two players – Curry is the other – who’ve shot 55% or better on at least 300 2-point attempts and 40% or better on at least 200 3-point attempts.

player_touches <- hoopR::nba_leaguedashptstats(season = "2022-23", player_or_team = "Player", pt_measure_type = "Possessions") %>%
  pluck("LeagueDashPtStats") %>%
  clean_names() %>%
  mutate(across(c(gp:pts_per_paint_touch), as.numeric))

player_touches %>%
  select(player_id, player_name, team_abbreviation, points, touches) %>%
  mutate(pts_touches = points / touches) %>%
  filter(touches >= 500) %>%
  arrange(-pts_touches)


# Donovan Mitchell --------------------------------------------------------

# 2. Tied for second with 22 3-pointers in the last four seconds of the shot clock.

shooting_dash <- hoopR::nba_playerdashptshots(season = "2022-23", player_id = 0) %>%
  pluck("ShotClockShooting") %>%
  clean_names() %>%
  mutate(across(c(fga_frequency:fg3_pct), as.numeric))

shooting_dash %>%
  mutate(player_name = str_split(player_name_last_first, ", "),
         player_name = map(player_name, rev),
         player_name =  map_chr(player_name, ~ paste(., collapse = " "))) %>%
  select(player_id, player_name, shot_clock_range, fg3m, fg3a, fg3_pct) %>%
  filter(shot_clock_range == "4-0 Very Late") %>%
  arrange(-fg3m)




# Ja Morant ---------------------------------------------------------------

# 1. Leads the league with 12.4 pick-and-roll ball-handler points per game. His 0.93 points per possession as a pick-and-roll ball-handler ranks 51st among 111 players with at least 100 total ball-handler possessions.

library(httr)
library(jsonlite)

source("https://raw.githubusercontent.com/sportsdataverse/hoopR/56c767cf4c171db1e0de6853ba35371343e7c1aa/R/utils_nba_stats.R")

get_synergy_stats <- function(season, season_type = "Regular Season", category, per_mode = "Totals", player_team = "P", type_group = "Offensive"){
  
  url <- glue::glue("https://stats.nba.com/stats/synergyplaytypes?LeagueID=00&PerMode={per_mode}&PlayType={category}&PlayerOrTeam={player_team}&SeasonType={URLencode(season_type)}&SeasonYear={season}&TypeGrouping={type_group}")
  
  res <- .nba_headers(url)
  
  synergy <- res[["resultSets"]][["rowSet"]][[1]] %>% 
    as.data.frame()
  
  colnames(synergy) <-  res[["resultSets"]][["headers"]][[1]]
  
  synergy <- synergy %>% 
    janitor::clean_names()
  
  return(as_tibble(synergy))
}


pr_ballhandlers <- get_synergy_stats("2022-23", category = "PRBallHandler") %>%
  mutate(across(c(percentile:fgmx), as.numeric))

pr_ballhandlers %>%
  select(player_id, player_name, team_abbreviation, play_type, gp, pts) %>%
  mutate(pts_game = pts / gp) %>%
  arrange(-pts_game)

pr_ballhandlers %>%
  select(player_id, player_name, team_abbreviation, play_type, poss, ppp) %>%
  filter(poss >= 100) %>%
  mutate(rank_ppp = rank(-ppp, ties.method = "first")) %>%
  arrange(rank_ppp) %>%
  slice(46:60)



# Julius Randle -----------------------------------------------------------

# 1. Ranks second in the league with 4.3 second-chance points per game.

pbp2023 %>%
  group_by(game_id, period, slug_team, start_poss) %>%
  filter(sum(msg_type == 4 & desc_value == 1 & act_type == 0) > 0) %>%
  mutate(min_off_reb = min(number_event[which(msg_type == 4 & desc_value == 1 & act_type == 0)])) %>%
  ungroup() %>%
  filter(number_event > min_off_reb,
         shot_pts > 0) %>%
  group_by(player_name = player1) %>%
  summarise(pts_2chance = sum(shot_pts)) %>%
  ungroup() %>%
  left_join(player_stats %>%
              select(player_name, games)) %>%
  mutate(pts_2chance_game = pts_2chance / games) %>%
  arrange(-pts_2chance_game)


# 3.His 80 3-pointers in the first quarter are more than any other player has in any quarter and more than he has in the second, third and fourth quarters combined (78).

pbp2023 %>%
  filter(msg_type == 1,
         shot_pts == 3) %>%
  count(period, player_name = player1) %>%
  pivot_wider(names_from = period, 
              values_from = n,
              names_prefix = "q",
              values_fill = 0) %>%
  arrange(-q1) 


# Domantas Sabonis --------------------------------------------------------

# 2. Has an effective field goal percentage of 62.9%, the best mark of his career and the fourth-best mark among 112 players with at least 500 field goal attempts.

player_logs <- nba_leaguegamelog(season = "2022-23", player_or_team = "P") %>%
  pluck("LeagueGameLog") %>%
  clean_names() %>%
  mutate(across(c(min:fantasy_pts), as.numeric))

player_logs %>%
  group_by(player_id, player_name) %>%
  summarise(across(c(fgm, fg3m, fga), sum)) %>%
  ungroup() %>%
  mutate(efg_pct = (fgm + (0.5 * fg3m)) / fga) %>%
  filter(fga >= 500) %>%
  arrange(-efg_pct)


# Pascal Siakam -----------------------------------------------------------

# 3. Has seen a jump in free throw rate, from 31.4 attempts per 100 shots from the field last season to 39.7 per 100 this season. That has allowed him to see a jump in true shooting percentage while seeing a drop in effective field goal percentage.

plan(multicore)
player_logs_multi <- map_df(year_to_season(2021:2022), ~ hoopR::nba_playergamelogs(season = ., player_id = .) %>%
                              pluck("PlayerGameLogs")) %>%
  clean_names()

player_logs_multi %>%
  mutate(across(c(min:plus_minus_rank), as.numeric)) %>%
  group_by(season = season_year, player_id, player_name) %>%
  summarise(across(c(fta, fga), sum)) %>%
  ungroup() %>%
  mutate(ft_rate = fta / fga) %>%
  filter(player_name == "Pascal Siakam")
  


# Jayson Tatum ------------------------------------------------------------

# 2. Has registered the two highest single-game plus-minus marks in the league this season, a plus-46 against Brooklyn on Feb. 1 and a plus-45 against Charlotte on Nov. 28. Overall, the Celtics have been 11.4 points per 100 possessions better with him on the floor (plus-9.6) than they’ve been with him off the floor (minus-1.8). That’s the eighth-biggest differential among 206 players that have played at least 1,000 minutes for a single team.

lineups2023 %>%
  separate_rows(lineup, sep = ", ") %>%
  group_by(game_id, game_date, player_name = lineup, slug_team, slug_opp) %>%
  summarise(across(c(pts_team:secs_played), sum)) %>%
  ungroup() %>%
  mutate(plus_minus = pts_team - pts_opp) %>%
  arrange(-plus_minus)

## on/off difference
with_players <- lineups2023 %>%
  filter(game_date <= "2023-02-15") %>%
  separate_rows(lineup, sep = ", ") %>%
  group_by(player_name = lineup, slug_team) %>%
  summarise(across(c(pts_team:secs_played), sum)) %>%
  ungroup() %>%
  mutate(pts100 = pts_team /poss_team * 100,
         opp100 = pts_opp / poss_opp * 100,
         net100 = pts100 - opp100)

without_players <- lineups2023 %>%
  filter(game_date <= "2023-02-15") %>%
  left_join(lineups2023 %>%
              separate_rows(lineup, sep = ", ") %>%
              group_by(slug_team) %>%
              summarise(all_players = str_split(paste(unique(lineup), collapse = ", "), ", ")) %>%
              ungroup()) %>%
  mutate(lineup_list = str_split(lineup, ", "),
         players_out = map2(lineup_list, all_players, ~ setdiff(.y, .x)),
         players_out = map_chr(players_out, ~ paste(., collapse = ", "))) %>%
  separate_rows(players_out, sep = ", ") %>%
  group_by(player_name = players_out, slug_team) %>%
  summarise(across(c(pts_team:poss_opp), sum)) %>%
  ungroup() %>%
  mutate(pts100_out = pts_team /poss_team * 100,
         opp100_out = pts_opp / poss_opp * 100,
         net100_out = pts100_out - opp100_out)

with_players %>%
  select(player_name, slug_team, secs_played, contains("100")) %>%
  left_join(without_players %>%
              select(player_name, slug_team, contains("100"))) %>%
  filter(secs_played >= 60 * 1000) %>%
  mutate(diff_net100 = net100 - net100_out) %>%
  arrange(-diff_net100) %>%
  head(15)
# small decimal discrepancies due to inconsistencies in official possession count on nba.com


# Zion Williamson ---------------------------------------------------------

# 3. His 0.24 made 3-pointers per game would be the second-fewest for any player averaging at least 25 points in the last 10 seasons, higher than only his own 0.16 3s per game two seasons ago.

plan(multicore)
boxscores_trad <- map_df(year_to_season(2013:2022), ~ hoopR::nba_playergamelogs(season = ., player_id = 0) %>%
                           pluck("PlayerGameLogs")) %>%
  clean_names()

boxscores_trad %>%
  mutate(across(c(min:nba_fantasy_pts_rank), as.numeric)) %>%
  group_by(season = season_year, player_id, player_name) %>%
  summarise(across(c(pts, fg3m), mean, .names = "{col}_game")) %>%
  ungroup() %>%
  filter(pts_game >= 25) %>%
  arrange(fg3m_game)
  
  
