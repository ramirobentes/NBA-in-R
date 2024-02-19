library(tidyverse)
library(hoopR)
library(janitor)

pbp2024 <- read_rds("https://github.com/ramirobentes/nba_pbp_data/raw/main/pbp-final-2024/data.rds")
lineups2024 <- read_csv("https://github.com/ramirobentes/nba_pbp_data/raw/main/lineup-final2024/data.csv")
player_stats <- lineups2024 %>%
  separate_rows(lineup_team, sep = ", ") %>%
  summarise(games = n_distinct(game_id),
            across(c(secs_played:pts_opp), sum),
            .by = lineup_team) %>%
  rename(player_name = lineup_team)
player_logs <- map_df(year_to_season(1996:2023),
                       function(x){
                         nba_leaguegamelog(season = x, player_or_team = "P") %>%
                           pluck("LeagueGameLog") %>%
                           mutate(season = x)
                         }) %>%
  clean_names() %>%
  mutate(across(c(min:fantasy_pts), as.numeric))
shots <- map_df(year_to_season(1996:2023),
                function(x){
                  nba_shotchartdetail(season = x, player_id = 0) %>%
                    pluck("Shot_Chart_Detail") %>%
                    clean_names() %>%
                    select(game_id:shot_distance) %>%
                    mutate(season = x)
                })


# Bam Adebayo -------------------------------------------------------------

# (FGM + (0.5 * 3PM)) / FGA

# Has shot 57.1% in the paint and has an effective field goal percentage of just 31.3% on shots from outside the paint. That’s the third biggest differential among 192 players with at least 100 field goal attempts both in and outside the paint, behind only those of Ausar Thompson and Giannis Antetokounmpo.
shots %>%
  filter(season == "2023-24") %>%
  count(player_id, player_name, event_type,
        shot_location = ifelse(shot_zone_basic %in% c("Restricted Area", "In The Paint (Non-RA)"), "paint", "outside"), 
        shot_type = word(shot_type, 1)) %>%
  summarise(fgm = sum(n[event_type == "Made Shot"]),
            fg3m = sum(n[event_type == "Made Shot" & shot_type == "3PT"]),
            fga = sum(n),
            .by = c(player_id, player_name, shot_location)) %>%
  mutate(efg_pct = (fgm + (0.5 * fg3m)) / fga) %>%
  select(-c(fgm, fg3m)) %>%
  pivot_wider(names_from = shot_location,
              values_from = c(fga, efg_pct),
              values_fill = 0) %>%
  filter(fga_outside >= 100 & fga_paint >= 100) %>%
  mutate(efg_pct_diff = efg_pct_paint - efg_pct_outside) %>%
  arrange(-efg_pct_diff)


  




# Giannis Antetokounmpo ---------------------------------------------------

# Has averaged 19.0 points per game (on the road*) in the restricted area, 2.1 more than any other player has averaged in the 28 seasons for which we have shot-location data.
shots %>%
  left_join(player_logs %>%
              mutate(team_location = ifelse(str_detect(matchup, "\\@"), "away", "home")) %>%
              distinct(game_id, team_id, team_location)) %>%
  filter(shot_zone_basic == "Restricted Area",
         team_location == "away",
         event_type == "Made Shot") %>%
  count(season, player_id, player_name, name = "fg") %>%
  summarise(pts = sum(fg * 2),
            .by = c(season, player_id, player_name)) %>%
  left_join(player_logs %>%
              filter(str_detect(matchup, "\\@")) %>%
              count(season, player_id, name = "games")) %>%
  mutate(pts_game = pts / games) %>%
  arrange(-pts_game)


# Has drawn 8.5 fouls per game, leading the league for the fifth straight season.
pbps <- map_df(2020:2024,
               function(x){
                 read_rds(glue::glue("https://github.com/ramirobentes/nba_pbp_data/raw/main/pbp-final-{x}/data.rds")) %>%
                   mutate(season = glue::glue("{x - 1}-{str_sub(x, 3, 4)}"))
               })

pbps %>%
  filter(msg_type == 6) %>%
  count(season, player_name = player3_name, name = "fouls_drawn") %>%
  filter(!is.na(player_name)) %>%
  left_join(player_logs %>% count(season, player_name = paste(player_id, player_name), name = "games")) %>%
  mutate(fd_game = fouls_drawn / games) %>%
  filter(fd_game == max(fd_game),
         .by = season)


# Paolo Banchero ----------------------------------------------------------

# Ranks second (behind Stephen Curry) with seven buckets (on 16 attempts) to tie or take the lead in the final minute of the fourth quarter or overtime.
pbp2024 %>%
  mutate(shot_type = ifelse(str_detect(description, "3pt"), 3, 2),
         margin_before = ifelse(team_abb == team_home | is.na(team_abb), hs - shot_pts - vs, vs - shot_pts - hs)) %>%
  filter(period >= 4,
         str_starts(clock, "01:00|00:"),
         msg_type %in% c(1, 2),
         shot_type >= abs(margin_before),
         margin_before <= 0) %>%
  count(shot_result = ifelse(msg_type == 1, "made", "missed"), player_name = player1_name) %>%
  pivot_wider(names_from = shot_result,
              values_from = n,
              values_fill = 0,
              names_prefix = "fg_") %>%
  mutate(fg_att = fg_made + fg_missed) %>%
  select(-fg_missed) %>%
  arrange(-fg_made)
  


# Scottie Barnes ----------------------------------------------------------

# Has scored just 0.71 points per possession on isolations, according to Synergy tracking, the lowest mark among 50 players with at least 75 isolation possessions.
synergy_isolation <- nba_synergyplaytypes(season = "2023-24",
                                          per_mode = "Totals",
                                          play_type = "Isolation",
                                          player_or_team = "P",
                                          type_grouping = "Offensive") %>%
  pluck("SynergyPlayType") %>%
  clean_names() %>%
  mutate(across(c(percentile:fgmx), as.numeric))

synergy_isolation %>%
  filter(poss >= 75) %>%
  select(player_id, player_name, team_name, play_type, pts, poss, ppp) %>%
  arrange(ppp)

# Leads the league in total miles traveled (147.7).
players_distance <- nba_leaguedashptstats(season = "2023-24",
                                          player_or_team = "Player",
                                          pt_measure_type = "SpeedDistance",
                                          per_mode = "Totals") %>%
  pluck("LeagueDashPtStats") %>%
  clean_names() %>%
  mutate(across(c(gp:avg_speed_def), as.numeric))

players_distance %>%
  select(player_id, player_name, team_abbreviation, gp, dist_miles_off, dist_miles_def, dist_miles) %>%
  arrange(-dist_miles)

# Devin Booker ------------------------------------------------------------

# His 7.0 assists per game (up from 5.5 last season) and assist/turnover ratio of 2.74 are both the highest marks of his career.
player_logs %>%
  summarise(team_abb = paste(unique(team_abbreviation), collapse = ", "),
            across(c(ast, tov), sum),
            .by = c(season, player_id, player_name)) %>%
  mutate(ast_tov = ast / tov) %>%
  filter(player_name == "Devin Booker") %>%
  arrange(-ast_tov)


# He’s shot 62.0% in the paint, the best mark of his career by a healthy margin.
shots %>%
  filter(player_name == "Devin Booker",
         shot_zone_basic %in% c("In The Paint (Non-RA)", "Restricted Area")) %>%
  summarise(fgm = sum(event_type == "Made Shot"),
            fga = n(),
            .by = c(season, player_id, player_name)) %>%
  mutate(fg_pct = fgm / fga) %>%
  arrange(-fg_pct)



# Jaylen Brown ------------------------------------------------------------

# Has been assisted on 46.0% of his field goals, the lowest rate of his career by a healthy margin. His own assists per game (3.7), assist ratio (14.8% of his possessions) and assist/turnover ratio (1.51) are all the highest marks of his career.
pbps <- map_df(2017:2024, function(x){
  read_rds(glue::glue("https://github.com/ramirobentes/nba_pbp_data/raw/main/pbp-final-{x}/data.rds")) %>%
    mutate(season = glue::glue("{x - 1}-{str_sub(x, 3, 4)}"))
})

## % of fgm assisted
pbps %>%
  filter(msg_type == 1,
         player1_name == "1627759 Jaylen Brown") %>%
  count(season, assisted = !is.na(player2_name)) %>%
  mutate(fgm_total = sum(n),
         .by = season) %>%
  filter(assisted) %>%
  select(season, fgm_assisted = n, fgm_total) %>%
  mutate(fgm_assisted_pct = fgm_assisted / fgm_total) %>%
  arrange(fgm_assisted_pct)

## assist ratio
pbps %>%
  filter((msg_type %in% c(1, 2, 5)) | (msg_type == 3 & str_detect(description, "1 of ") & !str_detect(description, "1 of 1")))  %>%
  select(season, game_id, event_num, msg_type, period, clock, team_abb, description, player1 = player1_name, player2 = player2_name) %>%
  pivot_longer(cols = c(player1, player2),
               names_to = "player_number",
               values_to = "player_name",
               names_prefix = "player") %>%
  filter(!is.na(player_name)) %>%
  mutate(play_type = case_when(msg_type %in% c(1, 2) & player_number == 1 ~ "fga",
                               msg_type == 1 & player_number == 2 ~ "assists",
                               msg_type == 5 ~ "turnover",
                               msg_type == 3 ~ "ft_trip")) %>%
  count(season, player_name, play_type) %>%
  pivot_wider(names_from = play_type,
              values_from = n,
              values_fill = 0) %>%
  mutate(assist_ratio = assists / (assists + fga + ft_trip + turnover)) %>%
  arrange(-assist_ratio) %>%
  filter(player_name == "1627759 Jaylen Brown")



# Jalen Brunson -----------------------------------------------------------

# Leads the league in total time of possession (433 minutes). Has averaged 5.85 seconds per touch and 5.58 dribbles per touch. Both are the highest rates among 258 players with at least 1,000 touches.
players_poss <- nba_leaguedashptstats(season = "2023-24", 
                               player_or_team = "Player", 
                               pt_measure_type = "Possessions",
                               per_mode = "Totals") %>%
  pluck("LeagueDashPtStats") %>%
  clean_names() %>%
  mutate(across(c(gp:pts_per_paint_touch), as.numeric))

players_poss %>%
  select(player_id, player_name, team_abbreviation, time_of_poss, avg_sec_per_touch, avg_drib_per_touch) %>%
  arrange(-time_of_poss)



# Has drawn 24 charges, second most in the league.
pbp2024 %>%
  filter(msg_type == 6,
         str_detect(description, "Charge")) %>%
  count(player_name = player3_name, sort = T, name = "charges_drawn")



# Stephen Curry -----------------------------------------------------------

# Has scored 162 points with the score within five points in the last five minutes, 54 more than any other player. His 28 clutch 3-pointers are 18 more than any other player has made, and his 28-for-58 (48.3%) on clutch 3s is the second-best mark among 14 players who’ve attempted at least 20. His 32-for-33 (97.0%) on clutch free throws is also the best mark among 26 players who’ve attempted at least 20.
pbp2024 %>%
  filter(game_date < as.Date("2024-02-15")) %>%
  filter(secs_game >= 2580,
         margin_before <= 5,
         shot_pts > 0) %>%
  summarise(total_pts = sum(shot_pts),
            .by = player1_name) %>%
  arrange(-total_pts)

pbp2024 %>%
  filter(game_date < as.Date("2024-02-15")) %>%
  filter(secs_game >= 2580,
         margin_before <= 5,
         msg_type %in% c(1, 2),
         str_detect(description, "3pt")) %>%
  count(player_name = player1_name, shot_result = ifelse(msg_type == 1, "made", "missed"), name = "fg") %>%
  mutate(fga = sum(fg),
         .by = player_name) %>%
  filter(shot_result == "made",
         fga >= 20) %>%
  transmute(player_name, fgm = fg, fga, fg_pct = fg / fga) %>%
  arrange(-fgm)
  


# Anthony Davis -----------------------------------------------------------

# Has been assisted 118 times by LeBron James. That’s the fourth-most assists from one player to a single teammate and 49 more assists than James had to Davis last season (69).
pbp2024 %>%
  filter(msg_type == 1) %>%
  count(player_fg = player1_name, player_assist = player2_name, sort = T, name = "fg") %>%
  filter(!is.na(player_assist))

# Has recorded assists on only 1.4% of his drives, the lowest rate among 144 players with at least 200 total drives.
players_drives <- nba_leaguedashptstats(season = "2023-24", 
                                        player_or_team = "Player", 
                                        pt_measure_type = "Drives",
                                        per_mode = "Totals") %>%
  pluck("LeagueDashPtStats") %>%
  clean_names() %>%
  mutate(across(c(gp:drive_pf_pct), as.numeric))

players_drives %>%
  filter(drives >= 200) %>%
  select(player_id, player_name, team_abbreviation, drives, drive_ast, drive_ast_pct) %>%
  arrange(drive_ast_pct)


# Luka Doncic -------------------------------------------------------------

# Leads the league with 141 pull-up 3-pointers. The 36.3% he’s shot on pull-up 3s is the best mark of his career.
shooting_league <- nba_playerdashptshots(season = "2023-24", player_id = 0) %>%
  pluck("GeneralShooting") %>%
  clean_names() %>% 
  mutate(across(c(fga_frequency:fg3_pct), as.numeric)) %>%
  mutate(player_name = str_split(player_name_last_first, ", "),
         player_name = map(player_name, rev),
         player_name =  map_chr(player_name, ~ paste(., collapse = " ")))

shooting_league %>%
  filter(shot_type == "Pull Ups") %>%
  select(player_id, player_name, g, shot_type, fg3m, fg3a, fg3_pct) %>%
  arrange(-fg3m)


shooting_luka <- map_df(year_to_season(2018:2023),
                        function(x){
                          nba_playerdashptshots(season = x, player_id = 1629029) %>%
                            pluck("GeneralShooting") %>%
                            mutate(season = x)
                        }) %>%
  clean_names() %>% 
  mutate(across(c(fga_frequency:fg3_pct), as.numeric))

shooting_luka %>%
  mutate(player_name = str_split(player_name_last_first, ", "),
         player_name = map(player_name, rev),
         player_name =  map_chr(player_name, ~ paste(., collapse = " "))) %>%
  filter(shot_type == "Pull Ups") %>%
  select(season, player_id, player_name, g, shot_type, fg3m, fg3a, fg3_pct) %>%
  arrange(-fg3_pct)


# Kevin Durant ------------------------------------------------------------

# Only player who’s averaged at least six points per game on drives, six points per game on catch-and-shoot jumpers and six points per game on pull-up jumpers.
players_drives %>%
  select(player_id, player_name, gp, drive_pts) %>%
  left_join(shooting_league %>%
              mutate(pts = fg2m * 2 + fg3m * 3) %>%
              select(player_id, player_name, shot_type, pts) %>%
              filter(shot_type %in% c("Catch and Shoot", "Pull Ups")) %>%
              pivot_wider(names_from = shot_type,
                          values_from = pts,
                          values_fill = 0,
                          names_prefix = "pts") %>%
              clean_names()) %>%
  mutate(across(c(contains("pts")), ~ . / gp, .names = "{col}_game")) %>%
  filter(drive_pts_game >= 6,
         pts_catch_and_shoot_game >= 6,
         pts_pull_ups_game >= 6)


# Has taken 15% of his 3-point attempts from the corners, the highest rate of his career. And he’s now shot 52-for-93 (55.9%) on corner 3s over the last four seasons (since returning from his Achilles injury).
kd_shots <- shots %>%
  filter(player_name == "Kevin Durant",
         str_detect(shot_zone_basic, "3|Backcourt")) %>%
  count(season, shot_location = ifelse(str_detect(shot_zone_basic, "Corner 3"), "corner", "other"), event_type, name = "fg")

kd_shots %>%
  summarise(fg_corner = sum(fg),
            .by = c(season, shot_location)) %>%
  mutate(fg_total = sum(fg_corner),
         .by = season) %>%
  filter(shot_location == "corner") %>%
  mutate(corner_rate = fg_corner / fg_total) %>%
  arrange(-corner_rate)

kd_shots %>%
  mutate(fga = sum(fg),
         .by = c(season, shot_location)) %>%
  filter(shot_location == "corner",
         event_type == "Made Shot") %>%
  arrange(season) %>%
  tail(4) %>%
  adorn_totals()

# Anthony Edwards ---------------------------------------------------------

# Is one of five players (all All-Stars) who’ve averaged at least 2.5 made 3-pointers and 5.5 made free throws per game. Both his free throw rate (33.6 attempts per 100 shots from the field) and his free throw percentage (84.0%) are career-best marks by healthy margins.
player_logs %>%
  filter(game_date < "2024-02-15",
         player_name == "Anthony Edwards") %>%
  summarise(across(c(fta, fga), sum),
            .by = c(season, player_id, player_name)) %>%
  mutate(ft_rate = fta / fga * 100) %>%
  arrange(-ft_rate)


# Joel Embiid -------------------------------------------------------------

# Opponents have shot 44.2% on shots he’s defended, with the expected field goal percentage on those shots being 50.6%. That’s the fourth-biggest differential among 180 players who’ve defended at least 200 shots.
players_defense <- nba_leaguedashptdefend(season = "2023-24",
                                          player_or_team = "Player",
                                          defense_category = "Overall") %>%
  pluck("LeagueDashPTDefend") %>%
  clean_names() %>%
  mutate(across(c(age:pct_plusminus), as.numeric))

players_defense %>%
  select(player_id = close_def_person_id, player_name, player_position, d_fgm, d_fga, d_fg_pct, normal_fg_pct, pct_plusminus) %>%
  filter(d_fga >= 200) %>%
  arrange(pct_plusminus)


# Paul George -------------------------------------------------------------

# Has played 1,169 minutes alongside Kawhi Leonard, 174 more than they played together last season (995).
lineups <- map_df(2023:2024,
                  function(x){
                    read_csv(glue::glue("https://github.com/ramirobentes/nba_pbp_data/raw/main/lineup-final{x}/data.csv")) %>%
                      mutate(season = glue::glue("{x - 1}-{str_sub(x, 3, 4)}"))
                  })

lineups %>%
  mutate(lineup_team = str_split(lineup_team, ", "),
         lineup_comb = map(lineup_team, ~ combn(., 2, simplify = FALSE))) %>%
  unnest_longer(lineup_comb) %>%
  mutate(lineup_comb = map(lineup_comb, sort)) %>%
  summarise(min_played = sum(secs_played) / 60,
            .by = c(season, lineup_comb)) %>%
  mutate(lineup_comb = map_chr(lineup_comb, ~ paste(sort(.x), collapse = ", "))) %>%
  filter(lineup_comb == "202331 Paul George, 202695 Kawhi Leonard")



# Shai Gilgeous-Alexander -------------------------------------------------

# Ranks third in cumulative plus-minus, with the Thunder having outscored their opponents by 402 points with him on the floor. They’ve been 10.7 points per 100 possessions better with him on the floor (plus-10.3) than they’ve been with him off the floor (minus-0.4).
lineups2024 %>%
  filter(team == "OKC") %>%
  mutate(sga_court = ifelse(str_detect(lineup_team, "Shai Gilgeous-Alexander"), "on", "off")) %>%
  summarise(across(c(poss_team:pts_opp), sum),
            .by = sga_court) %>%
  mutate(plus_minus = pts_team - pts_opp,
         pts100 = pts_team / poss_team * 100,
         opp100 = pts_opp / poss_opp * 100,
         net100 = pts100 - opp100)

# Leads the league in both steals (2.2) and deflections (3.6) per game.
players_hustle <- hoopR::nba_leaguehustlestatsplayer(season = "2023-24") %>%
  pluck("HustleStatsPlayer") %>%
  clean_names() %>%
  mutate(across(c(g:pct_box_outs_reb), as.numeric))

players_hustle %>%
  filter(g >= 25) %>%
  select(player_id, player_name, team_abbreviation, g, deflections) %>%
  mutate(deflections_game = deflections / g) %>%
  arrange(-deflections_game)  


# Tyrese Haliburton -------------------------------------------------------

# The Pacers have scored 124.5 points per 100 possessions with him on the floor. That’s the highest mark among 222 players who’ve averaged at least 20 minutes in 25 games or more.
lineups2024 %>%
  separate_rows(lineup_team, sep = ", ") %>%
  summarise(games = n_distinct(game_id),
            across(c(secs_played:pts_team), sum),
            .by = lineup_team) %>%
  rename(player_name = lineup_team) %>%
  mutate(pts100 = pts_team / poss_team * 100) %>%
  filter(secs_played / 60 / games >= 20,
         games >= 25) %>%
  arrange(-pts100)


# LeBron James ------------------------------------------------------------

# Has shot 29-for-50 (58.0%) with the score within five points in the last five minutes of the fourth quarter or overtime. That’s tied for the best mark among 29 players with at least 40 clutch field goal attempts.
pbp2024 %>%
  filter(game_date < as.Date("2024-02-15")) %>%
  filter(secs_game >= 2580,
         margin_before <= 5,
         msg_type %in% c(1, 2)) %>%
  count(player_name = player1_name, shot_result = ifelse(msg_type == 1, "made", "missed"), name = "fg") %>%
  mutate(fga = sum(fg),
         .by = player_name, shot_result) %>%
  filter(shot_result == "made",
         fga >= 40) %>%
  transmute(player_name, fgm = fg, fga, fg_pct = fg / fga) %>%
  arrange(-fg_pct)



# Nikola Jokic ------------------------------------------------------------

# True shooting percentage of 65.1% is down from a career-high 70.1% last season but is still the highest mark among 22 players who’ve averaged at least 24 points per game.
player_logs %>%
  summarise(games = n(),
            across(c(pts, fga, fta), sum),
            .by = c(season, player_id, player_name)) %>%
  mutate(ts_pct = pts / (2 * (fga + 0.44 * fta))) %>%
  filter(pts / games >= 24) %>%
  filter(player_name == "Nikola Jokic")

# Has averaged 10.7 elbow touches per game, most for any player in the last eight seasons (since Marc Gasol’s 11.6 in 2015-16).  
players_poss_multi <- map_df(year_to_season(2015:2023),
                             function(x){
                               nba_leaguedashptstats(season = x, 
                                                     player_or_team = "Player", 
                                                     pt_measure_type = "Possessions",
                                                     per_mode = "Totals") %>%
                                 pluck("LeagueDashPtStats") %>%
                                 mutate(season = x)
                             }) %>%
  clean_names() %>%
  mutate(across(c(gp:pts_per_paint_touch), as.numeric))

players_poss_multi %>%
  select(season, player_id, player_name, team_abbreviation, gp, elbow_touches) %>%
  mutate(et_game = elbow_touches / gp) %>%
  arrange(-et_game)



# Kawhi Leonard -----------------------------------------------------------

# The Clippers have allowed 9.3 fewer points per 100 possessions with him on the floor (110.1) than they have with him off the floor (119.4). That’s the second biggest on-off DefRtg differential among 240 players who’ve played at least 750 minutes for a single team.

team_players <- lineups2024 %>%
  separate_rows(lineup_team, sep = ", ") %>%
  summarise(all_players = str_split(paste(unique(lineup_team), collapse = ", "), ", "),
            .by = team)

players_on <- lineups2024 %>%
  separate_rows(lineup_team, sep = ", ") %>%
  summarise(across(c(secs_played, poss_opp, pts_opp), sum),
            .by = c(team, lineup_team)) %>%
  rename(player_name = lineup_team) %>%
  mutate(opp100 = pts_opp / poss_opp * 100)

players_off <- lineups2024 %>%
  left_join(team_players) %>%
  mutate(lineup_team = str_split(lineup_team, ", "),
         players_out = map2(lineup_team, all_players, ~ setdiff(.y, .x)),
         players_out = map_chr(players_out, ~ paste(., collapse = ", "))) %>%
  separate_rows(players_out, sep = ", ") %>%
  summarise(across(c(secs_played, poss_opp, pts_opp), sum),
            .by = c(team, players_out)) %>%
  rename(player_name = players_out) %>%
  mutate(opp100 = pts_opp / poss_opp * 100)

players_on %>%
  rename_at(vars(poss_opp:opp100), ~ paste0(., "_on")) %>%
  left_join(players_off %>% select(-secs_played) %>% rename_at(vars(poss_opp:opp100), ~ paste0(., "_off"))) %>%
  mutate(opp100_diff = opp100_on - opp100_off) %>%
  arrange(opp100_diff) %>%
  filter(secs_played >= 750 * 60)



# Damian Lillard ----------------------------------------------------------

# Has averaged just 24.6 points per game, down from 32.2 last season. That’s the third biggest drop among 302 players who’ve played at least 25 games in each of the last two seasons.
player_logs %>%
  filter(season %in% c("2022-23", "2023-24")) %>%
  summarise(games = n(),
            ppg = mean(pts),
            .by = c(season, player_id, player_name)) %>%
  filter(sum(games >= 25) == 2,
         .by = player_id) %>%
  select(-games) %>%
  pivot_wider(names_from = season,
              values_from = ppg,
              names_prefix = "ppg") %>%
  clean_names() %>%
  mutate(ppg_diff = ppg2023_24 - ppg2022_23) %>%
  arrange(ppg_diff)



# Tyrese Maxey ------------------------------------------------------------

# Has committed just 5.6 turnovers per 100 possessions used, the lowest rate among 60 players with a usage rate of 24% or higher.

players_usage <- nba_leaguedashplayerstats(season = "2023-24",
                                           measure_type = "Advanced") %>%
  pluck("LeagueDashPlayerStats") %>%
  clean_names() %>%
  mutate(across(c(w_pct:sp_work_net_rating_rank), as.numeric))

pbp2024 %>%
  filter((msg_type %in% c(1, 2, 5)) | (msg_type == 3 & str_detect(description, "1 of ") & !str_detect(description, "1 of 1")))  %>%
  select(game_id, event_num, msg_type, period, clock, team_abb, description, player1 = player1_name, player2 = player2_name) %>%
  pivot_longer(cols = c(player1, player2),
               names_to = "player_number",
               values_to = "player_name",
               names_prefix = "player") %>%
  filter(!is.na(player_name)) %>%
  mutate(play_type = case_when(msg_type %in% c(1, 2) & player_number == 1 ~ "fga",
                               msg_type == 1 & player_number == 2 ~ "assists",
                               msg_type == 5 ~ "turnover",
                               msg_type == 3 ~ "ft_trip")) %>%
  count(player_name, play_type) %>%
  pivot_wider(names_from = play_type,
              values_from = n,
              values_fill = 0) %>%
  mutate(tov_ratio = turnover / (assists + fga + ft_trip + turnover)) %>%
  arrange(-tov_ratio) %>%
  semi_join(player_stats %>%
              filter(games >= 25 & games * secs_played / 60 >= 20)) %>%
  semi_join(players_usage %>%
              filter(usg_pct >= 0.24) %>%
              transmute(player_name  = paste(player_id, player_name))) %>%
  arrange(tov_ratio)



# Donovan Mitchell --------------------------------------------------------

# The Cavs have outscored their opponents by 11.0 points per 100 possessions with him on the floor. That’s the second-highest mark among 170 players (highest among All-Stars) who’ve averaged at least 25 minutes in 25 games or more.
lineups2024 %>%
  separate_rows(lineup_team, sep = ", ") %>%
  summarise(games = n_distinct(game_id),
            across(c(secs_played:pts_opp), sum),
            .by = lineup_team) %>%
  rename(player_name = lineup_team) %>%
  mutate(pts100 = pts_team / poss_team * 100,
         opp100 = pts_opp / poss_opp * 100,
         net100 = pts100 - opp100) %>%
  filter(secs_played / 60 / games >= 25,
         games >= 25) %>%
  arrange(-net100)



# Julius Randle -----------------------------------------------------------

# One of three players – Antetokounmpo and Jokic are the others – who’ve averaged at least 20 points, nine rebounds and four assists in each of the last four seasons.
player_logs %>%
  filter(as.integer(season_id) >= 22020) %>%
  summarise(across(c(pts, reb, ast), mean),
            .by = c(season, player_id, player_name)) %>%
  filter(sum(pts >= 20 & reb >= 9 & ast >= 4) == 4,
         .by = c(player_id)) %>%
  count(player_id, player_name)

# Has taken 60% of his shots in the paint, up from 42% over the previous three seasons.
shots %>%
  filter(player_name == "Julius Randle",
         season %in% c("2020-21", "2021-22", "2022-23", "2023-24")) %>%
  mutate(season = ifelse(season == "2023-24", "2023-24", "previous3"),
         shot_location = ifelse(shot_zone_basic %in% c("Restricted Area", "In The Paint (Non-RA)"), "paint", "outside")) %>%
  count(season, shot_location, name = "fg") %>%
  mutate(fga = sum(fg),
         .by = season) %>%
  filter(shot_location == "paint") %>%
  transmute(season, shot_location, fg_paint = fg, fga, pct_fg_paint = fg / fga)



# Jayson Tatum ------------------------------------------------------------

# Only player with at least 60 dunks (64), 60 mid-range buckets (73-for-164 (44.5%)) and 60 3-pointers (159-for-438, 36.3%).
shots %>%
  filter(season == "2023-24",
         event_type == "Made Shot") %>%
  summarise(dunks = sum(str_detect(action_type, "Dunk")),
            midrange = sum(shot_zone_basic == "Mid-Range"),
            threes = sum(shot_type == "3PT Field Goal"),
            .by = c(player_id, player_name)) %>%
  filter(dunks >= 60,
         midrange >= 60,
         threes >= 60)



# Karl-Anthony Towns ------------------------------------------------------

#  One of four players – Durant, Leonard and Lauri Markkanen are the others – who’ve shot 55% or better on at least 300 2-point attempts and 40% or better on at least 200 3-point attempts.
player_logs %>%
  filter(season == "2023-24") %>%
  mutate(fg2m = fgm - fg3m,
         fg2a = fga - fg3a) %>%
  summarise(across(c(fg2m, fg2a, fg3m, fg3a), sum),
            .by = c(player_id, player_name)) %>%
  mutate(fg2pct = fg2m / fg2a,
         fg3pct = fg3m / fg3a) %>%
  filter(fg2a >= 300,
         fg3a >= 200,
         fg2pct >= 0.55,
         fg3pct >= 0.4)







# Trae Young --------------------------------------------------------------

# Ranks second with 10.9 assists per game and leads the league in total assists on dunks (140) for the sixth straight season.
pbps <- map_df(2023:2024,
               function(x){
                 read_rds(glue::glue("https://github.com/ramirobentes/nba_pbp_data/raw/main/pbp-final-{x}/data.rds")) %>%
                   mutate(season = glue::glue("{x - 1}-{str_sub(x, 3, 4)}"))
               })

pbps %>%
  filter(msg_type == 1,
         str_detect(description, "Dunk"),
         !is.na(player2_name)) %>%
  count(season, player_name = player2_name, sort = T, name = "dunk_assists") %>%
  filter(dunk_assists == max(dunk_assists),
         .by = season)
