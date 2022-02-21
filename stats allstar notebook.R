library(tidyverse)
library(hoopR)
library(janitor)
library(lubridate)
library(future)

# Article: https://www.nba.com/news/all-star-notebook-key-stats-on-every-2022-all-star

# Load main data ---------------------------------------------------------------

lineup_stats <- read_csv("https://github.com/ramirobentes/NBA-in-R/blob/master/lineup-stats/data.csv?raw=true")

shots_season <- nba_shotchartdetail(season = "2021-22",
                                    player_id = 0) %>%
  pluck("Shot_Chart_Detail") %>%
  clean_names()

pbp_final_gt <- read_csv("https://github.com/ramirobentes/NBA-in-R/releases/download/pbp-final-gt-9814b0d/data.csv",
                         col_types = c(clock = "c",
                                       start_poss = "c"))
# url above might change daily (copy latest on data.csv here https://github.com/ramirobentes/NBA-in-R/releases)

player_logs <- nba_leaguegamelog(season = "2021-22", player_or_team = "P") %>%
  pluck("LeagueGameLog") %>%
  clean_names() %>%
  mutate(across(c(min:fantasy_pts), as.numeric),
         game_date = as.Date(game_date),
         season = as.numeric(paste0("20", as.integer(str_sub(season_id, -2, -1)) + 1)))

other_seasons <- paste(2003:2020, str_pad(04:21, pad = 0, width = 2), sep = "-")
player_logs_others <- map_df(other_seasons, ~ nba_leaguegamelog(season = ., player_or_team = "P") %>%
                               pluck("LeagueGameLog")) %>%
  clean_names() %>%
  mutate(across(c(min:fantasy_pts), as.numeric),
         game_date = as.Date(game_date),
         season = as.numeric(paste0("20", as.integer(str_sub(season_id, -2, -1)) + 1)))

# finding number of games and minutes for players this season until date of article
player_stats <- lineup_stats %>%
  filter(game_date < "2022-02-17") %>%
  separate_rows(lineup, sep = ", ") %>%
  group_by(player_name = lineup) %>%
  summarise(games = n_distinct(game_id),
            min = sum(secs_played) / 60) %>%
  ungroup()

# Stats -------------------------------------------------------------------

# Jarrett Allen, C, Cleveland Cavaliers: ------------------------------

## 1. Ranks third in the league with 528 points in the restricted area. His 77.6% shooting in the restricted area ranks fourth among 55 players with at least 200 attempts.

shots_season %>%
  filter(ymd(game_date) < "2022-02-17") %>%
  filter(shot_zone_basic == "Restricted Area") %>%
  count(player_id, player_name, event_type) %>%
  group_by(player_id) %>%
  mutate(fga = sum(n)) %>%
  ungroup() %>%
  filter(event_type == "Made Shot") %>%
  transmute(player_name, fgm = n, fga, fg_pct = n / fga, pts_ra = n * 2) %>%
  filter(fga >= 200) %>%
  arrange(-fgm)  # to sort by fg_pct, change here

 
## 2. Opponents have shot 51.3% at the rim when he’s been there to protect it. That’s the third-best rim protection mark among 21 players who’ve defended at least 250 shots at the rim.

team_games <- lineup_stats %>%
  filter(game_date < "2022-02-17",
         slug_team == "CLE") %>%  # remove this line to get for every game. Getting the box scores will take a while though
  distinct(game_id)

track_box_fun <- function(x){
  hoopR::nba_boxscoreplayertrackv2(game_id = x) %>%
    pluck("PlayerStats")
}

track_box <- map_df(team_games$game_id, track_box_fun)

track_box %>%
  clean_names() %>%
  mutate(across(c(spd:dfg_pct), as.numeric)) %>%
  group_by(player_name) %>%
  summarise(across(c(dfgm, dfga), sum)) %>%
  ungroup() %>%
  mutate(dfg_pct = dfgm / dfga) %>%
  filter(dfga >= 250) %>%
  arrange(dfg_pct)

## 3. Has a free throw rate of 43.5 attempts per 100 shots from the field. That ranks 18th among 280 players with at least 200 field goal attempts, but is down from 60.2 per 100 last season. That’s the second-biggest drop among 231 players with at least 200 field goal attempts in each of the last two seasons.

### from pbp
pbp_final_gt %>%
  filter(game_date < "2022-02-17") %>%
  filter(msg_type %in% c(1:3)) %>%
  mutate(type_shot = ifelse(msg_type %in% c(1, 2), "fga", "fta")) %>%
  count(player_name = player1, type_shot) %>%
  pivot_wider(names_from = type_shot,
              values_from = n,
              values_fill = 0) %>%
  mutate(ft_100fg = fta / fga * 100) %>%
  filter(fga >= 200) %>%
  arrange(-ft_100fg)
  
### from logs
player_logs %>%
  filter(game_date < "2022-02-17") %>%
  group_by(player_name) %>%
  summarise(across(c(fga, fta), sum)) %>%
  ungroup() %>%
  mutate(ft_100fg = fta / fga * 100) %>%
  filter(fga >= 200) %>%
  arrange(-ft_100fg)

# Giannis Antetokounmpo, F, Milwaukee Bucks ------------------------------

## 1. Leads the league in scoring at 29.4 points per game. Also leads the league in points scored per 36 minutes (32.5).

### from pbp (minutes are more accurate)
pbp_final_gt %>%
  filter(game_date < "2022-02-17") %>%
  filter(shot_pts > 0) %>%
  group_by(player_name = player1) %>%
  summarise(pts = sum(shot_pts)) %>%
  ungroup() %>%
  left_join(player_stats) %>%
  mutate(pts_pg = pts / games,
         pts_36min = pts / min * 36) %>%
  filter(min >= 500) %>%
  arrange(-pts_36min)
  

### from logs
player_logs %>%
  filter(game_date < "2022-02-17") %>%
  group_by(player_name) %>%
  summarise(games = n_distinct(game_id),
            across(c(pts, min), sum)) %>%
  ungroup() %>%
  mutate(pts_pg = pts / games,
         pts_36min = pts / min * 36) %>%
  filter(min >= 500) %>%
  arrange(-pts_36min)

## 2. Has assisted on 3.9 3-pointers per game, most in the league.

pbp_final_gt %>%
  filter(game_date < "2022-02-17") %>%
  filter(msg_type == 1,
         desc_value == 3,
         !is.na(player2)) %>%
  count(player_name = player2, name = "assist_3s") %>%
  left_join(player_stats) %>%
  mutate(assist_3s_pg = assist_3s / games) %>%
  arrange(-assist_3s_pg)


## 3. Has drawn 8.6 fouls per game, which would be the highest average in the last six seasons. Has accounted for 65.9% of the Bucks’ free throw attempts while he’s been on the floor, the highest rate among 334 players who’ve played at least 500 minutes.

### fouls drawn per game
pbp_final_gt %>%
  filter(game_date < "2022-02-17") %>%
  filter(msg_type == 6,
         !str_detect(description, "Technical"), 
         !is.na(player3)) %>%
  count(player_name = player3, name = "fouls_drawn", sort = T) %>%
  left_join(player_stats) %>%
  mutate(fouls_drawn_pg = fouls_drawn / games) %>%
  arrange(-fouls_drawn_pg)

### % of Bucks fta while on the floor
pbp_final_gt %>%
  filter(game_date < "2022-02-17") %>%
  filter(msg_type == 3) %>%
  count(player_name = player1, slug_team, name = "fta") %>%
  left_join(pbp_final_gt %>%
              filter(game_date < "2022-02-17") %>%
              filter(msg_type == 6) %>%
              mutate(lineup_team = ifelse(slug_team == team_home, lineup_away, lineup_home)) %>%
              separate_rows(lineup_team, sep = ", ") %>%
              group_by(player_name = lineup_team, slug_team = ifelse(slug_team == team_home, team_away, team_home)) %>%
              summarise(team_fta_player_floor = sum(total_fta, na.rm = TRUE)) %>%
              ungroup()) %>%
  mutate(fta_player_pct = fta / team_fta_player_floor) %>%
  filter(fta >= 50) %>%
  arrange(-fta_player_pct)



# LaMelo Ball, G, Charlotte Hornets ------------------------------

## 1. One of four players (all All-Stars) averaging at least 20 points, seven rebounds and seven assists per game.

player_logs %>%
  filter(game_date < "2022-02-17") %>%
  group_by(player_id, player_name) %>%
  summarise(across(c(pts, ast, reb), mean)) %>%
  ungroup() %>%
  filter(pts >= 20,
         reb >= 7,
         ast >= 7)

## 3. The Hornets have averaged 103.6 possessions per 48 minutes with him on the floor. That’s the highest on-court mark for pace among 224 players who’ve averaged at least 20 minutes per game.
lineup_stats %>%
  filter(game_date < "2022-02-17") %>%
  separate_rows(lineup, sep = ", ") %>%
  group_by(player_name = lineup, slug_team) %>%
  summarise(games = n_distinct(game_id),
            across(c(poss_team, secs_played), sum)) %>%
  ungroup() %>%
  filter(secs_played / 60 / games >= 20,
         games >= 30) %>%
  mutate(poss_48min = poss_team / (secs_played / 60) * 48) %>%
  arrange(-poss_48min)
# small decimal discrepancies due to inconsistencies in official possession count on nba.com


# Devin Booker, G, Phoenix Suns ------------------------------

## 1. One of three players – Antetokounmpo and LeBron James are the others – who’ve averaged at least 25 points per game in each of the last four seasons.

bind_rows(player_logs, player_logs_others) %>%
  filter(season >= 2019) %>%
  group_by(player_id, player_name, season) %>%
  summarise(pts_total = sum(pts),
            games = n_distinct(game_id),
            pts_pg = mean(pts)) %>%
  ungroup() %>%
  filter(pts_pg >= 25) %>%
  add_count(player_id) %>%
  filter(n == 4) 

## 2. Has scored 0.461 points per touch, the third highest rate among 286 players with at least 1,000 total touches.

team_games <- lineup_stats %>%
  filter(game_date < "2022-02-17",
         slug_team == "PHX") %>%  # remove this line to get for every game. Getting the box scores will take a while though
  distinct(game_id)

track_box_fun <- function(x){
  hoopR::nba_boxscoreplayertrackv2(game_id = x) %>%
    pluck("PlayerStats")
}

track_box <- map_df(team_games$game_id, track_box_fun)

player_logs %>%
  filter(game_date < "2022-02-17") %>%
  group_by(player_name) %>%
  summarise(total_pts = sum(pts)) %>%
  ungroup() %>%
  left_join(track_box %>%
              clean_names() %>%
              mutate(across(c(spd:dfg_pct), as.numeric)) %>%
              group_by(player_name) %>%
              summarise(total_touches = sum(tchs)) %>%
              ungroup()) %>%
  filter(total_touches >= 1000) %>%
  mutate(pts_touch = total_pts / total_touches) %>%
  arrange(-pts_touch)

## 3. Has shot 22-for-35 (62.9%) on clutch shots, the third best mark among 95 players who’ve attempted at least 20.

pbp_final_gt %>%
  filter(game_date < "2022-02-17") %>%
  filter(margin_before <= 5 & secs_passed_game >= 2580) %>%
  filter(msg_type %in% c(1, 2)) %>%
  count(shot_result = ifelse(msg_type == 1, "made", "missed"), player_name = player1) %>%
  group_by(player_name) %>%
  mutate(fga = sum(n)) %>%
  ungroup() %>%
  filter(shot_result == "made") %>%
  transmute(player_name, fgm = n, fga, fg_pct = n / fga) %>%
  filter(fga >= 20) %>%
  arrange(-fg_pct)


# Jimmy Butler, F, Miami Heat ------------------------------

## 1. Has a free throw rate of 54.4 attempts per 100 shots from the field, the fourth highest rate among 280 players (highest among non-bigs) with at least 200 field goal attempts.

### see Jarrett Allen's stat #3

## 2. Has an effective field goal percentage of 35.4% on shots from outside the paint, the third worst mark among 257 players with at least 100 field goal attempts from the outside.

shots_season %>%
  filter(ymd(game_date) < "2022-02-17") %>%
  filter(!shot_zone_basic %in% c("In The Paint (Non-RA)", "Restricted Area")) %>%
  count(player_name, shot_type, event_type) %>%
  group_by(player_name) %>%
  mutate(total_fga = sum(n),
         total_fgm = sum(n[which(event_type == "Made Shot")])) %>%
  ungroup() %>%
  pivot_wider(names_from = c(shot_type, event_type),
              values_from = n,
              values_fill = 0) %>%
  clean_names() %>%
  mutate(eff_fg_pct = (total_fgm + (x3pt_field_goal_made_shot * 0.5)) / total_fga) %>%
  select(player_name, total_fgm, total_fga, total_3fg_made = x3pt_field_goal_made_shot, eff_fg_pct) %>%
  filter(total_fga >= 100) %>%
  arrange(eff_fg_pct)
  
## 3. One of eight players (Dejounte Murray is another) who’ve played at least 500 minutes and have more steals (69) than personal fouls (57). Has done so in each of his last six (and eight of his last nine) seasons.

### from logs
player_logs %>%
  filter(game_date < "2022-02-17") %>%
  group_by(player_id, player_name) %>%
  summarise(across(c(stl, pf, min), sum)) %>%
  ungroup() %>%
  filter(stl > pf,
         min >= 500) %>%
  arrange(-stl / pf)

# Stephen Curry, G, Golden State Warriors ------------------------------

## 1. Leads the league (by wide margins) in 3-point makes (251) and 3-point attempts (663). Has made a 3-pointer in a record 179 straight games.

### from pbp
pbp_final_gt %>%
  filter(game_date < "2022-02-17") %>%
  filter(msg_type %in% c(1, 2),
         desc_value == 3) %>%
  count(shot_result = ifelse(msg_type == 1, "made", "missed"), player_name = player1) %>%
  pivot_wider(names_from = shot_result,
              values_from = n,
              names_prefix = "fg3_") %>%
  transmute(player_name, fg3_made, fg3_att = fg3_made + fg3_missed) %>%
  arrange(-fg3_made)

### from logs
player_logs %>%
  group_by(player_name) %>%
  summarise(across(c(fg3m, fg3a), sum)) %>%
  ungroup() %>%
  arrange(-fg3m)


## 2. His 37.9% from 3-point range is the lowest mark of his career (not including the season in which he played only five games), and he’s seen a slightly bigger drop from last season in catch-and-shoot 3-point percentage (from 43.7% to 38.5%) than he has in pull-up 3-point percentage (40.9% to 37.5%). He’s shot worse from 3-point range (37.2%) and has been assisted on a far lower percentage of his 3s (50.4%) with Draymond Green off the floor than he has with Green on the floor (38.8%, 66.1%), though that 38.8% with Green on the floor would still be a career-low mark. Curry’s field goal percentage in the paint (53.8%) is also his lowest mark in the last eight seasons.

### assists on 3s with Draymond on/off the floor
pbp_final_gt %>%
  filter(game_date < "2022-02-17") %>%
  filter(msg_type == 1,
         desc_value == 3,
         player1 == "Stephen Curry") %>%
  mutate(lineup_team = ifelse(slug_team == team_home, lineup_home, lineup_away)) %>%
  count(draymond = ifelse(str_detect(lineup_team, "Draymond Green"), "on_floor", "off_floor"),
        assisted = !is.na(player2)) %>%
  group_by(draymond) %>%
  mutate(total_fg3m = sum(n)) %>%
  ungroup() %>%
  filter(assisted) %>%
  transmute(draymond, assisted_fg3m = n, total_fg3m, pct_assisted_fg3m = n / total_fg3m)


## 3. Leads the league (by a healthy margin) in cumulative plus-minus, with the Warriors having outscored their opponents by 474 points with him on the floor. The Warriors have been 15.0 points per 100 possessions better with him on the floor (+11.9) than they’ve been with him off the floor (-3.1). That’s the third biggest on-off differential among 330 players who’ve played at least 500 minutes for a single team.

### cumulative plus-minus
lineup_stats %>%
  filter(game_date < "2022-02-17") %>%
  separate_rows(lineup, sep = ", ") %>%
  group_by(player_name = lineup, slug_team) %>%
  summarise(games = n_distinct(game_id), 
            across(c(pts_team, pts_opp), sum)) %>%
  ungroup() %>%
  mutate(plus_minus = pts_team - pts_opp) %>%
  arrange(-plus_minus)

### on/off differential
with_players <- lineup_stats %>%
  filter(game_date < "2022-02-17") %>%
  separate_rows(lineup, sep = ", ") %>%
  group_by(player_name = lineup, slug_team) %>%
  summarise(across(c(pts_team:secs_played), sum)) %>%
  ungroup() %>%
  mutate(pts100 = pts_team /poss_team * 100,
         opp100 = pts_opp / poss_opp * 100,
         net100 = pts100 - opp100)

without_players <- lineup_stats %>%
  left_join(lineup_stats %>%
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
  filter(secs_played >= 60 * 500) %>%
  mutate(diff_net100 = net100 - net100_out) %>%
  arrange(-diff_net100)
# small decimal discrepancies due to inconsistencies in official possession count on nba.com

# DeMar DeRozan, G/F, Chicago Bulls ------------------------------

## 2. His 516 mid-range field goal attempts are 172 more than any other player has attempted this season and already more than any player attempted in either of the previous two seasons. His 50.2% shooting from mid-range ranks eighth among 62 players with at least 100 attempts.

shots_season %>%
  filter(ymd(game_date) < "2022-02-17") %>%
  filter(shot_zone_basic == "Mid-Range") %>%
  count(player_name, event_type) %>%
  group_by(player_name) %>%
  mutate(fga = sum(n)) %>%
  ungroup() %>%
  filter(event_type == "Made Shot",
         fga >= 100) %>%
  transmute(player_name, fgm = n, fga, fg_pct = fgm / fga) %>%
  arrange(-fga)  # change here to sort by fg_pct

## 3. Ranks second with 114 points scored in the clutch. Only three of his 61 clutch field goal attempts have come from 3-point range (38 have come from mid-range), with his two 3-point makes being buzzer-beating game-winners on consecutive days. For his career, he’s 19-for-107 (18%) on clutch 3-pointers (including playoffs), the worst mark among 209 players who’ve attempted at least 100 in the 26 years for which we have clutch data.

### total points clutch:

pbp_final_gt %>%
  filter(game_date < "2022-02-17") %>%
  filter(margin_before <= 5 & secs_passed_game >= 2580) %>%
  filter(msg_type %in% c(1, 3),
         shot_pts > 0) %>%
  group_by(player_name = player1) %>%
  summarise(total_pts_clutch = sum(shot_pts)) %>%
  ungroup() %>%
  arrange(-total_pts_clutch)

### clutch shots by zone
pbp_final_gt %>%
  filter(game_date < "2022-02-17") %>%
  filter(margin_before <= 5 & secs_passed_game >= 2580) %>%
  filter(msg_type %in% c(1, 2)) %>%
  left_join(shots_season %>%
              transmute(game_id = as.numeric(game_id), number_original = as.numeric(game_event_id), 
                        shot_zone_basic, event_type, shot_type)) %>%
  count(player_name = player1, event_type, shot_zone_basic, shot_type) %>%
  filter(player_name == "DeMar DeRozan") %>%
  arrange(shot_type, shot_zone_basic)

# Luka Doncic, G, Dallas Mavericks ----------------------------------------

## 3. Has been assisted on just 15.1% of his buckets, the lowest rate among 264 players with at least 100 total field goals (though up from just 13.6% last season).
pbp_final_gt %>%
  filter(game_date < "2022-02-17") %>%
  filter(msg_type == 1) %>%
  count(player_name = player1, assisted = !is.na(player2)) %>%
  group_by(player_name) %>%
  mutate(total_fgm = sum(n)) %>%
  ungroup() %>%
  filter(assisted,
         total_fgm >= 100) %>%
  transmute(player_name, assisted_fgm = n, total_fgm, assisted_pct = n / total_fgm) %>%
  arrange(assisted_pct)


# Kevin Durant, F, Brooklyn Nets ------------------------------------------

## 1. True shooting percentage of 62.6% ranks fifth among 108 players with at least 500 field goal attempts, but is his lowest mark in the last 10 seasons.

player_logs %>%
  filter(game_date < "2022-02-17") %>%
  group_by(player_id, player_name) %>%
  summarise(across(c(pts, fga, fta), sum, .names = "total_{col}")) %>%
  ungroup() %>%
  mutate(ts_att = total_fga + (0.44 * total_fta),
         ts_pct = total_pts / (2 * ts_att)) %>%
  filter(total_fga >= 500) %>%
  arrange(-ts_pct)

## 3. The league’s leading first-quarter scorer at 9.0 points per game. Only Ja Morant (9.2 in the third) has averaged more points in a single quarter.

pbp_final_gt %>%
  filter(game_date < "2022-02-17") %>%
  filter(shot_pts > 0) %>%
  group_by(period, player_name = player1) %>%
  summarise(pts_quarter = sum(shot_pts)) %>%
  ungroup() %>%
  left_join(lineup_stats %>%
              filter(game_date < "2022-02-17") %>%
              separate_rows(lineup, sep = ", ") %>%
              distinct(player_name = lineup, game_id, period) %>%
              count(player_name, period, name = "games")) %>%
  mutate(pts_quarter_pg = pts_quarter / games) %>%
  filter(period <= 4) %>%
  arrange(-pts_quarter_pg)


# Joel Embiid, C, Philadelphia 76ers --------------------------------------

## 1. Leads the league in usage rate at 37.6%, the fourth highest mark in the 26 seasons for which we have play-by-play data. His 3.4 minutes per game of possession ranks just 69th, but is the highest average of his career (and up from 2.9 minutes last season).

### usage
pbp_final_gt %>%
  filter(game_date < "2022-02-17") %>%
  filter(msg_type %in% c(1, 2, 5) | (msg_type == 6 & total_fta %in% c(2, 3))) %>%
  count(player_name = ifelse(msg_type %in% c(1, 2, 5), player1, player3), name = "used_player") %>%
  left_join(pbp_final_gt %>%
              filter(game_date < "2022-02-17") %>%
              filter(msg_type %in% c(1, 2, 5)) %>%
              mutate(lineup_team = ifelse(slug_team == team_home, lineup_home, lineup_away)) %>%
              separate_rows(lineup_team, sep = ", ") %>%
              count(player_name = lineup_team, name = "team_fg_tov")) %>%
  left_join(pbp_final_gt %>%
              filter(game_date < "2022-02-17") %>%
              filter(msg_type == 6 & total_fta %in% c(2, 3)) %>%
              mutate(lineup_team = ifelse(slug_team == team_away, lineup_home, lineup_away)) %>%
              separate_rows(lineup_team, sep = ", ") %>%
              count(player_name = lineup_team, name = "team_ft_trips")) %>%
  mutate(across(c(team_fg_tov, team_ft_trips), ~ coalesce(., 0)),
         usage = used_player / (team_fg_tov + team_ft_trips)) %>%
  arrange(-usage) %>%
  left_join(player_stats) %>%
  filter(min >= 100)
# small decimal discrepancies due to inconsistencies in official possession count on nba.com

## 3. Leads the league with 120 points scored in the clutch. His 50% shooting in the clutch ranks sixth among 41 players with at least 35 clutch field goal attempts.

### see DeMar DeRozan's stat #3

# Darius Garland, G, Cleveland Cavaliers ----------------------------------

## 1. Has seen big jumps in both usage rate (from 20.1% to 26.1%) and true shooting percentage (from 49.8% to 58.4%) from his rookie season to this (his third) season. His true shooting percentage jump from last season (54.7%) is the fourth biggest among 86 players with at least 500 field goal attempts in each of the last two seasons. Fred VanVleet and Ja Morant have seen the second and third biggest jumps, respectively.

bind_rows(player_logs_others, player_logs) %>%
  filter(season >= 2021) %>%
  filter(game_date < "2022-02-17") %>%
  group_by(season, player_id, player_name) %>%
  summarise(across(c(pts, fga, fta), sum, .names = "total_{col}")) %>%
  ungroup() %>%
  mutate(ts_att = total_fga + (0.44 * total_fta),
         ts_pct = total_pts / (2 * ts_att)) %>%
  filter(total_fga >= 500) %>%
  add_count(player_id, player_name) %>%
  filter(n == 2) %>%
  select(season, player_name, ts_pct) %>%
  pivot_wider(names_from = season,
              values_from = ts_pct) %>%
  clean_names() %>%
  mutate(ts_pct_jump = x2022 - x2021) %>%
  arrange(-ts_pct_jump)


## 2. One of four players (all All-Stars) who have shot 50% or better on at least 100 non-restricted area shots in the paint and 50% or better on at least 100 mid-range shots.

shots_season %>%
  filter(ymd(game_date) < "2022-02-17") %>%
  count(player_name, shot_zone_basic, event_type) %>%
  group_by(player_name, shot_zone_basic) %>%
  mutate(fga = sum(n)) %>%
  ungroup() %>%
  filter(event_type == "Made Shot",
         shot_zone_basic %in% c("In The Paint (Non-RA)", "Mid-Range"),
         fga >= 100) %>%
  transmute(player_name, shot_zone_basic, fgm = n, fga, fg_pct = n / fga) %>%
  add_count(player_name, over_50 = fg_pct >= 0.5) %>%
  filter(over_50,
         n == 2) %>%
  select(-c(n, over_50))

## 3. Has shot 1-for-19 (5.3%) on clutch 3-pointers, the worst mark among 42 players who’ve attempted at least 15.

pbp_final_gt %>%
  filter(game_date < "2022-02-17") %>%
  filter(margin_before <= 5 & secs_passed_game >= 2580) %>%
  filter(msg_type %in% c(1, 2),
         desc_value == 3) %>%
  count(shot_result = ifelse(msg_type == 1, "made", "missed"), player_name = player1) %>%
  group_by(player_name) %>%
  mutate(fga = sum(n)) %>%
  ungroup() %>%
  filter(shot_result == "made") %>%
  transmute(player_name, fgm = n, fga, fg_pct = n / fga) %>%
  filter(fga >= 15) %>%
  arrange(fg_pct)


# Rudy Gobert, C, Utah Jazz -----------------------------------------------

## 1. Leads the league in both total dunks (155) and dunks per game (3.5).

pbp_final_gt %>%
  filter(msg_type == 1,
         str_detect(description, "Dunk")) %>%
  count(player_name = player1, sort = T, name = "dunks") %>%
  left_join(player_stats) %>%
  mutate(dunks_pg = dunks / games)

## 2. Opponents have shot just 50.0% at the rim when he’s been there to protect it. That’s the best rim protection mark among 41 players who’ve defended at least 200 shots at the rim.

### see Jarrett Allen's stat #2

## 3. Has grabbed 21.9% of available rebounds while he’s been on the floor, the highest rate among 224 players who’ve averaged at least 20 minutes per game. Has six games of 20 or more rebounds, most in the league. Leads the league with 4.4 second chance points per game.

### rebound percentage:

pbp_final_gt %>%
  filter(msg_type == 4) %>%
  filter(!is.na(player1)) %>%
  count(player_name = player1, name = "total_reb", sort = T) %>%
  left_join(pbp_final_gt %>%
              filter(msg_type %in% c(2, 3),
                     !(msg_type == 3 & act_type %in% c(11, 13, 14, 16, 18:22, 25:26, 27:29)),
                     shot_pts == 0) %>%
              mutate(lineup = paste(lineup_home, lineup_away, sep = ", ")) %>%
              separate_rows(lineup, sep = ", ") %>%
              count(player_name = lineup, name = "reb_chances")) %>%
  mutate(reb_pct = total_reb / reb_chances) %>%
  arrange(-reb_pct) %>%
  left_join(player_stats) %>%
  filter(games >= 30,
         min >= games * 20)

### second chance pts

pbp_final_gt %>%
  group_by(game_id, period, slug_team, start_poss) %>%
  filter(sum(msg_type == 4 & desc_value == 1 & act_type == 0) > 0) %>%
  mutate(min_off_reb = min(number_event[which(msg_type == 4 & desc_value == 1 & act_type == 0)])) %>%
  ungroup() %>%
  filter(number_event > min_off_reb,
         shot_pts > 0) %>%
  group_by(player_name = player1) %>%
  summarise(pts_2chance = sum(shot_pts)) %>%
  ungroup() %>%
  left_join(player_stats) %>%
  mutate(pts_2chance_pg = pts_2chance / games) %>%
  arrange(-pts_2chance_pg) %>%
  filter(games >= 30)


# Draymond Green, F, Golden State Warriors --------------------------------

# 1. Only player averaging at least seven rebounds, seven assists, one steal and one block per game.

# 2. Has an effective field goal percentage of 56.7%, up from 49.2% last season. That’s the seventh biggest jump among 231 players with at least 200 field goal attempts in each of the last two seasons.

bind_rows(player_logs, player_logs_others) %>%
  filter(season >= 2021) %>%
  filter(game_date < "2022-02-17") %>%
  group_by(player_id, player_name, season) %>%
  summarise(across(c(fgm, fga, fg3m, fg3a), ~ sum(., na.rm = TRUE))) %>%
  ungroup() %>%
  mutate(eff_fg_pct = (fgm + (fg3m * 0.5)) / fga) %>%
  filter(fga >= 200) %>%
  add_count(player_id) %>%
  filter(n == 2) %>%
  select(-c(fgm:fg3a)) %>%
  pivot_wider(names_from = season,
              values_from = eff_fg_pct) %>%
  clean_names() %>%
  mutate(difference = x2022 - x2021) %>%
  arrange(-difference)
  

# 3. Has scored just 0.110 points per touch, the third lowest rate among 286 players with at least 1,000 total touches. Has recorded assists on 42.4% of his possessions, the highest rate among 289 players who’ve averaged at least 15 minutes per game.

pbp_final_gt %>%
  filter(game_date < "2022-02-17") %>%
  filter(msg_type == 1,
         !is.na(player2)) %>%
  count(player_name = player2, name = "total_ast") %>%
  left_join(pbp_final_gt %>%
              filter(game_date < "2022-02-17") %>%
              filter((msg_type %in% c(1, 2, 5)) | (msg_type == 3 & act_type %in% c(11, 13)),
                     !is.na(player1)) %>%
              count(player_name = player1, name = "fg_tov_ft")) %>%
  mutate(ast_rate = total_ast / (fg_tov_ft + total_ast)) %>%
  arrange(-ast_rate) %>%
  left_join(player_stats) %>%
  filter(games >= 30,
         min >= games * 15)


# James Harden, G, Philadelphia 76ers -------------------------------------

## 2. His 54.4% shooting in the restricted area is the worst mark among 55 players with at least 200 restricted-area attempts and his worst mark since his rookie season.

shots_season %>%
  filter(ymd(game_date) < "2022-02-17") %>%
  filter(shot_zone_basic == "Restricted Area") %>%
  count(player_name, event_type) %>%
  group_by(player_name) %>%
  mutate(fga = sum(n)) %>%
  ungroup() %>%
  filter(event_type == "Made Shot") %>%
  transmute(player_name, fgm = n, fga, fg_pct = n / fga) %>%
  filter(fga >= 200) %>%
  arrange(fg_pct)
  

## 3. Has traveled at an average of 3.60 miles per hour, the slowest rate among 333 players who’ve played at least 500 minutes.

team_games <- lineup_stats %>%
  filter(game_date < "2022-02-17",
         str_detect(lineup, "James Harden")) %>%  # remove this line to get for every game. Getting the box scores will take a while though
  distinct(game_id)

track_box_fun <- function(x){
  hoopR::nba_boxscoreplayertrackv2(game_id = x) %>%
    pluck("PlayerStats")
}

track_box <- map_df(team_games$game_id, track_box_fun)

track_box %>%
  clean_names() %>%
  mutate(across(c(spd:dfg_pct), as.numeric)) %>%
  group_by(player_name) %>%
  summarise(avg_spd = mean(spd)) %>%
  ungroup() %>%
  filter(player_name == "James Harden")


# LeBron James, F, Los Angeles Lakers -------------------------------------

## 1. Ranks third in scoring at 29.1 points per game. His 28.4 points per 36 minutes is the highest mark of his career.

bind_rows(player_logs, player_logs_others) %>%
  filter(game_date < "2022-02-17") %>%
  group_by(season, player_id, player_name, slug_team = team_abbreviation) %>%
  summarise(pts_total = sum(pts, na.rm = TRUE),
            min_total = sum(min, na.rm = TRUE),
            games = n_distinct(game_id)) %>%
  ungroup() %>%
  filter(player_name == "LeBron James") %>%
  mutate(pts_36min = pts_total / min_total * 36) %>%
  arrange(-pts_36min)

## 3. Leads the league with 4.8 fast break points per game. His 76.8% shooting in the restricted area ranks seventh among 55 players with at least 200 restricted-area attempts and is his best mark in the last eight seasons.

### see James Harden's stat #2

# Nikola Jokic, C, Denver Nuggets -----------------------------------------

## 1. Leads the league in both double-doubles (46) and triple-doubles (15). Though his minutes have dropped, he’s seen the league’s fourth biggest jump in rebounds per game (from 10.8 to 13.8) among 323 players who’ve played at least 25 games in each of the last two seasons.

player_logs %>%
  filter(game_date < "2022-02-17") %>%
  select(game_id, player_id, player_name, pts, ast, reb, stl, blk) %>%
  pivot_longer(cols = c(pts:blk),
               names_to = "category",
               values_to = "value") %>%
  count(game_id, player_name, double = value >= 10, name = "number_categories") %>%
  filter(double,
         number_categories > 1) %>%
  count(player_name, type = paste0("doubles_", number_categories)) %>%
  pivot_wider(names_from = type,
              values_from = n,
              values_fill = 0) %>%
  mutate(doubles_2 = doubles_2 + doubles_3) %>%
  arrange(-doubles_3)

## 3. The Nuggets have been 20.7 points per 100 possessions better with him on the floor (+10.1) than they’ve been with him off the floor (-10.6). That’s the biggest on-off differential among 330 players who’ve played at least 500 minutes for a single team

### see Stephen Curry's stat #3



# Zach LaVine, G/F, Chicago Bulls -----------------------------------------

selected_player_id <- player_logs %>%
  filter(player_name == "Zach LaVine") %>%
  distinct(player_id) %>%
  pull(player_id)

shooting_dash <- hoopR::nba_playerdashptshots(season = "2021-22", player_id = selected_player_id,
                                              date_to = "2022-02-17")

## 2. Has shot 59-for-121 (48.8%) on catch-and-shoot 3s, the best mark among 175 players who’ve attempted at least 100. Is 29-for-51 (56.9%) on corner 3-pointers, the best mark among 114 players who’ve attempted at least 50.

shooting_dash %>%
  pluck("GeneralShooting") %>%
  clean_names() %>%
  mutate(across(c(fga_frequency:fg3_pct), as.numeric)) %>%
  select(shot_type, fg3m, fg3a, fg3_pct)

## 3. Has an effective field goal percentage of 58.2% in the last four seconds of the shot clock, the best mark among 89 players with at least 50 field goal attempts late in the clock.

shooting_dash %>%
  pluck("ShotClockShooting") %>%
  clean_names() %>%
  mutate(across(c(fga_frequency:fg3_pct), as.numeric)) %>%
  select(shot_clock_range, fgm, fga, fg3m, fg3a) %>%
  mutate(eff_fg_pct = (fgm + (fg3m * 0.5)) / fga)



# Khris Middleton, F, Milwaukee Bucks -------------------------------------

## 1. Only player who’s shot 88% or better on at least 200 free throw attempts in each of the last three seasons.

bind_rows(player_logs, player_logs_others) %>%
  filter(season >= 2020) %>%
  filter(game_date < "2022-02-17") %>%
  group_by(season, player_id, player_name) %>%
  summarise(across(c(ftm, fta), sum)) %>%
  ungroup() %>%
  mutate(ft_pct = ftm / fta) %>%
  filter(fta >= 200,
         ft_pct >= 0.88) %>%
  add_count(player_id) %>%
  filter(n == 3) %>%
  select(-n)

## 2. Has taken 41.9% of his shots from 3-point range, the highest rate of his career. His free throw rate (30.0 attempts per 100 shots from the field is also the highest rate of his career.

bind_rows(player_logs, player_logs_others) %>%
  filter(game_date < "2022-02-17") %>%
  filter(player_name == "Khris Middleton") %>%
  group_by(player_name, season) %>%
  summarise(across(c(fga, fg3a), sum)) %>%
  ungroup() %>%
  mutate(pct_fg_3 = fg3a / fga) %>%
  arrange(-pct_fg_3)


# Donovan Mitchell, G, Utah Jazz ------------------------------------------

## 2. The Jazz have scored 118.7 points per 100 possessions with Mitchell on the floor. That’s the highest on-court mark among 336 players who’ve averaged at least 10 minutes per game.

lineup_stats %>%
  filter(game_date < "2022-02-17") %>%
  separate_rows(lineup, sep = ", ") %>%
  group_by(player_name = lineup, slug_team) %>%
  summarise(games = n_distinct(game_id),
            across(c(pts_team, poss_team, secs_played), sum)) %>%
  ungroup() %>%
  mutate(pts100 = pts_team / poss_team) %>%
  arrange(-pts100) %>%
  filter(games >= 30,
         secs_played >= games * 60 * 10)
# small decimal discrepancies due to inconsistencies in official possession count on nba.com

## 3. Registering career-high marks for both effective field goal percentage (53.8%) and true shooting percentage (57.8%). The latter mark ranks sixth among 29 guards with a usage rate of 25% or higher and at least 500 total field goal attempts.

bind_rows(player_logs, player_logs_others) %>%
  filter(game_date < "2022-02-17") %>%
  filter(player_name == "Donovan Mitchell") %>%
  group_by(season, player_name) %>%
  summarise(across(c(pts, fgm, fga, fg3m, fta), sum, .names = "total_{col}")) %>%
  ungroup() %>%
  mutate(ts_att = total_fga + (0.44 * total_fta),
         ts_pct = total_pts / (2 * ts_att),
         eff_fg_pct = (total_fgm + (total_fg3m * 0.5)) / total_fga) %>%
  arrange(-ts_pct)



# Ja Morant, G, Memphis Grizzlies -----------------------------------------

## 2. Leads the league with five field goals (on seven attempts) to tie or take the lead in the final minute of the fourth quarter or overtime.

pbp_final_gt %>%
  filter(game_date < "2022-02-17") %>%
  filter(period >= 4 & (clock == "1:00" | str_starts(clock, "00:"))) %>%
  mutate(diff_before = ifelse(slug_team == team_home, hs - vs - shot_pts, vs - hs - shot_pts)) %>%
  filter(msg_type %in% c(1, 2) & diff_before <= 0 & (diff_before >= -2 | (diff_before >= -3 & desc_value == 3))) %>%
  count(player_name = player1, shot_result = ifelse(msg_type == 1, "made", "missed")) %>%
  pivot_wider(names_from = shot_result,
              values_from = n,
              values_fill = 0) %>%
  arrange(-made) %>%
  transmute(player_name, fgm = made, fga = made + missed)

## 3. Has averaged 29.1 points per 36 minutes, up from 21.1 last season. That’s the biggest jump among 284 players who’ve played at least 500 minutes in each of the last two seasons. He’s also seen the ninth biggest jump in rebounds per 36 minutes (from 4.4 to 6.3) among that same group.

bind_rows(player_logs, player_logs_others) %>%
  filter(season >= 2021) %>%
  filter(game_date < "2022-02-17") %>%
  group_by(season, player_id, player_name) %>%
  summarise(across(c(pts, min, reb), sum)) %>%
  ungroup() %>%
  mutate(pts_36min = pts / min * 36,
         reb_36min = reb / min * 36) %>%
  filter(min >= 500) %>%
  add_count(player_id) %>%
  filter(n == 2) %>%
  select(-c(pts, reb, min, n)) %>%
  pivot_wider(names_from = season,
              values_from = c(pts_36min, reb_36min)) %>%
  mutate(diff_pts = pts_36min_2022 - pts_36min_2021,
         diff_reb = reb_36min_2022 - reb_36min_2021) %>%
  arrange(-diff_pts)  # change here to sort by diff_reb


# Dejounte Murray, G, San Antonio Spurs -----------------------------------

## 2. Leads the league in both steals (2.0) and deflections (3.9) per game.

team_games <- lineup_stats %>%
  filter(game_date < "2022-02-17",
         str_detect(lineup, "Dejounte Murray")) %>%  # remove this line to get for every game. Getting the box scores will take a while though
  distinct(game_id)

hustle_box_fun <- function(x){
  hoopR::nba_hustlestatsboxscore(game_id = x) %>%
    pluck("PlayerStats")
}

hustle_box <- map_df(team_games$game_id, hustle_box_fun)

hustle_box %>%
  clean_names() %>%
  mutate(across(c(pts:box_outs), as.numeric)) %>%
  filter(player_name == "Dejounte Murray") %>%
  summarise(deflections_pg = mean(deflections))

## 3. One of seven players (DeRozan, Durant, Embiid and Chris Paul are among the others) with at least 300 field goal attempts from outside the paint (102 players total) who have taken more mid-range shots (249) than 3-pointers (210). His 41.8% from mid-range ranks 40th among 62 players with at least 100 mid-range attempts.

shots_season %>%
  filter(ymd(game_date) < "2022-02-17") %>%
  filter(!shot_zone_basic %in% c("In The Paint (Non-RA)", "Restricted Area")) %>%
  count(player_name, shot_zone_basic) %>%
  group_by(player_name) %>%
  mutate(fga_out_paint = sum(n)) %>%
  ungroup() %>%
  pivot_wider(names_from = shot_zone_basic,
              values_from = n,
              values_fill = 0) %>%
  clean_names() %>%
  filter(fga_out_paint >= 300,
         mid_range / fga_out_paint > 0.5)


# Chris Paul, G, Phoenix Suns ---------------------------------------------

## 1. Leads the league with 10.7 assists per game and ranks second in assist/turnover ratio (4.49). His 125 assists to Booker, 121 assists to Deandre Ayton and 109 assists to Mikal Bridges are the second, third and fourth most assists from one player to a single teammate, trailing only Trae Young’s 135 assists to John Collins. Also leads the league with 28 clutch assists.

### assist/turnover ratio:
player_logs %>%
  filter(game_date < "2022-02-17") %>%
  group_by(player_id, player_name) %>%
  summarise(across(c(ast, tov), sum)) %>%
  ungroup() %>%
  mutate(ast_tov_ratio = ast / tov) %>%
  filter(ast >= 150) %>%
  arrange(-ast_tov_ratio)

### assists to teammates

pbp_final_gt %>%
  filter(msg_type == 1,
         !is.na(player2)) %>%
  count(assist_by = player2, assisted_to = player1, slug_team, sort = T)

### clutch assists

pbp_final_gt %>%
  filter(game_date < "2022-02-17") %>%
  filter(margin_before <= 5 & secs_passed_game >= 2580) %>%
  filter(msg_type == 1,
         !is.na(player2)) %>%
  count(clutch_assists = player2, sort = T)

# Jayson Tatum, F/G, Boston Celtics

## 1. Ranks fourth in cumulative plus-minus, with the Celtics having outscored their opponents by 394 points with him on the floor. They’ve been 12.7 points per 100 possessions better with him on the floor (+9.6) than they’ve been with him off the floor (-3.1). That’s the 10th biggest on-off differential among 330 players that have played at least 500 minutes for a single team.

### see Stephen Curry's stat #3

## 3. Has shot 32-for-33 (97.0%) on clutch free throws, the second best mark among 24 players with at least 20 attempts. Has shot just 2-for-23 on clutch 3-pointers, the worst mark among 16 players with at least 20 attempts.

pbp_final_gt %>%
  filter(game_date < "2022-02-17") %>%
  filter(margin_before <= 5 & secs_passed_game >= 2580) %>%
  filter(msg_type == 3) %>%
  count(player_name = player1, shot_result = ifelse(shot_pts == 0, "missed", "made")) %>%
  pivot_wider(names_from = shot_result,
              values_from = n,
              values_fill = 0) %>%
  transmute(player_name, ftm = made, fta = made + missed, ft_pct = made / (made + missed)) %>%
  filter(fta >= 20) %>%
  arrange(-ft_pct)

# Karl-Anthony Towns, C/F, Minnesota Timberwolves -----------

## 2. Has been assisted on 61.1% of his field goals, the lowest rate of his career.

pbp_final_gt %>%
  filter(game_date < "2022-02-17") %>%
  filter(msg_type == 1) %>%
  count(player_name = player1, assisted = !is.na(player2)) %>%
  group_by(player_name) %>%
  mutate(total_fgm = sum(n)) %>%
  ungroup() %>%
  filter(assisted) %>%
  transmute(player_name, assisted_fgm = n, total_fgm, assisted_pct = n / total_fgm) %>%
  filter(player_name == "Karl-Anthony Towns")



# Fred VanVleet, G, Toronto Raptors ---------------------------------------

## 2. Has shot 110-for-232 (47.4%) on catch-and-shoot 3s, the best mark among 108 players who’ve attempted at least 150. Leads the league in both second-chance 3-pointers (30) and clutch 3-pointers (17). His 17-for-37 (46%) shooting on clutch 3s ranks second among 16 players with at least 20 attempts. He’s 5-for-26 (19%) on clutch 2-point shots.

### second chance 3-pointers:

pbp_final_gt %>%
  group_by(game_id, period, slug_team, start_poss) %>%
  filter(sum(msg_type == 4 & desc_value == 1 & act_type == 0) > 0) %>%
  mutate(min_off_reb = min(number_event[which(msg_type == 4 & desc_value == 1 & act_type == 0)])) %>%
  ungroup() %>%
  filter(number_event > min_off_reb,
         shot_pts == 3,
         msg_type == 1) %>%
  count(player_name = player1, sort = T)



# Andrew Wiggins, F, Golden State Warriors --------------------------------

## 1. One of six players (Towns is one of the others) who have shot 50% or better on at least 300 2-point attempts and 40% or better on at least 200 3-point attempts.

shots_season %>%
  filter(ymd(game_date) < "2022-02-17") %>%
  count(player_name, shot_type, event_type) %>%
  group_by(player_name, shot_type) %>%
  mutate(fga = sum(n)) %>%
  ungroup() %>%
  filter(event_type == "Made Shot") %>%
  transmute(player_name, shot_type, fgm = n, fga, fg_pct = n / fga) %>%
  filter(((fg_pct >= 0.4 & shot_type == "3PT Field Goal") | (fg_pct >= 0.5 & shot_type == "2PT Field Goal")),
         ((fga >= 200 & shot_type == "3PT Field Goal") | (fga >= 300 & shot_type == "2PT Field Goal"))) %>%
  add_count(player_name) %>%
  filter(n == 2) %>%
  select(-n)

## 2. Has shot 60-for-121 (49.6%) on wide-open 3-pointers, the third best mark among 110 players who’ve attempted at least 100.

selected_player_id <- player_logs %>%
  filter(player_name == "Andrew Wiggins") %>%
  distinct(player_id) %>%
  pull(player_id)

shooting_dash <- hoopR::nba_playerdashptshots(season = "2021-22", player_id = selected_player_id,
                                              date_to = "2022-02-17")

shooting_dash %>%
  pluck("ClosestDefenderShooting") %>%
  clean_names() %>%
  mutate(across(c(fga_frequency:fg3_pct), as.numeric)) %>%
  select(close_def_dist_range, fg3m, fg3a, fg3_pct)


# Trae Young, G, Atlanta Hawks --------------------------------------------

## 2. Leads the league with 36 3-pointers (on 102 attempts – 35.3%) from 30 feet and out, 16 more than any other player (Curry and Garland are tied for second with 20).

shots_season %>%
  filter(ymd(game_date) < "2022-02-17") %>%
  mutate(shot_distance = as.numeric(shot_distance)) %>%
  filter(shot_distance >= 30) %>%
  count(player_name, event_type) %>%
  group_by(player_name) %>%
  mutate(fga = sum(n)) %>%
  ungroup() %>%
  filter(event_type == "Made Shot") %>%
  transmute(player_name, fgm = n, fga, fg_pct = n / fga) %>%
  arrange(-fgm)

