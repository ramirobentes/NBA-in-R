shots <- furrr::future_map_dfr(c("Regular Season", "Playoffs"), ~ nba_shotchartdetail(season = "2021-22",
                                                                                      season_type = .,
                                                                                      player_id = 0) %>%
                                 pluck("Shot_Chart_Detail")) %>%
  clean_names()

game_logs <- furrr::future_map_dfr(c("Regular Season", "Playoffs"), ~ nba_leaguegamelog(season = "2021-22", 
                                                                         season_type = .,
                                                                         player_or_team = "T") %>%
                                     pluck("LeagueGameLog")) %>%
  clean_names()

shots %>%
  filter(shot_zone_basic %in% c("In The Paint (Non-RA)", "Restricted Area"),
         event_type == "Made Shot") %>%
  count(game_id, game_date = lubridate::ymd(game_date), team_id) %>%
  left_join(game_logs %>%
              mutate(slug_opp = map2_chr(str_extract_all(matchup, "[A-Z]{3}"), team_abbreviation, setdiff)) %>%
              select(game_id, team_id, slug_opp)) %>%
  arrange(game_date) %>%
  mutate(pts_paint = n * 2,
         game = 1) %>%
  group_by(slug_opp) %>%
  mutate(across(c(pts_paint, game), ~ slide_dbl(., sum, .before = 5), .names = "rolling_{.col}")) %>%
  ungroup() %>%
  filter(rolling_game == 6) %>%
  mutate(pts_paint_game = rolling_pts_paint / rolling_game) %>%
  arrange(pts_paint_game) %>%
  select(-c(game_id, team_id, n, pts_paint, game))
  

