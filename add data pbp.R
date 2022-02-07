# library(lubridate)
# 
# # save by month -----------------------------------------------------------
# 
# 
# player_logs <- nba_leaguegamelog(season = "2021-22", player_or_team = "P") %>%
#   pluck("LeagueGameLog") %>%
#   clean_names()
# 
# 
# nba_com_pbp <- read_csv("pbp_nba_api_22.csv",
#                         col_types = c(cl = "c")) 
# 
# nba_com_pbp %>%
#   left_join(player_logs %>%
#               distinct(game_id, game_date)) %>%
#   #filter(is.na(game_date))
#   filter(month(game_date) == 1) %>%
#   select(-game_date) %>%
#   write_csv("pbp data months/pbp_hoopr_2201.csv")



# corrections to pbp ------------------------------------------------------

corrections <- tribble(
  ~game_id,     ~clock,   ~secs_passed_game, ~number_event, ~msg_type, ~act_type,                                              ~description,
  22100295,    "04:26",                1894,           283,         6,         2,    "[ORL] Lopez Foul: Shooting (3 PF) (1 FTA) (J Capers)",
  22100199,    "04:39",                1881,           271,         6,         1,           "[CLE] Wade Foul: Personal (3 PF) (T Brothers)",
  22100249,    "09:18",                1602,           265,         3,        18,         "[DET] Jackson Free Throw Flagrant 1 of 2 Missed",
  22100249,    "09:18",                1602,           267,         3,        19,  "[DET 79-67] Jackson Free Throw Flagrant 2 of 2 (5 PTS)",
  22100512,  "00:20.8",              2859.2,           473,         6,         1,            "[NYK] Burks Foul: Personal (4 PF) (M Boland)")







  
