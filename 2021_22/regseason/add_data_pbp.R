missing_starters <- tribble(
  ~game_id,      ~period,      ~slug_team,      ~player_name,
  22100041,            5,           "CHA",  "Gordon Hayward",
  22100291,            6,           "LAL",      "Malik Monk",
  22100353,            5,           "PHI",     "Danny Green",
  22100413,            5,           "BKN", "Kessler Edwards",
  22100688,            3,           "POR","Robert Covington",
  22100860,            5,           "OKC",   "Darius Bazley",
  22100967,            5,           "NOP",      "Tony Snell")

# corrections to pbp ------------------------------------------------------

corrections <- tribble(
  ~game_id,     ~clock,   ~secs_passed_game, ~number_event, ~msg_type, ~act_type,                                              ~description,
  22100295,    "04:26",                1894,           283,         6,         2,    "[ORL] Lopez Foul: Shooting (3 PF) (1 FTA) (J Capers)",
  22100199,    "04:39",                1881,           271,         6,         1,           "[CLE] Wade Foul: Personal (3 PF) (T Brothers)",
  22100249,    "09:18",                1602,           265,         3,        18,         "[DET] Jackson Free Throw Flagrant 1 of 2 Missed",
  22100249,    "09:18",                1602,           267,         3,        19,  "[DET 79-67] Jackson Free Throw Flagrant 2 of 2 (5 PTS)",
  22100512,  "00:20.8",              2859.2,           473,         6,         1,            "[NYK] Burks Foul: Personal (4 PF) (M Boland)")


# change order plays -------------------------------------------------------

change_order <- tribble(
  ~game_id,     ~number_original,     ~number_event,
  22101139,                  649,               466,
  22101139,                  648,               467,
  22101139,                  650,               469,
  22101139,                  654,               470,
  22101196,                  601,               400,
  22101196,                  577,               401)
