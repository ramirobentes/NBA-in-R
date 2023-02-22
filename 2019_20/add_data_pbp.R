missing_starters <- tribble(
  ~game_id,      ~period,      ~slug_team,          ~player_name,
  21900023,            5,           "DEN",      "Malik Beasley",
  21900120,            5,           "MIN",         "Treveon Graham",
  21900272,            5,           "ATL",   "De'Andre Hunter",
  21900409,            5,           "WAS",           "Ish Smith",
  21900502,            5,           "GSW",          "Damion Lee",
  21900550,            5,           "OKC",   "Terrance Ferguson",
  21900563,            5,           "DET",          "Tony Snell",
  21900696,            5,           "SAC",          "Harrison Barnes",
  21900787,            5,           "ATL",       "De'Andre Hunter",
  21900892,            5,           "HOU",      "Eric Gordon",
  21901281,            6,           "DEN",     "Monte Morris")



manual_changes <- tribble(
  ~game_id,     ~number_event,    ~shot_pts_home,     ~shot_pts_away,
  21900622,               328,                 0,                  2,
  21900742,               216,                 0,                  0,
  21900742,               219,                 0,                  1)



change_order <- tribble(
  ~game_id,     ~number_original,     ~number_event,
  21900811,                  597,               431,
  21900811,                  605,               432,
  21900811,                  612,               433,
  21900811,                  601,               434,
  21900811,                  606,               435,
  21900811,                  599,               436,
  21900811,                  600,               437)
