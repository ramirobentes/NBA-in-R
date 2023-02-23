missing_starters <- tribble(
  ~game_id,      ~period,      ~slug_team,          ~player_name,
  21700064,            5,           "WAS",     "Kelly Oubre Jr.",
  21700249,            5,           "BOS",          "Al Horford",
  21700482,            5,           "BKN",        "Allen Crabbe",
  21700584,            2,           "NOP",    "Dante Cunningham",
  21700607,            5,           "NYK",     "Michael Beasley",
  21700635,            5,           "NOP",           "Ian Clark",
  21700692,            6,           "NOP",       "Darius Miller",
  21700893,            5,           "MIL",        "Eric Bledsoe",
  21700966,            5,           "TOR",          "Kyle Lowry",
  21701103,            5,           "CHA",        "Dwayne Bacon",
  21701103,            5,           "NYK",        "Courtney Lee",
  21701136,            5,           "MIL",         "Jason Terry")


corrections_desc <- tribble(
  ~game_id,    ~number_event,                                              ~description,
  21700020,              408,  "[WAS] Gortat Foul: Loose Ball (4 PF) (2 FTA) (S Corbin)",
  21700020,              415,  "[WAS] Gortat Foul: Loose Ball (5 PF) (2 FTA) (S Corbin)",
  21700020,              422,  "[DET] Jackson Foul: Shooting (4 PF) (2 FTA) (S Corbin)",
  21700020,              429,  "[DET] Leuer Foul: Shooting (2 PF) (2 FTA) (K Cutler)",
  21700020,              441,  "[DET] Leuer Foul: Shooting (3 PF) (1 FTA) (K Cutler)",
  21700020,              462,  "[DET] Smith Foul: Personal Take (3 PF) (2 FTA) (S Corbin)")
