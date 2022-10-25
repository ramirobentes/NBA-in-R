missing_starters <- tribble(
  ~game_id,      ~period,      ~slug_team,          ~player_name,
  21800143,            6,           "CHI",      "Justin Holiday",
  21800143,            6,           "NYK",         "Noah Vonleh",
  21800216,            5,           "BOS",   "Marcus Morris Sr.",
  21800276,            3,           "DEN", "Juancho Hernangomez",
  21800371,            5,           "BKN",          "Joe Harris",
  21800565,            5,           "HOU",         "P.J. Tucker",
  21800619,            5,           "OKC",   "Terrance Ferguson",
  21800881,            5,           "UTA",          "Joe Ingles",
  21801070,            5,           "MEM",       "Bruno Caboclo",
  21801132,            5,           "GSW",      "Andre Iguodala",
  21801229,            5,           "UTA",     "Tyler Cavanaugh")


corrections <- tribble(
  ~game_id,     ~clock,   ~secs_passed_game,  ~period, ~number_event, ~msg_type, ~act_type,  ~desc_value,                                         ~description,
  21800534,    "12:00",                1440,        3,           237,         6,        11,            0,         "[GSW] Iguodala Technical (1 FTA) (J Tiven)",
  21800534,    "12:00",                1440,        3,           238,        11,         4,            0,         "[GSW] Iguodala Ejection:Other",
  21800036,    "02:17",                1303,        2,           222,         3,        28,            1,         "[ATL 57-58] Bazemore Free Throw Flagrant 2 of 3 (7 PTS)",
  21800036,    "02:17",                1303,        2,           223,         3,        29,            1,         "[ATL 57-58] Bazemore Free Throw Flagrant 3 of 3 (8 PTS)")


# change order plays -------------------------------------------------------

change_order <- tribble(
  ~game_id,     ~number_original,     ~number_event,
  21800578,                  246,               176,
  21800578,                  249,               177,
  21800578,                  250,               178,
  21800484,                  221,               158,
  21800484,                  223,               159,
  21800484,                  224,               160,
  21800484,                  225,               161,
  21800484,                  226,               162,
  21801124,                  616,               441,
  21801124,                  618,               442,
  21801124,                  619,               443,
  21801124,                  620,               444)
