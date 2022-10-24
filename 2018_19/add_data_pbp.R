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
  ~game_id,     ~clock,   ~secs_passed_game,  ~period, ~number_event, ~msg_type, ~act_type,                                              ~description,
  21800534,    "12:00",                1440,        3,           237,         6,        11,              "[GSW] Iguodala Technical (1 FTA) (J Tiven)",
  21800534,    "12:00",                1440,        3,           238,        11,         4,                           "[GSW] Iguodala Ejection:Other")


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
