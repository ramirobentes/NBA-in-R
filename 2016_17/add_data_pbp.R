missing_starters <- tribble(
  ~game_id,       ~period,      ~slug_team,          ~player_name,
  21600049,             5,           "MIA",        "Dion Waiters",
  21600253,             4,           "SAC",      "Garrett Temple",
  21600270,             5,           "OKC",      "Andre Roberson",
  21600270,             5,           "WAS",     "Otto Porter Jr.",
  21600359,             5,           "NOP",   "Langston Galloway",
  21600559,             6,           "POR",        "Allen Crabbe",
  21600976,             5,           "NOP",        "Solomon Hill")
  
  
  delete_event <- tribble(
  ~game_id, ~number_event,
  21600389,            397)


corrections <- tribble(
  ~game_id,     ~number_event,    ~msg_type, ~act_type,                                   ~description,
  21600009,               425,            6,        18,    "Delay Technical - MEM (1 FTA) (D Stafford)")


