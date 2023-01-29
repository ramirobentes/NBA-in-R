missing_starters <- tribble(
  ~game_id,      ~period,      ~slug_team,      ~player_name,
  22200025,            5,           "MIN",  "Jaden McDaniels",
  22200039,            5,           "WAS",     "Delon Wright",
  22200040,            5,           "UTA",      "Mike Conley",
  22200072,            5,           "BOS",       "Al Horford",
  22200117,            5,           "NOP",    "Naji Marshall",
  22200117,            5,           "LAL",    "Austin Reaves",
  22200325,            5,           "DET",   "Isaiah Stewart",
  22200440,            5,           "DAL",  "Tim Hardaway Jr.",
  22200519,            5,           "CHI",       "Zach LaVine",
  22200659,            5,           "TOR",    "Gary Trent Jr.",
  22200748,            5,           "SAS",  "Keita Bates-Diop")



corrections <- tribble(
  ~game_id,     ~number_event, ~msg_type, ~act_type,           ~description,
  22200094,               380,         3,        16,           "[PHX 93-88] Lee Free Throw Technical (5 PTS)")
