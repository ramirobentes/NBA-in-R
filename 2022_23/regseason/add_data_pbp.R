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
  22200748,            5,           "SAS",  "Keita Bates-Diop",
  22200758,            5,           "SAC",   "Harrison Barnes",
  22200892,            5,           "OKC",    "Jalen Williams",
  22201007,            5,           "MIA",         "Max Strus",
  22201194,            5,           "NOP",       "CJ McCollum",
  22201205,            5,           "ATL",        "Saddiq Bey")



corrections <- tribble(
  ~game_id,     ~number_event, ~msg_type, ~act_type,           ~description,
  22200094,               380,         3,        16,           "[PHX 93-88] Lee Free Throw Technical (5 PTS)",
  22201142,               437,         6,         3,           "[OKC] Jay Williams Foul: Loose Ball (2 PF) (2 FTA) (C Kirkland)",
  22201218,               448,         6,         1,           "[CHA] Bouknight Foul: Personal (3 PF) (2 FTA) (K Cutler)")



change_order <- tribble(
  ~game_id,     ~number_original,     ~number_event,
  22200771,                  495,               347,
  22200771,                  497,               348,
  22200771,                  498,               349,
  22200916,                  278,               199,
  22200916,                  280,               200,
  22200916,                  283,               202,)
