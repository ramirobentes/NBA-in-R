# corrections to pbp ------------------------------------------------------

corrections <- tribble(
  ~game_id,     ~clock,   ~secs_passed_game, ~number_event, ~msg_type, ~act_type,                                              ~description,
  12200026,    "09:59",                1561,           249,         6,         1,             "[SAS] Jones Foul: Personal (2 PF) (B Nansel)"
  )

# change order plays -------------------------------------------------------

change_order <- tribble(
  ~game_id,     ~number_original,     ~number_event,
  12200036,                  569,               401,
  12200036,                  571,               402,
  12200036,                  572,               403,
  12200036,                  574,               404,
)
