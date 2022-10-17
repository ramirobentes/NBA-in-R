# corrections to pbp ------------------------------------------------------

corrections <- tribble(
  ~game_id,     ~clock,   ~secs_passed_game, ~number_event, ~msg_type, ~act_type,                                              ~description,
  12200026,    "09:59",                1561,           249,         6,         1,             "[SAS] Jones Foul: Personal (2 PF) (B Nansel)"
  )

# change order plays -------------------------------------------------------

change_order <- tribble(
  ~game_id,     ~number_original,     ~number_event,
  12200036,                  404,               401,
  12200036,                  401,               402,
  12200036,                  402,               403,
  12200036,                  403,               404,
)
