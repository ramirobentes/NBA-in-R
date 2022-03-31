library(tidyverse)

lineup_stats <- read_csv("https://github.com/ramirobentes/NBA-in-R/blob/master/lineup-stats/data.csv?raw=true")

player_stints <- lineup_stats %>%
  group_by(game_id, slug_team) %>%
  mutate(final_time = cumsum(secs_played),
         initial_time = final_time - secs_played) %>%
  ungroup() %>%
  separate_rows(lineup, sep = ", ") %>%
  group_by(game_id, lineup) %>%
  mutate(stint_player = ifelse(initial_time != lag(final_time, default = 0), 1, 0),
         stint_player = cumsum(stint_player) + 1) %>%
  group_by(game_date, game_id, slug_team, slug_opp, player_name = lineup, stint_player) %>%
  summarise(initial_time = min(initial_time),
            final_time = max(final_time),
            stint_time = sum(secs_played)) %>%
  ungroup()

player_stints %>%
  filter(stint_time >= 12 * 60)
