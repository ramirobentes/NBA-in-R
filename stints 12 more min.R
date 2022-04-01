library(tidyverse)

lineup_stats <- read_csv("https://github.com/ramirobentes/NBA-in-R/blob/master/lineup-stats/data.csv?raw=true")

player_stints <- lineup_stats %>%
  group_by(game_id, slug_team) %>%
  mutate(final_time = cumsum(secs_played),
         initial_time = final_time - secs_played) %>%
  ungroup() %>%
  separate_rows(lineup, sep = ", ") %>%
  group_by(game_id, lineup) %>%
  mutate(across(c(initial_time, final_time), ~ round(., 1))) %>%
  mutate(stint_player = ifelse(initial_time != lag(final_time, default = 0), 1, 0),
         stint_player = cumsum(stint_player) + 1) %>%
  group_by(game_date, game_id, half = ifelse(period <= 2, "first", "second"), slug_team, slug_opp, player_name = lineup, stint_player) %>%
  summarise(initial_time = min(initial_time),
            final_time = max(final_time),
            stint_time = sum(secs_played)) %>%
  ungroup()

player_stints %>%
  filter(stint_time >= 12 * 60)



# From Play-by-Play

library(tidyverse)
library(hoopR)
library(janitor)

pbp_nba <- hoopR::nba_pbp(game_id = "0022101139") %>%
  clean_names()

players_subbed <- pbp_nba %>%
  filter(eventmsgtype == 8) %>%
  select(game_id, period, clock = pctimestring, player_in = player2_name, player_out = player1_name) %>%
  pivot_longer(cols = starts_with("player"),
               names_to = "subbed",
               values_to = "player_name",
               names_prefix = "player_")

players_first_sub <- players_subbed %>%
  distinct(game_id, period, player_name, .keep_all = TRUE) %>%
  distinct(game_id, period, player_name, subbed) %>%
  mutate(starter = ifelse(subbed == "out", 1, 0))

starters_quarters <- pbp_nba %>%
  filter(!(eventmsgtype == 6 & eventmsgactiontype %in% c(11, 12, 16, 18, 30))) %>%
  filter(!eventmsgtype %in% c(9, 11, 18)) %>% # timeout, ejection, replay
  select(game_id, period, contains("_name")) %>%
  pivot_longer(cols = contains("_name")) %>%
  filter(!is.na(value)) %>%
  distinct(game_id, period, player_name = value) %>%
  anti_join(players_first_sub) %>%
  bind_rows(players_first_sub %>%
              filter(starter == 1)) %>%
  select(game_id, period, player_name)

player_stints_qtr <- starters_quarters %>%
  mutate(clock = ifelse(period %in% c(1:4), "12:00", "5:00"),
         subbed = "in") %>%
  bind_rows(players_subbed) %>%
  group_by(game_id, period = as.numeric(period), player_name, subbed) %>%
  mutate(period_stint = row_number()) %>%
  ungroup() %>%
  pivot_wider(names_from = subbed,
              values_from = clock,
              values_fill = "0:00.0",
              names_prefix = "clock_") %>%
  arrange(game_id, period) %>%
  mutate(across(starts_with("clock"), ~ as.numeric(word(., 1, sep = ":")) * 60 + as.numeric(word(., 2, sep = ":")), 
                .names = "{col}_sec"),
         across(ends_with("_sec"),
                ~ ifelse(period %in% c(1:4), 720 - ., 300 - .) + ifelse(period %in% c(1:5), (period - 1) * 720, 2880 + (period - 5) * 300)),
         across(ends_with("_sec"), ~ ifelse(clock_out == 0:00, . + 0.05, .))) %>%
  group_by(game_id, player_name) %>%
  mutate(stint_player = ifelse(clock_in_sec != lag(clock_out_sec, default = 0), 1, 0),
         stint_player = cumsum(stint_player) + 1) %>%
  ungroup()
  
player_stints <- player_stints_qtr %>%
  group_by(game_id, half = ifelse(period <= 2, "first", "second"), player_name, stint_player) %>%
  summarise(initial_time = min(clock_in_sec),
            final_time = max(clock_out_sec)) %>%
  ungroup() %>%
  mutate(duration = final_time - initial_time)

player_stints %>%
  filter(duration >= 12 * 60)
