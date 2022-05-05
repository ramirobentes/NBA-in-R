library(tidyverse)
library(hoopR)
library(janitor)
library(future)

seasons <- year_to_season(1990:2021)
plan(multicore)
shots <- furrr::future_map_dfr(seasons, ~ nba_shotchartdetail(season = .,
                                                              season_type = "Playoffs",
                                                              player_id = 0) %>%
                                 pluck("Shot_Chart_Detail")) %>%
  clean_names()


shots %>%
  filter(shot_zone_basic == "Mid-Range") %>%
  mutate(game_date = lubridate::ymd(game_date)) %>%
  select(game_date, game_id, game_event_id, player_name, team_name, event_type) %>%
  group_by(player_name) %>%
  mutate(streak = cumsum(event_type == "Missed Shot")) %>%
  group_by(streak, player_name, event_type) %>%
  summarise(from_date = min(game_date),
            to_date = max(game_date),
            across_games = n_distinct(game_id),
            streak_total = n()) %>%
  ungroup() %>%
  filter(event_type == "Made Shot") %>%
  select(-c(streak, event_type)) %>%
  arrange(-streak_total)


shots %>%
  mutate(game_date = lubridate::ymd(game_date)) %>%
  filter(shot_zone_basic == "Mid-Range") %>%
  filter(player_name == "Chris Paul",
         lubridate::year(game_date) == 2022) %>%
  select(game_date, game_id, game_event_id, player_name, team_name, event_type) %>%
  group_by(player_name) %>%
  mutate(streak = cumsum(event_type != lag(event_type, default = "Missed Shot"))) %>%
  group_by(streak, player_name, event_type) %>%
  mutate(streak_number = ifelse(event_type == "Made Shot", row_number(), row_number() * -1)) %>%
  ungroup() %>%
  select(-streak) %>%
  mutate(shot_number = row_number(),
         event_type = fct_rev(as.factor(event_type))) %>%
  group_by(game_id) %>%
  mutate(game_first = ifelse(shot_number == min(shot_number), shot_number - 0.5, NA)) %>%
  ungroup() %>%
  ggplot(aes(x = shot_number, y = streak_number)) +
  geom_col(aes(fill = event_type)) +
  geom_vline(aes(xintercept = game_first)) +
  labs(title = "Chris Paul mid-range shots in the 2021-22 NBA Playoffs",
       subtitle = "Black lines represent start of every game",
       x = "shot number",
       y = "consecutive makes/misses") +
  theme_light() +
  theme(legend.position = "none")
