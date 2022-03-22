library(tidyverse)
library(hoopR)
library(janitor)

seasons <- paste(1990:2021, str_pad(91:22, pad = 0, width = 2), sep = "-")
player_logs_all <- map_df(seasons, ~ nba_leaguegamelog(season = ., player_or_team = "P") %>%
                               pluck("LeagueGameLog")) %>%
  clean_names() %>%
  mutate(across(c(min:fantasy_pts), as.numeric),
         game_date = as.Date(game_date))

games_season <- player_logs_all %>%
  group_by(season_id, team_id) %>%
  summarise(total_games = n_distinct(game_id)) %>%
  ungroup() %>%
  group_by(season_id) %>%
  summarise(total_games = mean(total_games))

ppg_leaders <- player_logs_all %>%
  group_by(season_id, player_id, player_name) %>%
  summarise(games = n_distinct(game_id),
            ppg = mean(pts)) %>%
  ungroup() %>%
  left_join(games_season) %>%
  filter(games / total_games >= 0.7) %>%
  arrange(-ppg) %>%
  group_by(season_id) %>%
  mutate(rank = row_number()) %>%
  ungroup() %>%
  filter(rank <= 3)

dates_season <- player_logs_all %>%
  mutate(game_date = as.Date(game_date)) %>%
  group_by(season_id) %>%
  summarise(game_date = seq.Date(min(game_date), max(game_date), by = "day")) %>%
  ungroup() %>%
  full_join(ppg_leaders %>%
              transmute(season_id, player_id, player_name, rank = as.factor(rank)))

ppg_race <- player_logs_all %>%
  mutate(game_date = as.Date(game_date)) %>%
  semi_join(ppg_leaders %>%
              select(season_id, player_id)) %>%
  select(season_id, game_date, player_id, player_name, pts) %>%
  group_by(season_id, player_id, player_name) %>%
  mutate(ppg = cummean(pts)) %>%
  ungroup() %>%
  full_join(dates_season) %>%
  arrange(game_date) %>%
  distinct(season_id, game_date, player_id, player_name, .keep_all = TRUE) %>%
  group_by(season_id, player_id, player_name) %>%
  mutate(ppg = zoo::na.locf0(ppg)) %>%
  group_by(game_date) %>%
  mutate(diff_range = max(ppg) - min(ppg)) %>%
  group_by(season_id) %>%
  filter(max(game_date) - game_date <= 30) %>%
  filter((season_id == "22021" & game_date == max(game_date)) | (season_id != "22021" & diff_range == min(diff_range))) %>%
  ungroup() %>%
  distinct(season_id, player_id, player_name, .keep_all = TRUE)

ppg_race %>%
  ggplot(aes(y = ppg,
             x = as.character(game_date))) +
  geom_line(color = "black", size = 1) +
  geom_point(aes(color = rank),
             size = 3) +
  scale_color_manual(values = c("#32CD32", "darkgray", "#CD7F32")) +
  theme_light() +
  theme(axis.text.x = element_text(angle = 90)) +
  theme(legend.position = "top") +
  labs(title = "The NBA scoring race has never been this close between 3 players",
       subtitle = "Comparing the current range between 1st and 3rd with the smallest range in last month of previous seasons",
       color = "PPG rank at end of season",
       y = "PPG",
       x = "")

