library(tidyverse)
library(nbastatR)


lineup_stats <- read_csv("https://raw.githubusercontent.com/ramirobentes/NBA-in-R/master/lineup-stats/data.csv")
player_logs <- game_logs(seasons = 2022, result_types = "player")

green_curry <- lineup_stats %>%
  filter(slugTeam == "GSW") %>%
  filter(str_detect(lineup, "Stephen Curry")) %>%
  mutate(with_draymond = str_detect(lineup, "Draymond Green")) %>%
  left_join(player_logs %>%
              filter(namePlayer == "Stephen Curry") %>%
              select(idGame, player_game = numberGamePlayerSeason)) %>%
  mutate(first_ten = player_game %in% 1:10,
         last_seven = player_game %in% max(player_game):(max(player_game) - 6))

green_curry %>%
  filter(first_ten) %>%
  group_by(with_draymond) %>%
  summarise(total_time = sum(totalTime)) %>%
  ungroup() %>%
  mutate(pct_time = total_time / sum(total_time))

green_curry %>%
  filter(last_seven) %>%
  group_by(with_draymond) %>%
  summarise(total_time = sum(totalTime)) %>%
  ungroup() %>%
  mutate(pct_time = total_time / sum(total_time))
