library(tidyverse)
library(nbastatR)

# Who are the best 3-pt shooters with at least 5 attempts per game?

# select season
year_season <- 2021

# get game logs for players
player_logs <- game_logs(year_season, result_types = "player")

player_logs %>%
  group_by(idPlayer, namePlayer) %>%
  summarise(across(c(fg3m, fg3a), sum),
            number_games = n()) %>%
  ungroup() %>%
  mutate(fg3pct = fg3m / fg3a) %>%
  arrange(desc(fg3pct)) %>%
  filter(fg3a / number_games >= 5)
 