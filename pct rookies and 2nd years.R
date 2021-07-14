library(tidyverse)
library(nbastatR)
library(future)

# Percentage of rookies and 2nd year players in each team and their records

# select season
year_season <- 2021

# get game logs for players
player_logs <- game_logs(year_season, result_types = "player")

# get players data
plan(multiprocess)
players_info <- player_profiles(player_ids = unique(player_logs$idPlayer))

# find percentage played by rookies and 2nd years
minutes_dist <- player_logs %>%
  left_join(players_info %>%
              select(idPlayer, countSeasonsPlayed)) %>%
  mutate(time_league = ifelse(countSeasonsPlayed %in% c(0, 1), "rookie_soph", "veteran")) %>%
  group_by(slugTeam, time_league) %>%
  summarise(total_minutes = sum(minutes)) %>%
  group_by(slugTeam) %>%
  mutate(pct_minutes = total_minutes / sum(total_minutes)) %>%
  ungroup()

# calculate team records and number of games over .500
teams_records <- player_logs %>%
  group_by(idGame, slugTeam) %>%
  summarise(team_pts = sum(pts)) %>%
  group_by(idGame) %>%
  mutate(result = ifelse(team_pts == max(team_pts), "win", "loss")) %>%
  ungroup() %>%
  count(slugTeam, result) %>%
  pivot_wider(names_from = result,
              values_from = n) %>%
  mutate(games_over_500 = win - loss)

# order by biggest percentage played by rookies/2nd years and join with games over .500
minutes_dist %>%
  filter(time_league == "rookie_soph") %>%
  arrange(desc(pct_minutes)) %>%
  left_join(teams_records %>%
              select(slugTeam, games_over_500))


