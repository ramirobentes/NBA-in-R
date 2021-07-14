library(tidyverse)
library(nbastatR)
library(future)

# Points in the paint differential in a season, by team

# select season
year_season <- 2021

# get games table to find team names
all_games <- game_logs(year_season, result_types = "team")

# get every shot of the season
plan(multiprocess)
shots_season <- teams_shots(team_ids = unique(all_games$idTeam),
                            seasons = year_season)

# calculate points in the paint
#points_paint <- 
paint_points_games <- shots_season %>%
  filter(zoneBasic %in% c("Restricted Area", "In The Paint (Non-RA)"),
         isShotMade) %>%
  count(idGame, idTeam) %>%
  left_join(all_games %>%
              distinct(idGame, idTeam, slugTeam, slugOpponent)) %>%
  select(-idTeam) %>%
  pivot_longer(cols = starts_with("slug"),
               names_to = "team",
               values_to = "slugTeam",
               names_prefix = "slug") %>%
  mutate(points = n * 2) # each field goal is worth 2 points

paint_points_games %>%
  group_by(slugTeam, team) %>%
  summarise(number_games = n_distinct(idGame),
            total_points = sum(points)) %>%
  ungroup() %>%
  pivot_wider(names_from = team,
              values_from = total_points) %>%
  mutate(difference = Team - Opponent,
         team_per_game = Team / number_games,
         opp_per_game = Opponent / number_games,
         diff_per_game = difference / number_games) %>%
  arrange(desc(diff_per_game))

# for comparison: https://www.nba.com/stats/teams/misc/?sort=PTS_PAINT&dir=-1&Season=2020-21&SeasonType=Regular%20Season