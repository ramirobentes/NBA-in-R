library(tidyverse)
library(nbastatR)
library(future)

year_season <- 2021
slug_team <- "DAL"

# Get all game logs for teams
all_games <- game_logs(year_season, result_types = "team")

all_games_id <- all_games %>%
  filter(slugTeam == slug_team) %>%
  distinct(idGame) %>%
  pull(idGame)

# Get advanced box scores for every game
plan(multiprocess)
advanced_teams <- box_scores(game_ids = all_games_id,
                             box_score_types = "Advanced",
                             result_types = "teams") 

all_games %>%
  filter(slugTeam == slug_team) %>%
  select(idGame, dateGame, slugTeam, slugOpponent, ptsTeam) %>%
  left_join(advanced_teams %>%
              unnest(dataBoxScore) %>%
              select(idGame, slugTeam, possessions)) %>%
  mutate(pts_per_poss = ptsTeam / possessions)

