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

# Find FG% in paint, mid-range and 3-point range for every team
shots_season %>%
  mutate(zone_new = case_when(str_detect(zoneBasic, "3|Backcourt") ~ "three",
                              zoneBasic %in% c("In The Paint (Non-RA)", "Restricted Area") ~ "paint",
                              zoneBasic == "Mid-Range" ~ "mid_range")) %>%
  count(zone_new, nameTeam, isShotMade) %>%
  group_by(nameTeam, zone_new) %>%
  mutate(fg_pct = n / sum(n)) %>%
  ungroup() %>%
  filter(isShotMade) %>%
  group_by(zone_new) %>%
  mutate(team_rank = dense_rank(desc(fg_pct))) %>%
  ungroup() %>%
  select(-c(n, isShotMade)) %>%
  pivot_wider(names_from = zone_new,
              values_from = c(fg_pct, team_rank))
