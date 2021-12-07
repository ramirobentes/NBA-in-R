library(tidyverse)
library(nbastatR)
library(lubridate)

team_logs <- game_logs(seasons = 2022, result_types = "team")
season_shots <- teams_shots(seasons = 2022,
                            team_ids = unique(team_logs$idTeam))

season_shots %>%
  # filter(ymd(dateGame) < as.Date("2021-12-06")) %>%
  count(namePlayer, nameTeam, typeEvent, zoneBasic) %>%
  group_by(namePlayer, nameTeam, zoneBasic) %>%
  mutate(attempts = sum(n),
         zone_pct = n / attempts) %>%
  ungroup() %>%
  filter(zoneBasic %in% c("Restricted Area", "In The Paint (Non-RA)", "Mid-Range")) %>%
  mutate(minimum = case_when(zoneBasic == "Restricted Area" ~ 75,
                             zoneBasic == "In The Paint (Non-RA)" ~ 50,
                             zoneBasic == "Mid-Range" ~ 35)) %>%
  filter(attempts >= minimum,
         typeEvent == "Made Shot") %>%
  group_by(zoneBasic) %>%
  mutate(player_rank = dplyr::dense_rank(-zone_pct)) %>%
  ungroup() %>%
  # filter(namePlayer == "Nikola Jokic") %>%
  select(namePlayer, nameTeam, zoneBasic, made = n, attempts, zone_pct, player_rank)