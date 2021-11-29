library(tidyverse)
library(future)
library(nbastatR)
library(janitor)
library(lubridate)

team_logs <- game_logs(seasons = c(2021, 2022), result_types = "team")
plan(multiprocess)
season_shots <- teams_shots(seasons = 2022,
                            team_ids = unique(team_logs$idTeam))

season_shots %>%
  filter(numberPeriod == 1,
         minutesRemaining > 6 | (minutesRemaining == 6 & secondsRemaining > 0)) %>%
  count(namePlayer, typeEvent, typeShot) %>%
  group_by(namePlayer) %>%
  mutate(total_fga = sum(n),
         total_fgm = sum(n[which(typeEvent == "Made Shot")])) %>%
  ungroup() %>%
  pivot_wider(names_from = c(typeEvent, typeShot),
              values_from = n,
              values_fill = 0) %>%
  clean_names() %>%
  mutate(eff_fg = (total_fgm + (made_shot_3pt_field_goal * 0.5)) / total_fga) %>%
  filter(total_fga >= 25) %>%
  select(name_player, eff_fg) %>%
  arrange(eff_fg)