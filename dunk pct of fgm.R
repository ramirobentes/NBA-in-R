library(hoopR)
library(tidyverse)
library(janitor)

shots_season <- nba_shotchartdetail(season = "2021-22",
                                    player_id = 0) %>%
  pluck("Shot_Chart_Detail") %>%
  clean_names()

shots_season %>%
  filter(event_type == "Made Shot") %>%
  count(player_name, dunk = str_detect(action_type, "Dunk")) %>%
  group_by(player_name) %>%
  mutate(fgm = sum(n)) %>%
  ungroup() %>%
  mutate(pct_dunk = n / fgm) %>%
  filter(dunk,
         fgm >= 50,
         pct_dunk >= 0.5) %>%
  arrange(-pct_dunk) %>%
  select(player_name, dunks = n, fgm, pct_dunk)