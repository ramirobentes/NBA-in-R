library(tidyverse)
library(hoopR)
library(janitor)

player_logs <- nba_leaguegamelog(season = "2021-22", player_or_team = "P") %>%
  pluck("LeagueGameLog") %>%
  clean_names()

chosen_player_id <- player_logs %>%
  filter(player_name == "Coby White") %>%
  distinct(player_id) %>%
  pull(player_id)

player_pass <- hoopR::nba_playerdashptpass(season = "2021-22",
                                           player_id = chosen_player_id,
                                           date_from = "2022-02-01",
                                           date_to = "2022-03-02")
player_pass %>%
  pluck("PassesReceived") %>%
  clean_names() %>%
  select(player_name_last_first, pass_type:pass_from, frequency:fg3_pct) %>%
  mutate(across(c(frequency:fg3_pct), as.numeric)) %>%
  arrange(-frequency)
