library(tidyverse)
library(hoopR)
library(janitor)

player_logs <- nba_leaguegamelog(season = "2021-22", player_or_team = "P") %>%
  pluck("LeagueGameLog") %>%
  clean_names() %>%
  mutate(across(c(min:fantasy_pts), as.numeric)) %>%
  mutate(slug_opp = str_extract_all(matchup, "[A-Z]{3}"),
         slug_opp = map2_chr(slug_opp, team_abbreviation, setdiff)) %>%
  rename(slug_team = team_abbreviation)

games_players <- player_logs %>%
  group_by(slug_team) %>%
  mutate(all_players = list(unique(player_name))) %>%
  group_by(game_id, slug_team, slug_opp, all_players, wl) %>%
  summarise(played_game = list(unique(player_name))) %>%
  ungroup() %>%
  mutate(dnp_game = map2(all_players, played_game, setdiff)) %>%
  left_join(player_logs %>%
              group_by(game_id, slug_team) %>%
              summarise(across(c(fg3m, fg3a), sum, .names = "{.col}_team")) %>%
              ungroup()) %>%
  left_join(player_logs %>%
              group_by(game_id, slug_opp = slug_team) %>%
              summarise(across(c(fg3m, fg3a), sum, .names = "{.col}_opp")) %>%
              ungroup())

stats_players <- games_players %>%
  unnest_longer(played_game) %>%
  group_by(slug_team, player_name = played_game) %>%
  summarise(wins = sum(wl == "W"),
            losses = sum(wl == "L"),
            across(contains("fg3"), sum, .names = "{.col}_played")) %>%
  ungroup() %>%
  left_join(games_players %>%
              unnest_longer(dnp_game) %>%
              group_by(slug_team, player_name = dnp_game) %>%
              summarise(wins_dnp = sum(wl == "W"),
                        losses_dnp = sum(wl == "L"), 
                        across(contains("fg3"), sum, .names = "{.col}_dnp")) %>%
              ungroup()) %>%
  pivot_longer(cols = contains("fg3"),
               names_to = c("result", "team", "player_status"),
               values_to = "value",
               names_sep =  "_") %>%
  pivot_wider(names_from = result,
              values_from = value) %>%
  mutate(fg3pct = fg3m / fg3a)

stats_players %>%
  pivot_wider(names_from = team,
              values_from = starts_with("fg3")) %>%
  filter(wins + losses >= 30,
         wins_dnp + losses_dnp >= 15) %>%
  mutate(wins = ifelse(player_status == "played", wins, wins_dnp),
         losses = ifelse(player_status == "played", losses, losses_dnp)) %>%
  select(slug_team, player_name, wins, losses, player_status, contains("pct")) %>%
  mutate(difference = fg3pct_team - fg3pct_opp) %>%
  group_by(slug_team, player_name) %>%
  mutate(diff_status = difference[which(player_status == "played")] - difference[which(player_status == "dnp")]) %>%
  ungroup() %>%
  arrange(diff_status)
