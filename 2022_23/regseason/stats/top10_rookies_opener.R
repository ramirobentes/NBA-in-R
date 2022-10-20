library(tidyverse)
library(hoopR)
library(janitor)

draft_history <- map(c(2000:2022), ~ nba_drafthistory(season = .)) %>%
  bind_rows() %>%
  pluck("DraftHistory") %>%
  clean_names()

seasons <- paste(2000:2022, str_pad(01:23, pad = 0, width = 2), sep = "-")
player_logs_multi <- map_df(seasons, ~ nba_leaguegamelog(season = ., player_or_team = "P") %>%
                               pluck("LeagueGameLog")) %>%
  clean_names() %>%
  mutate(across(c(min:fantasy_pts), as.numeric),
         game_date = as.Date(game_date))

player_logs_multi %>%
  semi_join(player_logs_multi %>%
              distinct(season_id, game_date, game_id, team_id) %>%
              group_by(season_id, team_id) %>%
              filter(row_number() == 1) %>%
              ungroup()) %>%
  select(player_id, season_id, game_id, game_date, player_name, pts) %>%
  mutate(season = str_sub(season_id, -4, -1)) %>%
  semi_join(draft_history %>%
              filter(as.numeric(overall_pick) <= 10) %>%
              transmute(season, player_id = person_id, overall_pick)) %>%
  group_by(season = paste(season, str_pad(as.numeric(str_sub(season, -2, -1)) + 1, pad = 0, width = 2), sep = "-")) %>%
  summarise(number_players = n(),
            pts_avg = round(mean(pts), 2),
            players = paste(paste0(player_name, " (", pts, ")"), collapse = ", ")) %>%
  ungroup %>%
  arrange(-pts_avg)
