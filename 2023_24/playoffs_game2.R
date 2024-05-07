library(tidyverse)
library(janitor)
library(hoopR)

series_logs_raw <- map_df(year_to_season(1980:2023), ~ nba_teamgamelogs(season = ., team_id = "", season_type = "Playoffs") %>%
                        pluck("TeamGameLogs")) %>%
  clean_names()

series_logs <- series_logs_raw %>%
  mutate(team_location = ifelse(str_detect(matchup, "\\@"), "away", "home"),
         series = str_extract_all(matchup, "[A-Z]{3}"),
         series = map(series, sort),
         series = map_chr(series, ~ paste(., collapse = " - ")),
         year = year(game_date),
         season = glue::glue("{year - 1}-{str_sub(year, 3, 4)}"),
         game_date = as.Date(game_date),
         across(c(pts, plus_minus), as.numeric)) %>%
  select(season, series, game_date, game_id, team_location, team_id, team_abb = team_abbreviation, wl, pts, plus_minus) 

game_numbers <- series_logs %>%
              distinct(game_date, game_id, season, series) %>%
              arrange(game_date) %>%
              mutate(game_number = row_number(),
                     .by = c(season, series))

g1_away_win <- series_logs %>%
  left_join(game_numbers) %>%
  filter(team_location == "home",
         wl == "L",
         game_number == 1)

g2_results <- series_logs %>%
  left_join(game_numbers) %>%
  filter(game_number == 2,
         team_location == "home") %>%
  semi_join(g1_away_win %>% select(season, series, team_id))

g2_results %>%
  filter(season != "2019-20") %>%
  count(wl)

g2_results %>%
  filter(season != "2019-20") %>%
  filter(wl == "W") %>%
  summarise(margin = mean(plus_minus))

g2_results %>%
  filter(season != "2019-20") %>%
  arrange(plus_minus)
