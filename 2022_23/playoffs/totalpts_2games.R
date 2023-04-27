
library(tidyverse)
library(hoopR)
library(janitor)
library(future)
library(slider)
library(gt)

seasons <- year_to_season(1946:2022)

playoffs_logs <- furrr::future_map_dfr(seasons, ~ nba_leaguegamelog(season = .,
                                                                   player_or_team = "P",
                                                                   season_type = "Playoffs")) %>%
  pluck("LeagueGameLog") %>%
  clean_names() %>%
  mutate(across(c(min:plus_minus), as.numeric))

playoffs_series <- playoffs_logs %>%
  transmute(season = paste(str_sub(season_id, 2, 5), as.integer(str_sub(season_id, 4, 5)) + 1, sep = "-"),
            game_id, game_date = as.Date(game_date), team_id, team_abbreviation, matchup, player_id, player_name, pts, wl) %>%
  mutate(series = map_chr(str_extract_all(matchup, "[A-Z]{3}"), ~ paste(sort(.), collapse = " vs ")))

pts_2games <- playoffs_series %>%
  group_by(season, series, player_id) %>%
  mutate(sum_pts = slide_dbl(pts, sum, .before = 1),
         wins = slide_sum(wl == "W", before = 1)) %>%
  ungroup() %>%
  arrange(-sum_pts) %>%
  filter(wins == 2) %>%
  select(season, player_id, player_name, team_id, team_abbreviation, series, pts = sum_pts) %>%
  head(10)

pts_2games %>%
  mutate(player_headshot = glue::glue("https://cdn.nba.com/headshots/nba/latest/260x190/{player_id}.png"),
         team_logo = glue::glue("https://cdn.nba.com/logos/nba/{team_id}/global/L/logo.svg")) %>%
  select(season, player_headshot, player_name, team_logo, series, pts) %>%
  gt() %>%
  cols_align(align = "center") %>%
  text_transform(locations = cells_body(c(team_logo, player_headshot)),
                 fn = function(x){
                   web_image(url = x,
                             height = 50)}) %>%
  tab_style(style = cell_text(weight = "bold"),
            locations = cells_column_labels(everything())) %>%
  cols_label(season = "Season",
             player_headshot = "",
             player_name = "Player",
             team_logo = "Team",
             series = "Series",
             pts = "Total Points") %>%
  cols_width(season ~ px(40),
             player_headshot ~ px(25),
             team_logo ~ px(30),
             player_name ~ px(40),
             series ~ px(30),
             pts ~ px(30)) %>%
  tab_header(title = md("**Highest point total in back-to-back wins in NBA playoff history**"),
             subtitle = "In the same playoff series") %>%
  tab_options(
    table.width = px(700),
    data_row.padding = px(1))
