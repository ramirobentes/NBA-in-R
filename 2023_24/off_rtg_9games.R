library(tidyverse)
library(hoopR)
library(janitor)
library(gt)
library(gtExtras)

advbox_function <- function(x){
  nba_teamgamelogs(season = x, measure_type = "Advanced", team_id = "") %>%
    pluck("TeamGameLogs") %>%
    clean_names()
}

tradbox_function <- function(x){
  nba_leaguegamelog(season = x, player_or_team = "T") %>%
    pluck("LeagueGameLog") %>%
    clean_names()
}

advbox <- map_df(year_to_season(1996:2023), advbox_function)
tradbox <- map_df(year_to_season(1996:2023), tradbox_function)

off_rtg <- tradbox %>%
  transmute(team_id, team_abbreviation, game_id, game_date, pts = as.integer(pts)) %>%
  left_join(advbox %>%
              transmute(team_id, team_abbreviation, game_id, poss = as.integer(poss), season = season_year)) %>%
  group_by(season, team_id) %>%
  filter(row_number() <= 9) %>%
  group_by(season, team_id, team_abbreviation) %>%
  summarise(games = n(),
            across(c(pts, poss), sum)) %>%
  ungroup()

off_rtg %>%
  filter(games == 9) %>%
  mutate(pts100 = pts / poss * 100) %>%
  arrange(-pts100) %>%
  head(10) %>%
  mutate(team_logo = glue::glue("https://cdn.nba.com/logos/nba/{team_id}/global/L/logo.svg"), .before = games) %>%
  select(-c(team_abbreviation, team_id)) %>%
  gt() %>%
  cols_align(align = "center") %>%
  text_transform(locations = cells_body(c(team_logo)),
                 fn = function(x){
                   web_image(url = x,
                             height = 65)}) %>%
  tab_style(style = cell_text(weight = "bold"),
            locations = cells_column_labels(everything())) %>%
  fmt_number(columns = c(pts100), use_seps = TRUE, decimals = 2) %>%
  cols_label(season = "Season",
             team_logo = "Team",
             games = "Games",
             pts = "Total Points",
             poss = "Total Poss",
             pts100 = "Off Rtg") %>%
  cols_width(season ~ px(30),
             team_logo ~ px(30),
             games ~ px(30),
             pts ~ px(30),
             poss ~ px(30),
             pts100 ~ px(40)) %>%
  tab_header(title = md("**Best offensive ratings through the first 9 games of the season**"),
             subtitle = "Since 1996-97") %>%
  tab_options(
    table.width = px(650),
    data_row.padding = px(1))
