library(tidyverse)
library(hoopR)
library(janitor)
library(future)
library(gt)

seasons <- year_to_season(1976:2021)
plan(multisession)
playoffs_logs <- furrr::future_map_dfr(seasons, ~ nba_leaguegamelog(season = ., 
                                                                    player_or_team = "T",
                                                                    season_type = "Playoffs")) %>%
  pluck("LeagueGameLog")

playoffs_logs %>%
  clean_names() %>%
  mutate(across(c(min:plus_minus), as.numeric)) %>%
  group_by(season_id, game_id) %>%
  summarise(total_min = sum(min)) %>%
  ungroup() %>%
  count(season_id, overtime = total_min > 480) %>%
  group_by(season_id) %>%
  mutate(total_games = sum(n),
         ot_pct = n / total_games) %>%
  ungroup() %>%
  filter(overtime) %>%
  arrange(ot_pct) %>%
  mutate(season = as.numeric(str_sub(season_id, -4, -1)),
         season = glue::glue("{season}-{str_sub(season + 1, -2, -1)}")) %>%
  select(season, n, total_games, ot_pct) %>%
  head(20) %>%
  gt() %>%
  cols_align(
    align = "center",
    columns = everything()
  ) %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_column_labels(everything())
  ) %>% 
  cols_label(
    season = "Season",
    n = "OT games",
    total_games = "Total games",
    ot_pct = "% of OT games"
  ) %>%
  fmt_percent(
    columns = ot_pct,
    decimals = 2
  ) %>%
  tab_header(
    title = md("**Percentage of overtime games in the NBA Playoffs**"),
    subtitle = "Since the 1976-77 season"
  ) %>%
  tab_options(
    table.width = px(500),
    data_row.padding = px(4)
  )
