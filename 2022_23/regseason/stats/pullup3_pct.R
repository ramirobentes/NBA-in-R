library(tidyverse)
library(hoopR)
library(janitor)
library(future)
library(gt)

plan(multicore)
shooting_dash <- map_df(year_to_season(2013:2022), ~ hoopR::nba_playerdashptshots(season = ., player_id = 0) %>%
                        pluck("GeneralShooting") %>%
                        mutate(season = .x))

pullup_table <- shooting_dash %>%
  clean_names() %>%
  filter(shot_type == "Pull Ups") %>%
  mutate(across(c(g, fg3m, fg3a), as.integer)) %>%
  mutate(fg3_pct = fg3m / fg3a,
         fg3_game = fg3a / g) %>%
  mutate(player_name = str_split(player_name_last_first, ", "),
         player_name = map(player_name, rev),
         player_name =  map_chr(player_name, ~ paste(., collapse = " "))) %>%
  select(season, player_id, player_name, games = g, shot_type, fg3m, fg3a, fg3_pct, fg3_game) %>%
  filter(fg3a >= 200) %>%
  arrange(-fg3_pct) %>%
  mutate(player_headshot = glue::glue("https://cdn.nba.com/headshots/nba/latest/260x190/{player_id}.png"))

pullup_gt <- pullup_table %>%
  select(player_headshot, player_name, season, fg3_game,  fg3m, fg3a, fg3_pct) %>%
  head(15) %>%
  gt() %>%
  cols_align(align = "center") %>%
  text_transform(locations = cells_body(player_headshot),
                 fn = function(x){
                   web_image(url = x,
                             height = 60)}) %>%
  tab_style(style = cell_text(weight = "bold"),
            locations = cells_column_labels(everything())) %>%
  fmt_percent(columns = c(fg3_pct), decimals = 1) %>%
  fmt_number(columns = c(fg3_game), decimals = 1) %>%
  cols_label(player_headshot = "",
             player_name = "Player",
             season = "Season",
             fg3_game = "FG3/G",
             fg3m = "FG3M",
             fg3a = "FG3A",
             fg3_pct = "FG3%") %>%
  cols_width(player_headshot ~ px(30),
             player_name ~ px(40),
             season ~ px(25),
             fg3_game ~ px(20),
             fg3m ~ px(20),
             fg3a ~ px(20),
             fg3_pct ~ px(20)) %>%
  tab_header(title = md("**Best pull up 3pt % in a season, since 2013-14**"),
             subtitle = "Minimum 200 pull up 3s attempted") %>%
  tab_options(
    table.width = px(600),
    data_row.padding = px(1))
