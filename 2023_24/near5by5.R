library(tidyverse)
library(hoopR)
library(janitor)
library(gt)

seasons <- year_to_season(2000:2023)
player_logs_raw <- map_df(seasons, 
                          function(x){
                            nba_leaguegamelog(season = x, player_or_team = "P") %>%
                              pluck("LeagueGameLog")})

player_logs <- player_logs_raw %>%
  clean_names() %>%
  mutate(across(c(min:plus_minus), as.numeric))

pending_one <- player_logs %>%
  mutate(season = as.numeric(str_sub(season_id, -4, -1)),
         season = glue::glue("{season}-{str_sub(season + 1, -2, -1)}")) %>%
  select(season, game_date, game_id, matchup, player_id, player_name, team_id, team_abbreviation, pts, reb, ast, stl, blk) %>%
  pivot_longer(cols = c(pts:blk),
               names_to = "category",
               values_to = "value") %>%
  filter(sum(value >= 5) >= 4,
         sum(value >= 4) == 5,
         .by = c(season, game_date, game_id, player_id, player_name, team_id))

near5 <- pending_one %>%
  filter(sum(value >= 5) == 4,
         sum(value >= 4) == 5,
         .by = c(game_id, player_id)) %>%
  anti_join(pending_one %>%
              filter(sum(value >= 5) == 5,
                     sum(value >= 4) == 5,
                     .by = c(game_id, player_id)) %>%
              select(game_date),
            by = join_by(game_date < game_date)) %>%
  pivot_wider(names_from = category,
              values_from = value) %>%
  select(-game_id) %>%
  mutate(player_headshot = glue::glue("https://cdn.nba.com/headshots/nba/latest/260x190/{player_id}.png"),
         team_logo = glue::glue('https://cdn.nba.com/logos/nba/{team_id}/global/L/logo.svg')) %>%
  select(-c(player_id, team_id))


near5 %>%
  mutate(overlaid = (glue::glue("<div class='parent'><img src='{team_logo}'/><div class='inner'><img src='{player_headshot}'width=100 height=66/></div><style>.parent{{width:120px;height:120px;position:relative;z-index:0;left:42px}}.inner{{position:absolute;z-index:0;bottom:0;left:10px;}}</style></div>")),
         overlaid = map(overlaid, gt::html)) %>%
  select(season, game_date, matchup, player_name, overlaid, pts, reb, ast, stl, blk) %>%
  gt() %>%
  cols_align(align = "center") %>%
  tab_style(style = cell_text(weight = "bold",
                              color = "black"),
            locations = cells_column_labels(everything())) %>%
  gtExtras::gt_merge_stack(col1 = overlaid, col2 = player_name, palette = c("black", "black"), font_size = c("100px", "16px"),
                           font_weight = c("normal", "normal")) %>%
  cols_label(overlaid = "player",
             game_date = "date",
             pts = "points",
             reb = "rebounds",
             ast = "assists",
             stl = "steals",
             blk = "blocks") %>%
  tab_options(
    data_row.padding = px(5),
    table.width = px(1000)) %>%
  tab_style(style = list(cell_text(weight = "normal")),
            locations = cells_body(
              columns = everything())) %>%
  tab_style(style = list(cell_text(color = "red")), 
            locations = cells_body(columns = stl, 
                                   rows = stl < 5)) %>%
  tab_style(style = list(cell_text(color = "red")), 
            locations = cells_body(columns = ast, 
                                   rows = ast < 5)) %>%
  tab_style(style = list(cell_text(color = "red")), 
            locations = cells_body(columns = blk, 
                                   rows = blk < 5)) %>%
  opt_table_font(font = google_font("Inter"))
