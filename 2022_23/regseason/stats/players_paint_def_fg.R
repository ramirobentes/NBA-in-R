library(tidyverse)
library(hoopR)
library(janitor)
library(gt)

pbp2023 <- read_rds("https://github.com/ramirobentes/NBA-in-R/blob/master/2022_23/regseason/pbp/pbp-poss-rs23/data.rds?raw=true")
lineups2023 <- read_rds("https://github.com/ramirobentes/NBA-in-R/blob/master/2022_23/regseason/pbp/lineup-stats-rs23/data.rds?raw=true")

shots_season <- nba_shotchartdetail(season = "2022-23",
                                    player_id = 0) %>%
  pluck("Shot_Chart_Detail") %>%
  clean_names()

paint_def <- pbp2023 %>%
  semi_join(shots_season %>%
              filter(shot_zone_basic %in% c("Restricted Area", "In The Paint (Non-RA)")) %>%
              transmute(game_id = as.integer(game_id), number_original = as.integer(game_event_id))) %>%
  mutate(team_def = ifelse(slug_team == team_home, team_away, team_home),
         def_on = str_split(ifelse(slug_team == team_home, lineup_away, lineup_home), ", ")) %>%
  select(game_id, period, clock, msg_type, description, team_def, def_on) %>%
  left_join(lineups2023 %>%
              separate_rows(lineup, sep = ", ") %>%
              group_by(team_def = slug_team) %>%
              summarise(all_players = str_split(paste(unique(lineup), collapse = ", "), ", ")) %>%
              ungroup()) %>%
  mutate(def_off = map2(all_players, def_on, setdiff)) %>%
  select(-all_players) %>%
  pivot_longer(cols = c(def_on, def_off),
               names_to = "floor",
               values_to = "player_name",
               names_prefix = "def_") %>%
  unnest_longer(player_name) %>%
  count(team_def, player_name, floor, msg_type, name = "fg") %>%
  group_by(team_def, player_name, floor) %>%
  mutate(fga = sum(fg)) %>%
  ungroup() %>%
  filter(msg_type == 1) %>%
  select(-msg_type) %>%
  pivot_wider(names_from = floor,
              values_from = c(fg, fga)) %>%
  filter(fga_on >= 200) %>%
  mutate(pct_on = fg_on / fga_on,
         pct_off = fg_off / fga_off,
         difference = pct_on - pct_off) %>%
  arrange(difference) %>%
  select(-starts_with("fg"))

paint_def %>%
  head(15) %>%
  left_join(shots_season %>%
              arrange(-as.integer(game_id)) %>%
              distinct(player_name, player_id, team_id)) %>%
  mutate(player_headshot = glue::glue("https://cdn.nba.com/headshots/nba/latest/260x190/{player_id}.png"),
         team_logo = glue::glue("https://cdn.nba.com/logos/nba/{team_id}/global/L/logo.svg")) %>%
  select(player_headshot, player_name, team_logo, pct_on, pct_off, difference) %>%
  gt() %>%
  cols_align(align = "center") %>%
  text_transform(locations = cells_body(c(team_logo, player_headshot)),
                 fn = function(x){
                   web_image(url = x,
                             height = 50)}) %>%
  tab_style(style = cell_text(weight = "bold"),
            locations = cells_column_labels(everything())) %>%
  fmt_percent(columns = c(pct_on, pct_off, difference), decimals = 1) %>%
  cols_label(player_headshot = "",
             player_name = "Player",
             team_logo = "",
             pct_on = "% On",
             pct_off = "% Off",
             difference = "Diff.") %>%
  cols_width(player_headshot ~ px(20),
             team_logo ~ px(20),
             player_name ~ px(40),
             pct_on ~ px(20),
             pct_off ~ px(20),
             difference ~ px(20)) %>%
  tab_style(style = list(
    cell_borders(side = "left", 
                 color = "black",
                 weight = px(1))),
    locations = cells_body(columns = c(team_logo)))  %>%
  tab_style(style = list(
    cell_borders(side = "right", 
                 color = "black",
                 weight = px(2))),
    locations = cells_body(columns = c(team_logo))) %>%
  tab_header(title = md("**Opponents FG% in the paint while player is on/off the floor:**"),
             subtitle = "Minimum 200 opp. paint FGA with player on") %>%
  tab_options(
    table.width = px(580),
    data_row.padding = px(1))
