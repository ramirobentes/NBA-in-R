library(tidyverse)
library(hoopR)
library(janitor)
library(gt)

seasons <- year_to_season(2013:2023)
player_logs <- map_df(seasons, 
                          function(x){
                            nba_leaguegamelog(season = x, player_or_team = "P") %>%
                              pluck("LeagueGameLog")}) %>%
  clean_names()


pbps <- map_df(2013:2024, function(x){
  read_rds(glue::glue("https://github.com/ramirobentes/nba_pbp_data/raw/main/pbp-final-{x}/data.rds")) %>%
    mutate(season = x)})
lineups_all <- map_df(2013:2024, function(x){
  read_csv(glue::glue("https://github.com/ramirobentes/nba_pbp_data/raw/main/lineup-final{x}/data.csv")) %>%
    mutate(season = x)})

times_players_all <- pbps %>%
  mutate(secs_prev = lag(secs_game, default = 0),
         .by = game_id) %>%
  filter(margin_before <= 5 & secs_game >= 2580) %>%
  mutate(secs_played = ifelse(secs_prev < 2580, 2580 - secs_prev, secs_game - secs_prev)) %>%
  pivot_longer(cols = c(lineup_home, lineup_away),
               names_to = "lineup_location",
               values_to = "lineup",
               names_prefix = "lineup_") %>%
  mutate(lineup_team = ifelse(lineup_location == "home", team_home, team_away)) %>%
  separate_rows(lineup, sep = ", ") %>%
  summarise(player_crunch = sum(secs_played) / 60,
            .by = c(season, lineup, lineup_team)) %>%
  rename(player_name = lineup,
         team = lineup_team) %>%
  full_join(lineups_all %>%
              separate_rows(lineup_team, sep = ", ") %>%
              summarise(player_total = sum(secs_played) / 60,
                        games_started = sum(period == 1 & stint == 1),
                        .by = c(season, lineup_team, team)) %>%
              rename(player_name = lineup_team)) %>%
  mutate(player_crunch = replace_na(player_crunch, 0))

games_crunch <- pbps %>%
  mutate(secs_prev = lag(secs_game, default = 0),
         .by = game_id) %>%
  filter(margin_before <= 5 & secs_game >= 2580) %>%
  mutate(secs_played = ifelse(secs_prev < 2580, 2580 - secs_prev, secs_game - secs_prev)) %>%
  summarise(crunch_total = sum(secs_played),
            .by = c(season, game_date, game_id, team_home, team_away)) %>%
  pivot_longer(cols = c(team_home, team_away),
               names_to = "team_location",
               values_to = "team") %>%
  summarise(crunch_team = sum(crunch_total) / 60,
            .by = c(season, team))


players_filtered <- times_players_all %>%
  filter(games_started >= 20,
         player_total >= 500) %>%
  arrange(player_crunch) %>%
  left_join(games_crunch) %>%
  separate(player_name, into = c("player_id", "player_name"), sep = " ", extra = "merge") %>%
  left_join(player_logs %>%
              distinct(team = team_abbreviation, team_id)) %>%
  mutate(player_headshot = glue::glue("https://cdn.nba.com/headshots/nba/latest/260x190/{player_id}.png"),
         team_logo = glue::glue('https://cdn.nba.com/logos/nba/{team_id}/global/L/logo.svg')) %>%
  mutate(player_headshot = ifelse(player_name == "Willie Green", "https://a.espncdn.com/combiner/i?img=/i/headshots/nba/players/full/2004.png&w=350&h=254", player_headshot)) %>%
  mutate(overlaid = glue::glue("<div class='parent'><img src='{team_logo}' class='background-image'/>
    <div class='inner'> <img src='{player_headshot}' width=100 height=66/>    </div></div><style> .parent {{width: 120px;height: 120px;position: relative;overflow: hidden; left:5px;}} .background-image {{position: absolute;top: -5px; left: 0; width: 100%;height: 100%; opacity: 0.5; }} .inner {{position: absolute;bottom: -5px;left: 10px; z-index: 1;}}</style>"),
         overlaid = map(overlaid, gt::html)) %>%
  select(season, player_name, overlaid, games_started, player_total, player_crunch, crunch_team)

players_filtered %>%
  head(10) %>%
  gt() %>%
  cols_align(align = "center") %>%
  tab_style(style = cell_text(weight = "bold",
                              color = "black"),
            locations = cells_column_labels(everything())) %>%
  gtExtras::gt_merge_stack(col1 = overlaid, col2 = player_name, palette = c("black", "black"), font_size = c("100px", "16px"),
                           font_weight = c("normal", "normal")) %>%
  fmt_number(columns = c(player_total, player_crunch, crunch_team), decimals = 1) %>%
  cols_label(overlaid = "player",
             games_started = "starts",
             player_total = "total min",
             player_crunch = "crunch min",
             crunch_team = "crunch min team") %>%
  cols_width(season ~ px(100),
             overlaid ~ px(130),
             games_started ~ px(100),
             player_total ~ px(100),
             player_crunch ~ px(100),
             crunch_team ~ px(100)) %>%
  tab_options(
    data_row.padding = px(5),
    table.width = px(700)) %>%
  tab_header(
    title = "Total crunch time minutes for players with at least 20 starts and 500 minutes in a season",
    subtitle = "Since 2012-13"
  ) %>%
  tab_style(style = list(cell_text(weight = "bold")),
            locations = cells_body(
              columns = c(player_crunch))) %>%
  opt_table_font(font = google_font("Inter"))
