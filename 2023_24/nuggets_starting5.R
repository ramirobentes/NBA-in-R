library(tidyverse)
library(glue)
library(slider)
library(gt)

player_logs <- nba_playergamelogs(season = "2023-24", player_id = "") %>%
  pluck("PlayerGameLogs") %>%
  clean_names() %>%
  mutate(across(c(player_id, team_id), as.numeric))

pbp_rs <- map_df(2023:2024,
                 ~ read_rds(glue("https://github.com/ramirobentes/nba_pbp_data/raw/main/pbp-final-{.}/data.rds")))
pbp_po <- map_df(2023:2024, 
                 ~ read_rds(glue("https://github.com/ramirobentes/nba_pbp_data/raw/main/pbp-final-playoffs{.}/data.rds")))

lineups_rs <- map_df(2023:2024,
                     ~ read_csv(glue("https://github.com/ramirobentes/nba_pbp_data/raw/main/lineup-final{.}/data.csv")))
lineups_po <- map_df(2023:2024, 
                     ~ read_csv(glue("https://github.com/ramirobentes/nba_pbp_data/raw/main/lineup-final-playoffs{.}/data.csv")))

players_in <- c("Jamal Murray", "Kentavious Caldwell-Pope", "Michael Porter Jr.", "Aaron Gordon", "Nikola Jokic")

lineup_stats <- bind_rows(lineups_rs, lineups_po) %>%
  filter(str_detect(lineup_team, paste0("(?=.*", (paste(players_in, collapse = ")(?=.*")), ")"))) %>%
  select(-game_date) %>%
  left_join(bind_rows(pbp_rs, pbp_po) %>%
              distinct(game_id, game_date)) %>%
  left_join(player_logs %>%
              distinct(opp_id = team_id, opp = team_abbreviation))

game_stretches <- lineup_stats %>%
  summarise(across(c(secs_played:pts_opp), sum),
            .by = c(game_date, game_id, team, opp_id)) %>%
  arrange(game_date) %>%
  mutate(plus_minus = pts_team - pts_opp) %>%
  mutate(ngames_sec = slide_dbl(secs_played, sum, .before = 5),
         ngames_ptsteam = slide_dbl(pts_team, sum, .before = 5),
         ngames_ptsopp = slide_dbl(pts_opp, sum, .before = 5),
         ngames_pm = slide_dbl(plus_minus, sum, .before = 5),
         opps = slide(opp_id, paste, .before = 5),
         dates = slide(game_date, paste, .before = 5)) %>%
  filter(row_number() >= 6) %>%
  arrange(ngames_pm) %>%
  filter(ngames_pm == min(ngames_pm) | game_date < "2024-04-18") %>%
  mutate(date_start = map_chr(dates, min),
         date_end = map_chr(dates, max),
         minutes = ngames_sec / 60) %>%
  select(date_start, date_end, minutes, pts_team = ngames_ptsteam, pts_opp = ngames_ptsopp, plus_minus = ngames_pm, opps) 
  
players_headshot <- players_in %>%
  enframe(name = NULL, value = "player_name") %>%
  left_join(player_logs %>%
              distinct(player_id, player_name)) %>%
  mutate(headshot_url = glue::glue("<img src='https://cdn.nba.com/headshots/nba/latest/260x190/{player_id}.png' width=80 height=50> &nbsp")) %>%
  summarise(headshot_url = paste(players_headshot, collapse = " ")) %>%
  pull(headshot_url)


game_stretches %>%
  head(10) %>%
  mutate(opps = map(opps, ~ glue::glue("<img src='https://cdn.nba.com/logos/nba/{.x}/global/L/logo.svg' style='margin-right:-25px;' width=55 height=39>")),
         opps = map_chr(opps, ~ paste(., collapse = " ")),
         # opps = paste0("<div class='images'>", opps, "</div>"),
         opps = map(opps, gt::html)) %>%
  gt() %>%
  cols_align(align = "center") %>%
  fmt_number(columns = c(minutes), decimals = 1) %>%
  tab_header(
    title = gt::html(glue::glue("<p class='' style='margin: 0px;'></p>{players_headshot}<p class='' style='margin: 5px;'></p><b>Worst 6-game stretches for Denver's main lineup"))) %>%
  cols_label(date_start = "Start",
             date_end = "End",
             minutes = "Minutes",
             pts_team = "Pts Team",
             pts_opp = "Pts Opp",
             plus_minus = "+/-",
             opps = "Opps") %>%
  cols_width(date_start ~ px(45),
             date_end ~ px(45),
             minutes ~ px(40),
             pts_team ~ px(40),
             pts_opp ~ px(40),
             plus_minus ~ px(30),
             opps ~ px(80)) %>%
  tab_options(
    table.width = px(900),
    data_row.padding = px(0)) %>%
  tab_style(style = list(cell_text(weight = "bold")),
            locations = cells_body(
              columns = c(plus_minus))) %>%
  tab_style(
    style = cell_text(color = "red"),
    locations = cells_body(
      columns = c(plus_minus),
      rows = plus_minus < 0
    )
  ) %>%
  tab_style(
    style = cell_text(color = "darkgreen"),
    locations = cells_body(
      columns = c(plus_minus),
      rows = plus_minus > 0
    )
  ) %>%
  opt_table_font(font = google_font("Inter"))


series_stats <- lineup_stats %>%
  filter(str_sub(game_id, 3, 3) == 4) %>%
  group_by(season = as.integer(paste0("20", str_sub(game_id, 4, 5))), series = str_sub(game_id, -3, -3), opp) %>%
  summarise(games = n_distinct(game_id),
            across(c(secs_played, pts_team, pts_opp), sum)) %>%
  ungroup() %>%
  mutate(season = paste(season, str_sub(season + 1, 3, 4), sep = "-"),
         plus_minus = pts_team - pts_opp)

series_stats %>%
  left_join(player_logs %>%
              distinct(opp_id = team_id, opp = team_abbreviation)) %>%
  mutate(opp_url = glue("https://cdn.nba.com/logos/nba/{opp_id}/global/L/logo.svg"),
         # opp_url = gt::html(opp_url),
         minutes = secs_played / 60) %>%
  select(season, series, opp_url, games, minutes, pts_team, pts_opp, plus_minus) %>%
  gt() %>%
  cols_align(align = "center") %>%
  text_transform(locations = cells_body(c(opp_url)),
                 fn = function(x){
                   web_image(url = x,
                             height = 65)}) %>%
  fmt_number(columns = c(minutes), decimals = 1) %>%
  tab_header(
    title = gt::html(glue::glue("<p class='' style='margin: 0px;'></p>{players_headshot}<p class='' style='margin: 5px;'></p><b>Plus-minus for Denver's main lineup in every playoffs series"))) %>%
  cols_label(season = "Season",
             series = "Series",
             opp_url = "Opp",
             games = "Games",
             minutes = "Minutes",
             pts_team = "Pts Team",
             pts_opp = "Pts Opp",
             plus_minus = "+/-") %>%
  cols_width(season ~ px(40),
             series ~ px(30),
             opp_url ~ px(40),
             games ~ px(30),
             minutes ~ px(35),
             pts_team ~ px(40),
             pts_opp ~ px(40),
             plus_minus ~ px(30)) %>%
  tab_options(
    table.width = px(900),
    data_row.padding = px(0)) %>%
  tab_style(style = list(cell_text(weight = "bold")),
            locations = cells_body(
              columns = c(plus_minus))) %>%
  tab_style(
    style = cell_text(color = "red"),
    locations = cells_body(
      columns = c(plus_minus),
      rows = plus_minus < 0
    )
  ) %>%
  tab_style(
    style = cell_text(color = "darkgreen"),
    locations = cells_body(
      columns = c(plus_minus),
      rows = plus_minus > 0
    )
  ) %>%
  opt_table_font(font = google_font("Inter"))
