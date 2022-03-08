library(tidyverse)
library(hoopR)
library(janitor)
library(gt)

lineup_stats <- read_csv("https://github.com/ramirobentes/NBA-in-R/blob/master/lineup-stats/data.csv?raw=true")
pbp_final_gt <- read_csv("https://github.com/ramirobentes/NBA-in-R/releases/download/pbp-final-gt-aae44ed/data.csv",
                         col_types = c(clock = "c",
                                       start_poss = "c"))

player_logs <- nba_leaguegamelog(season = "2021-22", player_or_team = "P") %>%
  pluck("LeagueGameLog") %>%
  clean_names()

# Pick-6 ------------------------------------------------------------------

pick6_steal <- pbp_final_gt %>%
  filter(msg_type == 1,
         lag(msg_type) == 5,
         lag(act_type) == 1,
         lag(player3) == player1,
         is.na(player2),
         lag(secs_passed_game) + 3 >= secs_passed_game)

pick6_steal %>%
  count(player_name = player1, sort = T, name = "pick6s") %>%
  left_join(player_logs %>%
              arrange(desc(as.Date(game_date))) %>%
              distinct(player_name, player_id, .keep_all = TRUE) %>%
              select(player_name, player_id, team_id)) %>%
  filter(pick6s >= 5) %>%
  mutate(headshot_url = glue::glue("https://ak-static.cms.nba.com/wp-content/uploads/headshots/nba/latest/260x190/{player_id}.png"),
         logo_url = glue::glue("https://cdn.nba.com/logos/nba/{team_id}/global/L/logo.svg")) %>%
  select(headshot_url, player_name, logo_url, pick6s) %>%
  gt() %>%
  gt::text_transform(locations = cells_body(c(headshot_url)),
                     fn = function(x){ 
                       web_image(url = x,
                                 height = 40)}) %>%
  gt::text_transform(locations = cells_body(c(logo_url)),
                     fn = function(x){ 
                       web_image(url = x,
                                 height = 30)}) %>%
  cols_align(
    align = "center",
    columns = c(player_name, headshot_url, pick6s)
  ) %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_column_labels(everything())
  ) %>% 
  tab_style(
    style = cell_text(color = "darkgreen"),
    locations = cells_body(
      columns = pick6s
    )
  ) %>%
  cols_label(
    headshot_url = "",
    logo_url = "Team",
    player_name = "Player",
    pick6s = "Pick 6"
  ) %>% 
  cols_width(
    headshot_url ~ px(15),
    logo_url ~ px(20),
    player_name ~ px(30),
    pick6s ~ px(20)
  ) %>% 
  tab_header(
    title = md("**NBA Leaders in Returned Pick 6s**"),
    subtitle = "When player steals pass and makes FG within 3 seconds"
  ) %>%
  tab_options(
    table.width = px(480),
    data_row.padding = px(4)
  )

pick6_pass <- pbp_final_gt %>%
  filter(msg_type == 5,
         lead(msg_type) == 1,
         act_type == 1,
         player3 == lead(player1),
         is.na(lead(player2)),
         secs_passed_game + 3 >= lead(secs_passed_game))

pick6_pass %>%
  count(player_name = player1, sort = T, name = "pick6s") %>%
  left_join(player_logs %>%
              arrange(desc(as.Date(game_date))) %>%
              distinct(player_name, player_id, .keep_all = TRUE) %>%
              select(player_name, player_id, team_id)) %>%
  filter(pick6s >= 4) %>%
  mutate(headshot_url = glue::glue("https://ak-static.cms.nba.com/wp-content/uploads/headshots/nba/latest/260x190/{player_id}.png"),
         logo_url = glue::glue("https://cdn.nba.com/logos/nba/{team_id}/global/L/logo.svg")) %>%
  select(headshot_url, player_name, logo_url, pick6s) %>%
  gt() %>%
  gt::text_transform(locations = cells_body(c(headshot_url)),
                     fn = function(x){ 
                       web_image(url = x,
                                 height = 40)}) %>%
  gt::text_transform(locations = cells_body(c(logo_url)),
                     fn = function(x){ 
                       web_image(url = x,
                                 height = 30)}) %>%
  cols_align(
    align = "center",
    columns = c(player_name, headshot_url, pick6s)
  ) %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_column_labels(everything())
  ) %>% 
  tab_style(
    style = cell_text(color = "darkgreen"),
    locations = cells_body(
      columns = pick6s
    )
  ) %>%
  cols_label(
    headshot_url = "",
    logo_url = "Team",
    player_name = "Player",
    pick6s = "Pick 6"
  ) %>% 
  cols_width(
    headshot_url ~ px(15),
    logo_url ~ px(20),
    player_name ~ px(30),
    pick6s ~ px(20)
  ) %>% 
  tab_header(
    title = md("**NBA Leaders in Thrown Pick 6s**"),
    subtitle = "When player throws stolen pass that leads to FG within 3 seconds"
  ) %>%
  tab_options(
    table.width = px(480),
    data_row.padding = px(4)
  )
