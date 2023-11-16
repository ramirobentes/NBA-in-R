library(tidyverse)
library(hoopR)
library(janitor)
library(gt)

lineups_stats <- read_csv("https://github.com/ramirobentes/nba_pbp_data/raw/main/lineup-final2024/data.csv")

team_logs <- nba_leaguegamelog(season = "2023-24", player_or_team = "T") %>%
  pluck("LeagueGameLog") %>%
  clean_names() %>%
  mutate(across(c(team_id), as.numeric))


player_teammates <- lineups_stats %>%
  separate_rows(lineup_team, sep = ", ") %>%
  group_by(game_id, period, stint, team) %>%
  mutate(player2 = str_split(paste(lineup_team, collapse = ", "), ", "),
         player2 = map2(player2, lineup_team, setdiff)) %>%
  ungroup() %>%
  rename(player1 = lineup_team) %>%
  select(game_id, period, stint, team, player1, secs_played, poss_team, pts_team, poss_opp, pts_opp, player2)

time_shared <- player_teammates %>%
  unnest_longer(player2) %>%
  group_by(team, player1, player2) %>%
  summarise(secs_shared = sum(secs_played)) %>%
  ungroup() %>%
  left_join(player_teammates %>%
              group_by(team, player1) %>%
              summarise(secs_player1 = sum(secs_played)) %>%
              ungroup()) %>%
  left_join(player_teammates %>%
              group_by(team, player2 = player1) %>%
              summarise(secs_player2 = sum(secs_played)) %>%
              ungroup()) %>%
  mutate(secs_pct_player1 = secs_shared / secs_player1)

time_shared %>%
  filter(secs_player1 >= 150 * 60,
         secs_player2 >= 150 * 60) %>%
  arrange(-secs_pct_player1) %>%
  head(10) %>%
  select(-secs_player2) %>%
  separate(player1, into = c("player1_id", "player1_name"), sep = " ", extra = "merge") %>%
  separate(player2, into = c("player2_id", "player2_name"), sep = " ", extra = "merge") %>%
  left_join(team_logs %>% distinct(team_id, team = team_abbreviation)) %>%
  mutate(player1_headshot = glue::glue("https://cdn.nba.com/headshots/nba/latest/260x190/{player1_id}.png"),
         player2_headshot = glue::glue("https://cdn.nba.com/headshots/nba/latest/260x190/{player2_id}.png"),
         team_logo = glue::glue('https://cdn.nba.com/logos/nba/{team_id}/global/L/logo.svg')) %>%
  mutate(across(c(secs_player1, secs_shared), 
                ~ paste0(floor(. / 60), ":", str_pad(round(. %% 60, 0), side = "left", width = 2, pad = 0)))) %>%
  select(team_logo, player1_headshot, player1_name, player2_headshot, player2_name, secs_player1, secs_shared, secs_pct_player1) %>%
  gt() %>%
  cols_align(align = "center") %>%
  text_transform(locations = cells_body(c(team_logo, player1_headshot, player2_headshot)),
                 fn = function(x){
                   web_image(url = x,
                             height = 60)}) %>%
  gtExtras::gt_merge_stack(col1 = player1_headshot, col2 = player1_name, palette = c("black", "black"), font_size = c("17px", "14px")) %>%
  gtExtras::gt_merge_stack(col1 = player2_headshot, col2 = player2_name, palette = c("black", "black"), font_size = c("17px", "14px")) %>%
  fmt_percent(columns = c(secs_pct_player1), decimals = 2) %>%
  cols_label(team_logo = "Team",
             player1_headshot = "Player",
             secs_player1 = "Time Player",
             player2_headshot = "Teammate",
             secs_shared = "Time Player w/ Teammate",
             secs_pct_player1 = "% of Time") %>%
  tab_header(
    title = "Players with biggest % of time on court shared with teammate",
    subtitle = "Both players with minimum 150 minutes played") %>%
  cols_width(team_logo ~ px(110),
             player1_headshot ~ px(120),
             player2_headshot ~ px(120),
             secs_player1 ~ px(120),
             secs_shared ~ px(120),
             secs_pct_player1 ~ px(120)) %>%
  tab_style(style = cell_text(weight = "bold"),
            locations = cells_column_labels(everything())) %>%
  tab_options(
    data_row.padding = px(5),
    table.width = px(900)) 
