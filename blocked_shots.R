library(tidyverse)
library(hoopR)
library(janitor)
library(gt)

pbp_final_gt <- read_rds("https://github.com/ramirobentes/NBA-in-R/blob/master/pbp-final-gt/data.rds?raw=true")

player_logs <- nba_leaguegamelog(season = "2021-22", player_or_team = "P") %>%
  pluck("LeagueGameLog") %>%
  clean_names() %>%
  mutate(across(c(min:fantasy_pts), as.numeric))

blocks_rebs <- pbp_final_gt %>%
  filter(msg_type != 9) %>%
  filter((msg_type == 2 & !is.na(player3)) | (lag(msg_type == 2) & !is.na(lag(player3)))) %>%
  select(game_id, period, clock, number_event, msg_type, act_type, team_home, team_away, slug_team, player1, player3, description, desc_value)

block_results <- blocks_rebs %>%
  filter(msg_type == 2) %>%
  transmute(game_id, period, clock, team_block = ifelse(slug_team == team_home, team_away, team_home), player_block = player3,
            sequence = row_number()) %>%
  left_join(blocks_rebs %>%
              filter(msg_type == 4) %>%
              mutate(type_reb = ifelse(is.na(player1), "out_bounds", "inbound")) %>%
              transmute(game_id, period, team_reb = slug_team, type_reb, off_rebound = desc_value,
                        sequence = row_number())) %>%
  count(player_name = player_block, type_reb, keeps_reb = ifelse(team_reb == team_block, "team", "opp"), name = "blocks")

blocks_table <- block_results %>%
  group_by(player_name) %>%
  mutate(total_blocks = sum(blocks)) %>%
  ungroup() %>%
  pivot_wider(names_from = c(type_reb, keeps_reb),
              values_from = blocks,
              values_fill = 0) %>%
  filter(total_blocks >= 60) %>%
  mutate(inbound_pct = round((inbound_team + inbound_opp) / total_blocks, 3),
         reb_pct = round((inbound_team + out_bounds_team) / total_blocks, 3)) %>%
  arrange(-reb_pct)

blocks_table %>%
  head(20) %>%
  left_join(player_logs %>%
              arrange(desc(as.Date(game_date))) %>%
              distinct(player_name, player_id, .keep_all = TRUE) %>%
              mutate(headshot_url = glue::glue("https://ak-static.cms.nba.com/wp-content/uploads/headshots/nba/latest/260x190/{player_id}.png"),
                     logo_url = glue::glue("https://cdn.nba.com/logos/nba/{team_id}/global/L/logo.svg")) %>%
              select(player_name, headshot_url, logo_url)) %>%
  select(headshot_url, player_name, logo_url, everything()) %>%
  gt() %>%
  cols_align(align = "center") %>%
  text_transform(locations = cells_body(c(headshot_url)),
                 fn = function(x){ 
                   web_image(url = x,
                             height = 40)}) %>%
  text_transform(locations = cells_body(c(logo_url)),
                 fn = function(x){ 
                   web_image(url = x,
                             height = 30)}) %>%
  tab_style(style = cell_text(weight = "bold"),
            locations = cells_column_labels(everything())) %>%
  fmt_percent(columns = c(reb_pct, inbound_pct), decimals = 1) %>%
  cols_label(headshot_url = "",
             logo_url = "Team",
             player_name = "Player",
             total_blocks = "Total",
             inbound_opp = "In+Opp",
             inbound_team = "In+Team",
             out_bounds_opp = "Out+Opp",
             out_bounds_team = "Out+Team",
             inbound_pct = "Inbound %",
             reb_pct = "Team reb %") %>%
  cols_width(headshot_url ~ px(20),
             logo_url ~ px(20),
             player_name ~ px(40),
             total_blocks ~ px(20),
             inbound_opp ~ px(20),
             inbound_team ~ px(20),
             out_bounds_opp ~ px(20),
             out_bounds_team ~ px(20),
             inbound_pct ~ px(25),
             reb_pct ~ px(25)) %>%
  tab_style(style = list(
    cell_borders(side = "left", 
                 color = "black",
                 weight = px(2))),
    locations = cells_body(columns = c(total_blocks))) %>% 
  tab_style(style = list(
    cell_borders(side = "left", 
                 color = "black",
                 weight = px(1))),
    locations = cells_body(columns = c(logo_url, inbound_pct))) %>% 
  tab_header(title = md("**What NBA players are best at getting possession after a block?**"),
             subtitle = "According to % of rebounds that go to the defensive team after a block (inbounds or out of bounds)") %>%
  tab_footnote(
    footnote = "Minimum 60 blocks",
    locations = cells_title(groups = "subtitle")) %>%
  tab_options(
    table.width = px(750),
    column_labels.font.size = 12,
    table.font.size = 14,
    heading.subtitle.font.size = 14,
    data_row.padding = px(1))
