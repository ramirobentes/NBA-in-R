library(tidyverse)
library(hoopR)
library(janitor)
library(gt)

lineup_stats <- read_csv("https://github.com/ramirobentes/NBA-in-R/blob/master/lineup-stats/data.csv?raw=true")
pbp_final_gt <- read_csv("https://github.com/ramirobentes/NBA-in-R/releases/download/pbp-final-gt-7eaf9a2/data.csv",
                         col_types = c(clock = "c",
                                       start_poss = "c"))

player_logs <- nba_leaguegamelog(season = "2021-22", player_or_team = "P") %>%
  pluck("LeagueGameLog") %>%
  clean_names()
  
  ts_clutch <- pbp_final_gt %>%
  filter(msg_type %in% c(1:3)) %>%
  count(player_name = player1, shot_type = ifelse(msg_type == 3, "fta", "fga"),
        clutch = margin_before <= 5 & secs_passed_game >= 2580) %>%
  pivot_wider(names_from = shot_type,
              values_from = n,
              values_fill = 0) %>%
  mutate(ts_att = fga + (0.44 * fta))

poss_clutch <- pbp_final_gt %>%
  filter(poss_home + poss_away == 1) %>%
  mutate(lineup_poss = ifelse(poss_home == 1, lineup_home, lineup_away)) %>%
  separate_rows(lineup_poss, sep = ", ") %>%
  count(player_name = lineup_poss, clutch = margin_before <= 5 & secs_passed_game >= 2580, name = "poss")

compare_tsa <- ts_clutch %>%
  left_join(poss_clutch) %>%
  select(-c(fga, fta)) %>%
  pivot_wider(names_from = clutch,
              values_from = c(ts_att, poss)) %>%
  filter(poss_FALSE >= 2000,
         poss_TRUE >= 120) %>%
  mutate(tsa100_reg = round(ts_att_FALSE / poss_FALSE * 100, 2),
         tsa100_clutch = round(ts_att_TRUE / poss_TRUE * 100, 2),
         difference = tsa100_clutch - tsa100_reg) %>%
  left_join(player_logs %>%
              arrange(desc(as.Date(game_date))) %>%
              distinct(player_name, player_id, .keep_all = TRUE) %>%
              mutate(headshot_url = glue::glue("https://ak-static.cms.nba.com/wp-content/uploads/headshots/nba/latest/260x190/{player_id}.png"),
                     logo_url = glue::glue("https://cdn.nba.com/logos/nba/{team_id}/global/L/logo.svg")) %>%
              select(player_name, headshot_url, logo_url))

compare_tsa %>%
  mutate(rank_reg = dense_rank(-tsa100_reg),
         rank_clutch = dense_rank(-tsa100_clutch)) %>%
  arrange(-difference) %>%
  head(20) %>%
  select(headshot_url, player_name, logo_url, tsa100_reg, tsa100_clutch, difference, rank_reg, rank_clutch) %>%
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
  cols_label(headshot_url = "",
             logo_url = "Team",
             player_name = "Player",
             tsa100_reg = "Non-clutch",
             tsa100_clutch = "Clutch",
             difference = "Diff") %>% 
  cols_width(headshot_url ~ px(20),
             logo_url ~ px(20),
             player_name ~ px(40),
             tsa100_reg ~ px(25),
             tsa100_clutch ~ px(25),
             difference ~ px(25)) %>%
  tab_spanner(label = md("**TSA per 100 poss**"),
    columns = c(tsa100_reg, tsa100_clutch)) %>%
  tab_style(style = list(
      cell_borders(side = "left", 
                   color = "black",
                   weight = px(2))),
      locations = cells_body(columns = c(tsa100_reg))) %>% 
  tab_style(style = list(
    cell_borders(side = "left", 
                 color = "black",
                 weight = px(1))),
    locations = cells_body(columns = c(logo_url))) %>% 
  tab_header(title = md("**True Shooting Attempts per 100 possessions<br>Clutch vs Non-Clutch**"),
             subtitle = "Number in parenthesis represents rank among qualifying players") %>%
  tab_options(
    table.width = px(520),
    column_labels.font.size = 12,
    data_row.padding = px(1)) %>%
  cols_merge(columns = c(tsa100_reg, rank_reg)) %>%
  cols_merge(columns = c(tsa100_clutch, rank_clutch)) %>%
  tab_style(style = list(cell_text(weight = "bold")),
            locations = cells_body(
              columns = difference)) %>%
  text_transform(
    locations = cells_body(columns = c(tsa100_reg, tsa100_clutch)),
    fn = function(x){
      stat_ts <- word(x, 1)
      rank_ts <- paste0("(", word(x, -1), ")")
      glue::glue(
        "<div style='line-height:10px'><span style='font-weight:bold;font-size:14px'>{stat_ts}</div>
        <div style='line-height:17px'><span style ='font-weight:bold;color:grey;font-size:10px'>{rank_ts}</span></div>")}) %>%
  data_color(
    columns = c(difference),
    colors = scales::col_numeric(
      palette = "RdYlGn",
      domain = c(min(compare_tsa$difference), max(compare_tsa$difference))
    )) %>%
  tab_footnote(
    footnote = "Minimum 2000 non-clutch and 120 clutch possessions",
    locations = cells_title(groups = "subtitle"))
