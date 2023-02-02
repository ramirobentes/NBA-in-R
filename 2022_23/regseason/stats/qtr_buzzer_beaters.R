library(tidyverse)
library(hoopR)
library(janitor)
library(gt)

pbp2023 <- read_rds("https://github.com/ramirobentes/NBA-in-R/blob/master/2022_23/regseason/pbp/pbp-poss-rs23/data.rds?raw=true")

team_logs <- nba_leaguegamelog(season = "2022-23", player_or_team = "T") %>%
  pluck("LeagueGameLog") %>%
  clean_names()

pbp2023 %>%
  filter(clock == "00:00.0",
         msg_type == 1,
         (period %in% c(1:3) | (period >= 4 & margin_before <= 0 & abs(margin_before) <= shot_pts))) %>%
  group_by(period, opp_team = ifelse(slug_team == team_home, team_away, team_home)) %>%
  summarise(times = n(),
            pts = sum(shot_pts)) %>%
  ungroup() %>%
  group_by(opp_team) %>%
  mutate(times_tot = sum(times),
         pts_tot = sum(pts)) %>%
  ungroup() %>%
  pivot_wider(names_from = period,
              values_from = c(times, pts),
              names_prefix = "q",
              values_fill = 0) %>%
  arrange(-times_tot, -pts_tot) %>%
  left_join(team_logs %>%
              distinct(team_id, opp_team = team_abbreviation) %>%
              mutate(team_logo = glue::glue('https://cdn.nba.com/logos/nba/{team_id}/global/L/logo.svg')) %>%
              select(opp_team, team_logo)) %>%
  head(10) %>%
  select(team_logo, starts_with("times_q"), starts_with("pts_q"), times_tot, pts_tot) %>%
  gt() %>%
  cols_align(align = "center") %>%
  text_transform(locations = cells_body(c(team_logo)),
                 fn = function(x){ 
                   web_image(url = x,
                             height = 50)}) %>%
  cols_label(team_logo = "Team Against",
             times_q1 = "Q1",
             times_q2 = "Q2",
             times_q3 = "Q3",
             times_q4 = "Q4",
             times_q5 = "OT1",
             times_tot = "Total") %>%
  tab_header(title = md("**Teams with most end-of-quarter buzzer beaters against (2022-23 season)**"),
             subtitle = "Only considered 4th quarter and OT shots to tie or take the lead") %>%
  cols_width(team_logo ~ px(150)) %>%
  tab_style(style = cell_text(weight = "bold"),
            locations = cells_column_labels(everything())) %>%
  cols_merge(columns = c(times_q1, pts_q1)) %>%
  cols_merge(columns = c(times_q2, pts_q2)) %>%
  cols_merge(columns = c(times_q3, pts_q3)) %>%
  cols_merge(columns = c(times_q4, pts_q4)) %>%
  cols_merge(columns = c(times_q5, pts_q5)) %>%
  cols_merge(columns = c(times_tot, pts_tot)) %>%
  text_transform(
    locations = cells_body(columns = c(times_q1, times_q2, times_q3, times_q4, times_q5, times_tot)),
    fn = function(x){
      times_bb <- word(x, 1)
      pts_bb <- paste0(word(x, -1), " pts")
      glue::glue(
        "<div style='line-height:10px'><span style='font-weight:bold;font-size:16px'>{times_bb}</div>
        <div style='line-height:17px'><span style ='font-weight:bold;color:grey;font-size:12px'>{pts_bb}</span></div>")}) %>%
  tab_options(
    table.width = px(700),
    data_row.padding = px(1)) %>%
  tab_style(style = list(
    cell_borders(side = "right", 
                 color = "black",
                 weight = px(1))),
    locations = cells_body(columns = c(team_logo)))  %>%
  tab_style(style = list(
    cell_borders(side = "left", 
                 color = "black",
                 weight = px(1))),
    locations = cells_body(columns = c(times_tot)))
