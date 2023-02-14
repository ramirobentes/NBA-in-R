library(tidyverse)
library(hoopR)
library(janitor)
library(gt)

player_logs <- nba_leaguegamelog(season = "2022-23", player_or_team = "P") %>%
  pluck("LeagueGameLog") %>%
  clean_names() %>%
  mutate(across(c(min:plus_minus), as.numeric))

lineups2023 <- read_rds("https://github.com/ramirobentes/NBA-in-R/blob/master/2022_23/regseason/pbp/lineup-stats-rs23/data.rds?raw=true")

# Change here to select other teams/players
team_comp <- "CHI"
players_comp <- c("DeMar DeRozan", "Nikola Vucevic", "Zach LaVine")

player_ids <- player_logs %>%
  distinct(player_name, player_id) %>%
  filter(player_name %in% players_comp)

team_ids <- player_logs %>%
  distinct(team_id, team_abbreviation) %>%
  filter(team_abbreviation == team_comp) %>%
  pull(team_id)

table_data <- lineups2023 %>%
  filter(slug_team == team_comp) %>%
  mutate(lineup_list = str_split(lineup, ", "),
         players_in = map2(lineup_list, list(players_comp), intersect)) %>%
  mutate(players = map_chr(players_in, ~ paste(., collapse = ", "))) %>%
  mutate(players_in = map(players_in, 
                    ~player_ids[match(.x, player_ids$player_name), ]$player_id)) %>%
  group_by(players_in) %>%
  summarise(across(c(pts_team:secs_played), sum)) %>%
  ungroup() %>%
  mutate(all_comp = list(unique(player_ids$player_id)), .before = everything(),
         headshot = map(all_comp, ~ glue::glue("https://cdn.nba.com/headshots/nba/latest/260x190/{.}.png"))) %>%
  mutate(headshot = map2(all_comp, players_in, 
                         ~ ifelse(.x %in% .y, 
                                  glue::glue("<img src='https://cdn.nba.com/headshots/nba/latest/260x190/{.x}.png' width=100 height=70> &nbsp"),
                                  glue::glue("<img src='https://cdn.nba.com/headshots/nba/latest/260x190/{.x}.png' width=100 height=70 style='filter: grayscale(100%)'> &nbsp")))) %>%
  mutate(headshot = map_chr(headshot, ~ paste(., collapse = " "))) %>%
  mutate(minutes = secs_played / 60,
         pts100 = pts_team / poss_team * 100,
         opp100 = pts_opp / poss_opp * 100,
         net100 = pts100 - opp100) %>%
  select(headshot, minutes, pts_team, pts_opp, poss_team, poss_opp, pts100, opp100, net100) %>%
  arrange(-minutes)


# GT ----------------------------------------------------------------------


table_data %>%
  mutate(headshot = map(headshot, gt::html)) %>%
  gt() %>%
  cols_align(align = "center") %>%
  fmt_number(columns = c(minutes, pts100, opp100, net100), decimals = 1) %>%
  cols_label(headshot = "Players",
             minutes = "Minutes",
             pts100 = "Off Rtg",
             opp100 = "Def Rtg",
             net100 = "Net Rtg") %>%
  tab_header(
    title = gt::html(glue::glue("<p class='' style='margin: -10px;'></p><img src='https://cdn.nba.com/logos/nba/{team_ids}/global/L/logo.svg' style='height:120px'> <p class='' style='margin: -10px;'></p><b>Team ratings with selected players on/off the floor")),
    subtitle = glue::glue("Selected players: {paste(player_ids$player_name, collapse = ', ')}")
  ) %>%
  cols_width(headshot ~ px(length(players_comp) * 33),
             minutes ~ px(20),
             pts100 ~ px(20),
             opp100 ~ px(20),
             net100 ~ px(20)) %>%
  tab_style(style = cell_text(weight = "bold"),
            locations = cells_column_labels(everything())) %>%
  tab_options(
    table.width = px(length(players_comp) * 230),
    data_row.padding = px(1)) %>%
  tab_footnote(
    footnote = md("Numbers in gray represent total points / total possessions")) %>%
  tab_footnote(
    footnote = md("Data: @nbainrstats")) %>%
  data_color(
    columns = c(net100),
    colors = scales::col_bin(
      bins = seq((max(c(abs(min(table_data$net100)), abs(max(table_data$net100)))) + 1) * -1,
                 max(c(abs(min(table_data$net100)), abs(max(table_data$net100)))) + 1,
                 by = 2),
      palette = c("red", "white", "green"),
    )) %>%
  cols_merge(columns = c(pts100, pts_team, poss_team)) %>%
  cols_merge(columns = c(opp100, pts_opp, poss_opp)) %>%
  text_transform(
    locations = cells_body(columns = c(pts100, opp100)),
    fn = function(x){
      stat_st <- word(x, 1)
      pts_st <- word(x, 2)
      poss_st <- word(x, 3)
      glue::glue(
        "<div style='line-height:10px'><span style='font-weight:bold;font-size:15px'>{stat_st}</div>
        <div style='line-height:17px'><span style ='font-weight:bold;color:grey;font-size:12px'>{pts_st}/{poss_st}</span></div>")}) %>%
  tab_style(style = list(cell_text(weight = "bold")),
            locations = cells_body(
              columns = c(minutes, net100)))
