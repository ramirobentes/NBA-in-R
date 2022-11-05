library(tidyverse)
library(hoopR)
library(janitor)
library(future)
library(gt)
library(gtExtras)

# Karl Towns year-over-year Restricted area attempts %

player_bio <- hoopR::nba_playerindex() %>%
  pluck("PlayerIndex") %>%
  clean_names() %>%
  filter(paste(player_first_name, player_last_name) == "Karl-Anthony Towns") %>%
  transmute(across(c(person_id, draft_year), as.integer))
  
seasons <- player_bio$draft_year:2022 %>%
  enframe(name = NULL) %>%
  mutate(season_id = paste(value, str_sub(value + 1, -2, -1), sep = "-")) %>%
  pull(season_id)

plan(multicore)
shots_player <- map_df(seasons, ~ nba_shotchartdetail(season = ., player_id = player_bio$person_id) %>% 
                      pluck("Shot_Chart_Detail") %>%
                      mutate(season = .x)) %>%
  clean_names()

player_image <- hoopR::nba_playerheadshot(player_id = player_bio$person_id)

gt_table <- shots_player %>%
  count(season, shot_zone_basic, name = "fga") %>%
  group_by(season) %>%
  mutate(total_fga = sum(fga),
         zone_pct = fga / total_fga) %>%
  ungroup() %>%
  filter(shot_zone_basic == "Restricted Area") %>%
  select(-shot_zone_basic) %>%
  mutate(zone_pct_bar = zone_pct * 100) %>%
  gt() %>%
  tab_header(title = md(glue::glue("<img src={player_image} style='height:100px'><br><b>% of Karl-Anthony Towns' FGA from the restricted area</b>")),
             subtitle = glue::glue("From {min(seasons)} to {max(seasons)}")) %>%
  cols_align(align = "center") %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_column_labels(everything())
  ) %>%
  fmt_percent(columns = zone_pct, decimals = 1) %>%
  cols_label(season = "Season",
             fga = "Rest. Area FGA",
             total_fga = "Total FGA",
             zone_pct = "% of FGA from RA",
             zone_pct_bar = "") %>%
  gt_plt_bar_pct(column = zone_pct_bar, scaled = TRUE, fill = "blue", background = "darkgray") %>%
  cols_width(zone_pct_bar ~ px(140)) %>%
  tab_options(
    column_labels.font.size = 12,
    data_row.padding = px(1))

gtsave(gt_table, "towns_zone_shots.png")


# With and without Gobert

pbp2023 <- read_rds("https://github.com/ramirobentes/NBA-in-R/blob/master/2022_23/regseason/pbp/pbp-poss-rs23/data.rds?raw=true")
shots_season <- nba_shotchartdetail(season = "2022-23",
                                    player_id = 0) %>%
  pluck("Shot_Chart_Detail") %>%
  clean_names()

pbp2023 %>%
  filter(msg_type %in% c(1, 2)) %>%
  left_join(shots_season %>%
              transmute(across(c(game_id, number_original = game_event_id), as.numeric), shot_zone_basic)) %>%
  filter(slug_team == "MIN",
         player1 == "Karl-Anthony Towns") %>%
  mutate(lineup_team = ifelse(slug_team == team_home, lineup_home, lineup_away)) %>%
  count(shot_zone_basic, lineup = ifelse(str_detect(lineup_team, "Rudy Gobert"), "with Gobert", "without Gobert"), name = "fga") %>%
  group_by(lineup) %>%
  mutate(total_fga = sum(fga),
         fga_pct = fga / total_fga) %>%
  ungroup() %>%
  filter(shot_zone_basic == "Restricted Area")
