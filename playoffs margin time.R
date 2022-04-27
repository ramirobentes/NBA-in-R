library(tidyverse)
library(hoopR)
library(janitor)
library(gt)

pbp_post22 <- read_rds("https://github.com/ramirobentes/NBA-in-R/blob/master/pbp-final-gt-post/data.rds?raw=true")
team_logs <- nba_leaguegamelog(season = "2021-22", player_or_team = "T", season_type = "Playoffs") %>%
  pluck("LeagueGameLog") %>%
  clean_names() %>%
  mutate(pts = as.numeric(pts))


pts_diff_time <- pbp_post22 %>%
  filter(!str_starts(game_id, "5")) %>%
  group_by(game_date, game_id, team_home, team_away, pts_diff = hs - vs) %>%
  summarise(total_secs = sum(secs_played)) %>%
  ungroup() %>%
  pivot_longer(cols = starts_with("team"),
               names_to = "location",
               values_to = "slug_team",
               names_prefix = "team_") %>%
  mutate(pts_diff = ifelse(location == "home", pts_diff, pts_diff * -1))


# Table

pts_diff_time %>%
  group_by(slug_team, situation = case_when(pts_diff == 0 ~ "tied",
                                          pts_diff > 0 ~ "lead",
                                          pts_diff < 0 ~ "trail")) %>%
  summarise(total = sum(total_secs)) %>%
  ungroup() %>%
  group_by(slug_team) %>%
  mutate(pct_time = total / sum(total)) %>%
  ungroup() %>%
  select(-total) %>%
  pivot_wider(names_from = situation,
              values_from = pct_time) %>%
  left_join(team_logs %>%
              group_by(game_id) %>%
              mutate(result = ifelse(pts > sum(pts) - pts, "win", "loss")) %>%
              ungroup() %>%
              group_by(slug_team = team_abbreviation, 
                       logo_url = glue::glue("https://cdn.nba.com/logos/nba/{team_id}/global/L/logo.svg")) %>%
              summarise(wins = sum(result == "win"),
                        losses = sum(result == "loss")) %>%
              ungroup() %>%
              mutate(record = paste(wins, losses, sep = "-"))) %>%
  arrange(-lead) %>%
  select(logo_url, lead, tied, trail, record) %>%
  gt() %>%
  cols_align(align = "center") %>%
  text_transform(locations = cells_body(c(logo_url)),
                 fn = function(x){ 
                   web_image(url = x,
                             height = 50)}) %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_column_labels(everything())
  ) %>%
  fmt_percent(columns = c(lead:trail), decimals = 1) %>%
  cols_label(logo_url = "") %>%
  tab_header(title = md("**Total time leading, tied and trailing in games**"),
             subtitle = "2021-22 playoffs") %>%
  tab_options(
    table.width = px(520),
    column_labels.font.size = 12,
    data_row.padding = px(1))
  
  
# Graph
 
pts_diff_time %>%
  group_by(slug_team, pts_diff) %>%
  summarise(total_secs = sum(total_secs) / 60) %>%
  ungroup() %>%
  mutate(situation = case_when(pts_diff == 0 ~ "tied",
                               pts_diff > 0 ~ "lead",
                               pts_diff < 0 ~ "trail")) %>%
  ggplot(aes(x = pts_diff, y = total_secs)) +
  geom_col(aes(fill = situation), color = "#BDC3B4") +
  scale_fill_manual(values = c("#05CE1C", "#F4CB25", "#F43725")) +
  facet_wrap(~ slug_team) +
  theme_light() +
  theme(legend.position = "none") +
  labs(title = "What teams have been more dominant in this playoffs?",
       subtitle = "Score margin total time for each playoff team",
       x = "margin",
       y = "time (in minutes)")
