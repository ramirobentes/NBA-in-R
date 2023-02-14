library(tidyverse)
library(hoopR)
library(janitor)
library(future)
library(gt)

player_info <- hoopR::nba_playerindex(season = "2022-23") %>%
  pluck("PlayerIndex") %>%
  clean_names()

plan(multicore)
player_logs <- map_df(year_to_season(2000:2022), ~ nba_leaguegamelog(season = ., player_or_team = "P") %>%
                        pluck("LeagueGameLog") %>%
                        mutate(season = .x)) %>%
  clean_names() %>%
  mutate(across(c(min:plus_minus), as.numeric))

player_logs_joined <- player_logs %>%
  left_join(player_info %>%
              transmute(player_id = person_id, 
                        draft_round, 
                        player_abb = paste(str_sub(player_first_name, 1, 1), player_last_name, sep = ". ")))

undrafted_comp <- player_logs_joined %>%
  group_by(season, team_id) %>%
  summarise(undrafted_number = n_distinct(player_id[which(is.na(draft_round))]),
            # undrafted_players_id = list(unique(player_id[which(is.na(draft_round))])),
            undrafted_min = sum(min[which(is.na(draft_round))]),
            total_min = sum(min)) %>%
  ungroup() %>%
  mutate(pct_min = undrafted_min / total_min,
         team_logo = glue::glue("https://cdn.nba.com/logos/nba/{team_id}/global/L/logo.svg")) %>%
  select(season, team_id, team_logo, undrafted_number, undrafted_min, total_min, pct_min) %>%
  arrange(-pct_min) %>%
  head(10) %>%
  mutate(season_team = paste(season, team_id))

chart_data <- player_logs_joined %>%
  filter(is.na(draft_round)) %>%
  group_by(season_team = paste(season, team_id), player_id, player_name) %>%
  summarise(player_min = sum(min)) %>%
  ungroup() %>%
  semi_join(undrafted_comp %>%
              select(season_team)) %>%
  select(-player_id) %>%
  nest(data = -season_team) %>%
  mutate(plot_code = map(
    data, 
    ~ggplot(., aes(y = fct_reorder(player_name, player_min), x = player_min)) +
      geom_col(fill = "blue") +
      theme_light(base_size = 34) +
      theme(plot.margin = unit(c(0, 0, -1.5, 0), "cm")) +
      scale_x_continuous(limits = c(0, 3000)) +
      labs(x = "", y = "") 
  )) %>%
  right_join(undrafted_comp) %>%
  select(season, team_logo, undrafted_min, total_min, pct_min, plot_code) %>%
  mutate(plot_image = NA) %>%
  arrange(-pct_min)
  
gt_table <- chart_data %>%
  gt() %>%
  cols_align(align = "center") %>%
  text_transform(locations = cells_body(c(team_logo)),
                 fn = function(x){ 
                   web_image(url = x,
                             height = 75)}) %>%
  text_transform(
    locations = cells_body(c(plot_image)),
    fn = function(x) {map(chart_data$plot_code, ggplot_image, height = 130, aspect_ratio = 3)}
  ) %>%
  cols_hide(c(plot_code)) %>%
  fmt_percent(columns = c(pct_min), decimals = 1) %>%
  fmt_number(columns = c(undrafted_min, total_min), use_seps = TRUE, decimals = 0) %>% 
  cols_label(team_logo = "Team",
             undrafted_min = "UD Min",
             total_min = "Total Min",
             pct_min = "UD %",
             plot_image = "Min by UD Players") %>%
  tab_header(
    title = "NBA teams with highest % of minutes played by undrafted players in a season",
    subtitle = "Since 2000-01") %>%
  cols_width(team_logo ~ px(110),
             undrafted_min ~ px(110),
             total_min ~ px(110),
             pct_min ~ px(110),
             plot_image ~ px(460)) %>%
  tab_style(style = cell_text(weight = "bold"),
            locations = cells_column_labels(everything())) %>%
  tab_options(
    data_row.padding = px(5),
    table.width = px(900)) %>%
  gtExtras::gt_merge_stack(col1 = team_logo, col2 = season, palette = c("black", "black"), font_size = c("17px", "14px"))

gtsave(gt_table, "twitter_img/undrafted_min.png")
