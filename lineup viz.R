library(tidyverse)
library(hoopR)
library(glue)
library(ggtext)
library(janitor)

lineup_stats <- read_csv("https://github.com/ramirobentes/NBA-in-R/blob/master/lineup-stats/data.csv?raw=true")

player_logs <- nba_leaguegamelog(season = "2021-22", player_or_team = "P") %>%
  pluck("LeagueGameLog") %>%
  clean_names()


# Over Season

chosen_team <- "PHI"
chosen_players <- c("James Harden", "Joel Embiid")

team_lineups <- lineup_stats %>%
  select(-c(pts_team:poss_opp)) %>%
  filter(slug_team == chosen_team) %>%
  mutate(lineup_list = str_split(lineup, ", ")) %>%
  group_by(game_id) %>%
  mutate(final_time = cumsum(secs_played),
         initial_time = final_time - secs_played) %>%
  ungroup() %>%
  mutate(players_floor = map2(lineup_list, list(chosen_players), intersect),
         players_floor = map_chr(players_floor, ~ paste(sort(.), collapse = ", ")),
         players_floor = case_when(str_detect(players_floor, "\\,") ~ glue("Both {players_floor} playing"),
                                   players_floor == "" ~ glue("Neither playing"),
                                   TRUE ~ glue("Only {players_floor} playing"))) %>%
  filter(period <= 4) %>%
  left_join(lineup_stats %>%
              filter(slug_team == chosen_team) %>%
              arrange(game_date) %>%
              distinct(game_id, game_date) %>%
              mutate(game_order = row_number()))

team_lineups %>%
  ggplot(aes(x = game_order, y = secs_played, group = desc(period))) +
  geom_col(aes(fill = players_floor), position = "stack") +
  coord_flip() +
  scale_x_reverse(breaks = round(unname(quantile(team_lineups$game_order, na.rm = TRUE), 1))) +
  expand_limits(x = 1, y = max(team_lineups$game_order)) +
  theme_light() +
  geom_hline(yintercept = c(720, 1440, 2160, 2880), color = "white") +
  scale_fill_manual(values = c("#1E0145", "#CECECE", "#98DEE9", "#F9A2D4")) +
  theme(legend.position = "top",
        legend.title = element_blank(),
        #text = element_text(size = 16),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        panel.grid = element_blank(),
        plot.subtitle = element_text(hjust = 0.5)) +
  labs(x = "",
       y = "",
       subtitle = glue::glue("Playing time for {paste(chosen_players, collapse = ', ')} in each {unique(team_lineups$slug_team)} game of the season")) +
  annotate("text", 0, 360, label = "Q1", color = "black", vjust = -0.5) +
  annotate("text", 0, 1080, label = "Q2", color = "black", vjust = -0.5) +
  annotate("text", 0, 1800, label = "Q3", color = "black", vjust = -0.5) +
  annotate("text", 0, 2520, label = "Q4", color = "black", vjust = -0.5)
  
  
  
# For individual games:

chosen_team <- "PHI"
chosen_date <- "2022-03-02"

indiv_game_lineups <- lineup_stats %>%
  filter(slug_team == chosen_team) %>%
  filter(game_date == chosen_date) %>%
  group_by(game_id) %>%
  mutate(final_time = cumsum(secs_played),
         initial_time = final_time - secs_played,
         plus_minus = pts_team - pts_opp) %>%
  ungroup() %>%
  separate_rows(lineup, sep = ", ") %>%
  group_by(lineup, period) %>%
  mutate(stint_player = ifelse(initial_time != lag(final_time, default = 0), 1, 0),
         stint_player = cumsum(stint_player)) %>%
  group_by(player_name = lineup, period, stint_player, slug_team, slug_opp, game_date) %>%
  summarise(initial_time = min(initial_time),
            final_time = max(final_time),
            stint_time = sum(secs_played),
            plus_minus = sum(plus_minus)) %>%
  group_by(player_name) %>%
  mutate(first_stint = min(initial_time),
         total_time = sum(stint_time)) %>%
  ungroup() %>%
  left_join(player_logs %>%
              distinct(player_name, player_id)) %>%
  mutate(headshot_url = glue::glue("<img src='https://ak-static.cms.nba.com/wp-content/uploads/headshots/nba/latest/260x190/{player_id}.png' width='50'/>")) %>%
  mutate(across(c(player_name, headshot_url), ~ fct_reorder(., -first_stint))) %>%
  ungroup()

indiv_game_lineups %>%
  ggplot() +
  geom_linerange(aes(x = headshot_url, ymin = initial_time, ymax = final_time, color = plus_minus), size = 17) +
  coord_flip()  +
  theme_light() + 
  scale_color_gradient2(low = "red", high = "darkgreen", mid = "gray", midpoint = 0) +
  geom_hline(yintercept = c(0, 720, 1440, 2160, 2880), color = "black") +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        panel.grid = element_blank(),
        plot.title = ggtext::element_markdown(),
        axis.text.y = ggtext::element_markdown()) +
  labs(title = "Player stints - lenght and plus-minus",
       subtitle = glue("During {unique(indiv_game_lineups$slug_team)} game vs {unique(indiv_game_lineups$slug_opp)} on {unique(indiv_game_lineups$game_date)}"),
       x = "",
       y = "",
       color = "stint +/-") +
  annotate("text", 0, 360, label = "Q1", color = "black", vjust = -0.5) +
  annotate("text", 0, 1080, label = "Q2", color = "black", vjust = -0.5) +
  annotate("text", 0, 1800, label = "Q3", color = "black", vjust = -0.5) +
  annotate("text", 0, 2520, label = "Q4", color = "black", vjust = -0.5)
