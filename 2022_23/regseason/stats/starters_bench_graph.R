library(tidyverse)
library(hoopR)
library(janitor)
library(ggimage)

player_logs <- nba_leaguegamelog(season = "2022-23", player_or_team = "P") %>%
  pluck("LeagueGameLog") %>%
  clean_names()

lineups2023 <- read_rds("https://github.com/ramirobentes/NBA-in-R/blob/master/2022_23/regseason/pbp/lineup-stats-rs23/data.rds?raw=true")

starters_net <- lineups2023 %>%
  left_join(lineups2023 %>%
              filter(period == 1,
                     stint == 1) %>%
              select(game_id, slug_team, starters = lineup)) %>%
  mutate(across(c(lineup, starters), ~ str_split(., ", "), .names = "{.col}_list"),
         number_starters = map_int(map2(lineup_list, starters_list, intersect), length)) %>%
  group_by(slug_team, lineup = ifelse(number_starters == 5, "starters", "bench")) %>%
  summarise(across(c(pts_team:poss_opp), sum)) %>%
  ungroup() %>%
  mutate(pts100 = pts_team / poss_team * 100,
         opp100 = pts_opp / poss_opp * 100,
         net100 = pts100 - opp100) %>%
  select(slug_team, lineup, net100) %>%
  left_join(player_logs %>%
              distinct(slug_team = team_abbreviation, team_id) %>%
              mutate(logo_url = glue::glue("https://cdn.nba.com/logos/nba/{team_id}/global/L/logo.svg"))) %>%
  pivot_wider(names_from = lineup,
              values_from = net100) %>%
  ggplot(aes(x = starters,
             y = bench)) +
  geom_vline(xintercept = 0, size = 0.7) +
  geom_hline(yintercept = 0, size = 0.7) +
  geom_image(aes(image = logo_url), size = 0.075) +
  theme_light() +
  scale_x_continuous(limits = c(-40, 40),
                     breaks = c(-20, 20),
                     minor_breaks = c(-20, 20)) +
  scale_y_continuous(limits = c(-20, 20),
                     minor_breaks = c(-10, 10),
                     breaks = c(-10, 10)) +
  theme(panel.grid.minor.x = element_line(color = "gray", size = 0.5),
        panel.grid.minor.y = element_line(color = "gray", size = 0.5),
        axis.title.x = element_text(size = 14, face = "bold"),
        axis.title.y = element_text(size = 14, face = "bold", angle = 0, vjust=.5),
        plot.title = element_text(hjust = 0.5, size = 36, face = "bold"),
        plot.subtitle = element_text(size = 14),
        plot.caption = element_text(hjust = 0)) +
  labs(title = "Net ratings by starter count",
       subtitle = "Which teams are winning their minutes with 5 starters on the court?\nHow about when one or more bench players enter the game?",
       caption = "Source: @NbaInRstats",
       x = "STARTERS ONLY",
       y = "1+ BENCH\nPLAYER") +
  annotate("text", x = -20, y = -10, label = "Starters losing,\nbench losing too", alpha = 0.3) +
  annotate("text", x = -30, y = 5, label = "Starters losing", alpha = 0.3) +
  annotate("text", x = 30, y = -4, label = "Starters building leads,\nbench giving it back", alpha = 0.3) +
  annotate("text", x = 22, y = 8, label = "Starters winning,\nbench is too", alpha = 0.3) +
  annotate("text", x = -5, y = 15, label = "Bench\npower", alpha = 0.3)
