library(tidyverse)
library(hoopR)
library(future)
library(janitor) 

plan(multisession)
players_tracking <- map_df(year_to_season(2013:2022), ~ hoopR::nba_leaguedashptstats(season = ., player_or_team = "Player") %>%
                             pluck("LeagueDashPtStats") %>%
                             mutate(season = .x)) %>%
  clean_names()

players_tracking %>%
  mutate(across(c(gp:avg_speed_def), as.numeric),
         dist_miles_game = dist_miles / gp) %>%
  filter(dist_miles_game >= 2.6) %>%
  count(season) %>%
  ggplot(aes(x = season, y = n)) +
  geom_col(fill = "red") +
  theme_light() +
  labs(title = "Number of players averaging 2.6+ miles traveled per game",
       x = "",
       y = "")
