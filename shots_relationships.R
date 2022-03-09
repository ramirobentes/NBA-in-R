library(tidyverse)
library(hoopR)
library(janitor)
library(ggimage)

player_logs <- nba_leaguegamelog(season = "2021-22", player_or_team = "P") %>%
  pluck("LeagueGameLog") %>%
  clean_names() %>%
  mutate(across(c(min:fantasy_pts), as.numeric),
         game_date = as.Date(game_date))

players_3shooters <- player_logs %>%
  group_by(player_id, player_name) %>%
  summarise(across(c(fg3m, fg3a), sum)) %>%
  ungroup() %>%
  mutate(fg3_pct = fg3m / fg3a) %>%
  filter(fg3a >= 150,
         (fg3m >= 150 | fg3_pct >= 0.4))

# shoot_dash <- map(unique(players_3shooters$player_id), ~ nba_playerdashptshots(season = "2021-22", player_id = .))
# this will take a while to load, so I uploaded the result:

shoot_dash <- read_rds("https://github.com/ramirobentes/NBA-in-R/blob/master/shoot_dashboard_2022.rds?raw=true")

shoot_dash_all <- map_df(c("TouchTimeShooting", "ClosestDefenderShooting", "DribbleShooting", "ShotClockShooting", "GeneralShooting"), 
                ~ map(shoot_dash, pluck(.)) %>% 
                  bind_rows() %>%
                  clean_names() %>%
                  select(player_id, description = 6, fg3m, fg3a)) %>%
  mutate(across(c(fg3m, fg3a), as.numeric))

wanted_description <- "6+ Feet - Wide Open"

players_3shooters %>%
  mutate(headshot_url = glue::glue("https://ak-static.cms.nba.com/wp-content/uploads/headshots/nba/latest/260x190/{player_id}.png")) %>%
  left_join(shoot_dash_all %>%
              filter(description == wanted_description) %>%
              rename(fg3m_desc = fg3m, fg3a_desc = fg3a)) %>%
  mutate(pct_desc = fg3a_desc / fg3a) %>%
  ggplot(aes(x = fg3_pct,
             y = pct_desc)) +
  geom_image(aes(image = headshot_url), size = 0.1) +
  scale_x_continuous(labels = scales::percent) +
  scale_y_continuous(labels = scales::percent) +
  theme_light() +
  labs(title = "Relationship between players 3FG% and their shot profile",
       subtitle = "Among players that are shooting over 40% (accuracy) or have made 150+ 3s (volume)",
       x = "FG3 %",
       y = paste0("% of FG3A that are wide open"))
