library(tidyverse)

lineup_stats <- read_csv("https://github.com/ramirobentes/NBA-in-R/blob/master/lineup-stats/data.csv?raw=true")

pbp_final_gt <- read_csv("https://github.com/ramirobentes/NBA-in-R/releases/download/pbp-final-gt-9814b0d/data.csv",
                         col_types = c(clock = "c",
                                       start_poss = "c"))
# url above might change daily (copy latest on data.csv here https://github.com/ramirobentes/NBA-in-R/releases)

pbp_final_gt %>%
  filter(msg_type %in% c(1, 2),
         desc_value == 2) %>%
  mutate(event_desc = case_when(msg_type == 1 ~ "made_fg",
                                msg_type == 2 & !is.na(player3) ~ "blocked",
                                msg_type == 2 & is.na(player3) ~ "missed_fg")) %>%
  count(player_name = player1, event_desc) %>%
  group_by(player_name) %>%
  mutate(fg2a = sum(n)) %>%
  ungroup() %>%
  mutate(event_pct = n / fg2a) %>%
  left_join(lineup_stats %>%
              separate_rows(lineup, sep = ", ") %>%
              group_by(player_name = lineup) %>%
              summarise(total_min = sum(secs_played) / 60) %>%
              ungroup()) %>%
  filter(total_min >= 1000,
         event_desc == "blocked") %>%
  arrange(-event_pct)




# By shot location:

library(tidyverse)
library(hoopR)
library(janitor)

lineup_stats <- read_csv("https://github.com/ramirobentes/NBA-in-R/blob/master/lineup-stats/data.csv?raw=true")

shots_season <- nba_shotchartdetail(season = "2021-22",
                                    player_id = 0) %>%
  pluck("Shot_Chart_Detail") %>%
  clean_names()

pbp_final_gt <- read_csv("https://github.com/ramirobentes/NBA-in-R/releases/download/pbp-final-gt-9814b0d/data.csv",
                         col_types = c(clock = "c",
                                       start_poss = "c"))
# url above might change daily (copy latest on data.csv here https://github.com/ramirobentes/NBA-in-R/releases)

blocked_shots_pct <- pbp_final_gt %>%
  filter(msg_type %in% c(1, 2),
         desc_value == 2) %>%
  mutate(event_desc = case_when(msg_type == 1 ~ "made_fg",
                                msg_type == 2 & !is.na(player3) ~ "blocked",
                                msg_type == 2 & is.na(player3) ~ "missed_fg")) %>%
  left_join(shots_season %>%
              transmute(game_id = as.numeric(game_id), number_original = as.numeric(game_event_id), player1 = player_name,
                        shot_zone_basic)) %>%
  count(player_name = as.factor(player1), event_desc = as.factor(event_desc), 
        shot_zone_basic = as.factor(shot_zone_basic), .drop = FALSE) %>%
  group_by(player_name, shot_zone_basic) %>%
  mutate(fg2a = sum(n)) %>%
  ungroup() %>%
  mutate(event_pct = n / fg2a) %>%
  left_join(lineup_stats %>%
              separate_rows(lineup, sep = ", ") %>%
              group_by(player_name = lineup) %>%
              summarise(total_min = sum(secs_played) / 60) %>%
              ungroup()) %>%
  filter(total_min >= 1000,
         #fg2a >= 10,
         event_desc == "blocked") %>%
  arrange(-event_pct, fg2a)

mean_zones <- blocked_shots_pct %>%
  group_by(shot_zone_basic) %>%
  summarise(across(c(n, fg2a), sum)) %>%
  ungroup() %>%
  mutate(blk_zone_pct = n / fg2a)

ggplot() +
  geom_histogram(data = blocked_shots_pct, 
                 aes(x = event_pct),
                 binwidth = 0.01,
                 color = "red",
                 fill = "darkred") +
  geom_vline(data = mean_zones, 
             aes(xintercept = blk_zone_pct),
             color = "black", linetype = "dashed", size = 1) +
  theme_light() +
  scale_x_continuous(labels = scales::percent) +
  labs(title = "Distribution of players % of 2-pt FGA blocked, by shot zone",
       subtitle = "Players with 1000+ minutes and 10+ FGA in zone",
       x = "blocked 2-pt FGA %",
       y = "") +
  facet_wrap(~ shot_zone_basic, scales = "free_y")
