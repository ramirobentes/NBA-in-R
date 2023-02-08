library(tidyverse)
library(hoopR)
library(janitor)

player_logs <- nba_leaguegamelog(season = "2022-23", player_or_team = "P") %>%
  pluck("LeagueGameLog") %>%
  clean_names() %>%
  mutate(across(c(min:plus_minus), as.numeric))

over20ppg <- player_logs %>%
  group_by(player_id, player_name) %>%
  summarise(games = n(),
            ftm = sum(ftm),
            ppg = mean(pts)) %>%
  ungroup() %>%
  filter(games >= 30) %>%
  arrange(-ppg) %>%
  filter(ppg >= 20)

shots_season <- nba_shotchartdetail(season = "2022-23",
                                    player_id = 0) %>%
  pluck("Shot_Chart_Detail") %>%
  clean_names()

player_info <- hoopR::nba_playerindex(season = "2022-23") %>%
  pluck("PlayerIndex") %>%
  clean_names()

table_zone <- shots_season %>%
  filter(event_type == "Made Shot") %>%
  mutate(shot_zone_basic = case_when(str_detect(shot_zone_basic, "Corner") ~ "Corner 3",
                                     shot_zone_basic == "Backcourt" ~ "Above the Break 3",
                                     TRUE ~ shot_zone_basic)) %>%
  group_by(player_id, player_name, zone_type = shot_zone_basic) %>%
  summarise(fgm = n(),
            pts = sum(parse_number(shot_type))) %>%
  ungroup() %>%
  semi_join(over20ppg) %>%
  select(player_id, player_name, zone_type, pts) %>%
  bind_rows(over20ppg %>%
              transmute(player_id, player_name, zone_type = "Free Throw", pts = ftm)) %>%
  group_by(player_id, player_name) %>%
  mutate(pct_type = pts / sum(pts)) %>%
  ungroup() %>%
  left_join(player_info %>%
              distinct(player_id = person_id, position)) %>%
  mutate(position = case_when(str_detect(position, "C") ~ "Big",
                              position == "G" ~ "Guard",
                              TRUE ~ "Wing"),
        zone_type = fct_relevel(zone_type, c("Free Throw", "Above the Break 3", "Corner 3",
                                            "Mid-Range", "In The Paint (Non-RA)", "Restricted Area")))

table_zone %>%
  group_by(zone_type) %>%
  slice_max(pct_type, n = 20) %>%
  ungroup() %>%
  mutate(zone_type = as.factor(zone_type),
         player_name = tidytext::reorder_within(player_name, pct_type, zone_type)) %>%
  ggplot(aes(y = player_name, x = pct_type)) +
  geom_col(aes(fill = position)) +
  ggthemes::scale_fill_colorblind() +
  geom_text(aes(y = player_name, x = pct_type, label = scales::percent(pct_type, accuracy = 0.1)), hjust = -0.08, size = 2.8) +
  tidytext::scale_y_reordered() +
  scale_x_continuous(labels = scales::percent_format(),
                     expand = c(0, 0),
                     limits = c(0, 1.05)) +
  theme_light() +
  facet_wrap(~zone_type, scale =  "free_y") +
  theme(legend.position = "bottom") +
  labs(title = "How the league's best scorers get their points",
       subtitle = "Highest % of total points scored from each area by players averaging 20+ points per game (min. 30 games)",
       x = "",
       y = "",
       fill = "Position:")
