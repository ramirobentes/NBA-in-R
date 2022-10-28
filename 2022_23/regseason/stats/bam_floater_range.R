library(tidyverse)
library(hoopR)
library(janitor)
library(future)
library(cowplot)

seasons <- 2017:2022 %>%
  enframe(name = NULL) %>%
  mutate(season_id = paste(value, str_sub(value + 1, -2, -1), sep = "-")) %>%
  pull(season_id)

plan(multicore)
shots_bam <- map_df(seasons, ~ nba_shotchartdetail(season = ., player_id = "1628389") %>% 
                      pluck("Shot_Chart_Detail") %>%
                      mutate(season = .x)) %>%
  clean_names()


# Most made floater range shots

shots_bam %>%
  filter(event_type == "Made Shot",
         shot_zone_basic == "In The Paint (Non-RA)") %>%
  count(player_name, game_id, game_date = lubridate::ymd(game_date), sort = T, name = "made_fg")

# Plot over years

shots_plot <- shots_bam %>%
  count(player_name, season, shot_zone_basic, name = "fga") %>%
  group_by(season) %>%
  mutate(total_fga = sum(fga),
         fga_pct = fga / total_fga) %>%
  ungroup() %>%
  filter(shot_zone_basic == "In The Paint (Non-RA)") %>%
  ggplot(aes(x = season,
             y = fga_pct)) +
  geom_col(fill = "red") +
  scale_y_continuous(labels = scales::percent) +
  theme_light() +
  labs(y = "% of FGA in floater range",
       title = "Every year, Bam Adebayo increases his share of floater range* shots",
       subtitle = "*paint shots outside of the restricted area")
  
bam_image <- hoopR::nba_playerheadshot(player_id = 1628389)

final_shots_plot <- ggdraw(shots_plot) + 
  draw_image(bam_image, x = 0.05, y = 1, hjust = -0.3, vjust = 1.35, width = 0.2, height = 0.3)

ggsave("twitter_img/bam_shots_zone.png", final_shots_plot, width = 8, height = 4.5, dpi = 600)
