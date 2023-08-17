library(tidyverse)
library(hoopR)
library(janitor)
library(ggrepel)
library(cowplot)


shots_function <- function(x, y){
  nba_shotchartdetail(season = x,
                      season_type = y,
                      player_id = 203076) %>%
    pluck("Shot_Chart_Detail")
}

years_ad <- crossing(seasons = year_to_season(2012:2022), 
                     types = c("Regular Season", "Playoffs"))

shots_ad_raw <- map2_df(.x = years_ad$seasons, .y = years_ad$types, .f = shots_function)

shots_ad <- shots_ad_raw %>%
  clean_names() %>%
  mutate(season_type = ifelse(str_sub(game_id, 1, 3) == "002", "Regular Season", "Playoffs"),
         season = glue::glue("20{str_sub(game_id, 4, 5)}"),
         season = glue::glue("{season}-{as.integer(str_sub(season, 3, 4))+1}"))

shot_stats <- shots_ad %>%
  filter(shot_zone_range == "16-24 ft." | str_detect(shot_zone_basic, "3")) %>%
  mutate(season_var = case_when(season_type == "Regular Season" ~ season,
                                season_type == "Playoffs" & season == "2019-20" ~ "Bubble",
                                TRUE ~ "Non-Bubble")) %>%
  count(season_var, event_type, shot_zone_basic = ifelse(str_detect(shot_zone_basic, "3"), "ThreePt", "Long2")) %>%
  group_by(season_var, shot_zone_basic) %>%
  mutate(total = sum(n),
         pct = n / total) %>%
  ungroup() %>%
  filter(event_type == "Made Shot")

plot_ad <- shot_stats %>%
  mutate(playoffs = ifelse(!str_detect(season_var, "\\d"), "Playoffs", "RS")) %>%
  select(-c(n, total, event_type)) %>%
  pivot_wider(names_from = shot_zone_basic,
              values_from = pct) %>%
  clean_names() %>%
  # mutate(headshot = ifelse(season_var == "Bubble", "https://cdn.nba.com/headshots/nba/latest/1040x760/203076.png", NA)) %>%
  ggplot(aes(x = long2, y = three_pt)) +
  geom_point(alpha = 0.5, 
             size = 4.5) +
  # ggimage::geom_image(aes(image = headshot), size = 0.14) +
  geom_label_repel(aes(label = season_var,
                       fill = playoffs),
                   check_overlap = T,
                   size = 3.5) +
  theme_light() +
  labs(x = "Long 2s FG%",
       y = "3pt FG%",
       title = "Anthony Davis' career FG% in 3-pointers and long 2-pointers",
       subtitle = 'On every <span style = "color:#FFC024">regular season</span>, bubble and non-bubble <span style = "color:#F524FF">playoffs</span>.') +
  scale_x_continuous(labels = scales::percent_format(accuracy = .1)) +
  scale_y_continuous(labels = scales::percent_format(accuracy = .1)) +
  scale_fill_manual(values = c("#F524FF", "#FFC024")) +
  theme(
    plot.subtitle = ggtext::element_markdown(lineheight = 1.7),
    legend.position = "none")

final_ad_plot <- ggdraw(plot_ad) + 
  draw_image("https://cdn.nba.com/headshots/nba/latest/1040x760/203076.png", 
             x = 0.15, y = 0.5, hjust = 0.5, vjust = 1.9, width = 0.15, height = 0.25)
  
  
ggsave("ad_shots.png", final_ad_plot, width = 9, height = 6.5, dpi = 600)
