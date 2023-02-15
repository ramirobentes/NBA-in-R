library(tidyverse)
library(hoopR)
library(janitor)
library(future)

plan(multicore)
players_drives <- map_df(year_to_season(2013:2022), ~ nba_leaguedashptstats(season = ., 
                                                                            player_or_team = "Player", 
                                                                            pt_measure_type = "Drives") %>%
                           pluck("LeagueDashPtStats") %>%
                           clean_names() %>%
                           mutate(season = .x))

plot_colors <- c("black", "red", "#619CFF")
names(plot_colors) <- c("LILLARD", "LILLARD 2023", "Others")

plot_dame <- players_drives %>%
  mutate(across(c(gp:drive_pf_pct), as.numeric)) %>%
  mutate(drives_game = drives / gp,
         player_color = case_when(season == "2022-23" & player_name == "Damian Lillard" ~ "LILLARD 2023",
                                  season != "2022-23" & player_name == "Damian Lillard" ~ "LILLARD",
                                  TRUE ~ "Others"),
         player_transp = ifelse(player_name == "Damian Lillard", 0, 1)) %>%
  filter(drives_game >= 10) %>%
  mutate(drive_fg_pct = drive_fg_pct * 100) %>%
  ggplot(aes(x = drives_game, y = drive_fg_pct)) +
  geom_point(aes(color = player_color, alpha = player_transp == 0), size = 4) +
  scale_color_manual(values = plot_colors, breaks = c("LILLARD", "LILLARD 2023")) +
  scale_y_continuous(limits = c(25, 65),
                     breaks = c(seq(25, 65, by = 10)),
                     labels = c(seq(25, 65, by = 10)),
                     expand = c(0, 0)) +
  scale_alpha_manual(values = c(0.4, 1), guide = FALSE) +
  theme_light() +
  theme(legend.position = "top",
        panel.border = element_blank()) +
  labs(x = "DRIVES PER GAME",
       y = "FG% ON DRIVES",
       title = "DAMIAN LILLARD DRIVES: FREQUENCY AND EFFICIENCY",
       subtitle = "Compared to all NBA players with 10+ drives per game, 2014-2023",
       color = "")

ggsave("twitter_img/dame_drives.png", plot_dame, width = 10, height = 6, dpi = 600)
