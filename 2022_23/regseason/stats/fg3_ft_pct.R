library(tidyverse)
library(hoopR)
library(janitor)

team_logs <- map_df(year_to_season(2000:2022), ~ nba_leaguegamelog(season = ., player_or_team = "T") %>%
                      pluck("LeagueGameLog") %>%
                      mutate(season = .x)) %>%
  clean_names() %>%
  mutate(across(c(min:plus_minus), as.numeric))

pct_ft_fg3 <- team_logs %>%
  filter(as.numeric(word(season, 1, sep = "-")) >= 2010) %>%
  group_by(season, team_id, team_abbreviation) %>%
  summarise(games = n(),
            across(c(ftm, fta, fg3m, fg3a), sum)) %>%
  ungroup() %>%
  mutate(ft_pct = ftm / fta,
         fg3_pct = fg3m / fg3a) %>%
  filter(fg3a / games >= 15)

pct_ft_fg3 %>%
  filter(fg3_pct <= 0.34) %>%
  arrange(-ft_pct)

ft_fg3_plot <- pct_ft_fg3 %>%
  mutate(logo_url = ifelse(team_abbreviation == "MIA" & season == "2022-23", 
                           glue::glue("https://cdn.nba.com/logos/nba/{team_id}/global/L/logo.svg"), 
                           NA)) %>%
  ggplot(aes(x = fg3_pct, y = ft_pct)) +
  geom_point(alpha = 0.5, size = 4) +
  geom_vline(xintercept = 0.355, color = "red", linewidth = 1.4, alpha = 0.5) +
  geom_hline(yintercept = 0.755, color = "red", linewidth = 1.4, alpha = 0.5) +
  annotate("text", x = 0.3, y = 0.7, label = "Bad FT,\nBad FG3", alpha = 0.8, size = 3, color = "red") +
  annotate("text", x = 0.42, y = 0.7, label = "Bad FT,\nGood FG3", alpha = 0.8, size = 3, color = "red") +
  annotate("text", x = 0.3, y = 0.8, label = "Good FT,\nBad FG3", alpha = 0.8, size = 3, color = "red") +
  annotate("text", x = 0.42, y = 0.8, label = "Good FT,\nGood FG3", alpha = 0.8, size = 3, color = "red") +
  annotate("text", x = 0.3315, y = 0.836, label = "2022-23", alpha = 0.8, size = 3) +
  annotate(geom = "curve", x = 0.4125, y = 0.839, xend = 0.42, yend = 0.83, curvature = -.3, arrow = arrow(length = unit(2, "mm"))) +
  annotate(geom = "text", x = 0.4145, y = 0.827, label = "2020-21 LAC", hjust = "left", size = 3) +
  annotate(geom = "curve", x = 0.3765, y = 0.661, xend = 0.385, yend = 0.664, curvature = -.3, arrow = arrow(length = unit(2, "mm"))) +
  annotate(geom = "text", x = 0.3858, y = 0.663, label = "2011-12 ORL", hjust = "left", size = 3) +
  ggimage::geom_image(aes(image = logo_url), size = 0.08) +
  #coord_fixed(ratio = 0.7) +
  scale_x_continuous(labels = scales::percent_format(accuracy = .1)) +
  scale_y_continuous(labels = scales::percent_format(accuracy = .1)) +
  labs(x = "FG3%",
       y = "FT%",
       title = "NBA teams' season FG3% and FT% since 2010-11",
       subtitle = "Minimum 15 team FG3A per game") +
  theme_light()

ggsave("twitter_img/ft_fg3_plot.png", ft_fg3_plot, width = 9, height = 7, dpi = 600)




