library(rtweet)
library(tidyverse)
library(hoopR)
library(janitor)
library(lubridate)

beam_tweets <- rtweet::search_tweets('"victory beam" OR "beam team" OR #beamteam OR (beam + (sacramento OR kings OR golden OR "light the" OR "light that"))', n = 10000,
                                   retryonratelimit = TRUE, include_rts = FALSE)

team_logo <- "https://cdn.nba.com/logos/nba/1610612758/global/L/logo.svg"

beam_plot <- beam_tweets %>%
  select(user_id, status_id, created_at, screen_name, text, favorite_count, retweet_count, quote_count, reply_count) %>%
  mutate(created_at_pt = created_at - lubridate::hours(8),
         interval = lubridate::floor_date(created_at_pt, unit = "5 minutes")) %>%
  filter(as.Date(created_at_pt) %in% as.Date(c("2022-11-17", "2022-11-15", "2022-11-13")),
         hour(created_at_pt) >= 18) %>%
  count(interval, sort = T) %>%
  ggplot(aes(x = interval, y = n)) +
  geom_col(fill = "#691FA6") +
  facet_wrap(~ as.Date(interval), scales = "free_x") +
  theme_light() +
  theme(strip.background = element_rect(fill = "black"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank()) +
  scale_x_datetime(date_breaks = "1 hour", date_labels = "%H:%M") +
  labs(x = "Pacific time",
       y = "Number of tweets",
       title = "Number of beam tweets during last 3 Sacramento Kings games",
       subtitle = 'Search string: "victory beam" OR "beam team" OR #beamteam OR (beam + (sacramento OR kings \nOR golden OR "light the" OR "light that")')

beam_final <- ggdraw(beam_plot) + 
  draw_image(team_logo, x = 0.05, y = 1, hjust = -0.3, vjust = 1.45, width = 0.2, height = 0.3)


# ggsave("beam_tweets.png", beam_final, width = 9, height = 6.5, dpi = 600)
