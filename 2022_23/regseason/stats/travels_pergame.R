library(tidyverse)
library(hoopR)
library(janitor)

pbp2023 <- read_rds("https://github.com/ramirobentes/NBA-in-R/blob/master/2022_23/regseason/pbp/pbp-poss-rs23/data.rds?raw=true")

travels_game <- pbp2023 %>%
  group_by(game_date) %>%
  summarise(travels = sum(msg_type == 5 & act_type == 4),
            games = n_distinct(game_id)) %>%
  ungroup() %>%
  mutate(travels_pg = travels / games)

travels_game %>%
  ggplot(aes(x = game_date, y = travels_pg)) +
  geom_line(linewidth = 1,
            color = "blue") +
  theme_light() +
  theme(plot.title = element_text(hjust = 0.5, size = 20),
        plot.subtitle = element_text(hjust = 0.5, size = 16),
        axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +
  scale_x_date(date_labels = "%Y-%m-%d", 
               breaks = seq.Date(min(pbp2023$game_date), max(pbp2023$game_date), by = "day")) + 
  labs(x = "",
       y = "",
       title = "Traveling violations per game",
       subtitle = "2022-23 season")
