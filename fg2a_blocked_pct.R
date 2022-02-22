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
