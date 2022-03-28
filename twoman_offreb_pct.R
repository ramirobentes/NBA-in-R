library(tidyverse)
library(hoopR)

lineup_stats <- read_csv("https://github.com/ramirobentes/NBA-in-R/blob/master/lineup-stats/data.csv?raw=true")
pbp_final_gt <- read_rds("https://github.com/ramirobentes/NBA-in-R/blob/master/pbp-final-gt/data.rds?raw=true")

rebpct_combs <- pbp_final_gt %>%
  filter(game_date < "2022-03-25") %>%
  filter(msg_type == 4 & act_type == 0 & desc_value == 1) %>%
  mutate(lineup_team = str_split(ifelse(slug_team == team_home, lineup_home, lineup_away), ", "),
         lineup_comb = map(lineup_team, ~ combn(., 2, simplify = FALSE))) %>%
  unnest_longer(lineup_comb) %>%
  count(lineup_comb, slug_team, name = "off_reb") %>%
  left_join(pbp_final_gt %>%
              filter(game_date < "2022-03-25") %>%
              filter(msg_type %in% c(2, 3),
                     !(msg_type == 2 & act_type %in% c(11, 13, 14, 16, 18:22, 25:26, 27:29)),
                     shot_pts == 0) %>%
              mutate(lineup_team = str_split(ifelse(slug_team == team_home, lineup_home, lineup_away), ", "),
                     lineup_comb = map(lineup_team, ~ combn(., 2, simplify = FALSE))) %>%
              unnest_longer(lineup_comb) %>%
              count(slug_team, lineup_comb, name = "off_reb_chances")) %>%
  mutate(off_reb_pct = off_reb / off_reb_chances)

time_combs <- lineup_stats %>%
  filter(game_date < "2022-03-25") %>%
  mutate(lineup = str_split(lineup, ", "),
         lineup_comb = map(lineup, ~ combn(., 2, simplify = FALSE))) %>%
  unnest_longer(lineup_comb) %>%
  group_by(lineup_comb, slug_team) %>%
  summarise(total_time = sum(secs_played)) %>%
  ungroup()

rebpct_combs %>%
  left_join(time_combs) %>%
  filter(total_time >= 500 * 60) %>%
  mutate(lineup_comb = map_chr(lineup_comb, ~ paste(.x, collapse = ", "))) %>%
  arrange(-off_reb_pct)
