library(hoopR)
library(tidyverse)

pbp_final_gt <- read_csv("https://github.com/ramirobentes/NBA-in-R/releases/download/pbp-final-gt-17cdf37/data.csv",
                         col_types = c(clock = "c",
                                       start_poss = "c"))
# url above might change daily (copy latest on data.csv here https://github.com/ramirobentes/NBA-in-R/releases/tag/pbp-final-gt-17cdf37)
lineup_stats <- read_csv("https://github.com/ramirobentes/NBA-in-R/blob/master/lineup-stats/data.csv?raw=true")

pbp_final_gt %>%
  filter(msg_type == 5) %>%
  count(lineup = ifelse(slug_team == team_home, lineup_away, lineup_home), name = "tov_forced") %>%
  left_join(lineup_stats %>%
              group_by(lineup, slug_team) %>%
              summarise(across(c(poss_opp, secs_played), sum)) %>%
              ungroup()) %>%
  filter(secs_played >= 200 * 60) %>%
  transmute(lineup, slug_team, tov_forced, poss_opp,
            mins_played = secs_played / 60,
            tov_rate = tov_forced / poss_opp * 100) %>%
  arrange(-tov_rate)