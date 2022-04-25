library(tidyverse)
library(hoopR)
library(janitor)

pbp21 <- read_rds("https://github.com/ramirobentes/NBA-in-R/blob/master/pbp-final-gt21/data.rds?raw=true")    
pbp22 <- read_rds("https://github.com/ramirobentes/NBA-in-R/blob/master/pbp-final-gt/data.rds?raw=true")
pbp_post21 <- read_rds("https://github.com/ramirobentes/NBA-in-R/blob/master/pbp-final-gt-post21/data.rds?raw=true")
pbp_post22 <- read_rds("https://github.com/ramirobentes/NBA-in-R/blob/master/pbp-final-gt-post/data.rds?raw=true")
lineup_stats21 <- read_csv("https://github.com/ramirobentes/NBA-in-R/blob/master/lineup-stats21/data.csv?raw=true")
lineup_stats22 <- read_csv("https://github.com/ramirobentes/NBA-in-R/blob/master/lineup-stats/data.csv?raw=true")
lineup_stats_post21 <- read_csv("https://github.com/ramirobentes/NBA-in-R/blob/master/lineup-stats-post21/data.csv?raw=true") 
lineup_stats_post22 <- read_csv("https://github.com/ramirobentes/NBA-in-R/blob/master/lineup-stats-post/data.csv?raw=true")

foul_stats <- bind_rows(pbp21, pbp22, pbp_post21, pbp_post22) %>%
  filter(!str_starts(game_id, "5")) %>%
  mutate(type = ifelse(str_starts(game_id, "4"), "playoffs", "regular")) %>%
  filter(slug_team == "MEM",
         msg_type == 6 & act_type %in% c(1, 2, 3, 4, 6, 9, 14, 15, 26, 28)) %>%
  filter(player1 == "Jaren Jackson Jr.") %>%
  mutate(lineup = ifelse(slug_team == team_home, lineup_home, lineup_away)) %>%
  mutate(lineup = case_when(str_detect(lineup, "Steven Adams") ~ "Adams",
                               str_detect(lineup, "Brandon Clarke") ~ "Clarke",
                               str_detect(lineup, "Xavier Tillman|Valanciunas") ~ "1+ other Big",
                               TRUE ~ "Lone")) %>%
  count(type, lineup) %>%
  left_join(bind_rows(lineup_stats21, lineup_stats22, lineup_stats_post21, lineup_stats_post22) %>%
              filter(!str_starts(game_id, "5")) %>%
              mutate(type = ifelse(str_starts(game_id, "4"), "playoffs", "regular")) %>%
              filter(slug_team == "MEM",
                     str_detect(lineup, "Jaren Jackson Jr.")) %>%
              mutate(lineup = case_when(str_detect(lineup, "Steven Adams") ~ "Adams",
                                           str_detect(lineup, "Brandon Clarke") ~ "Clarke",
                                           str_detect(lineup, "Xavier Tillman|Valanciunas") ~ "1+ other Big",
                                           TRUE ~ "Lone")) %>%
              group_by(type, lineup) %>%
              summarise(mins = round(sum(secs_played) / 60, 0)) %>%
              ungroup())

foul_stats %>%
  group_by(type = fct_rev(as.factor(type)), with_big = ifelse(lineup == "Lone", "no", "yes")) %>%
  mutate(across(c(n, mins), ~ ifelse(lineup == "1+ other Big", sum(.), .))) %>%
  ungroup() %>%
  mutate(foulrate = round(n / mins * 36, 1)) %>%
  mutate(lineup = ifelse(with_big == "yes", glue::glue("JJJ with {lineup}"), "JJJ as the only Big")) %>%
  arrange(with_big, lineup) %>%
  select(-c(n, with_big)) %>%
  pivot_wider(names_from = type,
              values_from = c("mins", "foulrate")) %>%
  select(lineup, mins_regular, mins_playoffs, foulrate_regular, foulrate_playoffs)
