library(tidyverse)
library(nbastatR)

lineup_stats <- read_csv("https://raw.githubusercontent.com/ramirobentes/NBA-in-R/master/lineup-stats/data.csv")
teams_table <- nba_teams() %>%
  filter(isNonNBATeam == 0)

lineups_most <- lineup_stats %>%
  group_by(lineup, slugTeam) %>%
  summarise(total_time = sum(totalTime)) %>%
  ungroup() %>%
  arrange(-total_time) %>%
  group_by(slugTeam) %>%
  filter(row_number() == 1) %>%
  ungroup() %>%
  left_join(teams_table %>%
              separate(colorsTeam, into = paste("color", 1:3, sep = ""), sep = ", ") %>%
              select(slugTeam, starts_with("color"))) %>%
  mutate(slugTeam = fct_reorder(slugTeam, total_time),
         lineup_last = str_split(lineup, ", "),
         lineup_last = map(lineup_last, ~ gsub("^\\S* ", "", .)),
         lineup_last = map_chr(lineup_last, ~ paste(., collapse = ", ")))

lineups_most %>%
  ggplot(aes(x = total_time / 60,
             y = slugTeam)) +
  geom_col(fill = lineups_most$color1,
           color = lineups_most$color2) +
  geom_text(aes(label = lineup_last,
                hjust = ifelse(total_time < mean(range(total_time)), -0.015, 1.02),
                color = I(ifelse(total_time < mean(range(total_time)), "black", "white"))),
            size = 3,
            fontface = "bold") +
  theme_light() +
  labs(x = "time (in minutes)",
       y = "",
       title = "Total time on the court for each team's most used lineup")