library(tidyverse)

final_poss_pack <- read_csv("https://github.com/ramirobentes/NBA-in-R/releases/download/final-poss-pack-6e1650e/data.csv")
# url above might change daily

final_poss_pack %>%
  mutate(clutch = ifelse(secsPassedGame >= 2580 & abs(marginBeforeHome) <= 5, "clutch", "non_clutch")) %>%
  filter(numberEventMessageType == 3) %>%
  mutate(shot_result = ifelse(shotPtsHome + shotPtsAway == 0, "missed", "made")) %>%
  count(namePlayer = namePlayer1, shot_result, clutch) %>%
  group_by(namePlayer, clutch) %>%
  mutate(total_ft = sum(n),
         ft_pct = n / total_ft) %>%
  ungroup() %>%
  filter(shot_result == "made") %>%
  select(-shot_result) %>%
  filter(total_ft >= 10) %>%
  pivot_wider(names_from = clutch,
              values_from = c(n, total_ft, ft_pct)) %>%
  filter(!is.na(ft_pct_clutch)) %>%
  mutate(difference = ft_pct_clutch - ft_pct_non_clutch) %>%
  arrange(-difference)
