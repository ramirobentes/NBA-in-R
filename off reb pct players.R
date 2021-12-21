library(tidyverse)

final_poss_pack <- read_csv("https://github.com/ramirobentes/NBA-in-R/releases/download/final-poss-pack-18fcc7e/data.csv")
# above link might change daily
lineup_stats <- read_csv("https://raw.githubusercontent.com/ramirobentes/NBA-in-R/master/lineup-stats/data.csv")

shots_rebs <- final_poss_pack %>%
  filter(numberEventMessageType %in% c(2, 4) | numberEventMessageType == 3 & shotPtsHome + shotPtsAway == 0) %>%
  filter(!(numberEventMessageType == 4 & numberEventActionType == 1),
         !(numberEventMessageType == 3 & numberEventActionType %in% c(11, 13, 14, 16, 18:22, 25:26, 27:29))) %>%
  mutate(slugTeamPlayer1 = case_when(is.na(slugTeamPlayer1) & descriptionPlayHome == "" ~ slugTeamAway,
                                     is.na(slugTeamPlayer1) & descriptionPlayVisitor == "" ~ slugTeamHome,
                                     TRUE ~ slugTeamPlayer1)) %>%
  group_by(idGame, numberPeriod, shot_reb = ifelse(numberEventMessageType == 4, "rebound", "shot")) %>%
  mutate(sequence_num = row_number()) %>%
  ungroup() %>%
  arrange(idGame, numberPeriod, sequence_num) %>%
  mutate(desc_type = case_when(numberEventMessageType == 4 & slugTeamPlayer1 == lag(slugTeamPlayer1) ~ "off reb",
                               numberEventMessageType == 4 & slugTeamPlayer1 != lag(slugTeamPlayer1) ~ "def reb",
                               TRUE ~ "missed shot")) %>%
  mutate(lineup_team = ifelse(slugTeamPlayer1 == slugTeamHome, lineupHome, lineupAway)) %>%
  select(idGame, slugTeam = slugTeamPlayer1, namePlayer = namePlayer1, desc_type, lineup_team)

shots_rebs %>%
  filter(desc_type == "off reb") %>%
  count(namePlayer, slugTeam, name = "off_reb") %>%
  left_join(shots_rebs %>%
              filter(desc_type == "missed shot") %>%
              separate_rows(lineup_team, sep = ", ") %>%
              count(namePlayer = lineup_team, slugTeam, name = "off_reb_chances")) %>%
  mutate(off_reb_pct = off_reb / off_reb_chances) %>%
  arrange(-off_reb_pct) %>%
  left_join(lineup_stats %>%
              separate_rows(lineup, sep = ", ") %>%
              group_by(namePlayer = lineup) %>%
              summarise(games = n_distinct(idGame),
                        total_time = sum(totalTime)) %>%
              ungroup()) %>%
  filter(total_time / games >= 15 * 60,
         games >= 15)