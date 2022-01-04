library(tidyverse)
library(nbastatR)

final_poss_pack <- read_csv("https://github.com/ramirobentes/NBA-in-R/releases/download/final-poss-pack-eb8415a/data.csv")
# url might change daily
team_logs <- game_logs(seasons = 2022, result_types = "team")
season_shots <- teams_shots(seasons = 2022,
                            team_ids = unique(team_logs$idTeam))

season_shots %>%
  filter(lubridate::ymd(dateGame) < as.Date("2022-01-03")) %>%
  left_join(final_poss_pack %>%
              mutate(lineup = ifelse(slugTeamPlayer1 == slugTeamHome, lineupHome, lineupAway)) %>%
              select(idGame, numberPeriod, idEvent = numberOriginal, lineup, slugTeam = slugTeamPlayer1)) %>%
  filter(slugTeam == "LAL") %>%
  mutate(lineup_type = ifelse(str_detect(lineup, "LeBron James") & !str_detect(lineup, "Anthony Davis|Dwight Howard|DeAndre Jordan"), 
                              "james_as_big", 
                              "other_lineups"),
         zone_type = ifelse(zoneBasic == "Restricted Area", "restricted_area", "other")) %>%
  count(namePlayer, zone_type, typeEvent, lineup_type) %>%
  filter(namePlayer %in% c("LeBron James", "Russell Westbrook")) %>%
  group_by(namePlayer, lineup_type) %>%
  summarise(total_rest_area = sum(n[which(zone_type == "restricted_area")]),
            total_fgm = sum(n[which(typeEvent == "Made Shot")]),
            total_fga = sum(n)) %>%
  ungroup() %>%
  mutate(fg_pct = total_fgm / total_fga,
         fg_rest_area = total_rest_area / total_fga)