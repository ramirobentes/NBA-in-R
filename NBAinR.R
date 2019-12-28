library(nbastatR)
library(tidyverse)
library(lubridate)
library(future)
library(janitor)


# Data sources ------------------------------------------------------------

# Get every game of the 2019/2020 season so far
games <- current_schedule() %>%
  filter(idGame >= 21900001,        # first regular season game
         dateGame < Sys.Date())

# Get every shot of the 2019/2020 season
plan(multiprocess)
shots_2020 <- teams_shots(teams = unique(games$nameTeamAway),
                         seasons = 2020)

# Get game logs for every game of the 2019/2020 season
plan(multiprocess)
logs_players <- game_logs(2020, type = "player")

# Get players data
plan(multiprocess)
players <- player_profiles(player_ids = unique(logs_players$idPlayer))



# Analysis ----------------------------------------------------------------

## Points in the paint differential per team (https://twitter.com/NbaInRstats/status/1210283385517727746)
shots_2020 %>%
  filter(ymd(dateGame) < as.Date("2019-12-19")) %>%     # only games played before the article was published
  filter(zoneBasic %in% c("Restricted Area", "In The Paint (Non-RA)")) %>%
  inner_join(shots_2020 %>%
               distinct(idGame, nameTeam) %>%
               arrange(idGame) %>%
               group_by(idGame) %>%
               mutate(nameOpponent = ifelse(row_number() == 1, nameTeam[2], nameTeam[1]))) %>%  # get opponent for every game
  filter(isShotMade == TRUE) %>%
  count(idGame, nameTeam, nameOpponent) %>%
  pivot_longer(cols = c("nameTeam", "nameOpponent"),
               names_to = "Type",
               values_to = "Team") %>%
  group_by(Team) %>%
  mutate(games = n_distinct(idGame)) %>%
  ungroup() %>%
  group_by(Type, Team, games) %>%
  summarise(Total = sum(n)) %>%
  ungroup() %>%
  mutate(Total = Total * 2) %>%     # every made shot in the paint is worth 2 points
  pivot_wider(names_from = Type,
              values_from = Total) %>%
  mutate(difference = nameTeam - nameOpponent) %>%
  mutate(teamGame = nameTeam/games,
         oppGame = nameOpponent/games,
         diffGame = difference/games) %>%
  arrange(desc(diffGame)) %>%
  select(team = Team, games, teamTotal = nameTeam, oppTotal = nameOpponent, diffTotal = difference, teamGame, oppGame, diffGame)
  


## Percentage of minutes played by rookies and 2nd year players per team (https://twitter.com/NbaInRstats/status/1210294571617443855)
logs_players %>%
  filter(dateGame < as.Date("2019-12-23")) %>%    # date of article
  left_join(players %>%
              select(idPlayer, countSeasonsPlayed)) %>%
  mutate(rookieOr2nd = ifelse(countSeasonsPlayed < 2 | is.na(countSeasonsPlayed), "rookieOr2nd", "notRookieOr2nd")) %>%
  group_by(nameTeam, rookieOr2nd) %>%
  summarise(totalMin = sum(minutes)) %>%
  ungroup() %>%
  pivot_wider(names_from = rookieOr2nd,
              values_from = totalMin,
              values_fill = list(totalMin = 0)) %>%
  mutate(percRookieOr2nd = rookieOr2nd/(rookieOr2nd + notRookieOr2nd)) %>%
  arrange(desc(percRookieOr2nd)) %>%
  left_join(logs_players %>%
              filter(dateGame < as.Date("2019-12-23")) %>%
              group_by(idGame, nameTeam) %>%
              summarise(totalPts = sum(pts)) %>%
              ungroup() %>%
              group_by(idGame) %>%
              mutate(Result = ifelse(totalPts == max(totalPts), "Win", "Loss")) %>%
              ungroup() %>%
              count(nameTeam, Result) %>%
              pivot_wider(names_from = Result,
                          values_from = n) %>%
              mutate(gamesUnder500 = Loss - Win))     # calculate team record and # of games under .500


## Best 3-point shooters (minimum 5 attempts per game) (https://twitter.com/NbaInRstats/status/1210297047737348096)
logs_players %>%
  filter(dateGame < as.Date("2019-12-23")) %>%    # date of article
  group_by(namePlayer) %>%
  summarise(total3made = sum(fg3m),
            total3att = sum(fg3a),
            games = n_distinct(idGame)) %>%
  ungroup() %>%
  mutate(att3game = total3att/games,
         fg3perc = total3made/total3att) %>%
  filter(att3game >= 5) %>%    # minimum 5 attempts per game
  arrange(desc(fg3perc))


## Number of 3s assisted by Sixers players (https://twitter.com/NbaInRstats/status/1210386691850166273)
sixers_games <- games %>%
  pivot_longer(cols = starts_with("nameTeam"),
               names_to = "locationTeam",
               values_to =  "nameTeam") %>%
  filter(nameTeam == "Philadelphia 76ers") %>%
  pull(idGame)    # getting every Sixers game

plan(multiprocess)
play_logs_sixers <- play_by_play_v2(sixers_games)   # getting play-by-play data from Sixers games

play_logs_sixers %>%
  pivot_longer(cols = starts_with("descriptionPlay"),
               names_to = "locationTeamPlay",
               values_to = "descriptionPlay") %>%
  filter(teamNamePlayer1 == "76ers") %>%
  filter(str_detect(descriptionPlay, "3PT"),
         !str_detect(descriptionPlay, "MISS")) %>%
  mutate(Detail = str_extract_all(descriptionPlay, "(?<=\\().+?(?=\\))")) %>%
  select(descriptionPlay, Detail) %>%
  unnest_wider(Detail) %>%
  rename(detailAst = 3) %>%
  count(playerAst = word(detailAst, 1)) %>%
  replace_na(list(playerAst = "No assist")) %>%
  mutate(percentage = n/sum(n)) %>%
  arrange(desc(percentage)) %>%
  adorn_totals("row")
         

## Find FG% in paint, from mid-range and 3-point range for every team (https://twitter.com/NbaInRstats/status/1210981275525361666)
shots_2020 %>%
  mutate(zoneBasicNew = case_when(
    str_detect(zoneBasic, "3") ~ "3point",
    zoneBasic == "Backcourt" ~ "3point",
    zoneBasic %in% c("Restricted Area", "In The Paint (Non-RA)") ~ "Paint",
    zoneBasic == "Mid-Range" ~ "MidRange",
    TRUE ~ zoneBasic
  )) %>%
  count(nameTeam, zoneBasicNew, isShotMade) %>%
  group_by(nameTeam, zoneBasicNew) %>%
  mutate(percMade = n/sum(n)) %>%
  ungroup() %>%
  filter(isShotMade == TRUE) %>%
  group_by(zoneBasicNew) %>%
  mutate(Rank = dense_rank(desc(percMade))) %>%
  select(-c(isShotMade, n)) %>%
  pivot_wider(names_from = zoneBasicNew,
              values_from = c(percMade, Rank)) %>%
  select(Team = nameTeam, 
         Paint = percMade_Paint, RankPaint = Rank_Paint,
         MidR = percMade_MidRange, RankMidR = Rank_MidRange,
         Three = percMade_3point, RankThree = Rank_3point
  ) %>%
  mutate_at(vars(c(Paint, MidR, Three)), list(~ paste0(round(. * 100, 1), "%")))