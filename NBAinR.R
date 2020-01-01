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


# Jimmy Butler's shooting averages in the clutch (https://twitter.com/NbaInRstats/status/1212099568973885441)
heat_games <- games %>%
  pivot_longer(cols = starts_with("nameTeam"),
               names_to = "locationTeam",
               values_to =  "nameTeam") %>%
  filter(nameTeam == "Miami Heat") %>%
  pull(idGame)

plan(multiprocess)
play_logs_heat <- play_by_play_v2(heat_games) 

play_logs_heat %>%
  mutate(marginScore = ifelse(row_number() == 1, 0, marginScore), 
         marginScore = zoo::na.locf(marginScore)) %>%               # replace NA with last non-NA value
  filter(abs(lag(marginScore)) <= 5,                                # the score margin before the play happened should be 5 or less
         minuteGame >= 43) %>%                                      # less than 5 minutes left in 4th or OT
  filter(namePlayer1 == "Jimmy Butler") %>% 
  pivot_longer(cols = starts_with("descriptionPlay"),
               names_to = "locationTeamPlay",
               values_to = "descriptionPlay") %>%
  filter(!is.na(descriptionPlay)) %>%
  mutate(descriptionPlay = str_to_lower(descriptionPlay)) %>%
  mutate(shotType = case_when(
    str_detect(descriptionPlay, "free throw") ~ "FreeThrow",
    str_detect(descriptionPlay, "3pt") ~ "3ptFieldGoal",
    str_detect(descriptionPlay, "jumper|layup|shot") & !str_detect(descriptionPlay, "3pt") ~ "2ptFieldGoal",
    TRUE ~ "No shot"
  )) %>%
  filter(shotType != "No shot") %>%
  count(shotType, shotResult = ifelse(str_detect(descriptionPlay, "miss"), "Missed", "Made")) %>%
  pivot_wider(names_from = shotResult,
              values_from = n) %>%
  mutate(Attempted = Made + Missed,
         Percentage = Made/Attempted) %>%
  select(-Missed)


# Get advanced box scores for every game of the 2019/2020 season
plan(multiprocess)
advanced_teams <- box_scores(game_ids = unique(games$idGame),
                             box_score_types = "Advanced",
                             result_types = "team")

# Calculate pts per possession, FG% for every Dallas game (https://twitter.com/NbaInRstats/status/1212112017340932096)
logs_players %>%
  group_by(idGame, slugTeam, slugOpponent) %>%
  summarise(totalPts = sum(pts),
            totalFgMade = sum(fgm),
            totalFgAtt = sum(fga)) %>%
  ungroup() %>%
  left_join(advanced_teams %>%
              unnest(dataBoxScore) %>%
              select(idGame, slugTeam, possessions)) %>%
  mutate(ptsPerPoss = totalPts/possessions,
         fgPct = totalFgMade/totalFgAtt) %>%
  select(-c(idGame, totalFgMade, totalFgAtt)) %>%
  filter(slugTeam == "DAL") %>%
  arrange(ptsPerPoss)

# Calculate pts per possession for Brooklyn games where Kyrie didn't play (https://twitter.com/NbaInRstats/status/1212432244775800838)
logs_players %>%
  filter(dateGame < as.Date("2019-12-30")) %>%
  group_by(idGame, slugTeam) %>%
  summarise(totalPts = sum(pts),
            whoPlayed = paste(unique(namePlayer), collapse = ", ")) %>%    # create column with every player who played in the game
  ungroup() %>%
  left_join(advanced_teams %>%
              unnest(dataBoxScore) %>%
              select(idGame, slugTeam, possessions)) %>%
  filter(slugTeam == "BKN") %>%
  filter(!str_detect(whoPlayed, "Kyrie Irving")) %>%                       # filter for games where Kyrie Irving did not play
  group_by(gameNumber = ifelse(row_number() <= 13, "First 13", "Last 7")) %>%
  summarise(numberGames = n(),
            totalPts = sum(totalPts),
            totalPoss = sum(possessions)) %>%
  ungroup() %>%
  mutate(ptsPerPoss = totalPts/totalPoss,
         ptsPerPoss = round(ptsPerPoss * 100, 1)) %>%
  mutate_all(list(~ as.character(.)))
