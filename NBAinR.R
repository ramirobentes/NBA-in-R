library(nbastatR)
library(tidyverse)
library(lubridate)
library(future)
library(janitor)

# Get every game of the 2019/2020 season so far
games <- current_schedule() %>%
  filter(idGame >= 21900001,        # first regular season game
         dateGame < Sys.Date())


# Get every shot of the 2019/2020 season
plan(multiprocess)
shots_2020 <- team_shots(teams = unique(games$nameTeamAway),
                         seasons = 2020)


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
  
