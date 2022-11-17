library(tidyverse)
library(hoopR)
library(janitor)

team_logs <- nba_teamgamelogs(season = "2022-23", team_id = "") %>%
  pluck("TeamGameLogs") %>%
  clean_names()

team_logs_adv <- nba_teamgamelogs(season = "2022-23", measure_type = "Advanced", team_id = "") %>%
  pluck("TeamGameLogs") %>%
  clean_names()

team_poss <- team_logs_adv %>%
  mutate(opp = map2_chr(str_extract_all(matchup, "[A-Z]{3}"), team_abbreviation, setdiff)) %>%
  transmute(game_id, team = team_abbreviation, opp, 
            poss = as.numeric(poss)) %>%
  pivot_longer(cols = c(team, opp),
               names_to = "names",
               values_to = "value") %>%
  group_by(names, value) %>%
  summarise(poss = sum(poss)) %>%
  ungroup()

tsa_plot <- team_logs %>%
  mutate(opp = map2_chr(str_extract_all(matchup, "[A-Z]{3}"), team_abbreviation, setdiff)) %>%
  transmute(game_id, team = team_abbreviation, opp, 
            across(c(fga, fta), as.numeric)) %>%
  pivot_longer(cols = c(team, opp),
               names_to = "names",
               values_to = "value") %>%
  group_by(names, value) %>%
  summarise(across(c(fga, fta), sum)) %>%
  ungroup() %>%
  mutate(tsa = fga + 0.44 * fta) %>%
  left_join(team_poss) %>%
  mutate(tsa100 = tsa / poss * 100) %>%
  select(names, team = value, tsa100) %>%
  pivot_wider(names_from = names,
              values_from = tsa100,
              names_prefix = "tsa_") %>%
  left_join(team_logs %>%
              distinct(team = team_abbreviation, team_id)) %>%
  mutate(logo_url = glue::glue("https://cdn.nba.com/logos/nba/{team_id}/global/L/logo.svg"),
         tsa_net = tsa_team - tsa_opp,
         team = fct_reorder(team, tsa_net)) %>%
  ggplot(aes(x = tsa_net, y = team)) +
  geom_col(aes(fill = tsa_net > 0)) +
  ggimage::geom_image(aes(image = logo_url), size = 0.045) +
  theme_light() +
  theme(legend.position = "none") +
  scale_x_continuous(limits = c(-10, 10)) +
  labs(title = "Net True Shooting Attempts per 100 possessions:",
       subtitle = "True Shooting Attempts calculated as FGA + 0.44 * FTA",
       y = "",
       x = "Net TSA / 100 poss")
