library(tidyverse)
library(nbastatR)

player_logs <- game_logs(seasons = 2022, result_types = "player")
team_info <- nba_teams() %>%
  filter(isNonNBATeam == 0)

players_ts <- player_logs %>%
  group_by(namePlayer, slugTeam) %>%
  summarise(total_pts = sum(pts),
            total_fga = sum(fga),
            total_fta = sum(fta)) %>%
  ungroup() %>%
  mutate(ts_att = total_fga + (0.44 * total_fta),
         ts_pct = total_pts / (2 * ts_att)) %>%
  group_by(slugTeam) %>%
  mutate(total_team = sum(ts_att)) %>%
  ungroup() %>%
  mutate(tsa_player = ts_att / total_team) %>%
  filter(tsa_player >= 0.05) %>%
  mutate(last_name = gsub("^\\S* ", "", namePlayer)) %>%
  left_join(team_info %>%
              distinct(slugTeam, idConference)) %>%
  select(namePlayer, slugTeam, tsa_player, last_name, ts_pct, idConference) %>%
  arrange(-tsa_player)

players_ts %>%
  filter(idConference == 1) %>%  # change to 2 for western conference teams
  mutate(namePlayer = fct_reorder(namePlayer, tsa_player)) %>%
  ggplot(aes(x = tsa_player, y = slugTeam, fill = ts_pct)) +
  geom_col() +
  scale_fill_gradient2(low = "darkred", mid = "white", high = "darkgreen", midpoint = .55) +
  geom_text(aes(label = last_name), 
            position = position_stack(vjust = 0.5),
            color = "black", size = 3.5) +
  theme_bw() +
  theme(legend.box.background = element_rect(colour = "black", size = 1.2)) +
  labs(titlel = "Eastern Conference Shooting Breakdown",
       x = "Proportion of Team True Shot Attempts",
       y = "Team",
       fill = "True Shooting %")