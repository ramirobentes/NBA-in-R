library(tidyverse)
library(hoopR)
library(gt)
library(httr)
library(jsonlite)
library(janitor)
library(glue)
library(lubridate)

# Getting data from pbpstats.com

onoff_call <- GET("https://api.pbpstats.com/get-wowy-combination-stats/nba?Season=2021-22&SeasonType=Regular%20Season&TeamId=1610612755&PlayerIds=201935%2C202699%2C203954%2C1630178&OnlyCommonGames=true")

onoff_table <- fromJSON(rawToChar(onoff_call$content)) %>%
  pluck("results") %>%
  as_tibble() %>%
  clean_names() %>%
  transmute(on_court = on, off_court = off, mins_played = round(minutes, 0), 
            across(c(pts100 = off_rtg, opp100 = def_rtg, net100 = net_rtg), ~ round(., 1)))  %>%
  mutate(across(c(on_court, off_court), ~ case_when(str_count(., "\\,") == 3 ~ "All",
                                                    . == "" ~ "None",
                                                    TRUE ~ .)),
         across(c(on_court, off_court), ~ str_split(., ", ")),
         across(c(on_court, off_court), ~ map_chr(map(., ~ gsub("^\\S* ", "", .)), ~ paste(sort(.), collapse = "-")))) %>%
  arrange(-mins_played, on_court) %>%
  filter(mins_played >= 25)

# Getting data from play-by-play scraper

lineup_stats <- read_csv("https://github.com/ramirobentes/NBA-in-R/blob/master/lineup-stats/data.csv?raw=true")

chosen_players <- list(c("Joel Embiid", "James Harden", "Tobias Harris", "Tyrese Maxey"))

games_all <- lineup_stats %>%
  separate_rows(lineup, sep = ", ") %>%
  group_by(game_id, slug_team) %>%
  summarise(all_players = list(unique(lineup))) %>%
  ungroup() %>%
  filter(map_lgl(all_players, ~ all(chosen_players[[1]] %in% .x))) %>%
  distinct(game_id, slug_team)

onoff_table <- lineup_stats %>%
  semi_join(games_all) %>%
  mutate(lineup_list = str_split(lineup, ", "),
         on_court = map2(lineup_list, chosen_players, intersect),
         off_court = map2(chosen_players, on_court, setdiff),
         across(c(on_court, off_court), ~ case_when(map_int(., length) == 0 ~ "None",
                                                    map_int(., length) == 4 ~ "All",
                                                    TRUE ~  map_chr(map(., ~ gsub("^\\S* ", "", .)), ~ paste(sort(.), collapse = "-"))))) %>%
  group_by(on_court, off_court) %>%
  summarise(across(c(pts_team:secs_played), sum)) %>%
  ungroup() %>%
  mutate(mins_played = round(secs_played / 60, 0),
         pts100 = round(pts_team / poss_team * 100, 1),
         opp100 = round(pts_opp / poss_opp * 100, 1),
         net100 = pts100 - opp100) %>%
  arrange(-mins_played) %>%
  filter(mins_played >= 25) %>%
  select(-c(pts_team:secs_played))

# GT table

onoff_table %>%
  gt() %>%
  cols_label(on_court = "ON COURT",
             off_court = "OFF COURT",
             mins_played = "MIN",
             pts100 = "O-RTG",
             opp100 = "D-RTG",
             net100 = "NET RTG")  %>% 
  cols_width(on_court ~ px(60),
             off_court ~ px(60),
             mins_played ~ px(20),
             pts100 ~ px(25),
             opp100 ~ px(25),
             net100 ~ px(30)) %>%
  tab_options(table.width = px(730),
              data_row.padding = px(5),
              column_labels.font.size = 14,
              heading.title.font.size = 21,
              table.font.size = 15,
              heading.subtitle.font.size = 17,
              source_notes.font.size = 17,
              column_labels.border.top.color = "white",
              column_labels.border.top.width = px(3),
              column_labels.border.bottom.color = "black",
              table.border.top.color = "white",
              table.border.bottom.color = "white",
              table_body.border.bottom.color = "white") %>%
  tab_header(title = "The Sixers are best with all four of their stars on the court",
             subtitle = html("Ratings for the Philadelphia 76ers by lineup combination using Joel Embiid, James<br>Harden, Tobias Harris and Tyrese Maxey")) %>%
  tab_source_note(source_note = md(glue("Only for games in which all four have played. Through {month(Sys.Date(), label = TRUE, abbr = FALSE)} {day(Sys.Date()) - 1}, {year(Sys.Date())}."))) %>%
  tab_source_note(source_note = md("SOURCE: PBP STATS")) %>%
  data_color(columns = net100,
             colors = scales::col_numeric(palette = c("#D861CD", "white", "#0DC846"),
                                          domain = c(min(onoff_table$net100), max(onoff_table$net100)))) %>%
  tab_style(style = list(cell_fill(color = "black", alpha = 0.055)),
            locations = cells_body(columns = mins_played)) %>%
  tab_style(style = cell_text(color = "black", align = "left"),
            locations = cells_title(c("title", "subtitle"))) %>%
  tab_style(style = list(cell_text(font = google_font(name = "Arimo"))),
            locations = list(cells_body(c(on_court, off_court)),
                             cells_column_labels(everything()),
                             cells_title("title"),
                             cells_title("subtitle"),
                             cells_source_notes())) %>%
  tab_style(style = list(cell_text(weight = "bold")),
            locations = list(cells_body(everything()),
                             cells_title("title"),
                             cells_source_notes())) %>%
  tab_style(style = list(cell_text(font = google_font(name = "Barlow Condensed"))),
            locations = list(cells_body(c(mins_played, pts100, opp100, net100)),
                             cells_source_notes())) %>%
  tab_style(style = list(cell_text(color = "black", weight = "bold", align = "left")),
            locations = cells_title("title")) %>%
  tab_style(style = list(cell_text(color = "darkgrey")),
            locations = cells_source_notes())
