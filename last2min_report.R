library(tidyverse)
library(jsonlite)
library(httr)
library(janitor)
library(hoopR)

player_logs <- nba_leaguegamelog(season = "2022-23", player_or_team = "T") %>%
  pluck("LeagueGameLog") %>%
  clean_names()

games <- player_logs %>%
  distinct(game_id) %>%
  mutate(url = glue::glue("https://official.nba.com/l2m/json/{game_id}.json"))

l2m_calls_fun <- function(x){
  
  url_pegar <- GET(x)
  url_content <- url_pegar$content %>%
    rawToChar() %>%
    fromJSON()
  
  url_content$l2m %>%
    bind_cols(url_content$game) %>%
    as_tibble()
  
}

l2m_calls <- map_df(games$url, possibly(l2m_calls_fun))

l2m_calls
