library(tidyverse)
library(rvest)

# Gets links for scoring summary info for each game of each season
get_schedule <- function(season) {
  
  url <- str_c("http://liiga.fi/ottelut/", season, "/runkosarja/")
  
  schedule <- url %>%
    read_html() %>%
    html_nodes('[title = "Tilastot"]') %>%
    html_attr("href") %>%
    str_c("http://liiga.fi/ottelut/", .) %>%
    as_tibble() %>%
    set_names("url") %>%
    mutate(season = season)
  
  return(schedule)
}

# Gets all the links into a data frame for all the seasons of interest
schedule <- map_df(c("2014-2015", "2015-2016", "2016-2017", "2017-2018"), get_schedule)

# Gets scoring summary box score info for specific game
get_box_score <- function(.row_num, .data) {

  progress_bar$tick()$print()
  
  seq(0, 0.5, by = 0.001) %>%
    sample(1) %>%
    Sys.sleep()
  
  selected_data <- .data %>% filter(row_number() == .row_num)
  
  page <- selected_data %>% 
    pull(url) %>% 
    read_html()
  
  game_states <- str_c(c("VL", "YV", "VM", "IM", "AV", "VT", "TV", "YV2", "SR", "RL", "TM"), collapse = "|")
  
  goals <- page %>%
    html_nodes(".goal-stats .ta-l") %>%
    html_text() %>%
    as_tibble() %>%
    set_names("jumbled_data") %>%
    mutate(jumbled_data = str_trim(jumbled_data, side = "both")) %>%
    mutate(jumbled_data = str_replace_all(jumbled_data, "[\r\n]", "")) %>%
    mutate(jumbled_data = str_split(jumbled_data, "\\+", simplify = TRUE, n = 2)[,1]) %>%
    mutate(game_strength = str_extract_all(jumbled_data, game_states)) %>%
    mutate(game_strength = str_replace_all(game_strength, "[[:punct:]]|^c", "")) %>%
    mutate(game_strength = if_else(game_strength == "haracter0", "", game_strength)) %>%
    mutate(game_strength = str_replace_all(game_strength, c("VL" = "SO", 
                                                            "YV" = "5v4", 
                                                            "VM" = "", 
                                                            "TM" = "EN", 
                                                            "RL" = "PS", 
                                                            "IM" = "EA", 
                                                            "AV" = "4v5", 
                                                            "VT" = "", 
                                                            "TV" = "4v4", 
                                                            "YV2" = "5v3", 
                                                            "SR" = "DP"))) %>%
    mutate(game_strength = if_else(game_strength == "", "5v5", game_strength)) %>%
    mutate(goal = str_split(jumbled_data, "\\(", simplify = TRUE, n = 2)[,1]) %>%
    mutate(goal = str_replace_all(goal, c("[[:digit:]]" = "", "\\#" = ""))) %>%
    mutate(goal = str_replace_all(goal, game_states, "")) %>%
    mutate(assists = str_split(jumbled_data, "\\(", simplify = TRUE, n = 2)[,2]) %>%
    mutate(primary_assist = str_split(assists, ",", simplify = TRUE, n = 2)[,1]) %>%
    mutate(primary_assist = str_split(primary_assist, "\\)", simplify = TRUE, n = 2)[,1]) %>%
    mutate(primary_assist = str_replace_all(primary_assist, c("[[:digit:]]" = "", "\\#" = ""))) %>%
    mutate(secondary_assist = str_split(assists, ",", simplify = TRUE, n = 2)[,2]) %>%
    mutate(secondary_assist = str_split(secondary_assist, "\\)", simplify = TRUE, n = 2)[,1]) %>%
    mutate(secondary_assist = str_replace_all(secondary_assist, c("[[:digit:]]" = "", "\\#" = ""))) %>%
    mutate(season = pull(selected_data, season)) %>%
    mutate_all(~ str_trim(., side = "both")) %>%
    mutate_all(as.character) %>%
    select(game_strength, goal, primary_assist, secondary_assist, season)

  home_team <- page %>%
    html_nodes(".home h2") %>%
    html_text()
  
  away_team <- page %>%
    html_nodes(".away h2") %>%
    html_text()
  
  score <- page %>%
    html_nodes(".goal .score") %>%
    html_text() %>%
    as_tibble() %>%
    set_names("score") %>%
    mutate(home_score = str_split(score, "", simplify = TRUE)[,1]) %>%
    mutate(away_score = str_split(score, "", simplify = TRUE)[,2]) %>%
    mutate_all(~ str_trim(., side = "both")) %>%
    mutate_all(as.character) %>%
    select(home_score, away_score)
  
  box_score_data <- goals %>%
    bind_cols(score) %>%
    mutate(home_team = home_team) %>%
    mutate(away_team = away_team) %>%
    mutate(team = case_when(home_score != lag(home_score, n = 1) ~ home_team,
                            away_score != lag(away_score, n = 1) ~ away_team,
                            pull(slice(., 1), home_score) == 1 ~ home_team,
                            pull(slice(., 1), away_score) == 1 ~ away_team)) %>%
    mutate_all(as.character) %>%
    select(team, game_strength, goal, primary_assist, secondary_assist, season)

  return(box_score_data)  
}

# Attempts the above function and moves on (and notifies user) if there's an error
try_get_box_score <- function(.row_num, .data) {
  
  tryCatch(get_box_score(.row_num, .data), 
           
           error = function(e) {
             print(e) 
             print(.row_num)
             data_frame()},
           
           warning = function(w) {
             print(w) 
             print(.row_num)
             data_frame()})
}

# Progress Bar
progress_bar <- schedule %>%
  as_tibble() %>%
  tally() %>%
  progress_estimated(min_time = 0)

# Gets scoring summary box score info for each game of every season of interest
sm_liiga_box_score_data <- 1:nrow(schedule) %>% map_df(try_get_box_score, .data = schedule)
