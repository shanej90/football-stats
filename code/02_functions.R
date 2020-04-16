#function to locate files---------------------------------------------
FileLocationFn <- function(url) {
  
  #url: online json file location
  
  json_data <- fromJSON(readLines(url))
  
  types <- json_data$resources$datahub$type
  
  json_data$resources$path[types == "derived/csv"]
  
}

#function to read data----------------------------
ReadFn <- function(locations) {
    
   read_csv(locations) %>%
      clean_names() %>%
      mutate(
        date = dmy(date),
        season = ifelse(
          month(date) >= 8, 
          paste0(year(date), "/", as.numeric(substr(year(date), 3, 4)) + 1), 
          paste0(year(date) - 1, "/", substr(year(date), 3, 4))
          ),
        match_id = str_pad(row_number(), width = 3, side = "left", pad = "0")
        ) %>%
    #convert home/away team to tall format
    pivot_longer(
      cols = c(home_team:away_team),
      names_to = "h_a",
      values_to = "team"
    ) %>%
    mutate(h_a = str_replace(h_a, "_team", "")) %>%
    #get goals data into tall format
    pivot_longer(
      cols = c(fthg:ftag),
      names_to = "temporary",
      values_to = "goals"
    ) %>%
    filter((h_a == "home" & temporary == "fthg") | (h_a == "away" & temporary == "ftag")) %>%
    select(-temporary, -ftr, -htr) %>% #first select so also using to get rid of some unnecessary columns
    #now get half-time goals in
    pivot_longer(
      cols = c(hthg:htag),
      names_to = "temporary",
      values_to = "ht_goals"
    ) %>%
    filter((h_a == "home" & temporary == "hthg") | (h_a == "away" & temporary == "htag")) %>%
    select(-temporary) %>%
    #total shots data in tall fromat
    pivot_longer(
      cols = c(hs:as),
      names_to = "temporary",
      values_to = "shots"
    ) %>%
    filter((h_a == "home" & temporary == "hs") | (h_a == "away" & temporary == "as")) %>%
    select(-temporary) %>%
    #shots on target in tall format
    pivot_longer(
      cols = c(hst:ast),
      names_to = "temporary",
      values_to = "on_target"
    ) %>%
    filter((h_a == "home" & temporary == "hst") | (h_a == "away" & temporary == "ast")) %>%
    select(-temporary) %>%
    #work out each team's points
    group_by(match_id) %>%
    mutate(points = case_when(
      goals > min(goals) ~ 3,
      goals < max(goals) ~ 0,
      TRUE ~ 1
      )
      ) %>%
    ungroup() %>%
    #work out cumulative points
    group_by(team) %>%
    mutate(cum_points = cumsum(points)) %>%
    ungroup()
      }
