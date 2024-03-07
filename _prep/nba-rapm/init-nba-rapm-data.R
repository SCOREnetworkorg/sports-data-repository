# PURPOSE: Initialize NBA RAPM data for 2022-2023 season

library(tidyverse)
library(hoopR)

# Gather games from last season -------------------------------------------

nba_game_log <- nba_leaguegamelog(season = 2022)
nba_game_log <- nba_game_log$LeagueGameLog

# Get the regular season game IDs:
nba_season_games <- nba_game_log |>
  pull(GAME_ID) |>
  unique()

# Generate the stint-level data -------------------------------------------

# Create a function that will be wrapped inside possibly to iterate across
# games, returning NULL if there is an error:

get_game_stint_data <- function(game_i) {
  
  game_data <- nba_pbp(game_id = game_i)
  
  # Populate first row values with 0 scores:
  game_data$home_score[1] <- 0
  game_data$away_score[1] <- 0
  game_data$score_margin[1] <- 0
  
  # Populate the missing values using fill()
  game_data <- game_data |>
    fill(home_score, away_score, score_margin) 
  
  # Get the home lineups:
  home_lineups <- apply(dplyr::select(game_data, home_player1, home_player2,
                                      home_player3, home_player4, home_player5),
                        1,
                        function(x) {
                          paste(sort(x), collapse = ("_"))
                        })
  
  # Repeat for away:
  away_lineups <- apply(dplyr::select(game_data, away_player1, away_player2,
                                      away_player3, away_player4, away_player5),
                        1,
                        function(x) {
                          paste(sort(x), collapse = ("_"))
                        })
  
  # Join these columns:
  game_data$home_lineup <- home_lineups
  game_data$away_lineup <- away_lineups
  
  # Now there are a couple of ways to figure out the stints, but I think the
  # best way is to use the substitution events - where a stint changes once
  # a substitution takes places (but only if the previous event was NOT a substitution)
  
  # Start by making an indicator for the stint change:
  game_data <- game_data |>
    mutate(is_sub = ifelse(event_type == 8, 1, 0),
           new_stint_start = ifelse((is_sub == 1) & (lead(is_sub) != 1),
                                    1, 0))
  
  # Hmm have to figure out the free throw issue - for substitutions that take place
  # during a free throw window, then the new stint should start after the free throw
  
  # Easiest way to track if substitution takes places before final free throw:
  game_data <- game_data |>
    mutate(sub_during_free_throw = case_when(
      (str_detect(visitor_description, "Free Throw 1 of 1") |
         str_detect(visitor_description, "Free Throw 2 of 2") |
         str_detect(visitor_description, "Free Throw 3 of 3")) &
        (lag(is_sub) == 1) ~ 1,
      (str_detect(home_description, "Free Throw 1 of 1") |
         str_detect(home_description, "Free Throw 2 of 2") |
         str_detect(home_description, "Free Throw 3 of 3")) &
        (lag(is_sub) == 1) ~ 1,
      .default = 0),
      # Now if the sub is followed by this, then set new_stint_start to 0, but
      # if this set a new stint to start post the final free throw:
      new_stint_start = ifelse(is_sub == 1 & lead(sub_during_free_throw) == 1,
                               0, new_stint_start),
      new_stint_start = ifelse(lag(sub_during_free_throw) == 1,
                               1, new_stint_start),
      new_stint_start = ifelse(is.na(new_stint_start), 0, new_stint_start)
    )
  
  # I think I can just filter out subs that are not new stints, and then just use
  # the cumulative sum of the new stint start to effectively create a stint ID:
  game_data <- game_data |>
    filter(!(is_sub == 1 & new_stint_start == 0)) |>
    mutate(stint_id = cumsum(new_stint_start) + 1)
  
  # Toughest part - need to count the number of possessions for each team during
  # the stint... will rely on this for counting when a possession ends:
  # https://squared2020.com/2017/09/18/deep-dive-on-regularized-adjusted-plus-minus-ii-basic-application-to-2017-nba-data-with-r/
  # "Recall that a possession is ended by a converted last free throw, made field goal, defensive rebound, turnover, or end of period"
  
  game_data <- game_data |>
    mutate(pos_ends = case_when(
      str_detect(home_description, " PTS") &
        str_detect(home_description, "Free Throw 1 of 2",
                   negate = TRUE) &
        str_detect(home_description, "Free Throw 2 of 3",
                   negate = TRUE) ~ 1, # made field goals or free throws
      str_detect(visitor_description, " PTS") & 
        str_detect(visitor_description, "Free Throw 1 of 2",
                   negate = TRUE) & 
        str_detect(visitor_description, "Free Throw 2 of 3",
                   negate = TRUE) ~ 1, 
      str_detect(tolower(visitor_description), "rebound") &
        str_detect(tolower(lag(home_description)), "miss ") ~ 1,
      str_detect(tolower(home_description), "rebound") &
        str_detect(tolower(lag(visitor_description)), "miss ") ~ 1,
      str_detect(tolower(home_description), " turnover") ~ 1,
      str_detect(tolower(visitor_description), " turnover") ~ 1,
      str_detect(neutral_description, "End") ~ 1,
      .default = 0
    ))
  
  # Now the final part - compute the stint level summaries:
  game_data |>
    group_by(stint_id) |>
    summarize(home_lineup = first(home_lineup),
              away_lineup = first(away_lineup),
              n_home_lineups = length(unique(home_lineup)),
              n_away_lineups = length(unique(away_lineup)),
              start_home_score = first(home_score),
              end_home_score = last(home_score),
              start_away_score = first(away_score),
              end_away_score = last(away_score),
              start_minutes = first(minute_game),
              end_minutes = last(minute_game),
              n_pos = sum(pos_ends),
              .groups = "drop") |>
    # Compute the necessary changes:
    mutate(home_points = end_home_score - start_home_score,
           away_points = end_away_score - start_away_score,
           minutes = end_minutes - start_minutes,
           margin = 100 * (home_points - away_points) / n_pos,
           game_id = game_i) |>
    # Just keep the main items:
    dplyr::select(game_id, stint_id, home_lineup, away_lineup, n_pos,
                  home_points, away_points, minutes, margin) |>
    # Finally remove the useless stints with 0 possessions:
    filter(n_pos != 0)
  
}

# Create possibly version of the game stint data function:
poss_get_game_stints <- possibly(.f = get_game_stint_data, 
                                 otherwise = NULL)

# And now apply this to each game in the season to generate the data:
season_stint_data <- map_dfr(nba_season_games, ~poss_get_game_stints(.x))

# How many games are in the data?
length(unique(season_stint_data$game_id))
# [1] 1225

# Nice - only missing 5 games

# Save this dataset:
write_csv(season_stint_data,
          "data/nba_2223_season_stints.csv")

# Create the RAPM design matrix -------------------------------------------

# Will now create three datasets:

# (1) The game-stint level context:
game_stint_context <- season_stint_data |>
  dplyr::select(game_id, stint_id, n_pos, home_points, away_points,
                minutes, margin)

# (2) home team players with values of 1:
# Offense players:
home_players_data <- season_stint_data %>%
  dplyr::select(game_id, stint_id, home_lineup) %>%
  separate_rows(home_lineup, sep = "_") %>%
  mutate(on_court = 1) %>%
  pivot_wider(id_cols = c("game_id", "stint_id"),
              names_from = home_lineup,
              values_from = on_court,
              values_fill = 0)

# Check that the columns are unique players:
home_players_cols <- colnames(home_players_data)[3:ncol(home_players_data)]
length(home_players_cols) == length(unique(home_players_cols))
# [1] TRUE - good

# (3) Repeat for away, except use -1 for away:
away_players_data <- season_stint_data %>%
  dplyr::select(game_id, stint_id, away_lineup) %>%
  separate_rows(away_lineup, sep = "_") %>%
  mutate(on_court = -1) %>%
  pivot_wider(id_cols = c("game_id", "stint_id"),
              names_from = away_lineup,
              values_from = on_court,
              values_fill = 0)

# Check that the columns are unique players:
away_players_cols <- colnames(away_players_data)[3:ncol(away_players_data)]
length(away_players_cols) == length(unique(away_players_cols))
# [1] TRUE - good

# Stack the home and away together, take the sum so that we get one row for 
# each game stint, where home lineup players are 1 and away lineup players are -1:
game_stint_players_data <- home_players_data |>
  bind_rows(away_players_data) |>
  group_by(game_id, stint_id) |>
  summarize(across(everything(), ~ sum(.x, na.rm = TRUE)),
            .groups = "drop")

# Join this back to the context and save:
game_stint_rapm_data <- game_stint_context |>
  left_join(game_stint_players_data,
            by = c("game_id", "stint_id"))

# Save this dataset:
write_csv(game_stint_rapm_data,
          # Compress this since it's fairly big
          "data/nba_2223_season_rapm_data.csv.gz")


# Get the player ids to join for viewing later ----------------------------

nba_players <- nba_commonallplayers(season = 2022)
nba_players <- nba_players$CommonAllPlayers

# Grab the player ids from the RAPM data:
player_ids <- game_stint_rapm_data |>
  dplyr::select(-c(game_id, stint_id, n_pos, home_points, away_points,
                   minutes, margin)) |>
  colnames() |>
  as.integer()

player_table <- nba_players |>
  filter(PERSON_ID %in% player_ids) |>
  dplyr::select(PERSON_ID, DISPLAY_FIRST_LAST) |>
  rename(player_id = PERSON_ID,
         player_name = DISPLAY_FIRST_LAST) |>
  distinct()

# Great - they are all in here
write_csv(player_table,
          "data/nba_2223_player_table.csv")



