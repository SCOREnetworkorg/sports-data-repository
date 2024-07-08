# PURPOSE: Get a table of the NFL during the Patrick Mahomes era, 2018 to now

library(tidyverse)
library(nflreadr)

# Load the data for the recent season -------------------------------------

# Considering the Mahomes era to start when he was named the starter
nfl_games <- load_schedules(seasons = 2018:2023)

# Set-up the data to be in an easy format ---------------------------------

nfl_games <- nfl_games |>
  dplyr::select(season, game_id, game_type, week, home_team, away_team, 
                home_score, away_score) |>
  mutate(game_outcome = case_when(
    home_score > away_score ~ 1,
    home_score == away_score ~ 0.5,
    .default = 0),
    score_diff = home_score - away_score)

# Save the data -----------------------------------------------------------

write_csv(nfl_games,
          "data/nfl_mahomes_era_games.csv")