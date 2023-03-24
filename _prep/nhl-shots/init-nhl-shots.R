# PURPOSE: Gather NHL shots during the 2021-2022 regular season

# First install the hockeyR package if not installed already:
# devtools::install_github("danmorse314/hockeyR")

library(hockeyR)
library(tidyverse)

# Load shots during 2022 season -------------------------------------------

# First load the pbp for 2021-2022 regular season:
nhl_pbp_data <- load_pbp(2021)


# Filter and clean data ---------------------------------------------------

nhl_shots_data <- nhl_pbp_data %>%
  filter(event_type %in% c("SHOT", "MISSED_SHOT", "GOAL", "BLOCKED_SHOT")) %>%
  dplyr::select(description, event_type, period, period_seconds_remaining,
                game_seconds_remaining, home_score, away_score, home_name, 
                away_name, event_team, 
                event_player_1_name, event_player_1_type,
                event_player_2_name, event_player_2_type,
                strength_code, x_fixed, y_fixed, shot_distance, shot_angle) %>%
  rename(shot_outcome = event_type)


# Save dataset ------------------------------------------------------------

write_csv(nhl_shots_data,
          # Compress this since it's fairly big
          "data/nhl-shots-2021.csv.gz")


