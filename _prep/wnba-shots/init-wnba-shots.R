# PURPOSE: Gather WNBA shots during the 2022 regular season

# First install the wehoop package if not installed already:
# install.packages("wehoop")

library(wehoop)
library(tidyverse)

# Load shots during 2022 season -------------------------------------------

# First load the pbp for 2022 regular season:
wnba_pbp_data <- load_wnba_pbp(2022)

# Filter and clean data ---------------------------------------------------

# Get the shots and clean this data a bit:
wnba_shots_data <- wnba_pbp_data %>%
  filter(shooting_play) %>%
  # Make a column with the name of the shooting team:
  mutate(shooting_team = ifelse(team_id == home_team_id, home_team_name,
                                away_team_name))

# Select and reorder columns:
clean_wnba_shots_data <- wnba_shots_data %>%
  dplyr::select(game_id, game_play_number, text, type_text, scoring_play, 
                score_value, coordinate_x, coordinate_y,
                shooting_team, home_team_name, away_team_name,
                home_score, away_score, qtr,
                start_quarter_seconds_remaining,
                start_game_seconds_remaining) %>%
  rename(desc = text,
         shot_type = type_text,
         made_shot = scoring_play,
         shot_value = score_value,
         quarter_seconds_remaining = start_quarter_seconds_remaining,
         game_seconds_remaining = start_game_seconds_remaining)

# Save dataset ------------------------------------------------------------

write_csv(clean_wnba_shots_data,
          # Compress this since it's fairly big
          "data/wnba-shots-2022.csv.gz")
