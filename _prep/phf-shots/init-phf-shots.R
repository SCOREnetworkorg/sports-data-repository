# PURPOSE: Gather PHF shots during the 2021-2022 season

# First install the fastRhockey package if not installed already:
# install.packages("fastRhockey")

library(fastRhockey)
library(tidyverse)

# Load shots during 2021 season -------------------------------------------

# First load the pbp for 2021-2022  season:
phf_pbp_data <- load_phf_pbp(2021)


# Filter and clean data ---------------------------------------------------

phf_shots_data <- phf_pbp_data %>%
  # Do not include Miss - these were only recorded for one game
  filter(play_type %in% c("Goal", "PP Goal", "SH Goal", "Shot", "Shot BLK")) %>%
  dplyr::select(play_description, play_type, period_id, time_remaining, sec_from_start, 
                home_team, away_team, home_goals, away_goals,
                team, player_name_1, player_name_2, goalie_involved,
                shot_result, on_ice_situation, home_score_total, away_score_total) %>%
  rename(shooting_team = team)


# Save dataset ------------------------------------------------------------

write_csv(phf_shots_data,
          "data/phf-shots-2021.csv.gz")


