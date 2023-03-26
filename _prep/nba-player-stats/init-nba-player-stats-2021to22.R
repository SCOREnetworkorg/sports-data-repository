# PURPOSE: Gather NBA player statistics in the 2021-2022 season

# First install the ballr package if not installed already:
# devtools::install_github("rtelmore/ballr")

# Create NBA per possession data ------------------------------------------

library(tidyverse)
library(ballr)

# Get the stats per possession level:
nba_2022_player_stats <- NBAPerGameStatisticsPer100Poss(season = 2022)

# Save the dataset dropping a few columns:
nba_2022_player_stats %>%
  select(-rk, -link, -x) %>%
  write_csv("data/nba-player-stats-2021.csv")


