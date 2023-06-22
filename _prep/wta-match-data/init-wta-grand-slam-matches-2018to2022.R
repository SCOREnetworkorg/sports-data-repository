# PURPOSE: Gather data on WTA Grand Slam matches from 2018 to 2022
#          Courtesy of Jeff Sackmann's famous tennis repository:
#          https://github.com/JeffSackmann/tennis_wta

library(tidyverse)

# Gather all WTA matches --------------------------------------------------

wta_2018_2022_matches <-
  map_dfr(c(2018:2022),
          function(year) {
            read_csv(paste0("https://raw.githubusercontent.com/JeffSackmann/tennis_wta/master/wta_matches_",
                            year, ".csv")) %>%
              mutate(winner_seed = as.character(winner_seed),
                     loser_seed = as.character(loser_seed))
          })

# Filter to only Grand Slam matches and clean -----------------------------

wta_grand_slam_matches <- wta_2018_2022_matches %>%
  filter(tourney_level == "G") %>%
  # Remove unnecessary columns for this:
  dplyr::select(-tourney_id, -tourney_level, -best_of,
                -draw_size, -match_num, -winner_id,
                -winner_entry, -loser_id, -loser_entry,
                -winner_rank_points, -loser_rank_points) %>%
  # Just capitalize the tourney_name to fix the US Open issue:
  mutate(tourney_name = toupper(tourney_name))

# Save --------------------------------------------------------------------

write_csv(wta_grand_slam_matches,
          "data/wta-grand-slam-matches-2018to2022.csv")

