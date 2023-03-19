# PURPOSE: Gather pitches thrown in both the 2019 and 2022 seasons by Justin Verlander.
#          The 2019 dataset will serve as the examples while the 2022 dataset
#          will be for the students to work with in exercises for the module.


# Load necessary packages -------------------------------------------------

library(baseballr)
library(tidyverse)

# Scrape the pitches in 2019 and 2022 separately --------------------------

# Unfortunately the lookup function for player IDs in baseballr appears to be
# broken, so I looked up Verlander's MLBAM id online (434378)

verlander_2019_data <-
  statcast_search_pitchers(start_date = "2019-03-01",
                           end_date = "2019-12-01",
                           pitcherid = 434378)

verlander_2022_data <-
  statcast_search_pitchers(start_date = "2022-03-01",
                           end_date = "2022-12-01",
                           pitcherid = 434378)


# Clean data for ISLE module ----------------------------------------------

# Set-up pipeline to clean data:
clean_pitch_data <- . %>%
  filter(game_type != "S", pitch_type != "") %>%
  unite("count", balls:strikes, sep = "-") %>%
  dplyr::select(pitch_type, count, release_speed, description)  

# Apply to both datasets:
verlander_2019_pitches <- verlander_2019_data %>%
  clean_pitch_data

verlander_2022_pitches <- verlander_2022_data %>%
  clean_pitch_data

# And save: 
write_csv(verlander_2019_pitches,
          "data/verlander-pitches-2019.csv")
write_csv(verlander_2022_pitches,
          "data/verlander-pitches-2022.csv")


# Convert to json for ISLE ------------------------------------------------

# The following commented out code shows how to save as json for ISLE

# library(jsonlite)
# 
# write(toJSON(verlander_2019_pitches, dataframe = 'columns'), 
#       'data/verlander_pitches_2019.json' )
# write(toJSON(verlander_2022_pitches, dataframe = 'columns'), 
#       'data/verlander_pitches_2022.json' )



