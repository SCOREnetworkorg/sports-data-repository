# PURPOSE: Gather historical NFL team statistics going back to 1999 using the 
#          nflverse suite of packages: https://nflverse.nflverse.com/

# First install the nflreadr package if not installed already:
# install.packages("nflreadr")

library(tidyverse)
library(nflreadr)

# Load the play-by-play data for regular season games ---------------------

# Will generate the data following the examples here: https://www.nflfastr.com/articles/beginners_guide.html

# Load all play-by-play data available to then compute team statistics for;
nfl_pbp <- load_pbp(1999:2022) %>%
  # Only looking at pass or runs in the regular season
  filter(rush == 1 | pass == 1, season_type == "REG", 
         # Ensure that we actually have the values to compute the stats
         !is.na(epa), !is.na(posteam), posteam != "") %>%
  # Just grab a subset of columns to work with:
  select(season, posteam, play_type, defteam, yards_gained, 
         air_yards, yards_after_catch, epa, wpa,
         complete_pass, interception, fumble_lost, touchdown)


# Create team summaries for offense and defense ---------------------------

# Now create two summaries of this data - one for offense and another for defense.
# For ease, first define a pipeline that will be applied to both - assuming
# the proper group by was performed:
summarize_team_stats <- . %>%
  # Cmpute various stats summarizing these before pivoting to wide:
  summarize(completion_percentage = mean(complete_pass, na.rm = TRUE),
            total_yards_gained = sum(yards_gained, na.rm = TRUE),
            ave_yards_gained = mean(yards_gained, na.rm = TRUE),
            total_air_yards = sum(air_yards, na.rm = TRUE),
            ave_air_yards = mean(air_yards, na.rm = TRUE),
            total_yac = sum(yards_after_catch, na.rm = TRUE),
            ave_yac = mean(yards_after_catch, na.rm = TRUE),
            n_plays = n(),
            n_interceptions = sum(interception, na.rm = TRUE),
            n_fumbles_lost = sum(fumble_lost, na.rm = TRUE),
            total_epa = sum(epa, na.rm = TRUE),
            ave_epa = mean(epa, na.rm = TRUE),
            total_wpa = sum(wpa, na.rm = TRUE),
            ave_wpa = mean(wpa, na.rm = TRUE),
            success_rate = mean(as.numeric(epa > 0))) %>%
  ungroup() %>%
  # Now pivot to be wider for play type (then drop the rushing for air and yac)
  pivot_wider(names_from = play_type,
              values_from = completion_percentage:success_rate) %>%
  dplyr::select(-completion_percentage_run, -total_air_yards_run, 
                -ave_air_yards_run, -total_yac_run, -ave_yac_run,
                -n_interceptions_run) %>%
  # Rename columns that don't need the play type suffix
  dplyr::rename(completion_percentage = completion_percentage_pass,
                total_air_yards = total_air_yards_pass,
                ave_air_yards = ave_air_yards_pass,
                total_yac = total_yac_pass,
                ave_yac = ave_yac_pass,
                n_interceptions = n_interceptions_pass)

# Now generate the offense summary
off_team_year_summary <- nfl_pbp %>%
  # First only use plays where it is a pass or run:
  filter(play_type %in% c("pass", "run")) %>%
  group_by(season, posteam, play_type) %>%
  summarize_team_stats
# Add offense_ prefix names to these columns
colnames(off_team_year_summary)[-c(1:2)] <- 
  paste0("offense_", colnames(off_team_year_summary)[-c(1:2)])

# Now create the same summary for defense:
def_team_year_summary <- nfl_pbp %>%
  # First only use plays where it is a pass or run:
  filter(play_type %in% c("pass", "run")) %>%
  group_by(season, defteam, play_type) %>%
  summarize_team_stats
# Add offense_ prefix names to these columns
colnames(def_team_year_summary)[-c(1:2)] <- 
  paste0("defense_", colnames(def_team_year_summary)[-c(1:2)])

# Join the two together then rename the team column:
nfl_team_year_summary <- off_team_year_summary %>%
  dplyr::inner_join(def_team_year_summary, 
                    by = c("season" = "season", "posteam" = "defteam")) %>%
  dplyr::rename(team = posteam)


# Load schedule information to join score differentials -------------------

# Now access the schedules to join outcomes 
nfl_schedules <- load_schedules(1999:2022)

# Clean up this data to only be regular season and update the team names:
reg_nfl_schedules <- nfl_schedules %>%
  dplyr::filter(game_type == "REG") %>%
  # Change OAK to LV:
  mutate(home_team = ifelse(home_team == "OAK", "LV", home_team),
         away_team = ifelse(away_team == "OAK", "LV", away_team)) %>%
  # Next STL to LA:
  mutate(home_team = ifelse(home_team == "STL", "LA", home_team),
         away_team = ifelse(away_team == "STL", "LA", away_team)) %>%
  # And finally with SD to LAC:
  mutate(home_team = ifelse(home_team == "SD", "LAC", home_team),
         away_team = ifelse(away_team == "SD", "LAC", away_team)) %>%
  # Create the winner column that allows for tied games:
  mutate(winner = ifelse(home_score > away_score,
                         home_team, ifelse(home_score < away_score,
                                           away_team, "tie")))

# For each team, compute their number of wins, losses, points_scored, points_allowed
# in each season by breaking them up by home and away games:
team_season_summary <- map_dfr(unique(reg_nfl_schedules$home_team),
                               function(nfl_team) {
                                 # First get home game summaries:
                                 home_game_summary <- reg_nfl_schedules %>%
                                   dplyr::filter(home_team == nfl_team) %>%
                                   dplyr::rename(points_scored = home_score,
                                                 points_allowed = away_score) %>%
                                   dplyr::mutate(won = ifelse(home_team == winner, 1, 0),
                                                 lost = ifelse(away_team == winner, 1, 0),
                                                 tied = ifelse(winner == "tie", 1, 0)) %>%
                                   dplyr::rename(team = home_team) %>%
                                   dplyr::select(team, season, points_scored, 
                                                 points_allowed, won, lost, tied) %>%
                                   group_by(team, season) %>%
                                   summarize(points_scored = sum(points_scored, na.rm = TRUE),
                                             points_allowed = sum(points_allowed, na.rm = TRUE),
                                             wins = sum(won, na.rm = TRUE),
                                             losses = sum(lost, na.rm = TRUE),
                                             ties = sum(tied, na.rm = TRUE)) %>%
                                   ungroup()
                                 
                                 # Now the away game:
                                 away_game_summary <- reg_nfl_schedules %>%
                                   dplyr::filter(away_team == nfl_team) %>%
                                   dplyr::rename(points_scored = away_score,
                                                 points_allowed = home_score) %>%
                                   dplyr::mutate(won = ifelse(away_team == winner, 1, 0),
                                                 lost = ifelse(home_team == winner, 1, 0),
                                                 tied = ifelse(winner == "tie", 1, 0)) %>%
                                   dplyr::rename(team = away_team) %>%
                                   dplyr::select(team, season, points_scored, 
                                                 points_allowed, won, lost, tied) %>%
                                   group_by(team, season) %>%
                                   summarize(points_scored = sum(points_scored, na.rm = TRUE),
                                             points_allowed = sum(points_allowed, na.rm = TRUE),
                                             wins = sum(won, na.rm = TRUE),
                                             losses = sum(lost, na.rm = TRUE),
                                             ties = sum(tied, na.rm = TRUE)) %>%
                                   ungroup()
                                 
                                 # Now stack them together and compute the 
                                 # summaries again:
                                 home_game_summary %>%
                                   bind_rows(away_game_summary) %>%
                                   group_by(team, season) %>%
                                   summarize(points_scored = sum(points_scored, na.rm = TRUE),
                                             points_allowed = sum(points_allowed, na.rm = TRUE),
                                             wins = sum(wins, na.rm = TRUE),
                                             losses = sum(losses, na.rm = TRUE),
                                             ties = sum(ties, na.rm = TRUE)) %>%
                                   ungroup()
                               })


# Join tables together and save -------------------------------------------

nfl_team_season_summary <- nfl_team_year_summary %>%
  inner_join(team_season_summary, by = c("season", "team")) %>%
  mutate(score_differential = points_scored - points_allowed)

# Save:
write_csv(nfl_team_season_summary,
          "data/nfl-team-statistics.csv")

