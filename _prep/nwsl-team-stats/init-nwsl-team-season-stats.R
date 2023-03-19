# PURPOSE: Gather team statistics for NWSL during the regular season

# First install the nwslr package if not installed already:
# devtools::install_github("nwslR/nwslR")

library(nwslR)
library(tidyverse)

# Gather team IDs ---------------------------------------------------------

team_table <- load_teams()

# Proceed through each team, storing their season stats -------------------

# Iterate through each team gathering regular season statistics across all of the
# seasons they played in:

team_season_stats <-
  map_dfr(1:nrow(team_table),
          function(team_i) {
            
            # Get the team id:
            team <- team_table$team_abbreviation[team_i]

            # Get their starting season:
            start_year <- team_table$first_season[team_i]
            # Ending season:
            end_year <- team_table$last_season[team_i]
            
            team_years <- start_year:end_year
            
            # Remove 2019 for ORL since the link is broken
            if (team == "ORL") {
              team_years <- setdiff(team_years, 2019)
            }
            
            # Remove 2020 since they did not play regular season:
            team_years <- setdiff(team_years, 2020)

            # Iterate over each team's season, returning their stats:
            map_dfr(team_years,
                    function(year) {
                        load_team_season_stats(team, as.character(year))
                    })
            
          })


# Create simplified version of dataset with goal differential -------------

# Load the glossary to view:
stat_glossary <- load_metrics()
team_stat_glossary <- stat_glossary %>%
  filter(metric %in% intersect(colnames(team_season_stats),
                               stat_glossary$metric))

# Now create the team summary table with goal differential and some example
# statistics to use:
team_summary_table <- team_season_stats %>%
  left_join(dplyr::select(team_table, team_id, team_name),
            by = "team_id") %>%
  mutate(goal_differential = goals - goals_conceded) %>%
  dplyr::select(team_name, season, games_played,
                goal_differential, goals, goals_conceded, 
                # Various types of stats relevant
                cross_accuracy, goal_conversion_pct,
                pass_pct, pass_pct_opposition_half,
                possession_pct, shot_accuracy, 
                tackle_success_pct)


# Save data ---------------------------------------------------------------

write_csv(team_summary_table,
          "data/nwsl-team-stats.csv")

# Example analysis with data ----------------------------------------------


# View distribution of goal differential ----------------------------------

team_summary_table %>%
  ggplot(aes(x = goal_differential)) +
  geom_histogram() +
  theme_bw()


# View relationship of goal differential as function of variables ---------

team_summary_table %>%
  ggplot(aes(x = pass_pct_opposition_half,
             y = goal_differential)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm") +
  theme_bw()

summary(lm(goal_differential ~ .,
           data = dplyr::select(team_summary_table,
                                goal_differential, cross_accuracy, 
                                goal_conversion_pct, pass_pct, 
                                pass_pct_opposition_half, possession_pct, 
                                shot_accuracy, tackle_success_pct)))

# Call:
#   lm(formula = goal_differential ~ ., data = dplyr::select(team_summary_table, 
#                                                            goal_differential, cross_accuracy, goal_conversion_pct, pass_pct, 
#                                                            pass_pct_opposition_half, possession_pct, shot_accuracy, 
#                                                            tackle_success_pct))
# 
# Residuals:
#   Min       1Q   Median       3Q      Max 
# -23.6409  -5.0157   0.6636   5.1623  24.4037 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)              -61.6594    45.8708  -1.344  0.18483    
# cross_accuracy             1.8022     0.5373   3.354  0.00151 ** 
#   goal_conversion_pct        2.7190     0.6008   4.526 3.63e-05 ***
#   pass_pct                  -1.8035     0.9636  -1.872  0.06701 .  
# pass_pct_opposition_half   0.8375     0.7326   1.143  0.25828    
# possession_pct             2.1071     0.6417   3.284  0.00185 ** 
#   shot_accuracy             -0.4226     0.4777  -0.885  0.38039    
# tackle_success_pct        -0.3357     0.2009  -1.671  0.10081    
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 10.55 on 51 degrees of freedom
# Multiple R-squared:  0.5683,	Adjusted R-squared:  0.509 
# F-statistic: 9.589 on 7 and 51 DF,  p-value: 1.522e-07


# View PCA output ---------------------------------------------------------

# Just look at the explanatory variables:
model_x <- as.matrix(dplyr::select(team_summary_table, 
                                   goal_differential,cross_accuracy, 
                                   goal_conversion_pct, pass_pct, 
                                   pass_pct_opposition_half, possession_pct, 
                                   shot_accuracy, tackle_success_pct))
pca_nwsl <- prcomp(model_x, center = TRUE, scale = TRUE)
summary(pca_nwsl)
# Importance of components:
#   PC1    PC2   PC3    PC4     PC5     PC6     PC7     PC8
# Standard deviation     1.6952 1.4415 1.043 0.9141 0.69942 0.55841 0.47616 0.31138
# Proportion of Variance 0.3592 0.2597 0.136 0.1045 0.06115 0.03898 0.02834 0.01212
# Cumulative Proportion  0.3592 0.6189 0.755 0.8594 0.92056 0.95954 0.98788 1.00000

library(factoextra)
# Designate to only label the variables:
fviz_pca_biplot(
  pca_nwsl, label = "var",
  # Change the alpha for observations 
  # which is represented by ind
  alpha.ind = .5,
  # Modify the alpha for variables (var):
  alpha.var = .75,
  col.var = "darkblue")
