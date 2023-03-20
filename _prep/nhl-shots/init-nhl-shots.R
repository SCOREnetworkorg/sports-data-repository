# PURPOSE: Gather NHL shots during the 2021-2022 regular season

# First install the hockeyR package if not installed already:
# devtools::install_github("danmorse314/hockeyR")

library(hockeyR)
library(tidyverse)


# Load shots during 2022 season -------------------------------------------

# First load the pbp for 2021-2022 regular season:
nhl_pbp_data <- load_pbp(2021)
