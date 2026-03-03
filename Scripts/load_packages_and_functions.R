if (!require("pacman"))
  install.packages("pacman")
pacman::p_load(
  here,
  dplyr,
  tidyverse,
  janitor,
  ISOweek,
  scales,
  patchwork
)

# when to start analyses
starting_date = "2023-01-10"
ending_date = "2024-12-21"

# minimum of treatment plats required per week and region / country
min_obs = 10

# determine path(s) where data are located and should be stored
read_data_here <-
  here(here(), "Data")
scripts_here <-
  here(here(), "Scripts")
results_here <-
  here(here(), "Results")