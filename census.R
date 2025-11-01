
# check to see if the GIS setup has already been run, if not, run it
if (!exists(".setup_complete_gis") || !.setup_complete_gis) {
  source("gis.R")
} else {
  message("gis.R has already been run.")
}

# load nypd precinct 2020 nyc-census data

nyc_census2020 <- read_csv("r-data/nyc-census/nyc_precinct_2020pop.csv")

# format `Precinct` to match variable name in sqf dfs
nyc_census2020$precinct <- sprintf("%03d", as.integer(nyc_census2020$precinct))

# nyc-census (only one race totals and total 2+ races)
nyc_census2020 <- nyc_census2020 %>%
  select(precinct, P1_001N, P1_002N, P1_003N, P1_004N, P1_005N, P1_006N, P1_007N, P1_008N, P1_009N, P2_002N, P2_003N) %>%
  rename_with(~ case_match(
    .,
    "P1_001N" ~ "total_population",
    "P1_002N" ~ "population_one_race",
    "P1_003N" ~ "white_alone",
    "P1_004N" ~ "black_alone",
    "P1_005N" ~ "american_indian_alone",
    "P1_006N" ~ "asian_alone",
    "P1_007N" ~ "pacific_islander_alone",
    "P1_008N" ~ "some_other_race_alone",
    "P1_009N" ~ "two_or_more_races",
    "P2_002N" ~ "hispanic_or_latino",
    "P2_003N" ~ "not_hispanic_or_latino",
    .default = .
  ))

# precincts, proportion of black residents
nyc_census2020 <- nyc_census2020 %>%
  mutate(black_prop = round(black_alone / total_population, 2))


# join nyc-census data to spatial precinct data
nyc_census2020 <- nyc_census2020 %>%
  mutate(precinct = as.integer(precinct)) %>%
  left_join(
    nypd_sf %>%
      mutate(Precinct = as.integer(Precinct)),
    by = c("precinct" = "Precinct")
  )



.setup_complete_census <- TRUE