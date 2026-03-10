
# load nypd precinct 2020 nyc-census data

nyc_census2020_pct <- read_csv("data/nyc-census/nyc_precinct_2020pop.csv")

# format `Precinct` to match variable name in sqf dfs
nyc_census2020_pct$precinct <- sprintf("%03d", as.integer(nyc_census2020_pct$precinct))

# nyc-census (only one race totals and total 2+ races)
nyc_census2020_pct <- nyc_census2020_pct |>
  select(precinct, P1_001N, P1_002N, P1_003N, P1_004N, P1_005N, P1_006N, P1_007N, P1_008N, P1_009N, P2_002N, P2_003N) |>
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
nyc_census2020_pct <- nyc_census2020_pct |>
  mutate(black_prop = round(black_alone / total_population, 2),
         white_prop = round(white_alone / total_population, 2),
         hisp_prop = round(hispanic_or_latino / total_population, 2))

# join nyc-census precinct-level data to spatial precinct data
nyc_census2020_pct <- nyc_census2020_pct |>
  mutate(precinct = as.integer(precinct)) |>
  left_join(
    nypd_sf |>
      mutate(Precinct = as.integer(Precinct)),
    by = c("precinct" = "Precinct")
  )

# set tigris options
library(tidycensus)
options(tigris_use_cache = TRUE)

# get nyc census tract data from tidycensus
nyc_census2020_trc <- get_decennial(
    geography = "tract",
    variables = c(
      "P1_001N", "P1_002N", "P1_003N", "P1_004N", "P1_005N",
      "P1_006N", "P1_007N", "P1_008N", "P1_009N", "P2_002N", "P2_003N"
    ),
    state = "NY",
    county = c("Bronx", "Kings", "New York", "Queens", "Richmond"),
    year = 2020,
    geometry = TRUE) |>
  pivot_wider(
    names_from = variable,
    values_from = value
  ) |>
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

.setup_complete_census <- TRUE