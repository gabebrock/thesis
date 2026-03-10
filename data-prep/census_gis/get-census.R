# --- setup ----
library(tidycensus)
library(sf)
options(tigris_use_cache = TRUE)


# --- check if cached RDS files exist; load and skip if so ----
.census_rds_files <- c(
  "data/data-final/census-gis/demo_trct.rds",
  "data/data-final/census-gis/demo_trct_tally.rds",
  "data/data-final/census-gis/demo_pct.rds",
  "data/data-final/census-gis/demo_pct_tally.rds"
)

# If .rds files exist in directory, just load them to environment
if (all(file.exists(.census_rds_files))) {
  message("Census RDS files found. loading from cache.")
  demo_trct       <- readRDS("data/data-final/census-gis/demo_trct.rds")
  demo_trct_tally <- readRDS("data/data-final/census-gis/demo_trct_tally.rds")
  demo_pct        <- readRDS("data/data-final/census-gis/demo_pct.rds")
  demo_pct_tally  <- readRDS("data/data-final/census-gis/demo_pct_tally.rds")
  .setup_complete_census <- TRUE
} else { # other wise run the tidycensus code

# --- precinct-level 2020 decennial census (from CSV) ----
#' Pre-downloaded file; use for static 2020 demographic baseline at precinct level.
nyc_census2020_pct <- readr::read_csv("data/nyc-census/nyc_precinct_2020pop.csv") %>%
  dplyr::select(
    precinct, P1_001N, P1_002N, P1_003N, P1_004N, P1_005N,
    P1_006N, P1_007N, P1_008N, P1_009N, P2_002N, P2_003N
  ) %>%
  dplyr::rename(
    total_population       = P1_001N,
    population_one_race    = P1_002N,
    white_alone            = P1_003N,
    black_alone            = P1_004N,
    american_indian_alone  = P1_005N,
    asian_alone            = P1_006N,
    pacific_islander_alone = P1_007N,
    some_other_race_alone  = P1_008N,
    two_or_more_races      = P1_009N,
    hispanic_or_latino     = P2_002N,
    not_hispanic_or_latino = P2_003N
  ) %>%
  dplyr::mutate(
    precinct   = as.integer(precinct),
    black_prop = round(black_alone       / total_population, 2),
    white_prop = round(white_alone       / total_population, 2),
    hisp_prop  = round(hispanic_or_latino / total_population, 2)
  ) %>%
  # attach precinct geometries
  dplyr::left_join(nypd_sf, by = c("precinct" = "precinct")) %>%
  sf::st_as_sf()


# --- ACS tract-level data retrieval function ----
#' Pulls ACS 5-year estimates for a single year at the census-tract level
#' for the five NYC counties. Returns an sf object with demographic variables.
#'
#' Variables:
#'   B01001_001  total population
#'   B03002_003  white alone, not Hispanic/Latino
#'   B03002_004  Black alone, not Hispanic/Latino
#'   B03002_012  Hispanic/Latino (any race)
#'   B01001_007:010 / 031:034  male/female age 18–24
#'   B25024_001/002  total housing units / units in 2+ unit structures
#'   B05012_003  foreign-born population
#'   B19013_001  median household income

age_vars    <- c("B01001_007", "B01001_008", "B01001_009", "B01001_010",
                 "B01001_031", "B01001_032", "B01001_033", "B01001_034")
housing_vars     <- c("B25024_001", "B25024_002")
foreign_born_var <- "B05012_003"
income_var       <- "B19013_001"

get_pop <- function(year) {
  tidycensus::get_acs(
    geography = "tract",
    variables = c(
      "B01001_001", "B03002_003", "B03002_004", "B03002_012",
      age_vars, housing_vars, foreign_born_var, income_var
    ),
    state    = "NY",
    county   = c("New York", "Kings", "Queens", "Bronx", "Richmond"),
    year     = year,
    geometry = TRUE
  ) %>%
    dplyr::select(GEOID, variable, estimate, geometry) %>%
    tidyr::pivot_wider(names_from = variable, values_from = estimate) %>%
    dplyr::mutate(
      year             = year,
      age_18_24_pop    = B01001_007 + B01001_008 + B01001_009 + B01001_010 +
                         B01001_031 + B01001_032 + B01001_033 + B01001_034,
      pct_multi_unit   = B25024_002 / B25024_001,
      pct_foreign_born = B05012_003 / B01001_001
    ) %>%
    dplyr::rename(
      total_pop     = B01001_001,
      white_pop     = B03002_003,
      black_pop     = B03002_004,
      hisp_pop      = B03002_012,
      foreign_born_pop = B05012_003,
      total_units   = B25024_001,
      multi_unit    = B25024_002,
      median_income = B19013_001
    ) %>%
    dplyr::select(
      GEOID, year, geometry,
      total_pop, white_pop, black_pop, hisp_pop,
      age_18_24_pop, foreign_born_pop, pct_foreign_born,
      total_units, multi_unit, pct_multi_unit,
      median_income
    )
}


# --- retrieve ACS data for 2009–2024 ----
years <- 2009:2024

demo_trct <- purrr::map_dfr(years, get_pop)


# --- 2. tally census tract-level data ----
demo_trct_tally <- demo_trct %>%
  sf::st_drop_geometry() %>%
  dplyr::group_by(year) %>%
  dplyr::summarize(
    n_tracts         = dplyr::n(),
    total_pop        = sum(total_pop,        na.rm = TRUE),
    black_pop        = sum(black_pop,        na.rm = TRUE),
    white_pop        = sum(white_pop,        na.rm = TRUE),
    hisp_pop         = sum(hisp_pop,         na.rm = TRUE),
    age_18_24_pop    = sum(age_18_24_pop,    na.rm = TRUE),
    foreign_born_pop = sum(foreign_born_pop, na.rm = TRUE),
    median_income    = mean(median_income,   na.rm = TRUE),
    .groups = "drop"
  ) %>%
  dplyr::mutate(
    pct_black = black_pop     / total_pop,
    pct_white = white_pop     / total_pop,
    pct_hisp  = hisp_pop      / total_pop,
    pct_18_24 = age_18_24_pop / total_pop,
    pct_foreign_born = foreign_born_pop / total_pop
  )


# --- spatial join: assign each tract to a precinct ----
#' Uses largest-overlap intersection so each tract gets one precinct.
demo_trct_pct <- sf::st_join(
  demo_trct %>% sf::st_make_valid(),
  nypd_sf   %>% sf::st_transform(sf::st_crs(demo_trct)) %>%
                sf::st_make_valid() %>% dplyr::select(precinct),
  join       = sf::st_intersects,
  largest    = TRUE
)


# --- aggregate to precinct × year ----
demo_pct <- demo_trct_pct %>%
  sf::st_drop_geometry() %>%
  dplyr::group_by(precinct, year) %>%
  dplyr::summarize(
    total_pop        = sum(total_pop,        na.rm = TRUE),
    black_pop        = sum(black_pop,        na.rm = TRUE),
    white_pop        = sum(white_pop,        na.rm = TRUE),
    hisp_pop         = sum(hisp_pop,         na.rm = TRUE),
    age_18_24_pop    = sum(age_18_24_pop,    na.rm = TRUE),
    foreign_born_pop = sum(foreign_born_pop, na.rm = TRUE),
    pct_foreign_born = sum(foreign_born_pop, na.rm = TRUE) / sum(total_pop, na.rm = TRUE),
    pct_multi_unit   = mean(pct_multi_unit,  na.rm = TRUE),
    median_income    = mean(median_income,   na.rm = TRUE),
    .groups = "drop"
  ) %>%
  dplyr::mutate(
    pct_black = black_pop     / total_pop,
    pct_white = white_pop     / total_pop,
    pct_hisp  = hisp_pop      / total_pop,
    pct_18_24 = age_18_24_pop / total_pop
  ) %>%
  dplyr::rename(pct = precinct)


# --- 3. tally precinct-level data ----
demo_pct_tally <- demo_pct %>%
  dplyr::group_by(year) %>%
  dplyr::summarize(
    n_precincts      = dplyr::n(),
    total_pop        = sum(total_pop,        na.rm = TRUE),
    black_pop        = sum(black_pop,        na.rm = TRUE),
    white_pop        = sum(white_pop,        na.rm = TRUE),
    hisp_pop         = sum(hisp_pop,         na.rm = TRUE),
    age_18_24_pop    = sum(age_18_24_pop,    na.rm = TRUE),
    foreign_born_pop = sum(foreign_born_pop, na.rm = TRUE),
    median_income    = mean(median_income,   na.rm = TRUE),
    .groups = "drop"
  ) %>%
  dplyr::mutate(
    pct_black        = black_pop     / total_pop,
    pct_white        = white_pop     / total_pop,
    pct_hisp         = hisp_pop      / total_pop,
    pct_18_24        = age_18_24_pop / total_pop,
    pct_foreign_born = foreign_born_pop / total_pop
  )


# --- save ----
#' `demo_trct`: row per census tract × year (~2,000 tracts × 16 years). 
#' Full tract-level data with all demographic variables and geometry.                   
saveRDS(demo_trct,       file = "data/data-final/census-gis/demo_trct.rds")

#' `demo_trct_tally`: row per year (16 rows). 
#' City-wide totals and proportions aggregated across all tracts, 
#' a summary/diagnostic table, useful for checking that the data looks reasonable across years.
saveRDS(demo_trct_tally, file = "data/data-final/census-gis/demo_trct_tally.rds")

#' same for precinct-level data
saveRDS(demo_pct,        file = "data/data-final/census-gis/demo_pct.rds")
saveRDS(demo_pct_tally,  file = "data/data-final/census-gis/demo_pct_tally.rds")

.setup_complete_census <- TRUE

} # end if/else cache check
