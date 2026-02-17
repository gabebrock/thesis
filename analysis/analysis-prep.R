
# --- Set up and load data ---

# This script prepares (1) stop-level data for outcome models and
# (2) precinct-(year|month) level panels for OLS analyses.
#
# Outputs written to ~Projects/thesis/analysis/data/derived/
# - sqf_ols.rds: stop-level modeling dataset
# - sqf_model_data.rda: bundle of derived datasets used across analyses

library(tidyverse)
library(data.table)
library(lubridate)
library(fixest)
library(ggplot2)
library(sf)

source("~/Projects/thesis/config/load.R")

# sqf_hist <- readRDS("../data/sqf_hist.rds")

# sqf_all_clean is created upstream (see /data/read_sqf-csv.R)
# Rebind here to make this script runnable after config/load.R has loaded objects.
sqf_all_clean <- sqf_all_clean

# Basic cleaning / convenience variables used repeatedly in models and summaries
sqf_all_clean <- sqf_all_clean %>%
  # create shorter variable names for frequent variables
  mutate(
    year    = year(as.Date(STOP_FRISK_DATE)),
    month   = month(as.Date(STOP_FRISK_DATE)),
    pct     = as.numeric(STOP_LOCATION_PRECINCT),
    race    = factor(SUSPECT_RACE_DESCRIPTION), 
    age     = as.numeric(SUSPECT_REPORTED_AGE),
    female  = if_else(SUSPECT_SEX == "F", 1L, 0L)
  )

# Convenience vector for the main race groups used in stop-level models
# I write this so much in the code base just just want to be sure `BWH` is created for renders
BWH <- c("BLACK", "WHITE", "HISPANIC")

# --- Code key outcomes ---

# Create unified (any) force variable.
# Note: Missing is explicitly mapped to 0, treating NA as “not flagged”.
sqf_all_clean <- sqf_all_clean %>%
  mutate(
    any_force = if_else(
      PHYSICAL_FORCE_CEW_FLAG == "Y" |
        PHYSICAL_FORCE_DRAW_POINT_FIREARM_FLAG == "Y" |
        PHYSICAL_FORCE_HANDCUFF_SUSPECT_FLAG == "Y" |
        PHYSICAL_FORCE_OC_SPRAY_USED_FLAG == "Y" |
        PHYSICAL_FORCE_OTHER_FLAG == "Y" |
        PHYSICAL_FORCE_RESTRAINT_USED_FLAG == "Y" |
        # PHYSICAL_FORCE_VERBAL_INSTRUCTION_FLAG == "Y" |
        PHYSICAL_FORCE_WEAPON_IMPACT_FLAG == "Y",
      1L, 0L, missing = 0L
    )
  )

# Create key outcome variables (coded as 0/1 integers)
sqf_all_clean <- sqf_all_clean %>%
  mutate(
    arrest    = if_else(SUSPECT_ARRESTED_FLAG == "Y", 1L, 0L),
    summons   = if_else(SUMMONS_ISSUED_FLAG == "Y", 1L, 0L),
    sanction  = if_else(arrest == 1 | summons == 1, 1L, 0L),
    contrab   = if_else(OTHER_CONTRABAND_FLAG == "Y", 1L, 0L),
    gun       = if_else(WEAPON_FOUND_FLAG == "Y" & FIREARM_FLAG == "Y", 1L, 0L, missing = 0L),
    knife     = if_else(WEAPON_FOUND_FLAG == "Y" & KNIFE_CUTTER_FLAG == "Y", 1L, 0L, missing = 0L),
    any_weap  = if_else(gun == 1 | knife == 1, 1L, 0L),
    any_seiz  = if_else(contrab == 1 | any_weap == 1, 1L, 0L),
    frisk     = if_else(FRISKED_FLAG == "Y", 1L, 0L),
    search    = if_else(SEARCHED_FLAG == "Y", 1L, 0L),
    force     = if_else(any_force == 1, 1L, 0L)
  )

# Descriptive outcome table (rates in percent)
sqf_all_clean %>%
  summarize(
    total_stops = n(),
    arrest_rate = mean(arrest, na.rm = TRUE) * 100,
    summons_rate = mean(summons, na.rm = TRUE) * 100,
    sanction_rate = mean(sanction, na.rm = TRUE) * 100,
    contrab_rate = mean(contrab, na.rm = TRUE) * 100,
    gun_rate = mean(gun, na.rm = TRUE) * 100,
    knife_rate = mean(knife, na.rm = TRUE) * 100,
    any_weap_rate = mean(any_weap, na.rm = TRUE) * 100,
    any_seiz_rate = mean(any_seiz, na.rm = TRUE) * 100,
    frisk_rate = mean(frisk, na.rm = TRUE) * 100,
    search_rate = mean(search, na.rm = TRUE) * 100,
    force_rate = mean(force, na.rm = TRUE) * 100
  ) %>%
  print()

# --- Build Reasonable Suspicion (RS) factors ---

# Helper to convert NYPD “Y” flags into 0/1 integers
yn <- function(x) as.integer(x == "Y")

# Binary day/night variable derived from STOP_FRISK_TIME.
# Assumption: If STOP_FRISK_TIME is missing/unparseable, treat as night (0).
sqf_all_clean <- sqf_all_clean %>%
  mutate(
    stop_time_hr = as.integer(substr(STOP_FRISK_TIME, 1, 2)),
    day_stop = if_else(stop_time_hr >= 6 & stop_time_hr < 18, 1L, 0L, missing = 0L)
  )

#' RS factors are engineered indicators (or small composites) intended to capture
#' the “reasonable suspicion” narrative in the UF-250s.

#' Several factors add together multiple flags (and/or day_stop) to create a
#' higher-level RS score with a small integer range.
sqf_all_clean <- sqf_all_clean %>%
  mutate(
    RS_fits    = yn(SUSPECTS_ACTIONS_DECRIPTION_FLAG),
    RS_furtive = yn(SUSPECTS_ACTIONS_IDENTIFY_CRIME_PATTERN_FLAG),
    RS_crimloc = yn(SUSPECTS_ACTIONS_PROXIMITY_TO_SCENE_FLAG) + yn(day_stop),
    RS_casing  = yn(SUSPECTS_ACTIONS_CASING_FLAG) + yn(SUSPECTS_ACTIONS_LOOKOUT_FLAG),
    RS_other   = yn(SUSPECTS_ACTIONS_OTHER_FLAG) + yn(OTHER_PERSON_STOPPED_FLAG),
    RS_drug    = yn(SUSPECTS_ACTIONS_DRUG_TRANSACTIONS_FLAG),
    RS_suspobj = yn(SUSPECTS_ACTIONS_CONCEALED_POSSESSION_WEAPON_FLAG),
    RS_appear  = yn(BACKROUND_CIRCUMSTANCES_SUSPECT_KNOWN_TO_CARRY_WEAPON_FLAG),
    RS_violent = yn(BACKROUND_CIRCUMSTANCES_VIOLENT_CRIME_FLAG)
  )

# --- Precinct‑level stop allocation vs crime and race ---

# Precinct-year panel: number of stops and (log) stops.
# This is the base panel used for OLS prediction of stop frequency.
pct_year <- sqf_all_clean %>%
  group_by(pct, year) %>%
  summarise(
    stops = n(),
    .groups = "drop"
  ) %>%
  mutate(log_stops = log(stops + 1),
         pct = as.numeric(pct))

# glimpse(pct_year)

# --- ---

# Join precinct-year demographics and crime.
#
# Inputs are built upstream:
# - ../data/pct_demo.rds (ACS-derived precinct demographics)
# - ../data/pct_crime.rds (precinct crime totals)
pct_demo  <- readRDS("../data/pct_demo.rds")
pct_crime <- readRDS("../data/pct_crime.rds")

# Remove geometry if present to avoid sf join issues
if (inherits(pct_demo, "sf")) {
  pct_demo <- st_drop_geometry(pct_demo)
}
if (inherits(pct_crime, "sf")) {
  pct_crime <- st_drop_geometry(pct_crime)
}

pct_year <- pct_year %>%
  # get crime and demo data from 
  # /analysis/pct_crime.R and /analysis/tracts_to_precincts.R
  left_join(pct_crime, by = c("pct", "year")) %>%
  left_join(pct_demo, by = c("pct", "year"))

# Filter to pre-2024 because ACS5 demographics are not available for 2024
pct_year <- pct_year %>%
  filter(year != 2024)

# Drop imaginary/invalid precinct from 2017 (precinct codes should be < 124)
pct_year <- pct_year %>%
  filter(pct < 124)

# --- variables for ols ---

# Attach precinct geometries and
# construct density / rates for precinct-year models.

# Note: This block is careful about sf objects to avoid geometry duplication.
nypd_sf <- readRDS("../data/sf/nypd_sf.rds")

# Drop geometry from pct_year if it exists
if (inherits(pct_year, "sf")) {
  pct_year <- st_drop_geometry(pct_year)
}

# Join and handle geometries
pct_year_ols <- pct_year %>%
  left_join(st_drop_geometry(nypd_sf), by = c("pct" = "Precinct"))

# Add geometry back from nypd_sf
pct_year_ols <- pct_year_ols %>%
  left_join(nypd_sf %>% select(Precinct, geometry), by = c("pct" = "Precinct")) %>%
  select(-any_of(c("OBJECTID", "Shape__Length", "Shape__Area")))

pct_year_ols <- sf::st_as_sf(pct_year_ols)

# Calculate area (in projected CRS) and create derived covariates used in OLS.
#
# - Crime rates are scaled per 1,000 population and logged with +1 to avoid log(0)
# - Lagged crime rates are 1-period lags within precinct
pct_year_ols <- pct_year_ols %>%
  st_transform(crs = 3857) %>% # project to meters so I can calculate area
  mutate(
    area_sq_meters = st_area(geometry),
    area_sq_miles  = as.numeric(area_sq_meters) * 3.861e-7, # square-meters to square-miles
    pct_black      = black_pop / total_pop,
    pct_white      = white_pop / total_pop,
    pct_hisp       = hisp_pop / total_pop,
    pop_density    = total_pop / area_sq_miles,
    pct_18_24      = age_18_24_pop / total_pop,
    
    # Calculate nonviolent crime by summing relevant categories
    nonviolent_crime = crime_Weapons + crime_Property + crime_Drug + 
      crime_Trespass + crime_QualityOfLife + crime_Other,
    
    # Log crime rates per 1000 people
    log_violent_rate    = log((crime_Violent + 1) / total_pop * 1000),
    log_nonviolent_rate = log((nonviolent_crime + 1) / total_pop * 1000),
    
    # previous-month lag logged crime rates by precinct
    lag_log_violent_rate = lag(log_violent_rate, 1),
    lag_log_nonviolent_rate = lag(log_nonviolent_rate, 1)
  )

# --- create data subsets for different mayoral administrations ---

# Convenience subsets for stratified models / plots
# Eric Adams (2022 - 2025)
pct_year_ols_adams <- pct_year_ols %>%
  filter(year >= 2022)

# Bill de Blasio (2014 - 2021)
pct_year_ols_blasio <- pct_year_ols %>%
  filter(year >= 2014 & year <= 2021)

# Michael Bloomberg (2002 - 2013)
pct_year_ols_bloomberg <- pct_year_ols %>%
  filter(year <= 2013)


# --- Monthly Analysis with Lagged Crime Rates ---

# This section builds a precinct-month panel suitable for models that use
# 1-month lagged crime rates.
#
# Important constraint: available crime data are annual; the code below
# approximates monthly crime by distributing annual totals evenly across months.

#' To estimate the effect of race on officer allocation while controlling for 
#' lagged crime rates, we need to restructure the data from annual to monthly 
#' observations and create one-month lags of crime rates. This approach assumes 
#' that police respond to changes in crime rates from month to month with 
#' increased police presence in month t+1.


# Create monthly precinct-level stops data (from stop-level events)

  # Create month-year variable and aggregate to monthly level
  pct_month <- sqf_all_clean %>%
    mutate(
      month = lubridate::month(STOP_FRISK_DATE),
      year_month = format(STOP_FRISK_DATE, "%Y-%m")
    ) %>%
    group_by(pct, year_month, month, year) %>%
    summarise(
      stops = n(),
      .groups = "drop"
    ) %>%
    mutate(
      log_stops = log(stops + 1),
      pct = as.numeric(pct),
      # Create date variable for proper ordering
      date = as.Date(paste0(year_month, "-01"))
    ) %>%
    arrange(pct, date)

    # glmipse(pct_month)

# Construct monthly crime rates.
#
# Because pct_crime is annual, expand to 12 rows per precinct-year and allocate
# annual rates evenly across months.

  # Examine the crime data structure
  # glmipse(pct_crime)
  
  # Since we only have annual crime data, we'll distribute it evenly across months
  pct_month_crime <- pct_crime %>%
    # join total pop from pct_demo
    left_join(pct_demo %>% select(pct, year, total_pop), by = c("pct", "year")) %>%
    # Expand each annual observation to 12 monthly observations
    tidyr::uncount(12) %>%
    group_by(pct, year) %>%
    mutate(
      month = row_number(),
      year_month = format(as.Date(paste0(year, "-", month, "-01")), "%Y-%m"),
      # Calculate nonviolent crime by summing relevant categories
      nonviolent_crime = crime_Weapons + crime_Property + crime_Drug + 
        crime_Trespass + crime_QualityOfLife + crime_Other,
      
      # Log crime rates per 1000 people
      violent_rate    = log((crime_Violent + 1) / total_pop * 1000),
      nonviolent_rate = log((nonviolent_crime + 1) / total_pop * 1000),
      # Distribute annual crime evenly across months (this is an approximation)
      violent_rate_month = violent_rate / 12,
      nonviolent_rate_month = nonviolent_rate / 12,
      date = as.Date(paste0(year_month, "-01")),
    ) %>%
    ungroup() %>%
    select(pct, year_month, month, year, date, violent_rate_month, nonviolent_rate_month)
  
  # glimpse(pct_month_crime)

# Add precinct area (derived from sf geometries) and build the final monthly panel.
#
# Note: Areas are computed once per precinct; we join those scalar values into
# the monthly panel to compute population density.

  # Join with precinct geometry from nypd_sf - but don't convert to sf yet
  pct_month_crime <- pct_month_crime %>%
    left_join(st_drop_geometry(nypd_sf) %>% select(Precinct), by = c("pct" = "Precinct"))
  
  # Calculate area for each precinct separately (no sf joins)
  pct_areas <- nypd_sf %>%
    filter(st_is_valid(geometry)) %>%  # Filter out invalid geometries
    mutate(
      area_sq_meters = as.numeric(st_area(geometry)),
      area_sq_miles = area_sq_meters * 3.861e-7
    ) %>%
    select(Precinct, area_sq_meters, area_sq_miles) %>%
    st_drop_geometry()
  
# Check which data frames are sf objects (debugging/guardrails)
  cat("pct_month is sf:", inherits(pct_month, "sf"), "\n")
  cat("pct_month_crime is sf:", inherits(pct_month_crime, "sf"), "\n") 
  cat("pct_demo is sf:", inherits(pct_demo, "sf"), "\n")

  # Join with stops data
  pct_month_full <- st_drop_geometry(pct_month) %>%
    left_join(pct_month_crime, by = c("pct", "year_month", "month", "year", "date")) %>%
    left_join(pct_demo, by = c("pct", "year")) %>%
    mutate(
      pct_black      = black_pop / total_pop,
      pct_white      = white_pop / total_pop,
      pct_hisp       = hisp_pop / total_pop,
      pct_18_24      = age_18_24_pop / total_pop,
      log_stops_rate = log((stops + 1) / total_pop * 1000)
    ) %>%
    filter(!is.na(violent_rate_month), !is.na(pct_black)) %>%
    left_join(pct_areas, by = c("pct" = "Precinct")) %>%
    mutate(pop_density = total_pop / area_sq_miles)
  
  # Sort by precinct and date for proper lagging
  pct_month_full <- pct_month_full %>%
    arrange(pct, date)

  # glimpse(pct_month_full)

# Create lagged variables (1-month lags, within precinct)

  # Create one-month lagged crime rates by precinct
  pct_month_lagged <- pct_month_full %>%
    group_by(pct) %>%
    arrange(date) %>%
    mutate(
      # One-month lagged crime rates
      lag_violent_rate = dplyr::lag(violent_rate_month, 1),
      lag_nonviolent_rate = dplyr::lag(nonviolent_rate_month, 1),
      
      # Also create lagged stops to capture persistence in police allocation
      lag_stops = dplyr::lag(stops, 1),
      lag_log_stops = dplyr::lag(log_stops, 1)
    ) %>%
    ungroup() %>%
    # Remove first month for each precinct due to missing lag
    filter(!is.na(lag_violent_rate))
  
  # Summary statistics
  cat("Monthly observations with lagged variables:", nrow(pct_month_lagged), "\n")
  cat("Number of precincts:", length(unique(pct_month_lagged$pct)), "\n")
  cat("Date range:", min(pct_month_lagged$date), "to", max(pct_month_lagged$date), "\n")

# Stop‑level models
#
# Drop raw string/flag columns after engineering outcomes + RS factors.
# This keeps sqf_all_clean/sqf_ols smaller and reduces accidental reuse of raw flags.
  
  discarded_vars <- c(
    "BACKROUND_CIRCUMSTANCES_SUSPECT_KNOWN_TO_CARRY_WEAPON_FLAG",
    "BACKROUND_CIRCUMSTANCES_VIOLENT_CRIME_FLAG",
    "FRISKED_FLAG",
    "FIREARM_FLAG",
    "KNIFE_CUTTER_FLAG",
    "OTHER_CONTRABAND_FLAG",
    "OTHER_PERSON_STOPPED_FLAG",
    "PHYSICAL_FORCE_CEW_FLAG",
    "PHYSICAL_FORCE_DRAW_POINT_FIREARM_FLAG",
    "PHYSICAL_FORCE_HANDCUFF_SUSPECT_FLAG",
    "PHYSICAL_FORCE_OC_SPRAY_USED_FLAG",
    "PHYSICAL_FORCE_OTHER_FLAG",
    "PHYSICAL_FORCE_RESTRAINT_USED_FLAG",
    "PHYSICAL_FORCE_WEAPON_IMPACT_FLAG",
    "SEARCHED_FLAG",
    "STOP_FRISK_DATE",
    "STOP_FRISK_TIME",
    "STOP_LOCATION_PRECINCT",
    "SUMMONS_ISSUED_FLAG",
    "SUSPECT_ARRESTED_FLAG",
    "SUSPECT_RACE_DESCRIPTION",
    "SUSPECT_REPORTED_AGE",
    "SUSPECT_SEX",
    "SUSPECTED_CRIME_DESCRIPTION",
    "SUSPECTS_ACTIONS_CASING_FLAG",
    "SUSPECTS_ACTIONS_CONCEALED_POSSESSION_WEAPON_FLAG",
    "SUSPECTS_ACTIONS_DECRIPTION_FLAG",
    "SUSPECTS_ACTIONS_DRUG_TRANSACTIONS_FLAG",
    "SUSPECTS_ACTIONS_IDENTIFY_CRIME_PATTERN_FLAG",
    "SUSPECTS_ACTIONS_LOOKOUT_FLAG",
    "SUSPECTS_ACTIONS_OTHER_FLAG",
    "SUSPECTS_ACTIONS_PROXIMITY_TO_SCENE_FLAG",
    "WEAPON_FOUND_FLAG"
  )
  
  sqf_all_clean <- sqf_all_clean %>%
    select(-all_of(discarded_vars))
  
# Stop-level data prep
    
# Restrict to main races (drop Others)
    sqf_ols <- sqf_all_clean %>%
      filter(race %in% BWH)
    
# Create race dummies (useful for non-formula interfaces and quick summaries)
    sqf_ols <- sqf_ols %>%
      mutate(
        Black = if_else(race == "BLACK", 1L, 0L),
        White = if_else(race == "WHITE", 1L, 0L),
        Hisp  = if_else(race == "HISPANIC", 1L, 0L),
      )
    
# Impute missing RS factors to 0 (NA interpreted as “no information / not flagged”)
    sqf_ols <- sqf_ols %>%
      mutate(across(starts_with("RS_"), ~if_else(is.na(.x), 0L, .x)))
    
# Save stop-level modeling dataset for reuse
    saveRDS(sqf_ols, file = "~/Projects/thesis/data/derived/sqf_ols.rds")

# Save a single bundle of derived data objects used by later analysis scripts
    save(
      sqf_all_clean,
      sqf_ols,
      pct_year,
      pct_year_ols,
      pct_year_ols_adams,
      pct_year_ols_blasio,
      pct_year_ols_bloomberg,
      pct_month,
      pct_month_crime,
      pct_month_full,
      pct_month_lagged,
      pct_demo,
      pct_crime,
      nypd_sf,
      file = "~/Projects/thesis/data/derived/sqf_model_data.rda"
    )


