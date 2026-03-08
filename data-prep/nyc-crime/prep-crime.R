# --- NYPD Complaint Data prep ----
#' Source: https://data.cityofnewyork.us/Public-Safety/NYPD-Complaint-Data-Historic/qgea-i56i
#' Split into two files: 2006–2019 and 2020–2024.
#'
#' Outputs:
#'   crime_pct_month.rds   — precinct × year × month crime counts + suspect race props
#'   crime_tract_month.rds — census tract × year × month (spatial join on lat/lon)
#'   crime_points.rds      — individual complaints with coordinates for dot maps
#'
#' Requires in environment: tracts_sf (crs = 4326, must have GEOID column)


# --- precinct-to-borough lookup ----
#' nypd_sf has no borough field; use static NYPD precinct assignments.
pct_boro_lookup <- dplyr::tibble(
  pct = c(
    1, 5, 6, 7, 9, 10, 13, 14, 17, 18, 19, 20, 22, 23, 24, 25, 26, 28, 30, 32, 33, 34,
    40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 52,
    60, 61, 62, 63, 66, 67, 68, 69, 70, 71, 72, 73, 75, 76, 77, 78, 79, 81, 83, 84, 88, 90, 94,
    100, 101, 102, 103, 104, 105, 106, 107, 108, 109, 110, 111, 112, 113, 114, 115, 116,
    120, 121, 122, 123
  ),
  pct_boro = c(
    rep("MANHATTAN",     22),
    rep("BRONX",         12),
    rep("BROOKLYN",      23),
    rep("QUEENS",        17),
    rep("STATEN ISLAND",  4)
  )
)


# --- column spec: only what is needed ----
#' Lat/Lon as double for spatial join; SUSP_RACE for suspect race proportions.
needed_cols <- readr::cols_only(
  CMPLNT_FR_DT = readr::col_character(),
  CMPLNT_TO_DT = readr::col_character(),
  ADDR_PCT_CD  = readr::col_character(),
  BORO_NM      = readr::col_character(),
  LAW_CAT_CD   = readr::col_character(),
  PD_DESC      = readr::col_character(),
  SUSP_RACE    = readr::col_character(),
  Latitude     = readr::col_double(),
  Longitude    = readr::col_double()
)


# --- offense regex patterns (compiled once, reused per file) ----
pat_violent <- stringr::regex(paste0(
  "assault|aggravated|robbery|kidnapping|menacing|strangulation|",
  "reckless endangerment|coercion|imprisonment|obstr breath|terroristic|",
  "riot|homicide|rape|terrorism|torture|harassment|",
  "sexual.?abuse|sex.?abuse|sodomy|sexual.?misconduct|sex.?misconduct|",
  "sex.?crime|sex.?offense|course.?of.?sexual|",
  "child.*endanger|endanger.*welfare|welfare.*child|",
  "stalking|luring.*child|use.*child.*sexual|sexual.*performance|",
  "intimate.?image|incest"
), ignore_case = TRUE)

pat_weapons <- stringr::regex(
  "weapon|firearm|pistol|rifle|knife|weap",
  ignore_case = TRUE
)

pat_property <- stringr::regex(paste0(
  "larceny|burglary|burglars|arson|mischief|criminal mis|",
  "stolen|forgery|fraud|theft|tampering|grand|petit|jostling|",
  "unauthorized use vehicle|theft of services"
), ignore_case = TRUE)

pat_drug <- stringr::regex(paste0(
  "controlled substance|cannabis|marijuana|drug|paraphernalia|",
  "sale school grounds|under the influence"
), ignore_case = TRUE)

pat_trespass <- stringr::regex("trespass", ignore_case = TRUE)

pat_qol <- stringr::regex(paste0(
  "disorderly|loitering|alcoholic|fireworks|intoxicated|impaired driving|",
  "noise|gambling|graffiti|peddling|nuisance|obscenity|prostitution|",
  "smoking|traffic|assembly|lewd|exposure of a person|",
  "unauth.*sale.*trans|sale.*transit service|transit.*fare|reckless.?driv"
), ignore_case = TRUE)


# --- helper: transform one chunk (shared by both files) ----
transform_chunk <- function(chunk) {
  chunk %>%
    dplyr::mutate(
      CMPLNT_FR_DT = lubridate::mdy(CMPLNT_FR_DT, quiet = TRUE),
      CMPLNT_TO_DT = lubridate::mdy(CMPLNT_TO_DT, quiet = TRUE),
      final_date   = dplyr::if_else(
        !is.na(CMPLNT_TO_DT) & (CMPLNT_TO_DT - CMPLNT_FR_DT) > lubridate::ddays(3),
        CMPLNT_TO_DT,
        CMPLNT_FR_DT
      ),
      year  = as.integer(lubridate::year(final_date)),
      month = as.integer(lubridate::month(final_date)),
      pct   = as.numeric(ADDR_PCT_CD),
      boro  = dplyr::na_if(BORO_NM, "(null)")
    ) %>%
    dplyr::left_join(pct_boro_lookup, by = "pct") %>%
    dplyr::mutate(boro = dplyr::coalesce(boro, pct_boro)) %>%
    dplyr::select(-pct_boro) %>%
    dplyr::mutate(
      felony      = dplyr::if_else(LAW_CAT_CD == "FELONY",      1L, 0L, missing = 0L),
      misdemeanor = dplyr::if_else(LAW_CAT_CD == "MISDEMEANOR", 1L, 0L, missing = 0L),
      violation   = dplyr::if_else(LAW_CAT_CD == "VIOLATION",   1L, 0L, missing = 0L),
      off_cat_broad = dplyr::case_when(
        stringr::str_detect(PD_DESC, pat_violent)  ~ "crime_Violent",
        stringr::str_detect(PD_DESC, pat_weapons)  ~ "crime_Weapons",
        stringr::str_detect(PD_DESC, pat_property) ~ "crime_Property",
        stringr::str_detect(PD_DESC, pat_drug)     ~ "crime_Drug",
        stringr::str_detect(PD_DESC, pat_trespass) ~ "crime_Trespass",
        stringr::str_detect(PD_DESC, pat_qol)      ~ "crime_QualityOfLife",
        TRUE                                        ~ "crime_Other"
      ),
      # BLACK HISPANIC / WHITE HISPANIC → "hisp"; unknown/null → NA (excluded from props)
      susp_race = dplyr::case_when(
        SUSP_RACE == "BLACK"                                       ~ "black",
        SUSP_RACE %in% c("WHITE HISPANIC", "BLACK HISPANIC")      ~ "hisp",
        SUSP_RACE == "WHITE"                                       ~ "white",
        SUSP_RACE %in% c("UNKNOWN", "(null)") | is.na(SUSP_RACE) ~ NA_character_,
        TRUE                                                        ~ "other"
      )
    ) %>%
    dplyr::filter(year >= 2009, year <= 2024)
}

# --- helper: aggregate a transformed chunk to precinct × month counts ----
agg_pct <- function(d) {
  d %>%
    dplyr::filter(pct >= 1, pct <= 123) %>%
    dplyr::group_by(pct, boro, year, month) %>%
    dplyr::summarize(
      n                   = dplyr::n(),
      felony              = sum(felony,                                 na.rm = TRUE),
      misdemeanor         = sum(misdemeanor,                            na.rm = TRUE),
      violation           = sum(violation,                              na.rm = TRUE),
      crime_Violent       = sum(off_cat_broad == "crime_Violent",       na.rm = TRUE),
      crime_Weapons       = sum(off_cat_broad == "crime_Weapons",       na.rm = TRUE),
      crime_Property      = sum(off_cat_broad == "crime_Property",      na.rm = TRUE),
      crime_Drug          = sum(off_cat_broad == "crime_Drug",          na.rm = TRUE),
      crime_Trespass      = sum(off_cat_broad == "crime_Trespass",      na.rm = TRUE),
      crime_QualityOfLife = sum(off_cat_broad == "crime_QualityOfLife", na.rm = TRUE),
      crime_Other         = sum(off_cat_broad == "crime_Other",         na.rm = TRUE),
      n_black             = sum(susp_race == "black", na.rm = TRUE),
      n_hisp              = sum(susp_race == "hisp",  na.rm = TRUE),
      n_white             = sum(susp_race == "white", na.rm = TRUE),
      n_known_race        = sum(!is.na(susp_race)),
      .groups = "drop"
    )
}

# --- helper: spatial join a chunk to tracts, aggregate to tract × month counts ----
#' tracts_sf must be in environment with crs = 4326 and a GEOID column.
agg_tract <- function(d) {
  pts_sf <- d %>%
    dplyr::filter(!is.na(Latitude), !is.na(Longitude)) %>%
    dplyr::select(
      year, month, off_cat_broad,
      felony, misdemeanor, violation,
      susp_race, Latitude, Longitude
    ) %>%
    sf::st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326)

  sf::st_join(
    pts_sf,
    tracts_sf %>% sf::st_make_valid() %>% dplyr::select(GEOID),
    join = sf::st_intersects
  ) %>%
    sf::st_drop_geometry() %>%
    dplyr::filter(!is.na(GEOID)) %>%
    dplyr::group_by(GEOID, year, month) %>%
    dplyr::summarize(
      n                   = dplyr::n(),
      felony              = sum(felony,                                 na.rm = TRUE),
      misdemeanor         = sum(misdemeanor,                            na.rm = TRUE),
      violation           = sum(violation,                              na.rm = TRUE),
      crime_Violent       = sum(off_cat_broad == "crime_Violent",       na.rm = TRUE),
      crime_Weapons       = sum(off_cat_broad == "crime_Weapons",       na.rm = TRUE),
      crime_Property      = sum(off_cat_broad == "crime_Property",      na.rm = TRUE),
      crime_Drug          = sum(off_cat_broad == "crime_Drug",          na.rm = TRUE),
      crime_Trespass      = sum(off_cat_broad == "crime_Trespass",      na.rm = TRUE),
      crime_QualityOfLife = sum(off_cat_broad == "crime_QualityOfLife", na.rm = TRUE),
      crime_Other         = sum(off_cat_broad == "crime_Other",         na.rm = TRUE),
      n_black             = sum(susp_race == "black", na.rm = TRUE),
      n_hisp              = sum(susp_race == "hisp",  na.rm = TRUE),
      n_white             = sum(susp_race == "white", na.rm = TRUE),
      n_known_race        = sum(!is.na(susp_race)),
      .groups = "drop"
    )
}

# --- helper: add race proportions after chunk counts are summed ----
add_race_props <- function(d) {
  d %>% dplyr::mutate(
    prop_black_susp = dplyr::if_else(n_known_race > 0, n_black / n_known_race, NA_real_),
    prop_hisp_susp  = dplyr::if_else(n_known_race > 0, n_hisp  / n_known_race, NA_real_),
    prop_white_susp = dplyr::if_else(n_known_race > 0, n_white / n_known_race, NA_real_)
  )
}


# --- check if cached RDS files exist; load and skip if so ----
.crime_rds_files <- c(
  "data/data-final/nyc-crime/crime_pct_month.rds",
  "data/data-final/nyc-crime/crime_tract_month.rds",
  "data/data-final/nyc-crime/crime_points.rds",
  "data/data-final/pct_month_lagged.rds"
)

if (all(file.exists(.crime_rds_files))) {
  message("Crime RDS files found — loading from cache.")
  crime_pct_month   <- readRDS("data/data-final/nyc-crime/crime_pct_month.rds")
  crime_tract_month <- readRDS("data/data-final/nyc-crime/crime_tract_month.rds")
  crime_points      <- readRDS("data/data-final/nyc-crime/crime_points.rds")
  pct_month_lagged  <- readRDS("data/data-final/pct_month_lagged.rds")
} else {

# --- file 1: 2006–2019 ----
d1 <- arrow::open_dataset("data/nypd-crime/NYPD_Complaint_Data_Historic.csv", format = "csv") |>
  dplyr::select(CMPLNT_FR_DT, CMPLNT_TO_DT, PD_DESC, ADDR_PCT_CD, BORO_NM,
                LAW_CAT_CD, SUSP_RACE, Latitude, Longitude) |>
  dplyr::collect()

d1 <- transform_chunk(d1)
pct1   <- agg_pct(d1)
tract1 <- agg_tract(d1)
pts1   <- d1 %>%
  dplyr::filter(!is.na(Latitude), !is.na(Longitude)) %>%
  dplyr::select(year, month, pct, boro, off_cat_broad, LAW_CAT_CD, susp_race, Latitude, Longitude)
rm(d1); gc()


# --- file 2: 2020–2024 ----
d2 <- arrow::open_dataset("data/nypd-crime/NYPD_Complaint_Data_Historic_1.csv", format = "csv") |>
  dplyr::select(CMPLNT_FR_DT, CMPLNT_TO_DT, ADDR_PCT_CD, BORO_NM,
                LAW_CAT_CD, PD_DESC, SUSP_RACE, Latitude, Longitude) |>
  dplyr::collect()

d2 <- transform_chunk(d2)
pct2   <- agg_pct(d2)
tract2 <- agg_tract(d2)
pts2   <- d2 %>%
  dplyr::filter(!is.na(Latitude), !is.na(Longitude)) %>%
  dplyr::select(year, month, pct, boro, off_cat_broad, LAW_CAT_CD, susp_race, Latitude, Longitude)
rm(d2); gc()


# --- bind and finalize ----
crime_pct_month <- dplyr::bind_rows(pct1, pct2) %>%
  add_race_props() %>%
  dplyr::mutate(
    nonviolent_crime = crime_Weapons + crime_Property + crime_Drug +
                       crime_Trespass + crime_QualityOfLife + crime_Other,
    year_month = sprintf("%04d-%02d", year, month),
    date       = as.Date(paste0(year_month, "-01"))
  )

crime_tract_month <- dplyr::bind_rows(tract1, tract2) %>%
  add_race_props() %>%
  dplyr::mutate(
    nonviolent_crime = crime_Weapons + crime_Property + crime_Drug +
                       crime_Trespass + crime_QualityOfLife + crime_Other,
    year_month = sprintf("%04d-%02d", year, month),
    date       = as.Date(paste0(year_month, "-01"))
  )

crime_points <- dplyr::bind_rows(pts1, pts2) %>%
  dplyr::mutate(
    year_month = sprintf("%04d-%02d", year, month),
    date       = as.Date(paste0(year_month, "-01"))
  )

rm(pct1, pct2, tract1, tract2, pts1, pts2); gc()


# --- shootings ----
shootings_pct_month <- readr::read_csv("data/nypd-crime/NYPD_Shootings_Data__Historic.csv") %>%
  dplyr::mutate(
    OCCUR_DATE = lubridate::mdy(OCCUR_DATE),
    year  = as.integer(lubridate::year(OCCUR_DATE)),
    month = as.integer(lubridate::month(OCCUR_DATE)),
    pct   = as.numeric(PRECINCT)
  ) %>%
  dplyr::group_by(pct, year, month) %>%
  dplyr::summarize(shootings = dplyr::n(), .groups = "drop")

crime_pct_month <- crime_pct_month %>%
  dplyr::left_join(shootings_pct_month, by = c("pct", "year", "month"))


# --- expand annual demographic data to monthly ----
#' Repeat each precinct × year row 12 times (one per month) and assign month.
demo_pct_month <- demo_pct %>%
  tidyr::uncount(12) %>%
  dplyr::group_by(pct, year) %>%
  dplyr::mutate(month = dplyr::row_number()) %>%
  dplyr::ungroup()


# --- precinct areas (sq miles) from nypd_sf ----
pct_areas <- nypd_sf %>%
  sf::st_make_valid() %>%
  dplyr::mutate(
    area_sq_meters = as.numeric(sf::st_area(geometry)),
    area_sq_miles  = area_sq_meters * 3.861e-7
  ) %>%
  sf::st_drop_geometry() %>%
  dplyr::select(Precinct, area_sq_miles)


# --- race-specific monthly stop counts (from SQF data) ----
#' Uses sqf_all with updated race labels (HISPANIC-BLACK, HISPANIC-WHITE, etc.)
race_pct_month <- sqf_all %>%
  dplyr::mutate(
    race = dplyr::case_when(
      SUSPECT_RACE_DESCRIPTION == "BLACK"                                        ~ "black",
      SUSPECT_RACE_DESCRIPTION %in% c("HISPANIC-BLACK", "HISPANIC-WHITE")       ~ "hisp",
      SUSPECT_RACE_DESCRIPTION == "WHITE"                                        ~ "white",
      TRUE                                                                        ~ "other"
    )
  ) %>%
  dplyr::group_by(STOP_LOCATION_PRECINCT, YEAR2, MONTH2) %>%
  dplyr::summarize(
    stops_black = sum(race == "black", na.rm = TRUE),
    stops_hisp  = sum(race == "hisp",  na.rm = TRUE),
    stops_white = sum(race == "white", na.rm = TRUE),
    .groups = "drop"
  ) %>%
  dplyr::rename(pct = STOP_LOCATION_PRECINCT, year = YEAR2, month = MONTH2) %>%
  dplyr::mutate(
    log_stops_black = log(stops_black + 1),
    log_stops_hisp  = log(stops_hisp  + 1),
    log_stops_white = log(stops_white + 1)
  )


# --- build precinct monthly panel ----
pct_month_full <- crime_pct_month %>%
  dplyr::left_join(demo_pct_month,   by = c("pct", "year", "month")) %>%
  dplyr::left_join(pct_areas,        by = c("pct" = "Precinct")) %>%
  dplyr::left_join(race_pct_month,   by = c("pct", "year", "month")) %>%
  dplyr::mutate(
    pct_black        = black_pop     / total_pop,
    pct_white        = white_pop     / total_pop,
    pct_hisp         = hisp_pop      / total_pop,
    pct_18_24        = age_18_24_pop / total_pop,
    pop_density      = total_pop     / area_sq_miles,
    violent_rate     = log((crime_Violent    + 1) / total_pop * 1000),
    nonviolent_rate  = log((nonviolent_crime + 1) / total_pop * 1000),
    shooting_rate    = log((shootings        + 1) / total_pop * 1000)
  ) %>%
  dplyr::filter(total_pop > 0)


# --- lagged variables (1-month, within precinct) ----
pct_month_lagged <- pct_month_full %>%
  dplyr::arrange(pct, date) %>%
  dplyr::group_by(pct) %>%
  dplyr::mutate(
    lag_violent_rate    = dplyr::lag(violent_rate,    1),
    lag_nonviolent_rate = dplyr::lag(nonviolent_rate, 1),
  ) %>%
  dplyr::ungroup() %>%
  dplyr::filter(!is.na(lag_violent_rate))  # drop first month per precinct


# --- save ----
saveRDS(crime_pct_month,   file = "data/data-final/nyc-crime/crime_pct_month.rds")
saveRDS(crime_tract_month, file = "data/data-final/nyc-crime/crime_tract_month.rds")
saveRDS(crime_points,      file = "data/data-final/nyc-crime/crime_points.rds")
saveRDS(pct_month_lagged,  file = "data/data-final/pct_month_lagged.rds")

} # end if/else cache check
