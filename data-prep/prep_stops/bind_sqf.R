# --- check if cached RDS file exists; load and skip if so ----
if (file.exists("data/data-final/nypd-stop/sqf_all.rds")) {
  message("sqf_all.rds found — loading from cache.")
  sqf_all <- readRDS("data/data-final/nypd-stop/sqf_all.rds")
} else {

# --- bind sqf_hist and sqf_legacy into one df (2009–2024) ----
#' sqf_hist   : modern xlsx format, 2017–2024 (from read_sqf-xlxs.R)
#' sqf_legacy : normalized legacy format, 2009–2016 (from sqf_legacy-prep.R)
#' Both are aligned to expected_sqf_data before binding.


# --- bind ----
sqf_diff <- setdiff(names(sqf_hist), names(sqf_legacy)) # check diff bt dfs before binding

#' Coerce shared columns to character before binding to avoid type conflicts
#' (e.g. STOP_FRISK_ID is double in sqf_legacy but character in sqf_hist).
#' Types are normalized after binding.
sqf_all <- dplyr::bind_rows(
  dplyr::mutate(sqf_hist,   dplyr::across(dplyr::everything(), as.character)),
  dplyr::mutate(sqf_legacy, dplyr::across(dplyr::everything(), as.character))
)


# --- restore types after binding ----
numeric_vars <- c(
  "STOP_ID", "STOP_FRISK_ID", "ISSUING_OFFICER_COMMAND_CODE", "SUPERVISING_OFFICER_COMMAND_CODE",
  "OBSERVED_DURATION_MINUTES", "STOP_DURATION_MINUTES", "SUSPECT_REPORTED_AGE", "SUSPECT_WEIGHT",
  "SUSPECT_HEIGHT", "STOP_LOCATION_PRECINCT", "STOP_LOCATION_X", "STOP_LOCATION_Y",
  "STOP_LOCATION_ZIP_CODE", "YEAR2", "MONTH2", "DAY2"
)

sqf_all <- sqf_all %>%
  dplyr::mutate(
    STOP_FRISK_DATE = as.Date(STOP_FRISK_DATE),
    dplyr::across(dplyr::any_of(numeric_vars), as.double)
  )


# --- count missing key identifiers ----
#' Record how many rows are missing each key field before any filtering.
sqf_missing_counts <- tibble::tibble(
  variable = c("SUSPECT_RACE_DESCRIPTION", "SUSPECT_REPORTED_AGE",
               "STOP_LOCATION_PRECINCT", "STOP_FRISK_DATE"),
  n_missing = c(
    sum(is.na(sqf_all$SUSPECT_RACE_DESCRIPTION)),
    sum(is.na(sqf_all$SUSPECT_REPORTED_AGE)),
    sum(is.na(sqf_all$STOP_LOCATION_PRECINCT)),
    sum(is.na(sqf_all$STOP_FRISK_DATE))
  ),
  pct_missing = round(n_missing / nrow(sqf_all) * 100, 2)
)

# --- final race normalization ----
unique(sqf_all$SUSPECT_RACE_DESCRIPTION)
#' Collapse variant labels that appear across different data vintages into a
#' single consistent value for each category.
sqf_all <- sqf_all %>%
  dplyr::mutate(
    SUSPECT_RACE_DESCRIPTION = dplyr::case_when(
      SUSPECT_RACE_DESCRIPTION %in% c("BLACK HISPANIC")                                              ~ "HISPANIC-BLACK",
      SUSPECT_RACE_DESCRIPTION %in% c("WHITE HISPANIC")                                              ~ "HISPANIC-WHITE",
      SUSPECT_RACE_DESCRIPTION %in% c("ASIAN/PAC.ISL", "ASIAN / PACIFIC ISLANDER")                  ~ "ASIAN-PI",
      SUSPECT_RACE_DESCRIPTION %in% c("AMER IND", "AMERICAN INDIAN/ALASKAN N",
                                       "AMERICAN INDIAN/ALASKAN NATIVE")                            ~ "NATIVE-AMER",
      SUSPECT_RACE_DESCRIPTION %in% c("MIDDLE EASTERN/SOUTHWEST", "MIDDLE EASTERN/SOUTHWEST ASIAN") ~ "MIDEAST",
      SUSPECT_RACE_DESCRIPTION %in% c("Other", "MALE")                                              ~ "OTHER",
      is.na(SUSPECT_RACE_DESCRIPTION)                                                                ~ "OTHER",
      TRUE ~ SUSPECT_RACE_DESCRIPTION
    )
  )

unique(sqf_all$SUSPECT_RACE_DESCRIPTION)

# --- frequency table: stops by race x year ----
sqf_race_year <- sqf_all %>%
  dplyr::count(YEAR2, SUSPECT_RACE_DESCRIPTION) %>%
  tidyr::pivot_wider(names_from = SUSPECT_RACE_DESCRIPTION, values_from = n, values_fill = 0) %>%
  dplyr::arrange(YEAR2)


# --- uniqueness check: pre-2017 stops (no persistent stop ID) ----
#' Post-2017 stops carry a persistent STOP_FRISK_ID that guarantees uniqueness.
#' For pre-2017 legacy data, IDs reset annually. We treat each row as a unique
#' stop and justify this with a duplicate-signature test.
#'
#' A "collision" is defined as two or more rows sharing the same date, minute,
#' grid coordinates (X/Y), and suspect demographics (race, sex, age). The
#' demographic fields are included because block-level coordinates and minute-
#' precision timestamps alone cannot distinguish two different people stopped at
#' the same corner in the same minute. Two rows that also share race + sex + age
#' are far more likely to be true duplicates than distinct stops.
#'
#' Collisions where at least one row carries OTHER_PERSON_STOPPED_FLAG == "Y"
#' are expected: officers recorded each person in a multi-person encounter as a
#' separate UF-250 row. Any residual collisions are apparent duplicates.
sqf_dupe_check <- sqf_all %>%
  dplyr::filter(
    YEAR2 < 2017,
    !is.na(STOP_FRISK_DATE), !is.na(STOP_FRISK_TIME),
    !is.na(STOP_LOCATION_X), !is.na(STOP_LOCATION_Y),
    !is.na(SUSPECT_RACE_DESCRIPTION), !is.na(SUSPECT_SEX),
    !is.na(SUSPECT_REPORTED_AGE)
  ) %>%
  dplyr::group_by(
    STOP_FRISK_DATE, STOP_FRISK_TIME,
    STOP_LOCATION_X, STOP_LOCATION_Y,
    SUSPECT_RACE_DESCRIPTION, SUSPECT_SEX, SUSPECT_REPORTED_AGE
  ) %>%
  dplyr::mutate(group_n = dplyr::n()) %>%
  dplyr::ungroup()

n_pre2017          <- nrow(sqf_dupe_check)
n_collision_rows   <- sqf_dupe_check %>% dplyr::filter(group_n > 1) %>% nrow()
n_multiperson_rows <- sqf_dupe_check %>%
  dplyr::filter(group_n > 1, OTHER_PERSON_STOPPED_FLAG == "Y") %>% nrow()
n_true_dup_rows    <- n_collision_rows - n_multiperson_rows

sqf_uniqueness_summary <- tibble::tibble(
  description = c(
    "pre-2017 stops with location, time & demographics",
    "rows in same-signature groups (>1)",
    "  of which: ≥1 row has OTHER_PERSON_STOPPED_FLAG = Y",
    "  residual apparent duplicates"
  ),
  n   = c(n_pre2017, n_collision_rows, n_multiperson_rows, n_true_dup_rows),
  pct = round(n / n_pre2017 * 100, 3)
)

print(sqf_uniqueness_summary)

rm(sqf_dupe_check, n_pre2017, n_collision_rows, n_multiperson_rows, n_true_dup_rows)


# --- save ----
saveRDS(sqf_all, file = "data/data-final/nypd-stop/sqf_all.rds")

} # end if/else cache check
