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


# --- save ----
saveRDS(sqf_all, file = "data/data-final/nypd-stop/sqf_all.rds")
