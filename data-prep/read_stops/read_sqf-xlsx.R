# --- read sqf xlsx files (2017–2024) ----
#' The post-2017 SQF data uses a standardized Excel format with consistent
#' column names, unlike the legacy CSV files which required renaming.


# --- reader function ----
#' Reads a single xlsx SQF file and coerces all columns to character
#' so yearly files can be safely bound together without type conflicts.
read_sqf_xlsx <- function(path) {
  readxl::read_excel(path, na = c("", "(null)")) %>%
    dplyr::mutate(dplyr::across(dplyr::everything(), as.character))
}


# --- file list ----
#' Paths to yearly SQF xlsx files (2017–2024)
sqf_files_xlsx <- c(
  "data/nypd-stop/sqf-2017.xlsx",
  "data/nypd-stop/sqf-2018.xlsx",
  "data/nypd-stop/sqf-2019.xlsx",
  "data/nypd-stop/sqf-2020.xlsx",
  "data/nypd-stop/sqf-2021.xlsx",
  "data/nypd-stop/sqf-2022.xlsx",
  "data/nypd-stop/sqf-2023.xlsx",
  "data/nypd-stop/sqf-2024.xlsx"
)


# --- read and combine ----
#' Stack all yearly files into one dataframe
sqf_hist <- sqf_files_xlsx %>%
  purrr::map_dfr(read_sqf_xlsx)


# --- align columns to expected schema ----
#' Add any columns present in expected_sqf_fields but missing from the xlsx
#' files (e.g. fields not collected in certain years) and fill with NA.
#' Then subset to only the expected columns to drop any extraneous fields.
missing_cols <- setdiff(expected_sqf_fields, names(sqf_hist))
sqf_hist[missing_cols] <- NA
sqf_hist <- sqf_hist[, expected_sqf_fields]


# --- normalize variable types ----
#' Numeric vars that arrive as character after the coercion step above
numeric_vars <- c(
  "STOP_ID", "STOP_FRISK_ID", "STOP_FRISK_TIME", "ISSUING_OFFICER_COMMAND_CODE", "SUPERVISING_OFFICER_COMMAND_CODE",
  "OBSERVED_DURATION_MINUTES", "STOP_DURATION_MINUTES", "SUSPECT_REPORTED_AGE", "SUSPECT_WEIGHT",
  "SUSPECT_HEIGHT", "STOP_LOCATION_PRECINCT", "STOP_LOCATION_X", "STOP_LOCATION_Y", "STOP_LOCATION_ZIP_CODE"
)

sqf_hist <- sqf_hist %>%
  dplyr::mutate(
    # parse date and time fields
    STOP_FRISK_DATE = as.Date(STOP_FRISK_DATE),
    YEAR2  = as.double(YEAR2),
    MONTH2 = as.double(MONTH2),
    DAY2   = as.double(DAY2),
    # cast numeric fields
    dplyr::across(dplyr::all_of(numeric_vars), as.double),
    # keep command codes as character (may contain leading zeros)
    STOP_FRISK_TIME = as.character(STOP_FRISK_TIME),
    ISSUING_OFFICER_COMMAND_CODE = as.character(ISSUING_OFFICER_COMMAND_CODE),
    SUPERVISING_OFFICER_COMMAND_CODE = as.character(SUPERVISING_OFFICER_COMMAND_CODE),
    # normalize race labels to a consistent set across years
    SUSPECT_RACE_DESCRIPTION = dplyr::case_when(
      SUSPECT_RACE_DESCRIPTION == "BLACK HISPANIC"                               ~ "HISPANIC-BLACK",
      SUSPECT_RACE_DESCRIPTION == "WHITE HISPANIC"                               ~ "HISPANIC-WHITE",
      SUSPECT_RACE_DESCRIPTION %in% c("ASIAN/PAC.ISL", "ASIAN / PACIFIC ISLANDER") ~ "ASIAN / PACIFIC ISLANDER",
      SUSPECT_RACE_DESCRIPTION %in% c("AMER IND", "AMERICAN INDIAN/ALASKAN N")   ~ "AMERICAN INDIAN/ALASKAN NATIVE",
      SUSPECT_RACE_DESCRIPTION %in% c("MIDDLE EASTERN/SOUTHWEST")                ~ "MIDDLE EASTERN/SOUTHWEST ASIAN",
      SUSPECT_RACE_DESCRIPTION %in% c(
        "BLACK", "WHITE", "HISPANIC-BLACK", "HISPANIC-WHITE",
        "ASIAN / PACIFIC ISLANDER", "AMERICAN INDIAN/ALASKAN NATIVE",
        "MIDDLE EASTERN/SOUTHWEST ASIAN"
      ) ~ SUSPECT_RACE_DESCRIPTION,
      is.na(SUSPECT_RACE_DESCRIPTION) ~ "OTHER",
      TRUE ~ "OTHER"
    )
  )

#' save 2016 to 2024 stop data to df
save(sqf_hist, file = "data/data-final/nypd-stop/sqf_hist.rda")
