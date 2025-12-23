
# since the Quarto docs may try to install a package during rendering
# setup CRAN mirror globally, so R knows which CRAN repository to use
options(repos = c(CRAN = "https://cloud.r-project.org"))


# list of required packages
required_packages <- c("tidyverse", "readxl", "mapview", "sf", 
                        "kableExtra", "viridis", "gridExtra", "tidycensus",
                        "broom", "corrr", "ggcorrplot", "factoextra", "FactoMineR", "magrittr",
                        "MatchIt", "marginaleffects", "dots",
                        "tinytext", "textdata", "lintr")

# check and install any missing packages
installed_packages <- rownames(installed.packages())
for (pkg in required_packages) {
  if (!pkg %in% installed_packages) {
    install.packages(pkg)
  }
}

# load libraries
library(tidyverse)
library(readxl)
library(mapview)
library(sf)
library(kableExtra)
library(viridis)
library(gridExtra)
library(tidycensus)
library(broom)
library(corrr)
library(ggcorrplot)
library(factoextra)
library(FactoMineR)
library(magrittr)
library(MatchIt)
library(marginaleffects)
library(dots)
library(lintr)

# define expected sqf data schema
expected_sqf_data <- c(
  "STOP_ID", "STOP_FRISK_DATE", "STOP_FRISK_TIME",
  "ISSUING_OFFICER_COMMAND_CODE", "SUPERVISING_OFFICER_COMMAND_CODE",
  "OBSERVED_DURATION_MINUTES", "STOP_DURATION_MINUTES",
  "SUSPECT_REPORTED_AGE", "SUSPECT_WEIGHT", "SUSPECT_HEIGHT",
  "STOP_LOCATION_PRECINCT", "STOP_LOCATION_X", "STOP_LOCATION_Y", "STOP_LOCATION_ZIP_CODE"
)

# Helper function: read + normalize
read_sqf <- function(path) {
  # treating (null) values as NA values, for simplicity in the analysis. note that where NA appears in the data, it is because the officer there was no data for that fields not that, it was not applicable to the situation.
  df <- read_excel(path, na = "(null)")

  # add missing expected fields with NA values
  missing_sqf_data <- setdiff(expected_sqf_data, names(df))
  df[missing_sqf_data] <- NA

  # coerce fields to standardized variable types
  df %>%
    dplyr::mutate(
      STOP_ID = as.double(.data$STOP_ID),
      STOP_FRISK_DATE = as.Date(.data$STOP_FRISK_DATE),
      STOP_FRISK_TIME = as.character(.data$STOP_FRISK_TIME),
      ISSUING_OFFICER_COMMAND_CODE = as.character(.data$ISSUING_OFFICER_COMMAND_CODE),
      SUPERVISING_OFFICER_COMMAND_CODE = as.character(.data$SUPERVISING_OFFICER_COMMAND_CODE),
      OBSERVED_DURATION_MINUTES = as.double(.data$OBSERVED_DURATION_MINUTES),
      STOP_DURATION_MINUTES = as.double(.data$STOP_DURATION_MINUTES),
      SUSPECT_REPORTED_AGE = as.double(.data$SUSPECT_REPORTED_AGE),
      SUSPECT_WEIGHT = as.double(.data$SUSPECT_WEIGHT),
      SUSPECT_HEIGHT = as.double(.data$SUSPECT_HEIGHT),
      STOP_LOCATION_PRECINCT = as.double(.data$STOP_LOCATION_PRECINCT),
      STOP_LOCATION_X = as.double(.data$STOP_LOCATION_X),
      STOP_LOCATION_Y = as.double(.data$STOP_LOCATION_Y),
      STOP_LOCATION_ZIP_CODE = as.double(.data$STOP_LOCATION_ZIP_CODE)
    )
}

# list of sqf data files
sqf_files <- c(
  "r-data/nypd-stop/sqf-2024.xlsx",
  "r-data/nypd-stop/sqf-2023.xlsx",
  "r-data/nypd-stop/sqf-2022.xlsx",
  "r-data/nypd-stop/sqf-2021.xlsx",
  "r-data/nypd-stop/sqf-2020.xlsx",
  "r-data/nypd-stop/sqf-2019.xlsx",
  "r-data/nypd-stop/sqf-2018.xlsx",
  "r-data/nypd-stop/sqf-2017.xlsx"
  # "r-data/nypd-stop/sqf-2016.csv"
  # "r-data/nypd-stop/sqf-2015.csv"
  # "r-data/nypd-stop/sqf-2014.csv"
  # "r-data/nypd-stop/sqf-2013.csv"
  # "r-data/nypd-stop/sqf-2012.csv"
  # "r-data/nypd-stop/sqf-2011.csv"
)

# read annual sqf files to historic df
sqf_hist <- sqf_files %>%
  map_dfr(read_sqf)


# quick text for Black, White, and Hispanic stops
BWH <- c("BLACK", "WHITE", "HISPANIC")

sqf_hist <- sqf_hist %>%
  dplyr::mutate(SUSPECT_RACE_DESCRIPTION = dplyr::case_when(.data$SUSPECT_RACE_DESCRIPTION %in%
                                                c("BLACK HISPANIC", "WHITE HISPANIC") ~ "HISPANIC",
                                              TRUE ~ .data$SUSPECT_RACE_DESCRIPTION),
         SUSPECT_RACE_DESCRIPTION = dplyr::case_when(.data$SUSPECT_RACE_DESCRIPTION %in%
                                                c("ASIAN/PAC.ISL") ~ "ASIAN",
                                              TRUE ~ .data$SUSPECT_RACE_DESCRIPTION),
         SUSPECT_RACE_DESCRIPTION = dplyr::case_when(.data$SUSPECT_RACE_DESCRIPTION %in%
                                                c("AMER IND", "AMERICAN INDIAN/ALASKAN N") ~ "AMERICAN INDIAN/ALASKAN NATIVE",
                                              TRUE ~ .data$SUSPECT_RACE_DESCRIPTION),
         SUSPECT_RACE_DESCRIPTION = dplyr::case_when(.data$SUSPECT_RACE_DESCRIPTION %in%
                                                c("MIDDLE EASTERN/SOUTHWEST") ~ "MIDDLE EASTERN/SOUTHWEST ASIAN",
                                              TRUE ~ .data$SUSPECT_RACE_DESCRIPTION))

# have months sort chronologically, not alphabetically
sqf_hist <- sqf_hist %>%
  dplyr::mutate(MONTH2 = factor(.data$MONTH2,
                         levels = month.name,
                         ordered = TRUE))

# count stop totals per month, per precinct
sqf_hist %>%
  group_by(STOP_LOCATION_PRECINCT, MONTH2) %>%
  count() %>%
  pivot_wider(names_from = MONTH2,
              values_from = n)

# complaint history
complaint_hist <- read_csv("r-data/nypd-crime/NYPD_Complaint_Data_Historic.csv", 
                           col_types = cols(CMPLNT_FR_DT = col_date(format = "%m/%d/%Y"), 
                                            CMPLNT_FR_TM = col_time(format = "%H:%M:%S"), 
                                            CMPLNT_TO_DT = col_date(format = "%m/%d/%Y"), 
                                            CMPLNT_TO_TM = col_time(format = "%H:%M:%S")))


complaint_hist <- complaint_hist %>%
  filter(CMPLNT_FR_DT > as.Date("2019-12-31"))

# shooting history
shooting_hist <- read_csv("r-data/nypd-crime/NYPD_Shootings_Data__Historic.csv", 
                                         col_types = cols(OCCUR_DATE = col_date(format = "%m/%d/%Y"), 
                                                          OCCUR_TIME = col_time(format = "%H:%M:%S")), 
                                         na = "null") %>%
  dplyr::mutate(PERP_RACE = dplyr::case_when(.data$PERP_RACE %in%
                                                c("BLACK HISPANIC", "WHITE HISPANIC") ~ "HISPANIC",
                                              TRUE ~ .data$PERP_RACE)) %>%
  dplyr::mutate(VIC_RACE = dplyr::case_when(.data$PERP_RACE %in%
                                               c("BLACK HISPANIC", "WHITE HISPANIC") ~ "HISPANIC",
                                             TRUE ~ .data$VIC_RACE))


if (!file.exists(".lintr")) {
  lintr::use_lintr(type = "tidyverse")
} else {
  message("lintr is already configured.")
}


.setup_complete <- TRUE

