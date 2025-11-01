
# since the Quarto docs may try to install a package during rendering
# setup CRAN mirror globally, so R knows which CRAN repository to use 
options(repos = c(CRAN = "https://cloud.r-project.org"))


# list of required packages
required_packages <- c("tidyverse", "readxl", "mapview", "sf", "tinytext", "textdata", "kableExtra")

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

# load 2024 sqf data
# treating (null) values as NA values, for simplicity in the analysis. note that where NA appears in the data, it is because the officer there was no data for that fields not that, it was not applicable to the situation.
sqf2024 <- read_excel("r-data/nypd-stop/sqf-2024.xlsx", na = "(null)")
sqf2023 <- read_excel("r-data/nypd-stop/sqf-2023.xlsx", na = "(null)")
sqf2022 <- read_excel("r-data/nypd-stop/sqf-2022.xlsx", na = "(null)")
sqf2021 <- read_excel("r-data/nypd-stop/sqf-2021.xlsx", na = "(null)")
sqf2020 <- read_excel("r-data/nypd-stop/sqf-2020.xlsx", na = "(null)")

sqf_years <- list(sqf2020, sqf2021, sqf2022, sqf2023, sqf2024)

sqf_hist <- sqf_years %>%
  lapply(function(df) df %>%
           mutate(
             STOP_FRISK_DATE = as.Date(STOP_FRISK_DATE),
             STOP_FRISK_TIME = as.character(STOP_FRISK_TIME),
             ISSUING_OFFICER_COMMAND_CODE = as.character(ISSUING_OFFICER_COMMAND_CODE),
             SUPERVISING_OFFICER_COMMAND_CODE = as.character(SUPERVISING_OFFICER_COMMAND_CODE),
             SUSPECT_REPORTED_AGE = as.double(SUSPECT_REPORTED_AGE),
             SUSPECT_WEIGHT = as.double(SUSPECT_WEIGHT),
             SUSPECT_HEIGHT = as.double(SUSPECT_HEIGHT),
             STOP_LOCATION_PRECINCT = as.double(STOP_LOCATION_PRECINCT),
             STOP_LOCATION_X = as.double(STOP_LOCATION_X),
             STOP_LOCATION_Y = as.double(STOP_LOCATION_Y)
           )) %>%
  bind_rows()

# quick text for Black, White, and Hispanic stops
BWH <- c("BLACK", "WHITE", "HISPANIC")

sqf_hist <- sqf_hist %>%
  filter(!is.na(SUSPECT_RACE_DESCRIPTION)) %>%
  mutate(SUSPECT_RACE_DESCRIPTION = case_when(SUSPECT_RACE_DESCRIPTION %in% 
                                                c("BLACK HISPANIC", "WHITE HISPANIC") ~ "HISPANIC",
                                              TRUE ~ SUSPECT_RACE_DESCRIPTION))

# have months sort chronologically, not alphabetically
sqf_hist <- sqf_hist %>%
  mutate(MONTH2 = factor(MONTH2,
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
  mutate(PERP_RACE = case_when(PERP_RACE %in% 
                                                c("BLACK HISPANIC", "WHITE HISPANIC") ~ "HISPANIC",
                                              TRUE ~ PERP_RACE))



.setup_complete <- TRUE
