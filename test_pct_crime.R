
#' from https://data.cityofnewyork.us/Public-Safety/NYPD-Complaint-Data-Historic/qgea-i56i/data_preview
#' but I actually got it from Kaggle bc the NYPD data would not download
#' https://data.cityofnewyork.us/Public-Safety/NYPD-Complaint-Data-Historic/qgea-i56i/data_preview
#'
#' if crime dataset doesn't already exist
if (!exists("test_pct_crime")) {
  
  #' the data is split into two files, 
  #' one with data 2006 to 2019 and one with data 2020 to 2024
  
  crime_data_historic_to2019 <- read_csv(
    "data/nypd-crime/NYPD_Complaint_Data_Historic.csv",
    col_types = cols(.default = col_character()) # treat all vars as chars for bind
  )
  
  crime_data_historic_to2024 <- read_csv(
    "data/nypd-crime/NYPD_Complaint_Data_Historic_1.csv",
    col_types = cols(.default = col_character()) # treat all vars as chars for bind
  )
  
  # bind complaint data together, 2006 to 2024
  test_pct_crime <- bind_rows(crime_data_historic_to2019, 
                              crime_data_historic_to2024)
}

# list of required variables for OLS models
req_vars <- c("pct", "boro", "year", "month",
              "felony", "misdemeanor", "violation",
              "crime_Violent", "crime_Weapons", "crime_Property", "crime_Drug", 
              "crime_Trespass", "crime_QualityOfLife", "crime_Other")

# ---- time variables ----
#' create `year` and `month` variables
#' lubridate is doing something weird with the dates, 
#' check to make sure the dates are just formatted weird 
weird_dates <- test_pct_crime %>% 
  arrange(CMPLNT_FR_DT) %>% 
  head(150) %>% 
  select(CMPLNT_NUM, CMPLNT_FR_DT)

#' pull the complaint numbers for the weird dates so 
#' I can check them in the original data frame
weird_dates_nums <- weird_dates %>% pull(CMPLNT_NUM)

#' check the original data frame for those complaint numbers to see if 
#' the dates are just formatted weirdly or if they are actually wrong
test_pct_crime %>%
  filter(CMPLNT_NUM %in% weird_dates_nums) %>%
  select(CMPLNT_NUM, CMPLNT_FR_DT, CMPLNT_TO_DT) %>%
  print(n = 150)

#' lubridate dates, 
#' if `CMPLNT_FR_DT` is more than 3 days before `CMPLNT_TO_DT`,
#' then use `CMPLNT_TO_DT` as the year/month, otherwise use `CMPLNT_FR_DT`
test_pct_crime <- test_pct_crime %>%
  mutate(
    CMPLNT_FR_DT = mdy(CMPLNT_FR_DT),
    CMPLNT_TO_DT = mdy(CMPLNT_TO_DT)
  )

#' create `year` and `month` variables
test_pct_crime <- test_pct_crime %>%
  mutate(
    final_date = if_else(!is.na(CMPLNT_TO_DT) # if start is is not empty
                         & (CMPLNT_TO_DT - CMPLNT_FR_DT) > ddays(3), # and if end date is more than 3 days after start date
                         CMPLNT_TO_DT, # then use end date
                         CMPLNT_FR_DT), # else use start date
    year  = as.integer(year(final_date)), # lube year from final date
    month = as.integer(month(final_date)) # lube month from final date
  )

#' create `pct` and `boro` variables
test_pct_crime <- test_pct_crime %>%
  mutate(pct = as.numeric(ADDR_PCT_CD),
         boro = BORO_NM
  )

# ---- offense categories ----
#' summary table of offenses
test_pct_crime %>%
  group_by(LAW_CAT_CD) %>%
  summarize(n = n())

#' create `felony`, `misdemeanor`, and `violation` variables
test_pct_crime <- test_pct_crime %>%
  mutate(felony = ifelse(LAW_CAT_CD == "FELONY", 1, 0),
         misdemeanor = ifelse(LAW_CAT_CD == "MISDEMEANOR", 1, 0),
         violation = ifelse(LAW_CAT_CD == "VIOLATION", 1, 0))

# list all unique offense descriptions in the data
sort(unique(test_pct_crime$OFNS_DESC))

#' categorize crimes into broad offense categories
test_pct_crime <- test_pct_crime %>%
  mutate(
    off_cat_broad = case_when(
      
      ## ── Violent ──────────────────────────
      str_detect(PD_DESC, 
                 "ASSAULT|AGGRAVATED|ROBBERY|KIDNAPPING|MENACING|STRANGULATION|
                  RECKLESS ENDANGERMENT|COERCION|IMPRISONMENT|HARASSMENT|
                  OBSTR BREATH|TERRORISTIC|RIOT|HOMICIDE|RAPE|SEX|
                 TERRORISM|TORTURE") ~ "Violent",
      
      ## ── Sex crimes (kept in Other per your rule) ──────
      str_detect(PD_DESC, 
                 "SODOMY|INCEST|
                  LEWD|SEX TRAFFICKING") ~ "Other",
      
      ## ── Weapons ───────────────────────────────────────
      str_detect(PD_DESC, 
                 "WEAPON|FIREARM|PISTOL|RIFLE|KNIFE|
                  CRIMINAL POSSESSION WEAPON|
                  UNLAWFUL POSS. WEAPON|WEAP|
                  WEAPONS") ~ "Weapons",
      
      ## ── Drug ──────────────────────────────────────────
      str_detect(PD_DESC, 
                 "CONTROLLED SUBSTANCE|CANNABIS|MARIJUANA|
                  DRUG|PARAPHERNALIA|
                  SALE SCHOOL GROUNDS|
                  UNDER THE INFLUENCE") ~ "Drug",
      
      ## ── Property ──────────────────────────────────────
      str_detect(PD_DESC, 
                 "LARCENY|BURGLARS|BURGLARY|ARSON|MISCHIEF|STOLEN|
                  FORGERY|FRAUD|THEFT|TAMPERING|
                  GRAND|PETIT|JOSTLING|
                  UNAUTHORIZED USE VEHICLE|
                  THEFT OF SERVICES") ~ "Property",
      
      ## ── Trespass ──────────────────────────────────────
      str_detect(PD_DESC, "TRESPASS") ~ "Trespass",
      
      ## ── Quality of Life / Disorder ────────────────────
      str_detect(PD_DESC,
                 "ANARCHY|ASSEMBLY|CHILD|DISORDERLY|LOITERING|ALCOHOLIC|
                  EDUCATION|FIREWORKS|INTOXICATED|IMPAIRED DRIVING|
                  NOISE|GAMBLING|GRAFFITI|PEDDLING|NUISANCE|
                 ADM.CODE|HEALTH CODE|LOITERING|OBSCENITY|
                 PUBLIC ADMINISTRATION|
                 TRAFFIC|PARKR&R|PROSTITUTION|
                 FIREWORKS|GYPSY CAB|
                 SMOKING") ~ "QualityOfLife",
      
      TRUE ~ "Other"
    ),
    
    off_cat_broad = factor(
      off_cat_broad,
      levels = c(
        "Murder", "Violent", "Weapons",
        "Property", "Drug", "Trespass",
        "QualityOfLife", "Other"
      )
    )
  )

# offense category frequency table
test_pct_crime %>%
  count(off_cat_broad) %>%
  mutate(
    percent = n / sum(n) * 100
  ) %>%
  arrange(desc(n))

#' list offenses being categorized as "Other"
test_pct_crime %>%
  filter(off_cat_broad == "Other") %>%
  distinct(PD_DESC) %>%
  arrange(PD_DESC) %>%
  print(n = Inf)

#' summarize crime data for each `pct`, `year`, and `month.`
#' calculate the total number of `felony`, `misdemeanor`, and `violation` offenses,
#' as well as the count of each broad offense category for each month.
test_pct_crime_month <- test_pct_crime %>% 
  # Group the data by precinct, year, and month.
  group_by(pct, year, month) %>%
  # calculate the total number of felony, misdemeanor, and violation offenses.
  summarize(
    felony = sum(felony, na.rm = TRUE),
    misdemeanor = sum(misdemeanor, na.rm = TRUE),
    violation = sum(violation, na.rm = TRUE),
    # calculate the count of each broad offense category.
    crime_Violent = sum(off_cat_broad == "Violent", na.rm = TRUE),
    crime_Weapons = sum(off_cat_broad == "Weapons", na.rm = TRUE),
    crime_Property = sum(off_cat_broad == "Property", na.rm = TRUE),
    crime_Drug = sum(off_cat_broad == "Drug", na.rm = TRUE),
    crime_Trespass = sum(off_cat_broad == "Trespass", na.rm = TRUE),
    crime_QualityOfLife = sum(off_cat_broad == "QualityOfLife", na.rm = TRUE),
    crime_Other = sum(off_cat_broad == "Other", na.rm = TRUE)
  ) %>%
  # remove the grouping variable
  ungroup()

test_pct_crime_month <- test_pct_crime_month %>%
  filter(year >= 2009 & year <= 2024,
         pct >= 1 & pct <= 123)

# ----- finish prep for OLS models ----

#' divide yearly demographic data into monthly data 
#' by uncounting each row 12 times and creating a `month` variable
if (!exists("pct_demo_expanded")) {
  pct_demo_month <- pct_demo %>% 
    uncount(12) %>% 
    group_by(pct, year) %>% 
    mutate(month = row_number()) %>% 
    ungroup()
  
  pct_demo_expanded <- TRUE
}

#' join total pop from pct_demo for density calculations
test_pct_crime_month <- test_pct_crime_month %>%
  mutate(year  = as.integer(year),
         month = as.integer(month)) 

test_pct_crime_month <- test_pct_crime_month %>%
  left_join(pct_demo_month, by = c("pct", "year", "month"))

# 
test_pct_crime_month <- test_pct_crime_month %>%
  mutate(year_month = format(as.Date(paste0(year, "-", month, "-01")), "%Y-%m")) %>%
  mutate(nonviolent_crime = crime_Weapons + crime_Property + crime_Drug + 
                            crime_Trespass + crime_QualityOfLife + crime_Other) %>%
  mutate(violent_rate    = log((crime_Violent + 1) / total_pop * 1000),
         nonviolent_rate = log((nonviolent_crime + 1) / total_pop * 1000)) %>%
  mutate(date = as.Date(paste0(year_month, "-01")))


library(blscrapeR)
library(dplyr)

county_fips <- c(
  Bronx = "36005",
  Brooklyn = "36047",
  Manhattan = "36061",
  Queens = "36081",
  `Staten Island` = "36085"
)

get_monthly_unemp <- function(fips) {
  series_id <- paste0("LAUCN", fips, "0000000003") # monthly unemployment rate
  bls_api(series_id,
          startyear = 2009,
          endyear = 2024,
          registrationKey = Sys.getenv("BLS_KEY")) %>%
    mutate(
      year = as.integer(year),
      month = match(periodName, month.name),  # converts "January" → 1, etc.
      unemployment_rate = as.numeric(value),
      county_fips = fips
    ) %>%
    select(year, month, unemployment_rate, county_fips)
}

nyc_unemp_month <- map_dfr(county_fips, get_monthly_unemp)

boro_fips <- tibble(
  BoroName = c("Bronx", "Brooklyn", "Manhattan", "Queens", "Staten Island"),
  county_fips = c("36005", "36047", "36061", "36081", "36085")
)

nyc_unemp_month <- nyc_unemp_month %>%
  left_join(boro_fips, by = "county_fips") %>%
  select(year, month, unemployment_rate, BoroName)

test_pct_crime_month <- test_pct_crime_month %>%
  left_join(nyc_unemp_month, by = c("year", "month", "BoroName")) %>%
  rename(unemp_rate = unemployment_rate)

#' add precinct area (derived from sf geometries) and build the final monthly panel.
#'
#' Note: Areas are computed once per precinct; we join those scalar values into
#' the monthly panel to compute population density.

  #' join with precinct geometry from nypd_sf - but don't convert to sf yet
  test_pct_crime_month <- test_pct_crime_month %>%
    left_join(st_drop_geometry(nypd_sf) %>% select(Precinct), by = c("pct" = "Precinct"))
  
  # calculate area for each precinct separately (no sf joins)
  pct_areas <- nypd_sf %>%
    filter(st_is_valid(geometry)) %>%  # Filter out invalid geometries
    mutate(
      area_sq_meters = as.numeric(st_area(geometry)),
      area_sq_miles = area_sq_meters * 3.861e-7
    ) %>%
    select(Precinct, area_sq_meters, area_sq_miles) %>%
    st_drop_geometry()
  
      cat("pct_month is sf:", inherits(pct_month, "sf"), "\n")
      cat("test_pct_crime_month is sf:", inherits(test_pct_crime_month, "sf"), "\n") 
      cat("pct_demo is sf:", inherits(pct_demo, "sf"), "\n")
      
# join with stops data
  test_pct_month_full <- st_drop_geometry(pct_month) %>%
    left_join(test_pct_crime_month, by = c("pct", "year_month")) %>%
    mutate(
      pct_black      = black_pop / total_pop,
      pct_white      = white_pop / total_pop,
      pct_hisp       = hisp_pop / total_pop,
      pct_18_24      = age_18_24_pop / total_pop,
      log_stops_rate = log((stops + 1) / total_pop * 1000),
      lag_unemp = dplyr::lag(unemp_rate, 1)
    ) %>%
    filter(!is.na(violent_rate), !is.na(pct_black)) %>%
    left_join(pct_areas, by = c("pct" = "Precinct")) %>%
    mutate(pop_density = total_pop / area_sq_miles)

  # Sort by precinct and date for proper lagging
  test_pct_month_full <- test_pct_month_full %>%
    mutate(date = date.x,
           month = month.x,
           year = year.x) %>%
    select(-date.x, -date.y, -month.x, -month.y, -year.x, -year.y) %>%
    arrange(pct, date)

# Create lagged variables (1-month lags, within precinct)
  
  # Create one-month lagged crime rates by precinct
  test_pct_month_full <- test_pct_month_full %>%
    group_by(pct) %>%
    arrange(date) %>%
    mutate(
      # One-month lagged crime rates
      lag_violent_rate = dplyr::lag(violent_rate, 1),
      lag_nonviolent_rate = dplyr::lag(nonviolent_rate, 1),
      
      # Also create lagged stops to capture persistence in police allocation
      lag_stops = dplyr::lag(stops, 1),
      lag_log_stops = dplyr::lag(log_stops, 1)
    ) %>%
    ungroup() 
  
  test_pct_month_full_lagged <- test_pct_month_full %>%
    # Remove first month for each precinct due to missing lag
    filter(!is.na(lag_violent_rate))
  
  # Summary statistics
  cat("Monthly observations with lagged variables:", nrow(test_pct_month_full_lagged), "\n")
  cat("Number of precincts:", length(unique(test_pct_month_full_lagged$pct)), "\n")
  cat("Date range:", min(test_pct_month_full_lagged$year_month), "to", max(test_pct_month_full_lagged$year_month), "\n")

  
saveRDS(test_pct_month_full_lagged, file = "data/test_pct_month_full_lagged.rds")

race_monthly_counts <- sqf_ols %>%
  group_by(year, month, pct) %>%
  summarize(
    stops_black = sum(race == "BLACK", na.rm = TRUE),
    stops_hisp = sum(race == "HISPANIC", na.rm = TRUE),
    stops_white = sum(race == "WHITE", na.rm = TRUE)
  ) %>%
  ungroup()

# Create logged versions of these counts (log(count + 1) to avoid log(0))
race_monthly_counts <- race_monthly_counts %>%
  mutate(
    log_stops_black = log(stops_black + 1),
    log_stops_hisp = log(stops_hisp + 1),
    log_stops_white = log(stops_white + 1)
  )

test_pct_month_full_lagged <- test_pct_month_full_lagged %>%
  left_join(race_monthly_counts, by = c("year", "month", "pct"))
