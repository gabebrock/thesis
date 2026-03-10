library(tidyverse)
library(sf)
library(tidycensus)
library(blscrapeR)
library(fixest)

# initialize geospatial data available at the geojson format
nyc_geojson <- tempfile(fileext = ".pgeojson")

# download boro boundaries json file
download.file(
  "https://services5.arcgis.com/GfwWNkhOj9bNBqoJ/arcgis/rest/services/NYC_Borough_Boundary/FeatureServer/0/query?where=1=1&outFields=*&outSR=4326&f=pgeojson",
  nyc_geojson)

# read the nyc json file to a shapefile
nyc_sf <- sf::read_sf(nyc_geojson)

saveRDS(nyc_sf, file = "data/sf/nyc_sf.rds")

# initialize geospatial data available at the geojson format
nypd_geojson <- tempfile(fileext = ".pgeojson")

# download boro boundaries json file
download.file("https://services5.arcgis.com/GfwWNkhOj9bNBqoJ/arcgis/rest/services/NYC_Police_Precincts/FeatureServer/0/query?where=1=1&outFields=*&outSR=4326&f=pgeojson",
              nypd_geojson)

# read the nyc json file to a shapefile
nypd_sf <- sf::read_sf(nypd_geojson)

# format `Precinct` to match variable name in sqf dfs
nypd_sf$Precinct <- sprintf("%03d", as.integer(nypd_sf$Precinct))
nypd_sf$Precinct <- as.integer(nypd_sf$Precinct)

saveRDS(nypd_sf, file = "data/sf/nypd_sf.rds")

# NYC Census Tracts
download.file("https://services5.arcgis.com/GfwWNkhOj9bNBqoJ/arcgis/rest/services/NYC_Census_Tracts_for_2020_US_Census/FeatureServer/0/query?where=1=1&outFields=*&outSR=4326&f=pgeojson",
              tract_geojson <- tempfile(fileext = ".pgeojson"))

# read the nyc census tract json file to a shapefile
tracts_sf <- sf::read_sf(tract_geojson)

# NYC Neighborhood Tabulation Areas (NTAs)
download.file("https://services5.arcgis.com/GfwWNkhOj9bNBqoJ/arcgis/rest/services/NYC_Neighborhood_Tabulation_Areas_2020/FeatureServer/0/query?where=1=1&outFields=*&outSR=4326&f=pgeojson",
              nta_geojson <- tempfile(fileext = ".pgeojson"))

saveRDS(tracts_sf, file = "data/sf/tracts_sf.rds")

source("~/Projects/thesis/config/census.R")

pct_demo <- readRDS("data/pct_demo.rds")

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
