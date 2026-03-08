library(tidycensus)

# --- helper: download ArcGIS FeatureServer layer to sf object ----
read_arcgis_sf <- function(url) {
  tmp <- tempfile(fileext = ".pgeojson")
  on.exit(unlink(tmp))
  download.file(url, tmp, quiet = TRUE)
  sf::read_sf(tmp)
}


# --- NYC borough boundaries (clipped to shoreline, May 2025) ----
# https://www.nyc.gov/content/planning/pages/resources/datasets/borough-boundaries
nyc_sf <- read_arcgis_sf(
  "https://services5.arcgis.com/GfwWNkhOj9bNBqoJ/arcgis/rest/services/NYC_Borough_Boundary/FeatureServer/0/query?where=1=1&outFields=*&outSR=4326&f=pgeojson"
)
saveRDS(nyc_sf, file = "data/data-final/census-gis/nyc_sf.rds")


# --- NYPD precinct boundaries (clipped to shoreline, May 2025) ----
# https://www.nyc.gov/content/planning/pages/resources/datasets/police-precincts
nypd_sf <- read_arcgis_sf(
  "https://services5.arcgis.com/GfwWNkhOj9bNBqoJ/arcgis/rest/services/NYC_Police_Precincts/FeatureServer/0/query?where=1=1&outFields=*&outSR=4326&f=pgeojson"
)
# Precinct as integer to match STOP_LOCATION_PRECINCT in sqf_all
nypd_sf <- nypd_sf %>%
  dplyr::mutate(Precinct = as.integer(Precinct))
saveRDS(nypd_sf, file = "data/data-final/census-gis/nypd_sf.rds")


# --- NYC Census Tracts (2020) ----
tracts_sf <- read_arcgis_sf(
  "https://services5.arcgis.com/GfwWNkhOj9bNBqoJ/arcgis/rest/services/NYC_Census_Tracts_for_2020_US_Census/FeatureServer/0/query?where=1=1&outFields=*&outSR=4326&f=pgeojson"
)
saveRDS(tracts_sf, file = "data/data-final/census-gis/ttracts_sf.rds")


# --- NYC Neighborhood Tabulation Areas (2020) ----
nta_sf <- read_arcgis_sf(
  "https://services5.arcgis.com/GfwWNkhOj9bNBqoJ/arcgis/rest/services/NYC_Neighborhood_Tabulation_Areas_2020/FeatureServer/0/query?where=1=1&outFields=*&outSR=4326&f=pgeojson"
)
saveRDS(nta_sf, file = "data/data-final/census-gis/nta_sf.rds")


# --- base precinct map ----
nypdMAP <- ggplot(nypd_sf) +
  geom_sf(fill = "white", color = "black", linewidth = 0.3) +
  theme_void()


.setup_complete_gis <- TRUE