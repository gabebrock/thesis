
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


# --- convert sqf_all stop coordinates to sf (non-destructive) ----
#' Keep sqf_all intact; create a separate sf object for spatial operations.
sqf_sf <- sqf_all %>%
  tidyr::drop_na(STOP_LOCATION_X, STOP_LOCATION_Y) %>%
  sf::st_as_sf(coords = c("STOP_LOCATION_X", "STOP_LOCATION_Y"), crs = 2263) %>%
  sf::st_transform(crs = 4326)


# --- dot map: stops by race ----
nycMAP_stops <- ggplot(nyc_sf) +
  geom_sf(fill = "white", color = "black", linewidth = 0.3) +
  geom_sf(
    data = sqf_sf,
    aes(color = SUSPECT_RACE_DESCRIPTION),
    size = 0.25, alpha = 0.4
  ) +
  coord_sf(xlim = c(-74.3, -73.65), ylim = c(40.48, 40.95)) +
  scale_color_viridis_d(option = "viridis") +
  labs(color = "Suspect Race or Ethnicity") +
  theme_void() +
  theme(
    legend.key       = element_rect(fill = NA, color = NA),
    legend.key.size  = unit(1.0, "cm"),
    legend.title     = element_text(size = 8),
    legend.text      = element_text(size = 7),
    legend.spacing.y = unit(0.2, "cm"),
    legend.box.spacing = unit(0.3, "cm")
  ) +
  guides(color = guide_legend(override.aes = list(shape = 15, size = 4)))


# --- base precinct map ----
nypdMAP <- ggplot(nypd_sf) +
  geom_sf(fill = "white", color = "black", linewidth = 0.3) +
  theme_void()


.setup_complete_gis <- TRUE