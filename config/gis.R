
# grab boro boundaries geoJSON file from NYC Planning website
# Borough Boundaries (Clipped to Shoreline)
# Date of Data: May 2025
# https://www.nyc.gov/content/planning/pages/resources/datasets/borough-boundaries

# initialize geospatial data available at the geojson format
nyc_geojson <- tempfile(fileext = ".pgeojson")

# download boro boundaries json file
download.file(
  "https://services5.arcgis.com/GfwWNkhOj9bNBqoJ/arcgis/rest/services/NYC_Borough_Boundary/FeatureServer/0/query?where=1=1&outFields=*&outSR=4326&f=pgeojson",
  nyc_geojson)

# read the nyc json file to a shapefile
nyc_sf <- read_sf(nyc_geojson)

# plot the NYC map
nycMAP <- ggplot(nyc_sf) +
  geom_sf(fill = "white", color = "black", linewidth = 0.3) +
  theme_void()

# Mapping Stops to NYC map
# convert coordinates from sqf data to sf object
sqf_hist <- sqf_hist |>
  drop_na(STOP_LOCATION_X, STOP_LOCATION_Y) |>
  st_as_sf(coords = c("STOP_LOCATION_X", "STOP_LOCATION_Y"), crs = 2263) |>
  st_transform(crs = 4326)

# dot stop map
nycMAP <- ggplot(nyc_sf) +
  geom_sf(fill = "white", color = "black", linewidth = 0.3) +
  geom_sf(
    data = sqf_hist,
    aes(geometry = geometry, color = SUSPECT_RACE_DESCRIPTION),
    size = 0.25,
    alpha = 0.4) +
  coord_sf(xlim = c(-74.3, -73.65), ylim = c(40.48, 40.95)) +
  theme_void() +
  scale_color_viridis_d(option = "viridis") +
  labs(color = "Suspect Race or Ethnicity") +
  theme(
    legend.key = element_rect(fill = NA, color = NA), legend.key.size = unit(1.0, "cm"),
    legend.title = element_text(size = 8), legend.text = element_text(size = 7),                     
    legend.spacing.y = unit(0.2, "cm"), legend.box.spacing = unit(0.3, "cm")
  ) +
  guides(color = guide_legend(override.aes = list(shape = 15, size = 4)))


# Create map of NYPD Precincts

# grab precinct boundaries geoJSON file from NYC Planning website
# Police Precincts (Clipped to Shoreline)
# Date of Data: May 2025
# https://www.nyc.gov/content/planning/pages/resources/datasets/police-precincts

# initialize geospatial data available at the geojson format
nypd_geojson <- tempfile(fileext = ".pgeojson")

# download boro boundaries json file
download.file("https://services5.arcgis.com/GfwWNkhOj9bNBqoJ/arcgis/rest/services/NYC_Police_Precincts/FeatureServer/0/query?where=1=1&outFields=*&outSR=4326&f=pgeojson",
              nypd_geojson)

# read the nyc json file to a shapefile
nypd_sf <- read_sf(nypd_geojson)

# format `Precinct` to match variable name in sqf dfs
nypd_sf$Precinct <- sprintf("%03d", as.integer(nypd_sf$Precinct))
nypd_sf$Precinct <- as.character(as.integer(nypd_sf$Precinct))

# plot the NYPD map
nypdMAP <- ggplot(nypd_sf) +
  geom_sf(fill = "white", color = "black", linewidth = 0.3) +
  theme_void()


# NYC Census Tracts
download.file("https://services5.arcgis.com/GfwWNkhOj9bNBqoJ/arcgis/rest/services/NYC_Census_Tracts_for_2020_US_Census/FeatureServer/0/query?where=1=1&outFields=*&outSR=4326&f=pgeojson",
              tract_geojson <- tempfile(fileext = ".pgeojson"))

# read the nyc census tract json file to a shapefile
tracts_sf <- read_sf(tract_geojson)


# NYC Neighborhood Tabulation Areas (NTAs)
download.file("https://services5.arcgis.com/GfwWNkhOj9bNBqoJ/arcgis/rest/services/NYC_Neighborhood_Tabulation_Areas_2020/FeatureServer/0/query?where=1=1&outFields=*&outSR=4326&f=pgeojson",
              nta_geojson <- tempfile(fileext = ".pgeojson"))

# read the nyc census tract json file to a shapefile
nta_sf <- read_sf(nta_geojson)

rm("nyc_geojson", "nypd_geojson", "tract_geojson", "nta_geojson")

.setup_complete_gis <- TRUE