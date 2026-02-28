# initialize geospatial data available at the geojson format
street_geojson <- tempfile(fileext = ".pgeojson")

# download streets geojson file
download.file(
  "https://raw.githubusercontent.com/fillerwriter/nyc-streets/0e21d84982948076bd8f144bbf9c202f41b18e32/nyc-streets.geojson",
  street_geojson)

# read the streets geojson file to a shapefile
street_sf <- sf::read_sf(street_geojson)

# initialize geospatial data available at the geojson format
nta_geojson <- tempfile(fileext = ".pgeojson")

# download nta geojson file
download.file(
  "https://services5.arcgis.com/GfwWNkhOj9bNBqoJ/arcgis/rest/services/NYC_Neighborhood_Tabulation_Areas_2020/FeatureServer/0/query?where=1=1&outFields=*&outSR=4326&f=pgeojson",
  nta_geojson)

# read the streets geojson file to a shapefile
nta_sf <- sf::read_sf(nta_geojson)

# Filter to just Brownsville NTA
brownsville_nta <- nta_sf |> filter(grepl("Brownsville", NTAName, ignore.case = TRUE))

# Filter to just precinct 73
precinct_73 <- nypd_sf |> filter(Precinct == 73)

# Filter streets to those within precinct 73 and clip to precinct boundary
precinct_73_streets <- sf::st_intersection(street_sf, precinct_73)

# Plot the separate sf objects
ggplot() +
  geom_sf(data = precinct_73_streets, color = "gray", fill = NA) +
  geom_sf(data = sqf_ols |> filter(STOP_LOCATION_PRECINCT == 73), 
          color = "red", alpha = 0.5, size = 0.3) +
  geom_sf(data = precinct_73, color = "black", fill = NA, size = 0.3) +
  geom_sf(data = brownsville_nta, color = "blue", fill = NA) +
  theme_void()

# Create interactive map with leaflet
library(leaflet)

leaflet() %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addPolygons(data = precinct_73, color = "black", fill = FALSE, weight = 2) %>%
  addPolygons(data = brownsville_nta, color = "blue", fill = FALSE, weight = 2) %>%
  addPolylines(data = precinct_73_streets, color = "gray", weight = 1) %>%
  addCircleMarkers(data = sqf_ols |> filter(STOP_LOCATION_PRECINCT == 73), 
                   color = "red", radius = 1, opacity = 0.7) %>%
  fitBounds(lng1 = -73.95, lat1 = 40.65, lng2 = -73.85, lat2 = 40.70)
