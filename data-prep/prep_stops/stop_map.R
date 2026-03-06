# --- stops per 100 sq m density grid, 2009–2024 ----

# project to UTM zone 18N (meters) for area calculations
nyc_m <- sf::st_transform(nyc_sf, crs = 32618)

# 100 m × 100 m grid clipped to NYC boundary
grid_nyc <- sf::st_make_grid(nyc_m, cellsize = 100, square = TRUE) %>%
  sf::st_as_sf() %>%
  dplyr::mutate(cell_id = dplyr::row_number()) %>%
  dplyr::filter(
    sf::st_intersects(., sf::st_union(nyc_m), sparse = FALSE)[, 1]
  )

# all stops with coordinates, 2009–2024
sqf_sf_all <- sqf_all %>%
  dplyr::filter(YEAR2 >= 2009, YEAR2 <= 2024) %>%
  tidyr::drop_na(STOP_LOCATION_X, STOP_LOCATION_Y) %>%
  sf::st_as_sf(coords = c("STOP_LOCATION_X", "STOP_LOCATION_Y"), crs = 2263) %>%
  sf::st_transform(crs = 32618)

# count stops per cell × year; fill missing cell/year combos with 0
stop_counts_yr <- sf::st_join(sqf_sf_all, grid_nyc, join = sf::st_within) %>%
  sf::st_drop_geometry() %>%
  dplyr::count(cell_id, YEAR2, name = "n_stops") %>%
  tidyr::complete(
    cell_id = grid_nyc$cell_id,
    YEAR2   = 2009:2024,
    fill    = list(n_stops = 0L)
  )

# join geometry, compute density, bin
grid_density_yr <- grid_nyc %>%
  dplyr::left_join(stop_counts_yr, by = "cell_id") %>%
  dplyr::mutate(
    stops_per_100sqm = n_stops / (100 * 100) * 100,
    density_bin      = cut(
      stops_per_100sqm,
      breaks         = c(-Inf, 0, 0.01, 0.03, 0.07, Inf),
      labels         = c("[0,0]", "(0,.01]", "(.01,.03]", "(.03,.07]", "(.07+]"),
      right          = TRUE,
      include.lowest = TRUE
    )
  ) %>%
  sf::st_transform(crs = 4326)

nyc_4326 <- sf::st_transform(nyc_sf, 4326)

nycMAP_density_grid <- ggplot() +
  geom_sf(data = grid_density_yr, aes(fill = density_bin), color = NA) +
  geom_sf(data = nyc_4326, fill = NA, color = "grey40", linewidth = 0.2) +
  scale_fill_manual(
    values = c(
      "[0,0]"     = "#1a1aff",
      "(0,.01]"   = "#00cccc",
      "(.01,.03]" = "#00cc00",
      "(.03,.07]" = "#ffff00",
      "(.07+]"    = "#cc0000"
    ),
    name = "Per 100 m²"
  ) +
  facet_wrap(~ YEAR2, ncol = 4) +
  coord_sf(xlim = c(-74.3, -73.65), ylim = c(40.48, 40.95)) +
  labs(title = "NYPD Stop-and-Frisk Density (2009–2024)",
       subtitle = "100 m × 100 m grid cells") +
  theme_void() +
  theme(
    plot.title    = element_text(size = 12, face = "bold"),
    plot.subtitle = element_text(size = 9, color = "grey40"),
    strip.text    = element_text(size = 7, face = "bold", margin = margin(b = 2)),
    legend.title  = element_text(size = 8),
    legend.text   = element_text(size = 7),
    panel.spacing = unit(0.5, "lines")
  )

# --- convert sqf_all stop coordinates to sf (non-destructive) ----
#' Keep sqf_all intact; create a separate sf object for spatial operations.
sqf_sf <- sqf_all %>%
  tidyr::drop_na(STOP_LOCATION_X, STOP_LOCATION_Y) %>%
  sf::st_as_sf(coords = c("STOP_LOCATION_X", "STOP_LOCATION_Y"), crs = 2263) %>%
  sf::st_transform(crs = 4326)


# --- dot map: stops by race ----
nyc_sf %>%
  ggplot() +
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
