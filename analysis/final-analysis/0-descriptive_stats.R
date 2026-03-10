library(dplyr)
library(sf)
library(ggplot2)


# --- Black population share per precinct (averaged across years) ----
p0_pct <- demo_pct %>%
  dplyr::group_by(pct) %>%
  dplyr::summarise(p0 = mean(pct_black, na.rm = TRUE), .groups = "drop")

# --- Black stop share per precinct ----
stop_race <- sqf_all %>%
  dplyr::filter(!is.na(SUSPECT_RACE_DESCRIPTION), !is.na(STOP_LOCATION_PRECINCT)) %>%
  dplyr::mutate(is_black = as.integer(SUSPECT_RACE_DESCRIPTION == "BLACK")) %>%
  dplyr::group_by(pct = STOP_LOCATION_PRECINCT) %>%
  dplyr::summarise(
    n_stops       = dplyr::n(),
    n_black_stops = sum(is_black, na.rm = TRUE),
    p_hat         = n_black_stops / n_stops,
    .groups = "drop"
  )

# --- z-statistic: H1: p_hat > p0 ----
disparity <- stop_race %>%
  dplyr::left_join(p0_pct, by = "pct") %>%
  dplyr::mutate(
    z        = (p_hat - p0) / sqrt(p0 * (1 - p0) / n_stops),
    sig_05   = z > 1.645,
    disparity_raw = p_hat - p0
  )

# --- join to precinct geometries ----
pct_map <- nypd_sf %>%
  dplyr::left_join(disparity, by = c("precinct" = "pct")) %>%
  sf::st_transform(crs = 3857)

borough_outline <- nyc_sf %>%
  sf::st_transform(crs = 3857)

# shared theme
map_theme <- theme_void(base_size = 11) +
  theme(
    plot.title      = element_text(face = "bold", hjust = 0.5, size = 13),
    plot.subtitle   = element_text(hjust = 0.5, color = "grey35", size = 9, margin = margin(t = 3, b = 6)),
    plot.caption    = element_text(hjust = 0, color = "grey50", size = 7.5, margin = margin(t = 8)),
    plot.margin     = margin(10, 10, 10, 10),
    legend.position        = "inside",
    legend.position.inside = c(0.18, 0.72),
    legend.direction = "vertical",
    legend.key.width  = unit(0.4, "cm"),
    legend.key.height = unit(0.5, "cm"),
    legend.title    = element_text(size = 6.5, vjust = 1),
    legend.text     = element_text(size = 6),
    legend.background = element_rect(fill = alpha("white", 0.7), color = NA)
  )

# --- map: raw disparity (p_hat - p0) ----
#' Sequential palette (all values positive); non-significant precincts hatched.
#' Percentage-point gap: Black share of stops vs. Black share of resident population
ggplot() +
  geom_sf(data = pct_map, aes(fill = disparity_raw), color = "white", linewidth = 0.15) +
  # crosshatch non-significant precincts
  geom_sf(
    data = dplyr::filter(pct_map, !sig_05),
    fill = NA, color = "black", linewidth = 0.6, linetype = "dashed"
  ) +
  geom_sf(data = borough_outline, fill = NA, color = "black", linewidth = 0.55) +
  scico::scale_fill_scico(
    palette   = "vik",
    direction = -1,
    na.value = "grey80",
    name     = "Disparity\n(% points)",
    labels   = scales::percent_format(accuracy = 1),
    guide    = guide_colorbar(title.position = "top", title.hjust = 0.5)
  ) +
  map_theme

ggsave("figures/fig_disparity_map_raw.png", width = 5, height = 5, dpi = 300)

# --- map: z-statistic (log-scaled for readability) ----
ggplot() +
  geom_sf(data = pct_map, aes(fill = log1p(z)), color = "white", linewidth = 0.15) +
  geom_sf(
    data = dplyr::filter(pct_map, !sig_05),
    fill = NA, color = "black", linewidth = 0.6, linetype = "dashed"
  ) +
  geom_sf(data = borough_outline, fill = NA, color = "black", linewidth = 0.55) +
  scico::scale_fill_scico(
    palette   = "vik",
    direction = -1,
    name      = "Z-statistic\n(log scale)",
    na.value  = "grey80",
    breaks    = log1p(c(1, 5, 10, 25, 50)),
    labels    = c("1", "5", "10", "25", "50"),
    guide     = guide_colorbar(title.position = "top", title.hjust = 0.5)
  ) +
  map_theme

ggsave("figures/fig_disparity_map_z.png", width = 5, height = 5, dpi = 300)

ggplot() +
  geom_sf(data = pct_map, aes(fill = z), color = "white", linewidth = 0.15) +
  geom_sf(
    data = dplyr::filter(pct_map, !sig_05),
    fill = NA, color = "black", linewidth = 0.6, linetype = "dashed"
  ) +
  geom_sf(data = borough_outline, fill = NA, color = "black", linewidth = 0.55) +
  scico::scale_fill_scico(
    palette   = "vik",
    direction = -1,
    name      = "Z-statistic",
    na.value  = "grey80",
    breaks    = c(1, 5, 10, 25, 50),
    labels    = c("1", "5", "10", "25", "50"),
    guide     = guide_colorbar(title.position = "top", title.hjust = 0.5)
  ) +
  map_theme
