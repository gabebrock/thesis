# Load and process real sqf_all data
library(dplyr)
library(ggplot2)
library(gridExtra)
library(scales)
library(lubridate)

# Process the real data
cat("Processing real sqf_all data...\n")

# Clean and prepare the data
data_clean <- sqf_all %>%
  # Filter for CPW stops and black/white suspects only
  filter(
    SUSPECTED_CRIME_DESCRIPTION == "CPW",
    SUSPECT_RACE_DESCRIPTION %in% c("BLACK", "WHITE")
  ) %>%
  # Create binary variables
  mutate(
    weapon_found = if_else(WEAPON_FOUND_FLAG == "Y", 1L, 0L, missing = 0L),
    race_clean = if_else(SUSPECT_RACE_DESCRIPTION == "BLACK", "BLACK", "WHITE"),
    precinct = as.factor(STOP_LOCATION_PRECINCT)
  ) %>%
  # Remove rows with missing key variables
  filter(!is.na(precinct), !is.na(weapon_found))

cat("CPW stops with black/white suspects:", nrow(data_clean), "\n")

# Calculate hit rates by precinct
precinct_stats <- data_clean %>%
  group_by(precinct) %>%
  summarize(
    n_stops = n(),
    n_weapons = sum(weapon_found),
    hit_rate = n_weapons / n_stops,
    
    # Black suspect stats
    n_black = sum(race_clean == "BLACK"),
    n_black_weapons = sum(weapon_found[race_clean == "BLACK"]),
    black_hit_rate = if_else(n_black > 0, n_black_weapons / n_black, NA_real_),
    
    # White suspect stats  
    n_white = sum(race_clean == "WHITE"),
    n_white_weapons = sum(weapon_found[race_clean == "WHITE"]),
    white_hit_rate = if_else(n_white > 0, n_white_weapons / n_white, NA_real_),
    
    # Demographic composition
    pct_white_stopped = n_white / (n_black + n_white),
    .groups = "drop"
  ) %>%
  # Filter out precincts with too few stops or missing demographics
  filter(
    n_stops >= 50,  # Minimum stops for reliable estimates
    !is.na(black_hit_rate),
    !is.na(white_hit_rate),
    !is.na(pct_white_stopped)
  )

cat("Precincts with sufficient data:", nrow(precinct_stats), "\n")

# Calculate overall averages for reference lines
overall_black_hit <- sum(data_clean$weapon_found[data_clean$race_clean == "BLACK"]) / 
                     sum(data_clean$race_clean == "BLACK")
overall_white_hit <- sum(data_clean$weapon_found[data_clean$race_clean == "WHITE"]) / 
                     sum(data_clean$race_clean == "WHITE")

cat("Overall black hit rate:", percent(overall_black_hit), "\n")
cat("Overall white hit rate:", percent(overall_white_hit), "\n")

# Panel (a): Hit rate comparison
panel_a <- ggplot(precinct_stats, aes(x = black_hit_rate, y = white_hit_rate)) +
  geom_point(aes(size = n_stops), alpha = 0.6, color = "steelblue") +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "gray50") +
  geom_hline(yintercept = overall_white_hit, linetype = "dashed", color = "red", alpha = 0.7) +
  geom_hline(yintercept = overall_black_hit, linetype = "dashed", color = "blue", alpha = 0.7) +
  scale_x_log10(labels = percent_format(accuracy = 1), limits = c(0.001, 0.5)) +
  scale_y_log10(labels = percent_format(accuracy = 1), limits = c(0.001, 0.5)) +
  scale_size_continuous(range = c(1, 8), name = "Number of stops") +
  labs(
    x = "Hit rate for black suspects",
    y = "Hit rate for white suspects",
    title = "(a) Within-area hit rate comparison"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.minor = element_blank(),
    legend.position = "bottom",
    plot.title = element_text(size = 12, face = "bold"),
    panel.background = element_rect(fill = "white"),
    plot.background = element_rect(fill = "white"),
    legend.background = element_rect(fill = "white")
  )

# Panel (b): Hit rate by percentage of white suspects
panel_b <- ggplot(precinct_stats, aes(x = pct_white_stopped, y = hit_rate)) +
  geom_point(aes(size = n_stops), alpha = 0.6, color = "steelblue") +
  geom_hline(yintercept = overall_white_hit, linetype = "dashed", color = "red", alpha = 0.7) +
  geom_hline(yintercept = overall_black_hit, linetype = "dashed", color = "blue", alpha = 0.7) +
  scale_x_log10(labels = percent_format(accuracy = 1), limits = c(0.001, 0.99)) +
  scale_y_log10(labels = percent_format(accuracy = 1), limits = c(0.001, 0.5)) +
  scale_size_continuous(range = c(1, 8), name = "Number of stops") +
  labs(
    x = "Percentage of stopped suspects who are white",
    y = "Hit rate",
    title = "(b) Hit rate by demographic composition"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.minor = element_blank(),
    legend.position = "bottom",
    plot.title = element_text(size = 12, face = "bold"),
    panel.background = element_rect(fill = "white"),
    plot.background = element_rect(fill = "white"),
    legend.background = element_rect(fill = "white")
  )

# Save panels as separate square images with white backgrounds
ggsave("images/panel_a_hit_rate_comparison.png", panel_a, width = 8, height = 8, dpi = 300)
ggsave("images/panel_b_demographic_composition.png", panel_b, width = 8, height = 8, dpi = 300)

# Also save combined version for reference
combined_plot <- grid.arrange(panel_a, panel_b, ncol = 2)
ggsave("hit_rate_analysis_combined.png", combined_plot, width = 16, height = 8, dpi = 300)

# Print summary statistics
cat("Summary statistics:\n")
cat("Overall black hit rate:", percent(overall_black_hit), "\n")
cat("Overall white hit rate:", percent(overall_white_hit), "\n")
cat("Correlation between % white stopped and hit rate:", cor(precinct_stats$pct_white_stopped, precinct_stats$hit_rate), "\n")
cat("Total stops analyzed:", sum(precinct_stats$n_stops), "\n")
cat("Number of precincts:", nrow(precinct_stats), "\n")
