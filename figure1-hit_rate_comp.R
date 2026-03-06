library(dplyr)
library(ggplot2)

# -------------------------------------------------------------------
# 1.  Calculate hit rates
# -------------------------------------------------------------------

hit_rates <- sqf_ols %>%
  filter(SUSPECT_RACE_DESCRIPTION %in% c("BLACK", "WHITE")) %>%
  group_by(pct, SUSPECT_RACE_DESCRIPTION) %>%
  summarise(
    stops    = n(),
    hits     = sum(frisk == 1 & any_seiz == 1, na.rm = TRUE),
    hit_rate = hits / stops,
    .groups  = "drop"
  ) %>%
  filter(stops >= 10)

black_df <- hit_rates %>%
  filter(SUSPECT_RACE_DESCRIPTION == "BLACK") %>%
  select(pct, black_rate = hit_rate, black_stops = stops)

white_df <- hit_rates %>%
  filter(SUSPECT_RACE_DESCRIPTION == "WHITE") %>%
  select(pct, white_rate = hit_rate, white_stops = stops)

hit_wide <- inner_join(black_df, white_df, by = "pct") %>%
  mutate(total_stops = black_stops + white_stops)

borough_lookup <- tibble::tribble(
  ~pct, ~borough,
  1,"Manhattan", 5,"Manhattan", 6,"Manhattan", 7,"Manhattan", 9,"Manhattan",
  10,"Manhattan", 13,"Manhattan", 14,"Manhattan", 17,"Manhattan", 18,"Manhattan",
  19,"Manhattan", 20,"Manhattan", 22,"Manhattan", 23,"Manhattan", 24,"Manhattan",
  25,"Manhattan", 26,"Manhattan", 28,"Manhattan", 30,"Manhattan", 32,"Manhattan",
  33,"Manhattan", 34,"Manhattan",
  40,"Bronx", 41,"Bronx", 42,"Bronx", 43,"Bronx", 44,"Bronx", 45,"Bronx",
  46,"Bronx", 47,"Bronx", 48,"Bronx", 49,"Bronx", 50,"Bronx", 52,"Bronx",
  60,"Brooklyn", 61,"Brooklyn", 62,"Brooklyn", 63,"Brooklyn", 66,"Brooklyn",
  67,"Brooklyn", 68,"Brooklyn", 69,"Brooklyn", 70,"Brooklyn", 71,"Brooklyn",
  72,"Brooklyn", 73,"Brooklyn", 75,"Brooklyn", 76,"Brooklyn", 77,"Brooklyn",
  78,"Brooklyn", 79,"Brooklyn", 81,"Brooklyn", 83,"Brooklyn", 84,"Brooklyn",
  88,"Brooklyn", 90,"Brooklyn", 94,"Brooklyn",
  100,"Queens", 101,"Queens", 102,"Queens", 103,"Queens", 104,"Queens",
  105,"Queens", 106,"Queens", 107,"Queens", 108,"Queens", 109,"Queens",
  110,"Queens", 111,"Queens", 112,"Queens", 113,"Queens", 114,"Queens", 115,"Queens",
  120,"Staten Island", 121,"Staten Island", 122,"Staten Island", 123,"Staten Island"
)

hit_wide <- hit_wide %>% left_join(borough_lookup, by = "pct")

# -------------------------------------------------------------------
# 2.  Overall hit rates for reference lines
# -------------------------------------------------------------------

overall_black <- sqf_ols %>%
  filter(SUSPECT_RACE_DESCRIPTION == "BLACK") %>%
  summarise(r = sum(frisk == 1 & any_seiz == 1, na.rm = TRUE) / n()) %>%
  pull(r)

overall_white <- sqf_ols %>%
  filter(SUSPECT_RACE_DESCRIPTION == "WHITE") %>%
  summarise(r = sum(frisk == 1 & any_seiz == 1, na.rm = TRUE) / n()) %>%
  pull(r)

# -------------------------------------------------------------------
# 3.  Plot
# -------------------------------------------------------------------

p <- ggplot(hit_wide, aes(x = black_rate, y = white_rate, size = total_stops)) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "grey40", linewidth = 0.5) +
  geom_hline(yintercept = overall_white, linetype = "dotted", color = "grey40", linewidth = 0.5) +
  geom_vline(xintercept = overall_black, linetype = "dotted", color = "grey40", linewidth = 0.5) +
  geom_point(aes(color = borough), alpha = 0.65) +
  scale_colour_viridis_d("Borough", option = "plasma") +
  scale_x_log10(
    labels = scales::percent_format(accuracy = 0.1),
    breaks = c(0.001, 0.003, 0.01, 0.03, 0.1, 0.3, 1),
    limits = c(0.01, 0.12),
    expand = expansion(mult = c(0.02, 0.05))
  ) +
  scale_y_log10(
    labels = scales::percent_format(accuracy = 0.1),
    breaks = c(0.001, 0.003, 0.01, 0.03, 0.1, 0.3, 1),
    limits = c(0.01, 0.12),
    expand = expansion(mult = c(0.02, 0.05))
  ) +
  scale_size_continuous(range = c(1, 14), guide = "none") +
  annotate("text", x = 0.0105, y = overall_white * 1.05,
           label = "Overall white hit rate", hjust = 0, size = 2.6,
           color = "grey10", fontface = "italic") +
  annotate("text", x = overall_black * 0.95, y = 0.011,
           label = "Overall black hit rate", hjust = -0, size = 2.6,
           color = "grey10", fontface = "italic", angle = 90) +
  annotate("text", x = 0.092, y = 0.099,
           label = "Equal hit rates", vjust = -0.1, hjust = 0.75, size = 2.6,
           color = "grey10", fontface = "italic", angle = 45) +
  labs(
    x = "Hit rate for Black suspects",
    y = "Hit rate for White suspects",
    caption = "Each dot represents a precinct; Dot size = total no. stops.\n A \"hit\" equals search == 1 + seizure == 1."
  ) +
  theme_bw(base_size = 11) +
  theme(
    panel.grid.minor    = element_blank(),
    panel.grid.major    = element_line(color = "grey93", linewidth = 0.4),
    axis.title          = element_text(size = 10, color = "grey20"),
    axis.text           = element_text(size = 8, color = "grey40"),
    plot.title          = element_text(size = 13, face = "bold", color = "grey10", margin = margin(b = 4)),
    plot.subtitle       = element_text(size = 8, color = "grey40", margin = margin(b = 10)),
    plot.margin         = margin(15, 20, 15, 15)
  )

print(p)

ggsave("hit_rate_scatter.png", p, width = 5.5, height = 3, dpi = 150)