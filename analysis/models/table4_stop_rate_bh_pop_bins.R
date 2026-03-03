#' Stop rates are disaggregated by precinct racial/ethnic composition, 
#' using categorical increments in the percentage of Black residents (pct_black) 
#' and Hispanic residents (pct_hisp): 20–40%, 40–60%, and 60–100%. 
#' 
#' Each model estimates effects relative to precincts with the lowest 
#' concentrations of Black or Hispanic residents. 
#' 
#' All models control for precinct crime conditions, including 
#' month-lagged logged rates of non-violent complaints (lag_log_nonviolent_rate) 
#' and violent complaints (lag_log_violent_rate), as well as additional 
#' precinct-level covariates: percentage of public housing (pct_public_housing), 
#' population density (pop_density), and percentage of males aged 18–24 (pct_18_24).

# Using pct_year_ols from ~/analysis/test-hitrate.qmd
pct_year_ols_w_race_bins <- test_pct_month_full_lagged %>%
  mutate(
    black_cat = cut(
      pct_black,
      breaks = c(0, 0.20, 0.40, 0.60, 1.00),
      labels = c("0-20", "20-40", "40-60", "60-100"),
      right = FALSE
    ),
    black_cat = factor(black_cat,
                       levels = c("0-20", "20-40", "40-60", "60-100")),
    hisp_cat = cut(
      pct_hisp,
      breaks = c(0, 0.20, 0.40, 0.60, 1.00),
      labels = c("0-20", "20-40", "40-60", "60-100"),
      right = FALSE
    ),
    hisp_cat = factor(hisp_cat,
                      levels = c("0-20", "20-40", "40-60", "60-100")),
  )

# Set the reference group (baseline) as the 0-20% for the regression
pct_year_ols_w_race_bins <- pct_year_ols_w_race_bins %>%
  mutate(
    black_cat = relevel(black_cat, ref = "0-20"),
    hisp_cat  = relevel(hisp_cat, ref = "0-20")
  )

# Model with Black concentration
model_black <- feols(
  log_stops ~ black_cat +
    lag_nonviolent_rate +
    lag_violent_rate +
    pct_public_housing +
    pop_density +
    pct_18_24 | BoroName,
  data = pct_year_ols_w_race_bins,
  cluster = "pct"
)

# black:bloomberg
feols(
  log_stops ~ black_cat +
    lag_nonviolent_rate +
    lag_violent_rate +
    pct_public_housing +
    pop_density +
    pct_18_24 | BoroName,
  data = pct_year_ols_w_race_bins %>% filter(year <= 2013),
  cluster = "pct"
)

# black:deblasio
feols(
  log_stops ~ black_cat +
    lag_nonviolent_rate +
    lag_violent_rate +
    pct_public_housing +
    pop_density +
    pct_18_24 | BoroName,
  data = pct_year_ols_w_race_bins %>% filter(year >= 2014 & year <= 2021),
  cluster = "pct"
)

# black:adams
feols(
  log_stops ~ black_cat +
    lag_nonviolent_rate +
    lag_violent_rate +
    pct_public_housing +
    pop_density +
    pct_18_24 | BoroName,
  data = pct_year_ols_w_race_bins %>% filter(year >= 2022),
  cluster = "pct"
)

# Model with Hispanics
model_hisp <- feols(
  log_stops ~ hisp_cat +
    lag_nonviolent_rate +
    lag_violent_rate +
    pct_public_housing +
    pop_density +
    pct_18_24 | BoroName,
  data = pct_year_ols_w_race_bins,
  cluster = "pct"
)

# Minority Model
model_bh <- feols(
  log_stops ~ black_cat + hisp_cat +
    lag_nonviolent_rate +
    lag_violent_rate +
    pct_public_housing +
    pop_density +
    pct_18_24 | BoroName,
  data = pct_year_ols_w_race_bins,
  cluster = "pct"
)

etable(
  model_bh,  # the combined model
  order = c(
    # Black bins first
    "black_cat20-40",
    "black_cat40-60",
    "black_cat60-100",
    
    # Then Hispanic bins
    "hisp_cat20-40",
    "hisp_cat40-60",
    "hisp_cat60-100",
    
    # Then covariates
    "lag_log_nonviolent_rate",
    "lag_log_violent_rate",
    "pct_public_housing",
    "pop_density",
    "pct_18_24"
  ),
  dict = c(
    "black_cat20-40" = "% Black: 20–40",
    "black_cat40-60" = "% Black: 40–60",
    "black_cat60-100" = "% Black: 60–100",
    "hisp_cat20-40"  = "% Hispanic: 20–40",
    "hisp_cat40-60"  = "% Hispanic: 40–60",
    "hisp_cat60-100" = "% Hispanic: 60–100",
    "lag_log_nonviolent_rate" = "Nonviolent Crime Complaints (Lagged, Log)",
    "lag_log_violent_rate"    = "Violent Crime Complaints (Lagged, Log)",
    "pct_public_housing"      = "% Public Housing",
    "pop_density"             = "Population Density",
    "pct_18_24"               = "% Population Age 18–24",
    "Constant"                = "Intercept"
  ),
  title = "Table 3: OLS Regression of Stop Rates per 10,000 Persons (Logged), Controlling for Crime Rates and Socioeconomic Factors, NYPD Precincts, 2009–2023",
  se.below = TRUE,
  view = TRUE
)

#' We estimate precinct-level regressions allowing time trends in pedestrian 
#' stops to vary flexibly with Black and Hispanic population shares, while 
#' controlling for lagged crime and other precinct characteristics.

