# Table 4. OLS Regression of Officer Stop Rates per 10,000 Persons (Logged)
# Controlling for Crime Rates and Socioeconomic Factors, NYPD Precincts, 2004–2014

# Load the prepared data from existing analysis
# source("../test-hitrate.qmd", local = TRUE)

# Filter to 2004-2014# Extend period to include all mayoral administrations (2004-2023)
pct_year_ols_admin <- pct_year_ols %>%
  filter(year >= 2004) %>%
  mutate(
    # Crime rates per 10,000 people (not logged yet)
    violent_rate_10k = (crime_Violent + 1) / total_pop * 10000,
    nonviolent_rate_10k = (nonviolent_crime + 1) / total_pop * 10000,
    
    # Stop rate per 10,000 (logged)
    stop_rate_10k = stops / total_pop * 10000,
    log_stop_rate = log(stop_rate_10k + 1)
  ) %>%
  # Create lagged variables by precinct
  group_by(pct) %>%
  arrange(year) %>%
  mutate(
    lag_violent_rate_10k = lag(violent_rate_10k),
    lag_nonviolent_rate_10k = lag(nonviolent_rate_10k),
    log_lag_violent_rate = log(lag_violent_rate_10k + 1),
    log_lag_nonviolent_rate = log(lag_nonviolent_rate_10k + 1)
  ) %>%
  ungroup() %>%
  filter(!is.na(lag_violent_rate_10k)) # Remove first year for each precinct

# Create data subsets for different mayoral administrations
pct_year_ols_bloomberg <- pct_year_ols_admin %>% filter(year <= 2011)
pct_year_ols_blasio <- pct_year_ols_admin %>% filter(year >= 2014 & year <= 2021)
pct_year_ols_adams <- pct_year_ols_admin %>% filter(year >= 2022)

# Create categorical variables for racial composition for each administration
pct_year_ols_bloomberg <- pct_year_ols_bloomberg %>%
  mutate(
    black_20_40 = ifelse(pct_black >= 0.20 & pct_black < 0.40, 1, 0),
    black_40_60 = ifelse(pct_black >= 0.40 & pct_black < 0.60, 1, 0),
    black_60_100 = ifelse(pct_black >= 0.60, 1, 0),
    hisp_20_40 = ifelse(pct_hisp >= 0.20 & pct_hisp < 0.40, 1, 0),
    hisp_40_60 = ifelse(pct_hisp >= 0.40 & pct_hisp < 0.60, 1, 0),
    hisp_60_100 = ifelse(pct_hisp >= 0.60, 1, 0)
  )

pct_year_ols_blasio <- pct_year_ols_blasio %>%
  mutate(
    black_20_40 = ifelse(pct_black >= 0.20 & pct_black < 0.40, 1, 0),
    black_40_60 = ifelse(pct_black >= 0.40 & pct_black < 0.60, 1, 0),
    black_60_100 = ifelse(pct_black >= 0.60, 1, 0),
    hisp_20_40 = ifelse(pct_hisp >= 0.20 & pct_hisp < 0.40, 1, 0),
    hisp_40_60 = ifelse(pct_hisp >= 0.40 & pct_hisp < 0.60, 1, 0),
    hisp_60_100 = ifelse(pct_hisp >= 0.60, 1, 0)
  )

pct_year_ols_adams <- pct_year_ols_adams %>%
  mutate(
    black_20_40 = ifelse(pct_black >= 0.20 & pct_black < 0.40, 1, 0),
    black_40_60 = ifelse(pct_black >= 0.40 & pct_black < 0.60, 1, 0),
    black_60_100 = ifelse(pct_black >= 0.60, 1, 0),
    hisp_20_40 = ifelse(pct_hisp >= 0.20 & pct_hisp < 0.40, 1, 0),
    hisp_40_60 = ifelse(pct_hisp >= 0.40 & pct_hisp < 0.60, 1, 0),
    hisp_60_100 = ifelse(pct_hisp >= 0.60, 1, 0)
  )

# Models for each mayoral administration
# Check data sizes before modeling
cat("Data observations by administration:\n")
cat("Bloomberg:", nrow(pct_year_ols_bloomberg), "\n")
cat("Blasio:", nrow(pct_year_ols_blasio), "\n") 
cat("Adams:", nrow(pct_year_ols_adams), "\n")

# -------------------------------------------------------------------------


model_stop_bloomberg <- feols(
  log_stop_rate ~ 
    black_20_40 + black_40_60 + black_60_100 +
    hisp_20_40 + hisp_40_60 + hisp_60_100 +
    log_lag_nonviolent_rate + log_lag_violent_rate +
    pct_public_housing + 
    pop_density + pct_18_24 +
    factor(year) | BoroName,
  data = pct_year_ols_bloomberg,
  cluster = ~BoroName
)

model_stop_blasio <- feols(
  log_stop_rate ~ 
    black_20_40 + black_40_60 + black_60_100 +
    hisp_20_40 + hisp_40_60 + hisp_60_100 +
    log_lag_nonviolent_rate + log_lag_violent_rate +
    pct_public_housing + 
    pop_density + pct_18_24 +
    factor(year) | BoroName,
  data = pct_year_ols_blasio,
  cluster = ~BoroName
)

model_stop_adams <- feols(
  log_stop_rate ~ 
    black_20_40 + black_40_60 + black_60_100 +
    hisp_20_40 + hisp_40_60 + hisp_60_100 +
    log_lag_nonviolent_rate + log_lag_violent_rate +
    pct_public_housing + 
    pop_density + pct_18_24 +
    factor(year) | BoroName,
  data = pct_year_ols_adams,
  cluster = ~BoroName
)

# Create formatted table comparing all mayoral administrations
etable(
  model_stop_bloomberg, model_stop_blasio, model_stop_adams,
  headers = c("Bloomberg", "Blasio", "Adams"),
  keep = c("%black_20_40", "%black_40_60", "%black_60_100",
           "%hisp_20_40", "%hisp_40_60", "%hisp_60_100",
           "%log_lag_nonviolent_rate", "%log_lag_violent_rate",
           "%pct_public_housing", 
           "%pop_density", "%pct_18_24", "%constant"),
  dict = c(
    black_20_40 = "% Black 20–40",
    black_40_60 = "% Black 40–60", 
    black_60_100 = "% Black 60–100",
    hisp_20_40 = "% Hispanic 20–40",
    hisp_40_60 = "% Hispanic 40–60",
    hisp_60_100 = "% Hispanic 60–100",
    log_lag_nonviolent_rate = "Non‑Violent Complaints per 10,000 (logged, lagged)",
    log_lag_violent_rate = "Violent or Weapon Complaints per 10,000 (logged, lagged)",
    pct_public_housing = "Percent Population in Public Housing",
    pop_density = "Population Density",
    pct_18_24 = "Percent Population Male Ages 18–24",
    constant = "Intercept"
  ),
  title = "Table 4. OLS Regression of Stop Rates per 10,000 Persons (Logged)
Controlling for Crime Rates and Socioeconomic Factors, by Mayoral Administration",
  signifCode = c("***" = 0.01, "**" = 0.05, "*" = 0.1),
  se.below = TRUE,
  fitstat = ~ r2 + n,
  digits = 3,
  extralines = list(
    "Observations" = c(
      ifelse(length(nobs(model_stop_bloomberg)) > 0, nobs(model_stop_bloomberg), 0),
      ifelse(length(nobs(model_stop_blasio)) > 0, nobs(model_stop_blasio), 0), 
      ifelse(length(nobs(model_stop_adams)) > 0, nobs(model_stop_adams), 0)
    )
  ),
  view = TRUE
)

# Print model summaries for detailed output
cat("\n=== Bloomberg Administration (2004-2013) ===\n")
summary(model_stop_bloomberg)

cat("\n=== Blasio Administration (2014-2021) ===\n")
summary(model_stop_blasio)

cat("\n=== Adams Administration (2022-present) ===\n")
summary(model_stop_adams)

# Save models for later use
save(model_stop_bloomberg, model_stop_blasio, model_stop_adams, 
     pct_year_ols_bloomberg, pct_year_ols_blasio, pct_year_ols_adams,
     file = "../data/table4_admin_models.rda")
