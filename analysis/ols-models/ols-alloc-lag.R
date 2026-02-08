
# create data subsets for different mayoral administrations
pct_month_lagged_adams <- pct_month_lagged %>%
  filter(year >= 2022)

pct_month_lagged_blasio <- pct_month_lagged %>%
  filter(year >= 2014 & year <= 2021)

pct_month_lagged_bloomberg <- pct_month_lagged %>%
  filter(year <= 2013)

# Function to run models for different administrations
run_allocation_models <- function(data, admin_name) {
  cat("\n=== Models for", admin_name, "===\n")
  
  # Model 1: Basic model with lagged crime rates
  mod_alloc_lag1 <- feols(
    log_stops ~ pct_black + pct_hisp + 
      lag_violent_rate + lag_nonviolent_rate +
      pop_density + pct_18_24 + pct_public_housing + 
      factor(year) + factor(month) | BoroName,
    data = data,
    cluster = "pct"
  )
  
  cat("\nModel 1: Basic lagged crime model\n")
  print(summary(mod_alloc_lag1))
  
  # Model 2: Include lagged stops to capture persistence
  mod_alloc_lag2 <- feols(
    log_stops ~ pct_black + pct_hisp + 
      lag_violent_rate + lag_nonviolent_rate +
      lag_log_stops +
      pop_density + pct_18_24 + pct_public_housing + 
      factor(year) + factor(month) | BoroName,
    data = data,
    cluster = "pct"
  )
  
  cat("\nModel 2: With lagged stops\n")
  print(summary(mod_alloc_lag2))
  
  # Model 3: Dynamic model with both current and lagged crime rates
  mod_alloc_dynamic <- feols(
    log_stops ~ pct_black + pct_hisp + 
      violent_rate_month + nonviolent_rate_month +
      lag_violent_rate + lag_nonviolent_rate +
      lag_log_stops +
      pop_density + pct_18_24 + pct_public_housing + 
      factor(year) + factor(month) | BoroName,
    data = data,
    cluster = "pct"
  )
  
  cat("\nModel 3: Dynamic model\n")
  print(summary(mod_alloc_dynamic))
  
  # Model 4: Model 3 + borough FE only (no year/month FE)
  mod4 <- feols(
    log_stops ~ pct_black + pct_hisp + 
      lag_violent_rate + lag_nonviolent_rate +
      lag_log_stops +
      pop_density + pct_18_24 + pct_public_housing | BoroName,
    data = data,
    cluster = "pct"
  )
  
  cat("\nModel 4: Borough FE only\n")
  print(summary(mod4))
  
  # Model 5: Model 4 + change in violent crime rate
  # Create change in violent crime rate
  data_with_change <- data %>%
    group_by(pct) %>%
    arrange(date) %>%
    mutate(
      change_violent_rate = violent_rate_month - lag(violent_rate_month)
    ) %>%
    ungroup() %>%
    filter(!is.na(change_violent_rate))
  
  mod5 <- feols(
    log_stops ~ pct_black + pct_hisp + 
      lag_violent_rate + lag_nonviolent_rate +
      change_violent_rate +
      pop_density + pct_18_24 + pct_public_housing | BoroName,
    data = data_with_change,
    cluster = "pct"
  )
  
  cat("\nModel 5: + Change in violent crime\n")
  print(summary(mod5))
  
  # Compare models
  cat("\n=== Model Comparison for", admin_name, "===\n")
  etable(mod_alloc_lag1, mod_alloc_lag2, mod_alloc_dynamic, mod4, mod5,
         headers = c("(1)", "(2)", "(3)", "(4)", "(5)"),
         keep = c("pct_black", "pct_hisp", "lag_violent_rate", "lag_nonviolent_rate", "lag_log_stops", "change_violent_rate", "constant"),
         extralines = list(
           "R-squared" = c("r2", "r2", "r2", "r2", "r2"), 
           "Observations" = c("nobs", "nobs", "nobs", "nobs", "nobs"),
           "Year FE" = c("Yes", "Yes", "Yes", "No", "No"),
           "Month FE" = c("Yes", "Yes", "Yes", "No", "No"),
           "Borough FE" = c("Yes", "Yes", "Yes", "Yes", "Yes")
         ),
         title = paste("Table X. OLS Regressions of Precinct Officer Allocations (logged) by Precinct Racial Composition and Social and Crime Conditions,", admin_name, "(b, SE, p)"),
         signifCode = c("***" = 0.001, "**" = 0.01, "*" = 0.05)
         )
  
  # Return model list
  return(list(
    basic = mod_alloc_lag1,
    with_lag_stops = mod_alloc_lag2,
    dynamic = mod_alloc_dynamic,
    borough_fe_only = mod4,
    with_change = mod5
  ))
}

# Run models for each administration
cat("Starting officer allocation analysis with lagged crime rates...\n")
cat("Total monthly observations:", nrow(pct_month_lagged), "\n")

# Overall models (base data) 
models_overall <- run_allocation_models(pct_month_lagged, "Overall")

# Bloomberg era
models_bloomberg <- run_allocation_models(pct_month_lagged_bloomberg, "Bloomberg")

# Blasio era  
models_blasio <- run_allocation_models(pct_month_lagged_blasio, "Blasio")

# Adams era
models_adams <- run_allocation_models(pct_month_lagged_adams, "Adams")

cat("\n=== Analysis Complete ===\n")


# --- General Models ----
etable(
  models_overall$basic,
  models_overall$with_lag_stops,
  models_overall$dynamic,
  models_overall$borough_fe_only,
  models_overall$with_change,
  keep = c("%pct_black", "%pct_hisp", "%constant"),
  dict = c(
    pct_black = "% Black",
    pct_hisp = "% Hispanic", 
    constant = "Constant"
  ),
  headers = c(
    "Base Model\n(Race Only)", 
    "Model 1\n+ Lagged Stops", 
    "Model 2\n+ Current Crime",
    "Model 3\n+ Borough FE Only",
    "Model 4\n+ Change in Crime"
  ),
  title = "Table 1. OLS Regressions of Precinct Officer Allocations (logged) by Precinct Racial Composition and Social and Crime Conditions, Monthly Data (b, SE, p)",
  digits = 3,
  signifCode = c("***" = 0.01, "**" = 0.05, "*" = 0.1),
  se.below = TRUE,
  fitstat = ~ r2 + n,
  extralines = list(
    "Year FE" = c("Yes", "Yes", "Yes", "No", "No"),
    "Month FE" = c("Yes", "Yes", "Yes", "No", "No"), 
    "Borough FE" = c("Yes", "Yes", "Yes", "Yes", "Yes"),
    "Clustered SE" = c("Precinct", "Precinct", "Precinct", "Precinct", "Precinct")
  ),
  view = TRUE
)
# -------


# --- Comparision Models ----
etable(
  models_overall$basic,
  models_overall$with_change,
  models_bloomberg$basic,
  models_bloomberg$with_change,
  models_blasio$basic,
  models_blasio$with_change,
  models_adams$basic,
  models_adams$with_change,
  keep = c("%pct_black", "%pct_hisp", "%constant"),
  dict = c(
    pct_black = "% Black",
    pct_hisp = "% Hispanic", 
    constant = "Constant"
  ),
  headers = c(
    "General\n Base Model", 
    "General\n Model 5", 
    "Bloomberg\n Base Model",
    "Bloomberg\n Model 5",
    "de Blasio\n Base Model",
    "de Blasio\n Model 5",
    "Adams\n Base Model",
    "Adams\n Model 5"
  ),
  title = "Table 1. OLS Regressions of Precinct Officer Allocations (logged) by Precinct Racial Composition and Social and Crime Conditions, Monthly Data (b, SE, p)",
  digits = 3,
  signifCode = c("***" = 0.01, "**" = 0.05, "*" = 0.1),
  se.below = TRUE,
  fitstat = ~ r2 + n,
  extralines = list(
    "Year FE" = c("Yes", "No", "Yes", "No", "Yes", "No", "Yes", "No"),
    "Month FE" = c("Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes"), 
    "Borough FE" = c("Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes"),
    "Clustered SE" = c("Precinct", "Precinct", "Precinct", "Precinct", "Precinct", "Precinct", "Precinct", "Precinct")
  ),
  view = TRUE
)


# -------