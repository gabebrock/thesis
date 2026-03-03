# Load required libraries
library(fixest)
library(tidyverse)

# Create datasets for different mayoral administrations if they don't exist
if (!exists("pct_month_lagged_bloomberg") || !exists("pct_month_lagged_blasio") || !exists("pct_month_lagged_adams")) {
  pct_month_lagged_bloomberg <- test_pct_month_full_lagged %>%
    dplyr::filter(year <= 2013)  # Bloomberg era: 2002-2013
  pct_month_lagged_blasio <- test_pct_month_full_lagged %>%
    dplyr::filter(year >= 2014 & year <= 2021)  # de Blasio era: 2014-2021
  pct_month_lagged_adams <- test_pct_month_full_lagged %>%
    dplyr::filter(year >= 2022)  # Adams era: 2022-present
}

test_pct_month_full_lagged <- test_pct_month_full_lagged %>%
  mutate(
    bloomberg_period = ifelse(year <= 2013, 1, 0),
    deblasio_period  = ifelse(year >= 2014 & year <= 2021, 1, 0),
    adams_period     = ifelse(year >= 2022, 1, 0)  # you already did this
  )

# Function to run allocation models for different administrations
# Estimates effect of race on monthly stop frequency with progressive controls
run_allocation_models <- function(data, admin_name) {
  cat("\n=== Models for", admin_name, "===\n")
  
  # Model 1: Base model with race variables and year fixed effects
  # Tests mod1_basic relationship between precinct racial composition and stop frequency
  mod1_basic <- feols(
    log_stops ~ pct_black + pct_hisp | pct + factor(year),
    data = data,
    cluster = "pct"
  )
  
  cat("\nModel 1: Race + Year FE\n")
  print(summary(mod1_basic))
  
  # Model 2: Add socioeconomic and demographic controls
  # Controls for population characteristics that might affect policing needs
  mod2_covariates <- feols(
    log_stops ~ pct_black + pct_hisp + 
      pop_density + pct_18_24 + pct_public_housing + log(total_pop) +
      pct_foreign_born + median_income  |
      pct + factor(year),
    data = data,
    cluster = "pct"
  )
  
  cat("\nModel 2: + Socioeconomic Controls\n")
  print(summary(mod2_covariates))
  
  # Model 3: Add lagged crime rates (dynamic model)
  # Tests whether police respond to crime rates from previous month
  mod3_dynamic <- feols(
    log_stops ~ pct_black + pct_hisp +
      pop_density + pct_18_24 + pct_public_housing + log(total_pop) +
      pct_foreign_born + median_income  +
      lag_violent_rate + lag_nonviolent_rate |
      pct + factor(year),
    data = data,
    cluster = "pct"
  )
  
  cat("\nModel 3: + Lagged Crime Rates\n")
  print(summary(mod3_dynamic))
  
  # Model 4: Add change in violent crime rate
  # Tests whether changes in crime rates affect police allocation
  mod4_violent <- feols(
    log_stops ~ pct_black + pct_hisp +
      pop_density + pct_18_24 + pct_public_housing + log(total_pop) +
      pct_foreign_born + median_income  +
      lag_violent_rate + lag_nonviolent_rate +
      (violent_rate - lag_violent_rate) / lag_violent_rate | # Change in violent crime
      pct + factor(year) + BoroName,
    data = data,
    cluster = "pct"
  )
  
  cat("\nModel 4: + \n𝚫 Violent Crime Rate")
  print(summary(mod4_violent))
  
  # Model 5: Add change in total crime rate
  mod5_crime <- feols(
    log_stops ~ pct_black + pct_hisp +
      pop_density + pct_18_24 + pct_public_housing + log(total_pop) +
      pct_foreign_born + median_income  +
      lag_violent_rate + lag_nonviolent_rate +
      (violent_rate - lag_violent_rate) +  # Change in violent crime
      (nonviolent_rate - lag_nonviolent_rate) / lag_violent_rate |  # Change in non-violent crime
      pct + factor(year) + BoroName,
    data = data,
    cluster = "pct"
  )
  
  cat("\nModel 5: + \n𝚫 Total Crime Rate\n")
  print(summary(mod4_violent))
  
  # Generate model comparison table
  cat("\n=== Model Comparison for", admin_name, "===\n")
  etable(mod1_basic, mod2_covariates, mod3_dynamic, mod4_violent, mod5_crime,
         headers = c("(1)", "(2)", "(3)", "(4)", "(5)"),
         keep = c("pct_black", "pct_hisp", "pop_density", "pct_18_24", "pct_public_housing", "log(total_pop)", "lag_violent_rate", "lag_nonviolent_rate", "constant"),
         extralines = list(
           "R-squared" = c("r2", "r2", "r2", "r2", "r2"), 
           "Observations" = c("nobs", "nobs", "nobs", "nobs", "nobs"),
           "Year FE" = c("Yes", "Yes", "Yes", "Yes", "Yes"),
           "Borough FE" = c("Yes", "Yes", "Yes", "Yes", "Yes")
         ),
         title = paste("Table X. OLS Regressions of Precinct Officer Allocations (logged) by Precinct Racial Composition and Social and Crime Conditions,", admin_name, "(b, SE, p)"),
         signifCode = c("***" = 0.001, "**" = 0.01, "*" = 0.05)
         )
  
  # Return models in a named list for further analysis
  return(list(
    mod1_basic = mod1_basic,           # Model 1: Race + Year FE
    mod2_covariates = mod2_covariates,  # Model 2: + Socioeconomic controls
    mod3_dynamic = mod3_dynamic,       # Model 3: + Lagged crime rates
    mod4_violent = mod4_violent,       # Model 4: + Violent Crime change
    mod5_crime = mod5_crime            # Model 5: + Total crime change
  ))
}

# ------- MAIN ANALYSIS: Run models for each mayoral administration -------

cat("Starting officer allocation analysis with lagged crime rates...\n")
cat("Total monthly observations:", nrow(test_pct_month_full_lagged), "\n")

# Run models for each time period
models_overall <- run_allocation_models(test_pct_month_full_lagged, "Overall")      # Full sample
models_bloomberg <- run_allocation_models(pct_month_lagged_bloomberg, "Bloomberg")  # 2002-2013
models_blasio <- run_allocation_models(pct_month_lagged_blasio, "Blasio")         # 2014-2021
models_adams <- run_allocation_models(pct_month_lagged_adams, "Adams")             # 2022-present

cat("\n=== Analysis Complete ===\n")

# ------- LaTex Tables -------


# Table 1: Main results showing model progression for overall sample
table1_stop_freq <- etable(
  models_overall$mod1_basic,
  models_overall$mod2_covariates,
  models_overall$mod3_dynamic,
  models_overall$mod4_violent,
  models_overall$mod5_crime,
  keep = c("%pct_black", "%pct_hisp", "%constant"),
  dict = c(
    pct_black = "% Black",
    pct_hisp = "% Hispanic", 
    constant = "Constant"
  ),
  headers = c(
    "(1)\nRace Only", 
    "(2)\n+ Covariates", 
    "(3)\n+ Lagged Crime",
    "(4)\n+ Violent Crime Change",
    "(5)\n+ Total Crime Change"
  ),
  title = "Table 1. OLS Regressions of Precinct Officer Allocations (logged) by Precinct Racial Composition and Social and Crime Conditions, Monthly Data (b, SE, p)",
  digits = 3,
  signifCode = c("***" = 0.01, "**" = 0.05, "*" = 0.1),
  se.below = TRUE,
  fitstat = ~ r2 + n,
  extralines = list(
    "Year FE" = c("Yes", "Yes", "Yes", "Yes", "Yes"),
    "Borough FE" = c("Yes", "Yes", "Yes", "Yes", "Yes"),
    "Clustered SE" = c("Precinct", "Precinct", "Precinct", "Precinct", "Precinct")
  ),
  view = FALSE,
  tex = TRUE
)
# -------


# Table 2: Comparison across mayoral administrations (Models 1 and 4)
table1b_stop_freq_comp <- etable(
  models_overall$mod1_basic,
  models_overall$mod5_crime,
  models_bloomberg$mod1_basic,
  models_bloomberg$mod5_crime,
  models_blasio$mod1_basic,
  models_blasio$mod5_crime,
  models_adams$mod1_basic,
  models_adams$mod5_crime,
  keep = c("%pct_black", "%pct_hisp", "%constant"),
  dict = c(
    pct_black = "% Black",
    pct_hisp = "% Hispanic", 
    constant = "Constant"
  ),
  headers = c(
    "Overall\n(1)", 
    "Overall\n(5)", 
    "Bloomberg\n(1)",
    "Bloomberg\n(5)",
    "de Blasio\n(1)",
    "de Blasio\n(5)",
    "Adams\n(1)",
    "Adams\n(5)"
  ),
  title = "Table 2. OLS Regressions of Precinct Officer Allocations (logged) by Precinct Racial Composition and Social and Crime Conditions, Monthly Data (b, SE, p)",
  digits = 3,
  signifCode = c("***" = 0.01, "**" = 0.05, "*" = 0.1),
  se.below = TRUE,
  fitstat = ~ r2 + n,
  extralines = list(
    "Year FE" = c("Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes"),
    "Borough FE" = c("Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes"),
    "Clustered SE" = c("Precinct", "Precinct", "Precinct", "Precinct", "Precinct", "Precinct", "Precinct", "Precinct")
  ),
  view = FALSE,
  tex = TRUE
)
