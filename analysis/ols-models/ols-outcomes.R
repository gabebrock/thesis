# OLS models of stop outcomes ("hit rates")
# Probability models for frisk, search, contraband, weapons, and force outcomes

# Create data subsets for different mayoral administrations
sqf_adams <- sqf_ols %>% filter(year >= 2022)
sqf_blasio <- sqf_ols %>% filter(year >= 2014 & year <= 2021)
sqf_bloomberg <- sqf_ols %>% filter(year <= 2013)

# Function to run outcome models for different administrations
run_outcome_models <- function(data, admin_name) {
  cat("\n=== Outcome Models for", admin_name, "===\n")
  
  # Remove rows with missing data for model variables
  complete_data <- data %>%
    filter(!is.na(frisk) & !is.na(search) & !is.na(contrab) & !is.na(any_weap) & !is.na(force) &
           !is.na(Black) & !is.na(Hisp) & !is.na(age) & !is.na(female) & 
           !is.na(pct) & !is.na(year) & !is.na(off_cat) &
           !is.na(RS_furtive) & !is.na(RS_crimloc) & !is.na(RS_casing) & 
           !is.na(RS_other) & !is.na(RS_drug) & !is.na(RS_suspobj) & 
           !is.na(RS_appear) & !is.na(RS_violent))
  
  cat("Complete observations:", nrow(complete_data), "\n")
  
  # Frisk model
  frisk_model <- feols(
    frisk ~ Black + Hisp + age + female +
      RS_furtive + RS_crimloc + RS_casing + RS_other +
      RS_drug + RS_suspobj + RS_appear + RS_violent +
      i(off_cat) | pct + year + interaction(pct, year),
    data = complete_data,
    cluster = "pct"
  )
  
  # Search model  
  search_model <- feols(
    search ~ Black + Hisp + age + female +
      RS_furtive + RS_crimloc + RS_casing + RS_other +
      RS_drug + RS_suspobj + RS_appear + RS_violent +
      i(off_cat) | pct + year + interaction(pct, year),
    data = complete_data,
    cluster = "pct"
  )
  
  # Contraband model
  contraband_model <- feols(
    contrab ~ Black + Hisp + age + female +
      RS_furtive + RS_crimloc + RS_casing + RS_other +
      RS_drug + RS_suspobj + RS_appear + RS_violent +
      i(off_cat) | pct + year + interaction(pct, year),
    data = complete_data,
    cluster = "pct"
  )
  
  # Weapons model
  weapon_model <- feols(
    any_weap ~ Black + Hisp + age + female +
      RS_furtive + RS_crimloc + RS_casing + RS_other +
      RS_drug + RS_suspobj + RS_appear + RS_violent +
      i(off_cat) | pct + year + interaction(pct, year),
    data = complete_data,
    cluster = "pct"
  )
  
  # Force model
  force_model <- feols(
    force ~ Black + Hisp + age + female +
      RS_furtive + RS_crimloc + RS_casing + RS_other +
      RS_drug + RS_suspobj + RS_appear + RS_violent +
      i(off_cat) | pct + year + interaction(pct, year),
    data = complete_data,
    cluster = "pct"
  )
  
  # Print model summaries
  cat("\nFrisk Model:\n")
  print(summary(frisk_model))
  
  cat("\nSearch Model:\n")
  print(summary(search_model))
  
  cat("\nContraband Model:\n")
  print(summary(contraband_model))
  
  cat("\nWeapons Model:\n")
  print(summary(weapon_model))
  
  cat("\nForce Model:\n")
  print(summary(force_model))
  
  # Return model list
  return(list(
    frisk = frisk_model,
    search = search_model,
    contraband = contraband_model,
    weapon = weapon_model,
    force = force_model
  ))
}

# Run outcome models for each administration
cat("Starting stop outcome modeling...\n")

# Overall models
models_overall <- run_outcome_models(sqf_ols, "Overall")

# Bloomberg era
models_bloomberg <- run_outcome_models(sqf_bloomberg, "Bloomberg")

# Blasio era  
models_blasio <- run_outcome_models(sqf_blasio, "Blasio")

# Adams era
models_adams <- run_outcome_models(sqf_adams, "Adams")

cat("\n=== Outcome Analysis Complete ===\n")

# Create comparison table for frisk models
etable(
  models_overall$frisk,
  models_bloomberg$frisk,
  models_blasio$frisk,
  models_adams$frisk,
  keep = c("Black", "Hisp", "constant"),
  headers = c("Overall", "Bloomberg", "de Blasio", "Adams"),
  title = "Table X. OLS Models of Frisk Outcomes (b, SE, p)",
  digits = 3,
  signifCode = c("***" = 0.01, "**" = 0.05, "*" = 0.1),
  se.below = TRUE,
  fitstat = ~ r2 + n,
  extralines = list(
    "Precinct FE" = c("Yes", "Yes", "Yes", "Yes"),
    "Year FE" = c("Yes", "Yes", "Yes", "Yes"),
    "Precinct×Year FE" = c("Yes", "Yes", "Yes", "Yes"),
    "Clustered SE" = c("Precinct", "Precinct", "Precinct", "Precinct")
  ),
  view = TRUE
)

# Create comparison table for search models
etable(
  models_overall$search,
  models_bloomberg$search,
  models_blasio$search,
  models_adams$search,
  keep = c("Black", "Hisp", "constant"),
  headers = c("Overall", "Bloomberg", "de Blasio", "Adams"),
  title = "Table X. OLS Models of Search Outcomes (b, SE, p)",
  digits = 3,
  signifCode = c("***" = 0.01, "**" = 0.05, "*" = 0.1),
  se.below = TRUE,
  fitstat = ~ r2 + n,
  extralines = list(
    "Precinct FE" = c("Yes", "Yes", "Yes", "Yes"),
    "Year FE" = c("Yes", "Yes", "Yes", "Yes"),
    "Precinct×Year FE" = c("Yes", "Yes", "Yes", "Yes"),
    "Clustered SE" = c("Precinct", "Precinct", "Precinct", "Precinct")
  ),
  view = TRUE
)