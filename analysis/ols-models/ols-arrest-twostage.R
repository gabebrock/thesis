# Two-Stage Arrest Models
# Stage 1: Logistic model predicts probability of any sanction
# Stage 2: OLS model predicts arrest conditional on sanction, using predicted probability

# Create data subsets for different mayoral administrations
sqf_adams <- sqf_ols %>% filter(year >= 2022)
sqf_blasio <- sqf_ols %>% filter(year >= 2014 & year <= 2021)
sqf_bloomberg <- sqf_ols %>% filter(year <= 2013)

# Function to run two-stage models for different administrations
run_twostage_models <- function(data, admin_name) {
  cat("\n=== Two-Stage Models for", admin_name, "===\n")
  
  # Remove rows with missing data for model variables
  complete_data <- data %>%
    filter(!is.na(sanction) & !is.na(arrest) & !is.na(Black) & !is.na(Hisp) & 
           !is.na(age) & !is.na(female) & !is.na(pct) & !is.na(year) &
           !is.na(RS_furtive) & !is.na(RS_crimloc) & !is.na(RS_casing) & 
           !is.na(RS_other) & !is.na(RS_drug) & !is.na(RS_suspobj) & 
           !is.na(RS_appear) & !is.na(RS_violent) & !is.na(off_cat))
  
  cat("Original observations:", nrow(data), "\n")
  cat("Complete observations:", nrow(complete_data), "\n")
  
  # Stage 1: Logistic model for any sanction
  stage1_logit <- feglm(
    sanction ~ Black + Hisp + age + female +
      RS_furtive + RS_crimloc + RS_casing + RS_other +
      RS_drug + RS_suspobj + RS_appear + RS_violent +
      i(off_cat) | pct + year + interaction(pct, year),
    data = complete_data,
    cluster = "pct",
    family = binomial(link = "logit")
  )
  
  cat("\nStage 1: Logistic model for any sanction\n")
  print(summary(stage1_logit))
  
  # Generate predicted probabilities from Stage 1
  # Extract the actual data used by the model and add predictions
  model_indices <- attr(stage1_logit$fitted_values, "names")
  complete_data <- complete_data[model_indices, ]
  complete_data$pred_sanction_prob <- stage1_logit$fitted_values
  
  # Stage 2: OLS model for arrest conditional on sanction
  # Only include observations where sanction could occur
  stage2_ols <- feols(
    arrest ~ Black + Hisp + age + female +
      RS_furtive + RS_crimloc + RS_casing + RS_other +
      RS_drug + RS_suspobj + RS_appear + RS_violent +
      pred_sanction_prob + i(off_cat) | pct + year + interaction(pct, year),
    data = complete_data,
    cluster = "pct"
  )
  
  cat("\nStage 2: OLS model for arrest with predicted sanction probability\n")
  print(summary(stage2_ols))
  
  # Return both models
  return(list(
    stage1_logit = stage1_logit,
    stage2_ols = stage2_ols,
    data_with_predictions = complete_data
  ))
}

# Run two-stage models for each administration
cat("Starting two-stage arrest modeling...\n")

# Overall models
models_overall <- run_twostage_models(sqf_ols, "Overall")

# Bloomberg era
models_bloomberg <- run_twostage_models(sqf_bloomberg, "Bloomberg")

# Blasio era  
models_blasio <- run_twostage_models(sqf_blasio, "Blasio")

# Adams era
models_adams <- run_twostage_models(sqf_adams, "Adams")

cat("\n=== Two-Stage Analysis Complete ===\n")

# Create comparison table of Stage 2 models
etable(
  models_overall$stage2_ols,
  models_bloomberg$stage2_ols,
  models_blasio$stage2_ols,
  models_adams$stage2_ols,
  keep = c("Black", "Hisp", "pred_sanction_prob", "constant"),
  headers = c("Overall", "Bloomberg", "de Blasio", "Adams"),
  title = "Table X. Two-Stage Arrest Models: Stage 2 OLS Results (b, SE, p)",
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
