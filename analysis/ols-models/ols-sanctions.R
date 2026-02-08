# Load required libraries
library(fixest)
library(dplyr)

# Define common model formula and fixed effects
base_formula <- "~ Black + Hisp + age + female +
  RS_furtive + RS_crimloc + RS_casing + RS_other +
  RS_drug + RS_suspobj + RS_appear + RS_violent +
  i(off_cat)"

fixed_effects <- "| pct + year + interaction(pct, year)"
cluster_var <- "pct"

# Function to run feols model with consistent parameters
run_feols_model <- function(outcome, data, description) {
  cat(paste("\n===", description, "===\n"))
  
  model <- feols(
    as.formula(paste(outcome, base_formula, fixed_effects)),
    data = data,
    cluster = cluster_var
  ) %>%
    summary()
  
  print(model)
  return(model)
}

# Function to run feglm model (logistic)
run_feglm_model <- function(outcome, data, description) {
  cat(paste("\n===", description, "(Logistic) ===\n"))
  
  model <- feglm(
    as.formula(paste(outcome, base_formula, fixed_effects)),
    data = data,
    cluster = cluster_var,
    family = binomial(link = "logit")
  ) %>%
    summary()
  
  print(model)
  return(model)
}

# ---- Any Sanction Models ----
sanction_models <- list(
  run_feols_model("sanction", sqf_ols, "Any Sanction - Full Sample"),
  run_feglm_model("sanction", sqf_ols, "Any Sanction - Full Sample"),
  run_feols_model("sanction", sqf_bloomberg, "Any Sanction - Bloomberg Admin."),
  run_feols_model("sanction", sqf_blasio, "Any Sanction - De Blasio Admin."),
  run_feols_model("sanction", sqf_adams, "Any Sanction - Adams Admin.")
)

# ---- Arrest Models ----
arrest_models <- list(
  run_feols_model("arrest", sqf_ols, "Arrest-Only - Full Sample"),
  run_feols_model("arrest", sqf_bloomberg, "Arrest-Only - Bloomberg Admin."),
  run_feols_model("arrest", sqf_blasio, "Arrest-Only - De Blasio Admin."),
  run_feols_model("arrest", sqf_adams, "Arrest-Only - Adams Admin.")
)

# ---- Summons Models ----
summons_models <- list(
  run_feols_model("summons", sqf_ols, "Summons-Only - Full Sample"),
  run_feols_model("summons", sqf_bloomberg, "Summons-Only - Bloomberg Admin."),
  run_feols_model("summons", sqf_blasio, "Summons-Only - De Blasio Admin."),
  run_feols_model("summons", sqf_adams, "Summons-Only - Adams Admin.")
)

# ---- Generate Tables ----
cat("\n=== Generating Model Tables ===\n")

# Variables to exclude from tables
exclude_vars <- c("RS_furtive", "RS_crimloc", "RS_casing", "RS_other", 
                  "RS_drug", "RS_suspobj", "RS_appear", "RS_violent", "off_cat")

# Sanction models table
cat("\n--- Sanction Models Table ---\n")
sanction_table <- etable(sanction_models, 
                         headers = c("Full (Linear)", "Full (Logistic)", 
                                   "Bloomberg", "De Blasio", "Adams"),
                         title = "Sanction Models by Administration",
                         drop = exclude_vars,
                         vcov = "standard",
                         signifCode = c("***"=0.01, "**"=0.05, "*"=0.1),
                         digits = 3,
                         digits.stats = 3,
                         keep = c("Black", "Hisp", "age", "female"),
                         view = TRUE)
print(sanction_table)

# Arrest models table  
cat("\n--- Arrest Models Table ---\n")
arrest_table <- etable(arrest_models,
                       headers = c("Full Sample", "Bloomberg", "De Blasio", "Adams"),
                       title = "Arrest Models by Administration",
                       drop = exclude_vars,
                       vcov = "standard",
                       signifCode = c("***"=0.01, "**"=0.05, "*"=0.1),
                       digits = 3,
                       digits.stats = 3,
                       keep = c("Black", "Hisp", "age", "female"),
                       view = TRUE)
print(arrest_table)

# Summons models table
cat("\n--- Summons Models Table ---\n")
summons_table <- etable(summons_models,
                        headers = c("Full Sample", "Bloomberg", "De Blasio", "Adams"),
                        title = "Summons Models by Administration",
                        drop = exclude_vars,
                        vcov = "standard",
                        signifCode = c("***"=0.01, "**"=0.05, "*"=0.1),
                        digits = 3,
                        digits.stats = 3,
                        keep = c("Black", "Hisp", "age", "female"),
                        view = TRUE)
print(summons_table)

# ---- Summary ----
cat("\n=== Model Summary ===\n")
cat(paste("Total models run:", length(c(sanction_models, arrest_models, summons_models)), "\n"))
cat("Models include: sanction (linear & logistic), arrest, summons\n")
cat("Administrations: Full sample, Bloomberg, De Blasio, Adams\n")
