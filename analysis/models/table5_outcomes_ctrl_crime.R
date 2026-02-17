# --- Data preparation ----

# Data prep

  # Using `sqf_ols` from ~/analysis/test-hitrate.qmd, chunk {r stop-level data prep}
  # `sqf_ols` is saved to an rds file in that chunk and referenced here for rendering
  sqf_ols <- readRDS("~/Projects/thesis/data/derived/sqf_ols.rds")

  if (inherits(sqf_ols, "sf")) {
    sqf_ols <- sf::st_drop_geometry(sqf_ols)
  }
  sfc_cols <- vapply(sqf_ols, inherits, logical(1), what = "sfc")
  if (any(sfc_cols)) {
    sqf_ols <- sqf_ols[, !sfc_cols, drop = FALSE]
  }
  
  # load libraries
  library(tidyverse)
  library(fixest) # for regressions
  library(modelsummary) # for printing regression tables
  
  # check for missing values in variables used in models
  colSums(is.na(sqf_ols))
  
  # impute zeros for NA values for `sqf_ols$female` 
  # (assuming cop would have indicated a female detainee)
  sqf_ols$female[is.na(sqf_ols$female)] <- 0

# Subsettings SQF data by admin

  # Create data subsets for different mayoral administrations
  sqf_bloomberg <- filter(sqf_ols, year >= 2006 & year <= 2013)
  sqf_deblasio <- filter(sqf_ols, year >= 2014 & year <= 2021)
  sqf_adams <- filter(sqf_ols, year >= 2022)

# --- Model 1: basic OLS with fixed effects (without interactions) ----

  # Define function to estimate model 1 across different datasets
  model1 <- function(data) {
    feols(
      sanction ~ Black + Hisp + age + female + 
        RS_furtive + RS_crimloc + RS_casing + RS_other +
        RS_drug + RS_suspobj + RS_appear + RS_violent | 
        pct + year + pct:year + off_cat_broad,
      data = data,
      cluster = ~ pct
    )
  }
  
  # Estimate model 1 for each dataset
  model1_all <- model1(sqf_ols)
  model1_bloomberg <- model1(sqf_bloomberg)
  model1_deblasio <- model1(sqf_deblasio)
  model1_adams <- model1(sqf_adams)
  
  
# --- Model 1b: basic OLS with fixed effects disagreggated by arrests and summons ---

  # Define function to estimate model 1 across different datasets
  
  model1b <- function(data) {
    feols(
      c(arrest, summons, sanction) ~ Black + Hisp + age + female + 
        RS_furtive + RS_crimloc + RS_casing + RS_other +
        RS_drug + RS_suspobj + RS_appear + RS_violent | 
        pct + year + pct:year + off_cat_broad,
      data = data,
      cluster = ~ pct
    )
  }
  
  # Estimate model 1 for each dataset
  model1b_all <- model1b(sqf_ols)
  model1b_bloomberg <- model1b(sqf_bloomberg)
  model1b_deblasio <- model1b(sqf_deblasio)
  model1b_adams <- model1b(sqf_adams)
  
  
# --- Model 2: OLS with (race and suspicion factors) interactions ---
  
  #' (A + B + C)*(X + Y + Z) in fixest() expands to main effects plus all 
  #' interactions between suspicion factors and race dummies automatically.
  
  # Model 2a: OLS for sanction outcome (no interactions)
  model2a <- function(data) { 
    feols(
      sanction ~ 
        RS_furtive + RS_crimloc + RS_casing + RS_other +
        RS_drug + RS_suspobj + RS_appear + RS_violent + 
        Black + Hisp +
        age + female |
        pct + year + pct:year + off_cat_broad, # fixed effects
      data = data,
      cluster = ~pct
    )
  } 
  
  # Estimate model 2 for each dataset
  model2a_all <- model2a(sqf_ols)
  model2a_bloomberg <- model2a(sqf_bloomberg)
  model2a_deblasio <- model2a(sqf_deblasio)
  model2a_adams <- model2a(sqf_adams)
  
  # Model 2b: OLS for sanction outcome (with interactions)
  model2b <- function(data) { 
    feols(
      sanction ~ 
        RS_furtive + RS_crimloc + RS_casing + RS_other +
        RS_drug + RS_suspobj + RS_appear + RS_violent +
        (RS_furtive + RS_crimloc + RS_casing + RS_other +
           RS_drug + RS_suspobj + RS_appear + RS_violent) * Black +
        (RS_furtive + RS_crimloc + RS_casing + RS_other +
           RS_drug + RS_suspobj + RS_appear + RS_violent) * Hisp +
        age + female |
        pct + year + pct:year + off_cat_broad, # fixed effects
      data = data,
      cluster = ~pct
    )
  } 
  
  # Estimate model 2 for each dataset
  model2b_all <- model2b(sqf_ols)
  model2b_bloomberg <- model2b(sqf_bloomberg)
  model2b_deblasio <- model2b(sqf_deblasio)
  model2b_adams <- model2b(sqf_adams)
  
  etable(
    model2b_all,
    model2b_bloomberg,
    model2b_deblasio,
    model2b_adams,
    order = c("^Black$", "^Hisp$", "^age$", "^female$",
              "^RS_furtive$", "^RS_crimloc$", "^RS_casing$", "^RS_other$",
              "^RS_drug$", "^RS_suspobj$", "^RS_appear$", "^RS_violent$"),
    view = TRUE
  )
  
  
  

# --- Model 3: Two-stage model for arrest conditional on sanction ----
  
  
  # Define function to estimate two-stage arrest model
  estimate_arrest_model <- function(data) {
    # Step 1: Estimate the probability of sanction (logit)
    sanction_model <- feglm(
      sanction ~ Black + Hisp + age + female + 
        RS_furtive + RS_crimloc + RS_casing + RS_other +
        RS_drug + RS_suspobj + RS_appear + RS_violent |
        pct + year + pct:year + off_cat_broad,
      data = data,
      cluster = ~ pct,
      family = binomial("logit")
    )
    
    # Append predicted probability of sanction to data frame
    data$prob_sanction <- NA_real_
    prob_hat <- predict(sanction_model, type = "response")
    if (length(prob_hat) == nrow(data)) { #' If the nrows of the predicted probs. 
      #' equals the nrows in the data, then
      data$prob_sanction <- prob_hat      #' directly assign the predicted probs.
      #' to the new column, `prob_sanction`.  
      # Handle missing values in the prediction model
    } else if (!is.null(sanction_model$obs_selection) &&    #' Otherwise, if `obs_selection` exists and has entries,
               length(sanction_model$obs_selection) >= 1) { #' assign the predicted probs. to the selected rows
      idx <- sanction_model$obs_selection[[1]] #' Get the indices of the selected rows
      data$prob_sanction[idx] <- prob_hat      #' Assign the predicted probs. to those rows
    } else {
      stop("Could not align predicted probabilities with input data: model predictions length does not match nrow(data) and obs_selection is unavailable.")
    }
    
    # Sanity check
    sum(is.na(data$prob_sanction)) # NAs should equal dropped obs in Step 1
    summary(data$prob_sanction)
    
    #' `prob_sanction`:
    #' The officer’s predicted propensity to sanction this person, based on 
    #' observable characteristics, suspicion factors, and fixed effects.
    
    # Step 2: Model decision to arrest conditional on sanction
    arrest_model <- feols(
      arrest ~ Black + Hisp + age + female + prob_sanction |
        pct + year + pct:year + off_cat_broad,
      data = subset(data, sanction == 1),
      cluster = ~ pct
    )
    
    #' `arrest_model`:
    #' Given two people who look equally likely to deserve some punishment, 
    #' are officers more likely to arrest one group than another instead of issuing a summons?
    
    return(arrest_model)
  }
  
  # Run model for each dataset
  arrest_model_all <- estimate_arrest_model(sqf_ols)
  arrest_model_bloomberg <- estimate_arrest_model(sqf_bloomberg)
  arrest_model_deblasio <- estimate_arrest_model(sqf_deblasio)
  arrest_model_adams <- estimate_arrest_model(sqf_adams)
  
  
# --- Model prints --- 
  
  # Model 1a
  etable(
    model1_all, model1_bloomberg, model1_deblasio, model1_adams,
    digits = 3,                 # round coefficients to 3 decimals
    signif.code = c("***"=0.001, "**"=0.01, "*"=0.05, "."=0.1), # custom significance
    order = c("Black","Hisp","age","RS_furtive","RS_crimloc","RS_casing",
              "RS_other","RS_drug","RS_suspobj","RS_appear","RS_violent"), # variable order
    dict = c(
      Black = "Black",
      Hisp = "Hispanic",
      age = "Age",
      RS_furtive = "RS: Furtive",
      RS_crimloc = "RS: Crime Location",
      RS_casing = "RS: Casing",
      RS_other = "RS: Other",
      RS_drug = "RS: Drug",
      RS_suspobj = "RS: Suspicious Object",
      RS_appear = "RS: Appearance",
      RS_violent = "RS: Violent"
    ),
    headers = c(
      "All Stops",
      "Bloomberg Admin.",
      "De Blasio Admin.",
      "Adams Admin."
    ),
    view = TRUE
  )
  
  # Model 1b
  etable(
    model1b_all,
    model1b_bloomberg,
    model1b_deblasio,
    model1b_adams,
    keep = c("Black", "Hisp"), view = T
  )
  
  
  # Model 2a
  etable(
    model2a_all,
    model2a_bloomberg,
    model2a_deblasio,
    model2a_adams,
    order = c("^Black$", "^Hisp$", "^age$", "^female$",
              "^RS_furtive$", "^RS_crimloc$", "^RS_casing$", "^RS_other$",
              "^RS_drug$", "^RS_suspobj$", "^RS_appear$", "^RS_violent$"),
    drop = "x",
    view = TRUE
  )
  
  # Model 2b
  etable(
    model2b_all,
    model2b_bloomberg,
    model2b_deblasio,
    model2b_adams,
    order = c("^Black$", "^Hisp$", "^age$", "^female$",
              "^RS_furtive$", "^RS_crimloc$", "^RS_casing$", "^RS_other$",
              "^RS_drug$", "^RS_suspobj$", "^RS_appear$", "^RS_violent$"),
    view = TRUE
  )
  
  # Model 3
  etable(arrest_model_all, arrest_model_bloomberg, arrest_model_deblasio, arrest_model_adams,
         view = TRUE)