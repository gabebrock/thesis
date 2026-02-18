
# --- Data preparation ----

# Using `sqf_ols` from ~/analysis/test-hitrate.qmd, chunk {r stop-level data prep}
# `sqf_ols` is saved to an rds file in that chunk and referenced here for rendering
sqf_ols <- readRDS("~/Projects/thesis/data/derived/sqf_ols.rds")

# load libraries
library(tidyverse)
library(fixest) # for regressions
library(sf)
library(kableExtra)
library(knitr)


# check for missing values in variables used in models
colSums(is.na(sqf_ols))

# impute zeros for NA values for `sqf_ols$female` 
# (assuming cop would have indicated a female detainee)
sqf_ols$female[is.na(sqf_ols$female)] <- 0

# --- Frisk and Use of Force Analysis ---

# Create new variables for conditional frisk/force

  # variable for unproductive frisk with no search or seizure
  sqf_ols <- sqf_ols %>%
    mutate(unprod_frisk = if_else(
      frisk == 1 & search == 0 & sanction == 0 & any_seiz == 0, 
      1, 0
      ))
  
  # variable for frisk when no suspicion of weapon or violence
  sqf_ols <- sqf_ols %>%
    mutate(
      extra_frisk = if_else(
        frisk == 1 & RS_suspobj == 0 & RS_violent == 0,
        1, 0
      ))
  
  # variable for use of force when no suspicion of weapon or violence
  sqf_ols <- sqf_ols %>%
    mutate(
      extra_force = if_else(
        force == 1 & RS_suspobj == 0 & RS_violent == 0, 
        1, 0
      ))

# Create data subsets for different mayoral administrations
sqf_bloomberg <- dplyr::filter(sqf_ols, year >= 2006 & year <= 2013)
sqf_deblasio <- dplyr::filter(sqf_ols, year >= 2014 & year <= 2021)
sqf_adams <- dplyr::filter(sqf_ols, year >= 2022)

  # Create summary table for frisk and force measures
  measures <- c("frisk", "extra_frisk", "unprod_frisk", "force", "extra_force")
  measure_labels <- c(
    frisk = "Frisk",
    extra_frisk = "Extra Frisk",
    unprod_frisk = "Unproductive Frisk",
    force = "Force",
    extra_force = "Extra Force"
  )

  summarize_measures <- function(df, admin_name) {
    tibble(
      Admin = admin_name,
      Measure = unname(measure_labels[measures]),
      Mean = round(vapply(measures, function(m) mean(df[[m]], na.rm = TRUE), numeric(1)), 3),
      SD = round(vapply(measures, function(m) sd(df[[m]], na.rm = TRUE), numeric(1)), 3)
    )
  }

  frisk_force_cat_table <- dplyr::bind_rows(
    summarize_measures(sqf_ols, "Overall"),
    summarize_measures(sqf_bloomberg, "Bloomberg"),
    summarize_measures(sqf_deblasio, "DeBlasio"),
    summarize_measures(sqf_adams, "Adams")
  )

  
  # Pivot the table so each Admin is a column
  frisk_force_wide <- frisk_force_cat_table %>%
    select(Admin, Measure, Mean) %>%  # focus on Mean (or include SD separately)
    pivot_wider(names_from = Admin, values_from = Mean)
  
  # View nicely
  kable(frisk_force_wide, caption = "Mean Frisk and Force by Administration", digits = 3)

## --- Frisk Analysis --- 

# Unproductive Frisk models
  # All Frisks: Basic OLS with fixed effects (without interactions)
  frisk_model1 <- feols(
    frisk ~ Black + Hisp + age + female +
      RS_furtive + RS_crimloc + RS_casing +
      RS_other + RS_drug + RS_suspobj + RS_appear + RS_violent |
      pct + year + pct:year + off_cat_broad,
    data = sqf_ols,
    cluster = ~ pct
  )
  
  # All Frisks: OLS w/ race x RS interactions
  frisk_model2 <- feols(
    frisk ~
      (RS_furtive + RS_crimloc + RS_casing + RS_other + 
         RS_drug + RS_suspobj + RS_appear + RS_violent) *
      (Black + Hisp) +
      age + female |
      pct + year + pct:year + off_cat_broad,
    data = sqf_ols,
    cluster = ~ pct
  )
  
  sqf_ols_nogeo <- st_drop_geometry(sqf_ols)
  
  # Unproductive Frisks: Basic OLS with fixed effects (without interactions)
  frisk_model3 <- feols(
    unprod_frisk ~ Black + Hisp + age + female +
      RS_furtive + RS_crimloc + RS_casing +
      RS_other + RS_drug + RS_suspobj + RS_appear + RS_violent |
      pct + year + pct:year + off_cat_broad,
    data = subset(sqf_ols_nogeo, frisk == 1),
    cluster = ~ pct
  )
  
  # Unproductive Frisks: OLS w/ race x RS interactions
  frisk_model4 <- feols(
    unprod_frisk ~
      (RS_furtive + RS_crimloc + RS_casing + RS_other + 
         RS_drug + RS_suspobj + RS_appear + RS_violent) *
      (Black + Hisp) +
      age + female |
      pct + year + pct:year + off_cat_broad,
    data = subset(sqf_ols, frisk == 1),
    cluster = ~ pct
  )
  
  etable(
    frisk_model1, frisk_model2,
    frisk_model3, frisk_model4,
    view = TRUE
  )

# Wald tests for interactions
# test the joint nullity of the set of coefficients for race x RS interactions
wald(frisk_model2, keep = "RS_.*:Black")
wald(frisk_model2, keep = "RS_.*:Hisp")

wald(frisk_model4, keep = "RS_.*:Black")
wald(frisk_model4, keep = "RS_.*:Hisp")




## --- Force Analysis ---

  # Any Use of Force: Basic OLS with fixed effects (without interactions)
  force_model1 <- feols(
    force ~ Black + Hisp + age + female +
      RS_furtive + RS_crimloc + RS_casing +
      RS_other + RS_drug + RS_suspobj + RS_appear + RS_violent |
      pct + year + pct:year + off_cat_broad,
    data = sqf_ols,
    cluster = ~ pct
  )
  
  # Any Use of Force: OLS w/ race x RS interactions
  force_model2 <- feols(
    force ~
      (RS_furtive + RS_crimloc + RS_casing + RS_other + 
         RS_drug + RS_suspobj + RS_appear + RS_violent) *
      (Black + Hisp) +
      age + female |
      pct + year + pct:year + off_cat_broad,
    data = sqf_ols,
    cluster = ~ pct
  )
  
  # Extra Use of Force: Basic OLS with fixed effects (without interactions)
  force_model3 <- feols(
    extra_force ~ Black + Hisp + age + female +
      RS_furtive + RS_crimloc + RS_casing +
      RS_other + RS_drug + RS_suspobj + RS_appear + RS_violent |
      pct + year + pct:year + off_cat_broad,
    data = subset(sqf_ols, frisk == 1),
    cluster = ~ pct
  )
  
  # Extra Use of Force: OLS w/ race x RS interactions
  force_model4 <- feols(
    extra_force ~
      (RS_furtive + RS_crimloc + RS_casing + RS_other + 
         RS_drug + RS_suspobj + RS_appear + RS_violent) *
      (Black + Hisp) +
      age + female |
      pct + year + pct:year + off_cat_broad,
    data = subset(sqf_ols, frisk == 1),
    cluster = ~ pct
  )
  
  etable(force_model1, force_model2, force_model3, force_model4,
         tex = TRUE)