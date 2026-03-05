# ============================================================
# Replication Code: MacDonald & Braga (2019)
# "Did Post-Floyd et al. Reforms Reduce Racial Disparities
#  in NYPD Stop, Question, and Frisk Practices?"
# Justice Quarterly, 36(5), 954-983
#
# Adapted to: sqf_all_clean (your actual data object)
# N = 1,492,630 stops
# ============================================================

library(tidyverse)
library(sf)
library(ebal)          # entropy balancing
library(fixest)        # fast Poisson FE (fepois)
library(broom)         # tidy model output
library(fastDummies)   # dummy_cols()
library(knitr)         # kable tables


# ============================================================
# SECTION 1: PREPARE INDIVIDUAL-LEVEL DATA
# ============================================================

sqf <- sqf_all_clean %>%
  st_drop_geometry() %>%
  mutate(
    
    # --- Race indicators ---
    race_cat = case_when(
      race == "BLACK"           ~ "Black",
      race == "HISPANIC"        ~ "Hispanic",
      race == "WHITE"           ~ "White",
      TRUE                      ~ "Other"
    ),
    Black    = as.integer(race_cat == "Black"),
    Hispanic = as.integer(race_cat == "Hispanic"),
    
    # --- Male (already coded as 0/1 via female) ---
    male = 1L - female,
    
    # --- Age groups matching paper ---
    age_grp = cut(
      age,
      breaks = c(-Inf, 9, 15, 19, 24, 34, 64, Inf),
      labels = c("lt10","age1015","age1619",
                 "age2024","age2534","age3564","age65p"),
      right  = TRUE
    ),
    
    # --- Crime category: use all 8 off_cat_broad levels directly ---
    crime_cat = fct_recode(
      off_cat_broad,
      "murder"        = "Murder",
      "violent"       = "Violent",
      "weapons"       = "Weapons",
      "property"      = "Property",
      "drugs"         = "Drug",
      "trespass"      = "Trespass",
      "qualityoflife" = "QualityOfLife",
      "other"         = "Other"
    ),
    
    # --- Precinct as factor ---
    precinct = as.factor(pct),
    
    # --- Month-year label ---
    month_yr = sprintf("%d-%02d", year, month),
    
    # --- Shift from hour (paper uses 3 patrol shifts) ---
    shift = case_when(
      stop_time_hr >= 8  & stop_time_hr < 16 ~ "day",
      stop_time_hr >= 16 & stop_time_hr < 24 ~ "evening",
      TRUE                                    ~ "night"
    ) %>% factor(levels = c("day","evening","night")),
    
    # --- day_stop is binary (0/1) — likely daytime stop indicator ---
    # Use directly; rename for clarity
    daytime_stop = day_stop
    # NOTE: day-of-week is not available in sqf_all_clean.
    # If you have a date column, add:
    #   dow = lubridate::wday(date_col, label = TRUE)
    # and then dummy_cols("dow") below.
  )


# ============================================================
# SECTION 2: BALANCE COVARIATES (for entropy balancing)
# One-hot encode categorical variables
# ============================================================

# Dummy-encode crime_cat, age_grp, shift, precinct
# remove_first_dummy = TRUE drops one category per group to avoid
# the dummy variable trap (perfect collinearity within ebalance)
# Confirmed fix: age dummies had all row sums == 1, breaking ebalance
sqf_dummies <- sqf %>%
  fastDummies::dummy_cols(
    select_columns  = c("crime_cat", "age_grp", "shift", "precinct"),
    remove_first_dummy      = TRUE,   # drop one per group — required for ebalance
    remove_selected_columns = TRUE
  )

# Build covariate list (exclude outcome & ID cols)
# These mirror Table 2 in the paper
balance_vars <- c(
  # Crime type (8 categories)
  grep("^crime_cat_", names(sqf_dummies), value = TRUE),
  # Age groups
  grep("^age_grp_",   names(sqf_dummies), value = TRUE),
  # Gender
  "male",
  # Time of day
  grep("^shift_",     names(sqf_dummies), value = TRUE),
  "daytime_stop",     # binary day/night indicator
  # Reasonable suspicion bases
  grep("^RS_",        names(sqf_dummies), value = TRUE),
  # Precinct fixed effects
  grep("^precinct_",  names(sqf_dummies), value = TRUE)
  # NOTE: add grep("^dow_", ...) here if you derive day-of-week from a date column
)

# Drop any all-NA columns (some RS_ fields are sparse)
balance_vars <- balance_vars[
  map_lgl(balance_vars, ~ !all(is.na(sqf_dummies[[.x]])))
]

# Replace NA in RS_ vars with 0 (not checked = absent)
sqf_dummies <- sqf_dummies %>%
  mutate(across(starts_with("RS_"), ~ replace_na(as.integer(.x > 0), 0L)))


# ============================================================
# SECTION 3: DOUBLY ROBUST ESTIMATION FUNCTION
# Entropy balancing + weighted logistic regression
# Returns OR, SE, and prevalence for treated vs counterfactual
# ============================================================

dr_estimate <- function(data, treatment_var, outcome_var, covariates) {
  
  # Keep treated group + comparison (White/Other)
  d <- data %>%
    filter(.data[[treatment_var]] == 1L |
             race_cat %in% c("White", "Other")) %>%
    mutate(D = .data[[treatment_var]]) %>%
    filter(!is.na(.data[[outcome_var]]))
  
  trt <- d$D
  Y   <- d[[outcome_var]]
  
  # --- Clean covariate matrix before ebalance -----------------
  
  X_mat <- as.matrix(d[, covariates])
  X_mat[is.na(X_mat)] <- 0
  
  # Step 1: drop zero-variance columns (constant in full sample)
  keep <- apply(X_mat, 2, var) > 0
  X_mat <- X_mat[, keep]
  
  # Step 2: drop columns that are constant within EITHER group
  #         (perfect separation — ebalance cannot handle these)
  treated_means <- colMeans(X_mat[trt == 1, , drop = FALSE])
  control_means <- colMeans(X_mat[trt == 0, , drop = FALSE])
  sep <- (treated_means == 0 & control_means == 0) |
    (treated_means == 1 & control_means == 1) |
    (treated_means == 0 & control_means == 1) |
    (treated_means == 1 & control_means == 0)
  X_mat <- X_mat[, !sep, drop = FALSE]
  
  # Step 3: drop near-zero variance columns (var < 0.001)
  #         these are very sparse dummies that cause numerical instability
  nzv <- apply(X_mat, 2, var) >= 0.001
  X_mat <- X_mat[, nzv, drop = FALSE]
  
  # Step 4: drop linearly dependent columns (rank deficiency, full matrix)
  qr_fit <- qr(X_mat)
  X_mat  <- X_mat[, qr_fit$pivot[seq_len(qr_fit$rank)], drop = FALSE]
  
  # Step 5: drop columns redundant in the CONTROL matrix specifically
  # ebalance checks rank of controls only — precinct dummies with very
  # few control observations cause collinearity here even when the full
  # matrix is full rank (confirmed in failing years 2014-2022)
  X_ctrl_check <- X_mat[trt == 0, , drop = FALSE]
  qr_ctrl      <- qr(X_ctrl_check)
  if (qr_ctrl$rank < ncol(X_ctrl_check)) {
    keep_idx <- qr_ctrl$pivot[seq_len(qr_ctrl$rank)]
    dropped  <- colnames(X_mat)[qr_ctrl$pivot[(qr_ctrl$rank + 1):ncol(X_ctrl_check)]]
    dropped  <- dropped[!is.na(dropped)]
    if (length(dropped) > 0) {
      message("  Dropping ", length(dropped),
              " control-collinear columns: ",
              paste(dropped, collapse = ", "))
    }
    X_mat <- X_mat[, keep_idx, drop = FALSE]
  }
  
  cat("Balancing on", ncol(X_mat), "covariates for",
      treatment_var, "~", outcome_var, "\n")
  
  # --- Entropy balancing --------------------------------------
  eb <- tryCatch(
    ebalance(Treatment = trt, X = X_mat, print.level = 0),
    error = function(e) {
      message("ebalance failed for ", treatment_var, " ~ ", outcome_var,
              ": ", e$message)
      NULL
    }
  )
  
  if (is.null(eb)) {
    return(tibble(treatment = treatment_var, outcome = outcome_var,
                  OR = NA, OR_SE = NA,
                  prev_treated = NA, prev_control_wt = NA,
                  n_treated = sum(trt == 1), n_control = sum(trt == 0)))
  }
  
  # Weights: 1 for treated, EB weight for controls
  w <- rep(1.0, nrow(d))
  w[trt == 0] <- eb$w
  
  # --- Doubly robust logistic regression ---
  formula_dr <- reformulate(
    termlabels = c("D", colnames(X_mat)),
    response   = outcome_var
  )
  
  mod <- glm(formula_dr, data = d, weights = w,
             family = binomial(link = "logit"),
             control = list(maxit = 100))
  
  # ATT: predict under observed race vs counterfactual
  d_treated    <- filter(d, D == 1)
  d_counter    <- mutate(d_treated, D = 0)
  
  p1 <- mean(predict(mod, newdata = d_treated, type = "response"),
             na.rm = TRUE)
  p0 <- mean(predict(mod, newdata = d_counter, type = "response"),
             na.rm = TRUE)
  
  OR    <- exp(coef(mod)["D"])
  OR_SE <- sqrt(vcov(mod)["D", "D"]) * OR   # delta method
  
  tibble(
    treatment       = treatment_var,
    outcome         = outcome_var,
    OR              = OR,
    OR_SE           = OR_SE,
    prev_treated    = p1,
    prev_control_wt = p0,
    n_treated       = sum(trt == 1),
    n_control       = sum(trt == 0)
  )
}


# ============================================================
# SECTION 4: RUN INTERNAL BENCHMARKING BY YEAR
# Tables 3 & 4 (stop outcomes) and Table 5 (hit rates)
# ============================================================

outcomes_main    <- c("frisk","search","summons","arrest","force")
outcomes_hitrate <- c("contrab","any_weap")   # hit rates from searches

# crime_cat levels: murder, violent, weapons, property, drugs, trespass, qualityoflife, other

years <- sort(unique(sqf_dummies$year))

# Leaner covariate set for hit rate models (no precinct dummies)
# Rationale: hit rate models restrict to searched individuals only,
# leaving control groups as small as n=99 (2016) — too few to support
# 70+ precinct dummies. Conditioning on search already implicitly
# controls for location because both groups had to clear the same
# officer's search threshold in the same location. The hit rate
# question is purely: conditional on being searched, does race predict
# finding contraband/weapons? Balance on stop context only.
# Limitation: should be noted when reporting results.
balance_vars_hitrate <- c(
  grep("^crime_cat_", names(sqf_dummies), value = TRUE),
  grep("^age_grp_",   names(sqf_dummies), value = TRUE),
  grep("^shift_",     names(sqf_dummies), value = TRUE),
  grep("^RS_",        names(sqf_dummies), value = TRUE),
  "male", "daytime_stop"
)

# Apply same all-NA filter as balance_vars
balance_vars_hitrate <- balance_vars_hitrate[
  map_lgl(balance_vars_hitrate, ~ !all(is.na(sqf_dummies[[.x]])))
]

# ---- Table 3: Black vs similarly-situated others -----------
cat("Running DR models for Black...\n")
results_black <- map_dfr(years, function(yr) {
  d <- filter(sqf_dummies, year == yr)
  map_dfr(outcomes_main, ~ dr_estimate(d, "Black", .x, balance_vars)) %>%
    mutate(year = yr)
})

# ---- Table 4: Hispanic vs similarly-situated others --------
cat("Running DR models for Hispanic...\n")
results_hispanic <- map_dfr(years, function(yr) {
  d <- filter(sqf_dummies, year == yr)
  map_dfr(outcomes_main, ~ dr_estimate(d, "Hispanic", .x, balance_vars)) %>%
    mutate(year = yr)
})

# ---- Table 5: Hit rates (searches only) --------------------
# Hit rate models pool years into 2-year blocks within mayoral
# administrations. Annual models fail in most years because the
# search-restricted control group (non-Black searched individuals)
# is too small to support entropy balancing even without precinct
# dummies. Pooling is theoretically justified — mayoral administrations
# represent distinct SQF policy regimes (Bloomberg, de Blasio, Adams).
#
# Periods:
#   Bloomberg: 2008-09, 2010-11, 2012-13
#   de Blasio: 2014-15, 2016-17, 2018-19, 2020-21
#   Adams:     2022-23, 2024-25
# All periods have n_control >= 500 after search == 1 restriction.

# Hit rate models pool by mayoral administration.
# Annual and 2-year models failed due to insufficient control group
# sizes in the search-restricted sample. Administration-level pooling
# is theoretically justified — each mayor represented a distinct
# SQF policy regime. Year dummies within each period absorb
# within-administration temporal variation.
#   Bloomberg: 2009-2013
#   de Blasio: 2014-2021
#   Adams:     2022-present
sqf_dummies <- sqf_dummies %>%
  mutate(period = case_when(
    year %in% 2009:2013 ~ "Bloomberg",
    year %in% 2014:2021 ~ "de Blasio",
    year >= 2022        ~ "Adams",
    TRUE                ~ NA_character_
  ) %>% factor(levels = c("Bloomberg", "de Blasio", "Adams")))

periods <- c("Bloomberg", "de Blasio", "Adams")

cat("Running DR hit rate models (pooled by mayoral period)...\n")

run_hitrates <- function(trt_var) {
  map_dfr(periods, function(p) {
    d <- sqf_dummies %>%
      filter(period == p, search == 1,
             .data[[trt_var]] == 1 | race_cat %in% c("White", "Other"))
    # Add period as a covariate to absorb within-period year variation
    d <- d %>%
      fastDummies::dummy_cols(
        select_columns       = "year",
        remove_first_dummy   = TRUE,
        remove_selected_columns = FALSE
      )
    year_dummies <- grep("^year_", names(d), value = TRUE)
    covs <- c(balance_vars_hitrate, year_dummies)
    covs <- covs[map_lgl(covs, ~ .x %in% names(d) && !all(is.na(d[[.x]])))]
    
    map_dfr(outcomes_hitrate,
            ~ dr_estimate(d, trt_var, .x, covs)) %>%
      mutate(period = p)
  })
}

hitrates_black    <- run_hitrates("Black")
hitrates_hispanic <- run_hitrates("Hispanic")


# ============================================================
# SECTION 5: BALANCE DIAGNOSTICS (Table 2)
# Standardized differences before & after entropy weighting
# ============================================================

check_balance <- function(data, trt_var, covariates, yr) {
  d <- data %>%
    filter(year == yr,
           .data[[trt_var]] == 1 | race_cat %in% c("White", "Other")) %>%
    mutate(D = .data[[trt_var]])
  
  X_mat <- as.matrix(d[, covariates])
  X_mat[is.na(X_mat)] <- 0
  keep  <- apply(X_mat, 2, var) > 0
  X_mat <- X_mat[, keep]
  trt   <- d$D
  
  eb <- ebalance(Treatment = trt, X = X_mat, print.level = 0)
  
  w <- rep(1.0, nrow(d))
  w[trt == 0] <- eb$w
  
  std_diff <- function(x, trt, w = NULL) {
    m1 <- mean(x[trt == 1], na.rm = TRUE)
    m0 <- if (is.null(w)) mean(x[trt == 0], na.rm = TRUE) else
      weighted.mean(x[trt == 0], w[trt == 0], na.rm = TRUE)
    s1 <- sd(x[trt == 1], na.rm = TRUE)
    if (s1 == 0) return(0)
    (m1 - m0) / s1
  }
  
  tibble(
    variable         = colnames(X_mat),
    mean_treated     = colMeans(X_mat[trt == 1, ], na.rm = TRUE),
    mean_control_raw = colMeans(X_mat[trt == 0, ], na.rm = TRUE),
    mean_control_wt  = apply(X_mat[trt == 0, ], 2,
                             function(x) weighted.mean(x, eb$w, na.rm = TRUE)),
    SD_before        = map_dbl(colnames(X_mat),
                               ~ std_diff(X_mat[, .x], trt)),
    SD_after         = map_dbl(colnames(X_mat),
                               ~ std_diff(X_mat[, .x], trt, w))
  )
}

# Run for first available year as demonstration (matches Table 2)
yr_demo <- min(years)
balance_black    <- check_balance(sqf_dummies, "Black",    balance_vars, yr_demo)
balance_hispanic <- check_balance(sqf_dummies, "Hispanic", balance_vars, yr_demo)

cat("\n--- Balance diagnostics (Black), year =", yr_demo, "---\n")
print(balance_black, n = 20)


# ============================================================
# SECTION 6: EXTERNAL BENCHMARKING
# Requires place-level panel (stop rate ~ % Black/Hispanic + controls)
# If you have census tract / block-level crime & demographics,
# plug them in here. Structure shown below.
# ============================================================

# ---- 6a. Build monthly stop counts per precinct ------------
# (Precinct is the coarsest geographic unit reliably in your data)
# Swap for census block if you have geocoded crime data

precinct_monthly <- sqf %>%
  count(precinct, year, month, month_yr, name = "stops") %>%
  arrange(precinct, year, month)

# ---- 6b. Lag crime by 1 month within precinct --------------
# You need a separate crime dataset aggregated to precinct x month
# Example structure:
#   crime_monthly: precinct, year, month, month_yr, crime_count

# Uncomment and adapt once crime panel is available:
# precinct_panel <- precinct_monthly %>%
#   left_join(crime_monthly, by = c("precinct","year","month")) %>%
#   group_by(precinct) %>%
#   arrange(year, month) %>%
#   mutate(crime_lag1 = lag(crime_count, 1)) %>%
#   ungroup() %>%
#   left_join(precinct_demographics,   # % Black, % Hispanic, SES
#             by = "precinct")

# ---- 6c. Poisson FE model (Table 6 analog) -----------------
# Uncomment once precinct_panel is ready:

# ext_models <- map(years, function(yr) {
#   d <- filter(precinct_panel, year == yr, pop_total > 0)
#   fepois(
#     stops ~ pct_black + pct_hispanic + pct_other +
#             crime_lag1 + ses_index |
#             precinct + month_yr,
#     data    = d,
#     offset  = ~ log(pop_total),
#     cluster = ~ precinct
#   )
# })
# names(ext_models) <- paste0("yr_", years)
# map(ext_models, summary)

# ---- 6d. Figure 2 analog -----------------------------------
# ext_coefs <- imap_dfr(ext_models, function(mod, nm) {
#   tidy(mod, conf.int = TRUE) %>%
#     filter(term %in% c("pct_black","pct_hispanic")) %>%
#     mutate(year = as.integer(str_remove(nm, "yr_")))
# })
#
# ggplot(ext_coefs,
#        aes(x = factor(year), y = estimate,
#            ymin = conf.low, ymax = conf.high,
#            color = term, group = term)) +
#   geom_pointrange(position = position_dodge(0.3)) +
#   geom_hline(yintercept = 0, linetype = "dashed") +
#   labs(title = "Racial composition as predictor of stop rate by year",
#        x = "Year", y = "Coefficient (log stop rate)",
#        color = NULL) +
#   theme_bw()


# ============================================================
# SECTION 7: FORMATTED OUTPUT TABLES
# ============================================================

# ---- Table 1: Racial composition of stops by year ----------
table1 <- sqf %>%
  count(year, race_cat) %>%
  group_by(year) %>%
  mutate(total = sum(n), pct = round(100 * n / total, 1)) %>%
  ungroup() %>%
  select(year, race_cat, n, pct) %>%
  pivot_wider(
    names_from  = race_cat,
    values_from = c(n, pct),
    names_glue  = "{race_cat}_{.value}"
  ) %>%
  arrange(year)

cat("\n=== Table 1: Racial distribution by year ===\n")
print(kable(table1, digits = 1))


# ---- Tables 3 & 4: DR results formatted --------------------
format_dr <- function(df, label) {
  df %>%
    mutate(
      sig   = if_else(!is.na(OR_SE) & abs(log(OR)) / OR_SE > 2.576, "**", ""),
      OR_fmt = sprintf("%.3f%s", OR, sig),
      SE_fmt = sprintf("(%.4f)", OR_SE),
      prev_treated    = round(prev_treated, 4),
      prev_control_wt = round(prev_control_wt, 4)
    ) %>%
    select(year, outcome, OR_fmt, SE_fmt, prev_treated, prev_control_wt) %>%
    arrange(year, outcome)
}

cat("\n=== Table 3: Black vs Similarly-Situated Others ===\n")
print(kable(format_dr(results_black, "Black")))

cat("\n=== Table 4: Hispanic vs Similarly-Situated Others ===\n")
print(kable(format_dr(results_hispanic, "Hispanic")))

# Hit rate tables use period instead of year
format_dr_period <- function(df) {
  df %>%
    mutate(
      sig    = if_else(!is.na(OR_SE) & abs(log(OR)) / OR_SE > 2.576, "**", ""),
      OR_fmt = sprintf("%.3f%s", OR, sig),
      SE_fmt = sprintf("(%.4f)", OR_SE),
      prev_treated    = round(prev_treated, 4),
      prev_control_wt = round(prev_control_wt, 4)
    ) %>%
    select(period, outcome, OR_fmt, SE_fmt,
           prev_treated, prev_control_wt, n_treated, n_control) %>%
    arrange(period, outcome)
}

cat("\n=== Table 5: Hit Rates — Black (pooled by mayoral period) ===\n")
print(kable(format_dr_period(hitrates_black)))

cat("\n=== Table 5: Hit Rates — Hispanic (pooled by mayoral period) ===\n")
print(kable(format_dr_period(hitrates_hispanic)))


# ============================================================
# SECTION 8: SAVE OUTPUTS
# ============================================================

write_csv(results_black,     "output_table3_black_dr.csv")
write_csv(results_hispanic,  "output_table4_hispanic_dr.csv")
write_csv(hitrates_black,    "output_table5_hitrates_black.csv")
write_csv(hitrates_hispanic, "output_table5_hitrates_hispanic.csv")
write_csv(balance_black,     "output_table2_balance_black.csv")
write_csv(balance_hispanic,  "output_table2_balance_hispanic.csv")
write_csv(table1,            "output_table1_racial_composition.csv")

cat("\nDone. All outputs written.\n")