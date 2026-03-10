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

# ============================================================
# Main outcome models run two ways:
#   (A) By administration — 3 pooled periods with year dummies
#   (B) By year — annual where viable; failing years (2014-2016,
#       2020-2021) pooled within administration using year dummies
#
# Failing years identified from prior diagnostic runs:
#   Bloomberg: all annual models converge (2009-2013)
#   de Blasio: 2014, 2015, 2016, 2020, 2021 fail annually
#              -> pool as "de Blasio 2014-16" and "de Blasio 2020-21"
#   Adams:     all annual models converge (2022-2024)
# ============================================================

# Administration labels for pooled models
sqf_dummies <- sqf_dummies %>%
  mutate(admin = case_when(
    year %in% 2009:2013 ~ "Bloomberg",
    year %in% 2014:2021 ~ "de Blasio",
    year >= 2022        ~ "Adams",
    TRUE                ~ NA_character_
  ) %>% factor(levels = c("Bloomberg", "de Blasio", "Adams")))

# Year-or-pool label: annual for viable years, pooled for failing years
sqf_dummies <- sqf_dummies %>%
  mutate(year_pool = case_when(
    # Bloomberg — all annual
    year == 2009 ~ "2009",
    year == 2010 ~ "2010",
    year == 2011 ~ "2011",
    year == 2012 ~ "2012",
    year == 2013 ~ "2013",
    # de Blasio — pool failing years
    year %in% 2014:2016 ~ "de Blasio 2014-16",
    year == 2017        ~ "2017",
    year == 2018        ~ "2018",
    year == 2019        ~ "2019",
    year %in% 2020:2021 ~ "de Blasio 2020-21",
    # Adams — all annual
    year == 2022 ~ "2022",
    year == 2023 ~ "2023",
    year == 2024 ~ "2024",
    TRUE ~ NA_character_
  ) %>% factor(levels = c(
    "2009","2010","2011","2012","2013",
    "de Blasio 2014-16","2017","2018","2019","de Blasio 2020-21",
    "2022","2023","2024"
  )))

year_pools <- levels(sqf_dummies$year_pool)
admins     <- c("Bloomberg", "de Blasio", "Adams")

# Helper: run DR models over a grouping variable
run_main <- function(trt_var, group_var, group_levels) {
  map_dfr(group_levels, function(g) {
    d <- sqf_dummies %>%
      filter(.data[[group_var]] == g)
    # Add year dummies if pooled period spans multiple years
    n_years <- n_distinct(d$year)
    if (n_years > 1) {
      d <- d %>%
        fastDummies::dummy_cols(
          select_columns          = "year",
          remove_first_dummy      = TRUE,
          remove_selected_columns = FALSE
        )
      year_dummies <- grep("^year_", names(d), value = TRUE)
      covs <- c(balance_vars, year_dummies)
    } else {
      covs <- balance_vars
    }
    # Keep only covariates that:
    #   (a) exist in d
    #   (b) are not all-NA
    #   (c) are numeric (guards against factor/character columns)
    covs <- covs[map_lgl(covs, function(v) {
      v %in% names(d) &&
        !all(is.na(d[[v]])) &&
        is.numeric(d[[v]])
    })]
    map_dfr(outcomes_main, ~ dr_estimate(d, trt_var, .x, covs)) %>%
      mutate(group = g)
  })
}

# ---- (A) By administration ---------------------------------
cat("Running DR models by administration — Black...\n")
results_black_admin    <- run_main("Black",    "admin", admins)

cat("Running DR models by administration — Hispanic...\n")
results_hispanic_admin <- run_main("Hispanic", "admin", admins)

# ---- (B) Annual / pooled-within-admin ----------------------
cat("Running DR models by year/pool — Black...\n")
results_black_year    <- run_main("Black",    "year_pool", year_pools)

cat("Running DR models by year/pool — Hispanic...\n")
results_hispanic_year <- run_main("Hispanic", "year_pool", year_pools)

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
# Poisson FE regression: stop rate ~ racial composition + controls
# Unit of analysis: precinct x year (annual panel)
# Replicates Fagan (2010) / MacDonald & Braga (2019) Table 6
#
# Inputs required in environment:
#   sqf       — individual-level stop data (from Section 1)
#   pct_crime — precinct x year crime counts
#   pct_demo  — precinct x year demographics
#                 must contain: pct, year, total_pop, pct_black,
#                 pct_hisp, pct_white, median_income,
#                 pct_foreign_born, pct_public_housing
# ============================================================

library(tidyverse)
library(sf)
library(fixest)   # fepois
library(broom)    # tidy()

# ---- 6a. Build annual stop counts per precinct -------------

pct_stops <- sqf %>%
  mutate(pct = as.integer(as.character(precinct))) %>%
  count(pct, year, name = "stops")

# ---- 6b. Build panel ---------------------------------------
# Crime is lagged 1 year: crime in t-1 predicts stops in t
# Rationale: lagged crime captures the underlying crime environment
# that shapes police deployment decisions, not contemporaneous
# endogenous response

panel <- pct_demo %>%
  st_drop_geometry() %>%
  ungroup() %>%          # drop BoroName, pct grouping from pct_demo
  # Stop counts
  left_join(pct_stops, by = c("pct", "year")) %>%
  # Lagged crime: shift pct_crime forward by 1 year before joining
  left_join(
    pct_crime %>%
      mutate(year_join = year + 1L) %>%
      select(pct, year_join,
             crime_Violent, crime_Weapons, crime_Property,
             crime_Drug, crime_Trespass, crime_QualityOfLife,
             crime_Other),
    by = c("pct", "year" = "year_join")
  ) %>%
  mutate(
    pct   = as.factor(pct),
    stops = replace_na(stops, 0L),
    
    # Total lagged crime across all categories
    crime_total_lag = crime_Violent + crime_Weapons + crime_Property +
      crime_Drug    + crime_Trespass +
      crime_QualityOfLife + crime_Other,
    
    # Residual racial group (non-Black, non-Hispanic, non-White)
    pct_other = pmax(0, 1 - pct_black - pct_hisp - pct_white)
  ) %>%
  filter(
    !is.na(total_pop),
    total_pop > 0,
    !is.na(crime_total_lag)
  )

# impute with borough median for each year
panel <- panel %>%
  group_by(BoroName, year) %>%
  mutate(
    median_income    = if_else(is.nan(median_income),
                               median(median_income, na.rm = TRUE),
                               median_income),
    pct_public_housing = if_else(is.nan(pct_public_housing),
                                 median(pct_public_housing, na.rm = TRUE),
                                 pct_public_housing)
  ) %>%
  ungroup()

# Verify NaN resolved
panel %>%
  filter(pct %in% c("50", "114")) %>%
  select(pct, year, median_income, pct_public_housing) %>%
  print(n = 5)

# Then rerun the SES PCA from scratch on the imputed panel
ses_input <- panel %>%
  ungroup() %>%
  select(median_income, pct_foreign_born, pct_public_housing) %>%
  mutate(across(everything(), ~ scale(.x)[, 1]))

ses_complete    <- complete.cases(ses_input)
pca_fit         <- prcomp(ses_input[ses_complete, ],
                          center = TRUE, scale. = TRUE)
panel$ses_index <- NA_real_
panel$ses_index[ses_complete] <- predict(
  pca_fit, newdata = ses_input[ses_complete, ]
)[, 1]

cat("Missing ses_index after imputation:", sum(is.na(panel$ses_index)), "\n")

cat("Panel dimensions:", nrow(panel), "rows |",
    n_distinct(panel$pct), "precincts |",
    n_distinct(panel$year), "years\n")

# ---- 6c. SES index via PCA ---------------------------------
# First principal component of: median income, % foreign born,
# % public housing — mirrors paper's socioeconomic control index

ses_input <- panel %>%
  ungroup() %>%
  select(median_income, pct_foreign_born, pct_public_housing) %>%
  mutate(across(everything(), ~ scale(.x)[, 1]))

ses_complete    <- complete.cases(ses_input)
pca_fit         <- prcomp(ses_input[ses_complete, ],
                          center = TRUE, scale. = TRUE)
panel$ses_index <- NA_real_
panel$ses_index[ses_complete] <- predict(
  pca_fit, newdata = ses_input[ses_complete, ]
)[, 1]

cat("PCA variance explained by PC1:",
    round(summary(pca_fit)$importance[2, 1] * 100, 1), "%\n")
cat("Missing ses_index:", sum(is.na(panel$ses_index)), "\n")

# ---- 6d. Per-year Poisson FE models ------------------------
# Model (per year):
#   log(E[stops] / total_pop) =
#     β1*pct_black + β2*pct_hisp + β3*pct_other +
#     β4*crime_total_lag + β5*ses_index + precinct FE
#
# Key test: is β1 (pct_black) or β2 (pct_hisp) significant
# after controlling for crime, SES, and precinct FE?
# If yes: racial composition predicts stop rates beyond crime levels
# If no: stop rates explained by crime environment alone

panel <- panel %>%
  mutate(admin = case_when(
    year %in% 2009:2013 ~ "Bloomberg",
    year %in% 2014:2021 ~ "de Blasio",
    year >= 2022        ~ "Adams",
    TRUE                ~ NA_character_
  ) %>% factor(levels = c("Bloomberg", "de Blasio", "Adams")))

# ============================================================
# The per-year models are underpowered — too few precincts
# with non-zero stops in low-volume years (2014+).
# Switch to pooled models as primary analysis:
#   (1) Full pooled (precinct + year FE)
#   (2) By administration (precinct + year FE within admin)
# Per-year models only valid for Bloomberg era (2009-2013)
# when stop volume was high enough to populate all precincts.
# ============================================================

# ---- Pooled model: all years -------------------------------
ext_pooled <- fepois(
  stops ~ pct_black + pct_hisp + pct_other +
    crime_total_lag + ses_index | pct + year,
  data    = filter(panel, !is.na(ses_index)),
  offset  = ~ log(total_pop),
  cluster = ~ pct
)
summary(ext_pooled)

# ---- By administration -------------------------------------
ext_admin_models <- map(c("Bloomberg", "de Blasio", "Adams"), function(adm) {
  d <- filter(panel, admin == adm, !is.na(ses_index))
  cat("\nAdmin:", adm, "| N precincts:", n_distinct(d$pct),
      "| N obs:", nrow(d), "\n")
  fepois(
    stops ~ pct_black + pct_hisp + pct_other +
      crime_total_lag + ses_index | pct + year,
    data    = d,
    offset  = ~ log(total_pop),
    cluster = ~ pct
  )
})
names(ext_admin_models) <- c("Bloomberg", "de Blasio", "Adams")
map(ext_admin_models, summary)

# ---- 6e. Extract racial composition coefficients -----------

ext_coefs <- imap_dfr(ext_models, function(mod, nm) {
  tidy(mod, conf.int = TRUE) %>%
    filter(term %in% c("pct_black", "pct_hisp")) %>%
    mutate(year = as.integer(str_remove(nm, "yr_")))
})

print(ext_coefs)

# ---- 6f. Pooled model (precinct + year FE) -----------------
# Single model across all years with precinct and year FE
# Gives overall estimate of racial composition effect

ext_pooled <- fepois(
  stops ~ pct_black + pct_hisp + pct_other +
    crime_total_lag + ses_index | pct + year,
  data    = filter(panel, !is.na(ses_index)),
  offset  = ~ log(total_pop),
  cluster = ~ pct
)

cat("\n=== Pooled model (all years, precinct + year FE) ===\n")
print(summary(ext_pooled))

# ---- 6g. Administration-pooled models ----------------------
# Separate pooled model per mayoral administration
# Mirrors the internal benchmark periodization

panel <- panel %>%
  mutate(admin = case_when(
    year %in% 2009:2013 ~ "Bloomberg",
    year %in% 2014:2021 ~ "de Blasio",
    year >= 2022        ~ "Adams",
    TRUE                ~ NA_character_
  ) %>% factor(levels = c("Bloomberg", "de Blasio", "Adams")))

ext_admin_models <- map(c("Bloomberg", "de Blasio", "Adams"), function(adm) {
  d <- filter(panel, admin == adm, !is.na(ses_index))
  tryCatch(
    fepois(
      stops ~ pct_black + pct_hisp + pct_other +
        crime_total_lag + ses_index | pct + year,
      data    = d,
      offset  = ~ log(total_pop),
      cluster = ~ pct
    ),
    error = function(e) {
      message("Admin model failed for ", adm, ": ", e$message)
      NULL
    }
  )
})
names(ext_admin_models) <- c("Bloomberg", "de Blasio", "Adams")

ext_admin_coefs <- imap_dfr(ext_admin_models, function(mod, nm) {
  if (is.null(mod)) return(NULL)
  tidy(mod, conf.int = TRUE) %>%
    filter(term %in% c("pct_black", "pct_hisp")) %>%
    mutate(admin = nm)
})

cat("\n=== Administration-pooled external benchmark ===\n")
print(ext_admin_coefs)

# ---- 6h. Figure 2: racial composition by year --------------

ggplot(ext_coefs,
       aes(x = factor(year), y = estimate,
           ymin = conf.low, ymax = conf.high,
           color = term, shape = term, group = term)) +
  geom_pointrange(position = position_dodge(0.4), size = 0.5) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey40") +
  scale_color_manual(
    values = c("pct_black" = "black", "pct_hisp" = "grey40"),
    labels = c("pct_black" = "% Black", "pct_hisp" = "% Hispanic")
  ) +
  scale_shape_manual(
    values = c("pct_black" = 16, "pct_hisp" = 15),
    labels = c("pct_black" = "% Black", "pct_hisp" = "% Hispanic")
  ) +
  # Shade administration eras
  annotate("rect", xmin = 0.5,  xmax = 5.5,  ymin = -Inf, ymax = Inf,
           fill = "steelblue", alpha = 0.05) +
  annotate("rect", xmin = 5.5,  xmax = 13.5, ymin = -Inf, ymax = Inf,
           fill = "firebrick", alpha = 0.05) +
  annotate("rect", xmin = 13.5, xmax = Inf,  ymin = -Inf, ymax = Inf,
           fill = "forestgreen", alpha = 0.05) +
  annotate("text", x = 3,    y = Inf, vjust = 1.5,
           label = "Bloomberg", size = 3, color = "steelblue") +
  annotate("text", x = 9.5,  y = Inf, vjust = 1.5,
           label = "de Blasio", size = 3, color = "firebrick") +
  annotate("text", x = 14.5, y = Inf, vjust = 1.5,
           label = "Adams", size = 3, color = "forestgreen") +
  labs(
    title    = "Racial composition as predictor of precinct stop rate by year",
    subtitle = "Poisson FE; offset = log(total_pop); clustered SEs at precinct",
    x = "Year", y = "Coefficient (log stop rate per capita)",
    color = NULL, shape = NULL,
    caption  = "Controls: lagged total crime, SES index (PC1), precinct FE"
  ) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# ---- 6i. Save outputs --------------------------------------

write_csv(ext_coefs,       "output_ext_benchmark_annual.csv")
write_csv(ext_admin_coefs, "output_ext_benchmark_admin.csv")

cat("\nSection 6 complete.\n")


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

cat("\n=== Table 3a: Black vs Similarly-Situated Others (by administration) ===\n")
print(kable(format_dr(results_black_admin %>% rename(year = group), "Black")))

cat("\n=== Table 3b: Black vs Similarly-Situated Others (annual / pooled) ===\n")
print(kable(format_dr(results_black_year %>% rename(year = group), "Black")))

cat("\n=== Table 4a: Hispanic vs Similarly-Situated Others (by administration) ===\n")
print(kable(format_dr(results_hispanic_admin %>% rename(year = group), "Hispanic")))

cat("\n=== Table 4b: Hispanic vs Similarly-Situated Others (annual / pooled) ===\n")
print(kable(format_dr(results_hispanic_year %>% rename(year = group), "Hispanic")))

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
# Hit Rate Visualizations
# hitrates_black and hitrates_hispanic
# Pooled by mayoral administration (Bloomberg, de Blasio, Adams)
# ============================================================

library(tidyverse)

# ---- Combine and prepare -----------------------------------

hitrates <- bind_rows(
  hitrates_black    %>% mutate(group = "Black"),
  hitrates_hispanic %>% mutate(group = "Hispanic")
) %>%
  mutate(
    # Significance flag (p < 0.01 two-tailed, z > 2.576)
    z_stat = log(OR) / OR_SE,
    sig    = case_when(
      abs(z_stat) > 3.291 ~ "p < 0.001",
      abs(z_stat) > 2.576 ~ "p < 0.01",
      abs(z_stat) > 1.960 ~ "p < 0.05",
      TRUE                ~ "n.s."
    ) %>% factor(levels = c("p < 0.001","p < 0.01","p < 0.05","n.s.")),
    
    # 95% CI on OR scale
    OR_lo = exp(log(OR) - 1.96 * OR_SE),
    OR_hi = exp(log(OR) + 1.96 * OR_SE),
    
    # Absolute prevalence gap (percentage points)
    gap_pp = (prev_treated - prev_control_wt) * 100,
    
    # Outcome labels
    outcome_label = recode(outcome,
                           "contrab"  = "Contraband found",
                           "any_weap" = "Weapon found"
    ),
    
    # Period as ordered factor
    period = factor(period,
                    levels = c("Bloomberg", "de Blasio", "Adams")),
    
    # Group label
    group = factor(group, levels = c("Black", "Hispanic"))
  )

# ---- Colour palette ----------------------------------------
admin_cols <- c(
  "Bloomberg" = "#2166ac",
  "de Blasio" = "#d6604d",
  "Adams"     = "#4dac26"
)

sig_shapes <- c(
  "p < 0.001" = 18,   # filled diamond
  "p < 0.01"  = 16,   # filled circle
  "p < 0.05"  = 15,   # filled square
  "n.s."      = 1     # open circle
)

# ============================================================
# FIGURE A: Odds ratios by administration and group
# ============================================================

fig_or <- ggplot(hitrates,
                 aes(x = period, y = OR,
                     ymin = OR_lo, ymax = OR_hi,
                     color = period, shape = sig,
                     group = interaction(group, outcome_label))) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "grey50") +
  geom_pointrange(size = 0.6, linewidth = 0.7,
                  position = position_dodge(0.5)) +
  facet_grid(outcome_label ~ group) +
  scale_color_manual(values = admin_cols, guide = "none") +
  scale_shape_manual(values = sig_shapes, name = "Significance") +
  scale_y_log10(
    breaks = c(0.3, 0.5, 0.75, 1, 1.5, 2, 3),
    labels = c("0.3","0.5","0.75","1","1.5","2","3")
  ) +
  labs(
    title    = "Hit Rate Disparities by Mayoral Administration",
    subtitle = "OR > 1: minority group more likely to yield outcome conditional on search\nOR < 1: minority group less likely (lower evidentiary threshold for search)",
    x        = NULL,
    y        = "Odds Ratio (log scale)",
    caption  = "Doubly robust estimation; entropy balancing on crime type, age, shift, RS basis, gender.\n95% CIs shown. Reference group: White/Other individuals searched in same context."
  ) +
  theme_bw(base_size = 11) +
  theme(
    strip.background = element_rect(fill = "grey92"),
    strip.text       = element_text(face = "bold"),
    panel.grid.minor = element_blank(),
    legend.position  = "bottom"
  )

print(fig_or)
ggsave("fig_hitrates_OR.pdf", fig_or, width = 7, height = 6)
ggsave("fig_hitrates_OR.png", fig_or, width = 7, height = 6, dpi = 300)


# ============================================================
# FIGURE B: Absolute prevalence gap (percentage points)
# Treated minus counterfactual weighted control
# ============================================================

fig_gap <- ggplot(hitrates,
                  aes(x = period, y = gap_pp,
                      fill = period, alpha = sig)) +
  geom_col(position = position_dodge(0.7), width = 0.6) +
  geom_hline(yintercept = 0, linewidth = 0.5, color = "grey30") +
  geom_text(
    aes(label = sprintf("%+.1f pp", gap_pp),
        vjust = if_else(gap_pp >= 0, -0.4, 1.3)),
    position = position_dodge(0.7),
    size = 3, color = "grey20"
  ) +
  facet_grid(outcome_label ~ group) +
  scale_fill_manual(values = admin_cols, name = "Administration") +
  scale_alpha_manual(
    values = c("p < 0.001" = 1, "p < 0.01" = 0.9,
               "p < 0.05" = 0.7, "n.s." = 0.35),
    name = "Significance"
  ) +
  labs(
    title    = "Absolute Hit Rate Gap by Mayoral Administration",
    subtitle = "Positive = minority group more likely to yield outcome;\nNegative = minority group less likely (lower search threshold)",
    x        = NULL,
    y        = "Prevalence gap (percentage points)",
    caption  = "Gap = predicted probability for minority group minus counterfactual (similarly-situated non-minority).\nBars faded when not statistically significant (p > 0.05)."
  ) +
  theme_bw(base_size = 11) +
  theme(
    strip.background = element_rect(fill = "grey92"),
    strip.text       = element_text(face = "bold"),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    legend.position  = "bottom"
  )

print(fig_gap)
ggsave("fig_hitrates_gap.pdf", fig_gap, width = 7, height = 6)
ggsave("fig_hitrates_gap.png", fig_gap, width = 7, height = 6, dpi = 300)


# ============================================================
# FIGURE C: Predicted prevalence — treated vs counterfactual
# Side-by-side bars showing raw rates
# ============================================================

prev_long <- hitrates %>%
  select(group, outcome_label, period, prev_treated, prev_control_wt, sig) %>%
  pivot_longer(
    cols      = c(prev_treated, prev_control_wt),
    names_to  = "type",
    values_to = "prev"
  ) %>%
  mutate(
    type = recode(type,
                  "prev_treated"    = "Minority group",
                  "prev_control_wt" = "Comparison\n(reweighted)"
    ) %>% factor(levels = c("Comparison\n(reweighted)", "Minority group"))
  )

fig_prev <- ggplot(prev_long,
                   aes(x = period, y = prev * 100,
                       fill = type,
                       group = interaction(period, type))) +
  geom_col(position = position_dodge(0.7), width = 0.6) +
  facet_grid(outcome_label ~ group, scales = "free_y") +
  scale_fill_manual(
    values = c("Minority group" = "grey25",
               "Comparison\n(reweighted)" = "grey75"),
    name = NULL
  ) +
  labs(
    title    = "Predicted Hit Rates: Minority Group vs Counterfactual",
    subtitle = "Counterfactual = similarly-situated non-minority individual, entropy-balanced",
    x        = NULL,
    y        = "Predicted probability (%)",
    caption  = "Doubly robust estimation pooled by mayoral administration."
  ) +
  theme_bw(base_size = 11) +
  theme(
    strip.background   = element_rect(fill = "grey92"),
    strip.text         = element_text(face = "bold"),
    panel.grid.minor   = element_blank(),
    panel.grid.major.x = element_blank(),
    legend.position    = "bottom",
    axis.text.x        = element_text(angle = 20, hjust = 1)
  )

print(fig_prev)
ggsave("fig_hitrates_prev.pdf", fig_prev, width = 7, height = 6)
ggsave("fig_hitrates_prev.png", fig_prev, width = 7, height = 6, dpi = 300)

cat("Figures saved: fig_hitrates_OR, fig_hitrates_gap, fig_hitrates_prev\n")


# ============================================================
# SECTION 8: SAVE OUTPUTS
# ============================================================

write_csv(results_black_admin,    "output_table3a_black_admin.csv")
write_csv(results_black_year,     "output_table3b_black_year.csv")
write_csv(results_hispanic_admin, "output_table4a_hispanic_admin.csv")
write_csv(results_hispanic_year,  "output_table4b_hispanic_year.csv")
write_csv(hitrates_black,    "output_table5_hitrates_black.csv")
write_csv(hitrates_hispanic, "output_table5_hitrates_hispanic.csv")
write_csv(balance_black,     "output_table2_balance_black.csv")
write_csv(balance_hispanic,  "output_table2_balance_hispanic.csv")
write_csv(table1,            "output_table1_racial_composition.csv")

cat("\nDone. All outputs written.\n")