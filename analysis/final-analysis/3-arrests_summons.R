library(fixest)
library(dplyr)

# --- load data ----
sqf_all <- readRDS("data/data-final/nypd-stop/sqf_all.rds")

# --- RS flag and force flag definitions ----
RS_flags <- c(
  "BACKROUND_CIRCUMSTANCES_VIOLENT_CRIME_FLAG",
  "BACKROUND_CIRCUMSTANCES_SUSPECT_KNOWN_TO_CARRY_WEAPON_FLAG",
  "BACKROUND_CIRCUMSTANCES_OTHER_SUSPECT_WEAPON_FLAG",
  "BACKROUND_CIRCUMSTANCES_ATTIRE_FLAG",
  "BACKROUND_CIRCUMSTANCES_VIOLENT_CRIME_SUSPECT_FLAG",
  "BACKROUND_CIRCUMSTANCES_RECENT_CRIME_FLAG",
  "BACKROUND_CIRCUMSTANCES_VERBAL_THREATS_FLAG",
  "BACKROUND_CIRCUMSTANCES_FURTIVE_FLAG",
  "BACKROUND_CIRCUMSTANCES_BULGE_FLAG",
  "SUSPECTS_ACTIONS_CASING_FLAG",
  "SUSPECTS_ACTIONS_CONCEALED_POSSESSION_WEAPON_FLAG",
  "SUSPECTS_ACTIONS_DECRIPTION_FLAG",
  "SUSPECTS_ACTIONS_DRUG_TRANSACTIONS_FLAG",
  "SUSPECTS_ACTIONS_IDENTIFY_CRIME_PATTERN_FLAG",
  "SUSPECTS_ACTIONS_LOOKOUT_FLAG",
  "SUSPECTS_ACTIONS_OBJECTS_FLAG",
  "SUSPECTS_ACTIONS_CLOTHING_FLAG",
  "SUSPECTS_ACTIONS_EVASION_FLAG",
  "SUSPECTS_ACTIONS_ASSOCIATION_FLAG",
  "SUSPECTS_ACTIONS_FURTIVE_FLAG",
  "SUSPECTS_ACTIONS_CHANGE_DIRECTION_FLAG",
  "SUSPECTS_ACTIONS_VIOLENT_CRIME_FLAG",
  "SUSPECTS_ACTIONS_INCIDENT_FLAG",
  "SUSPECTS_ACTIONS_TIME_FLAG",
  "SUSPECTS_ACTIONS_SOUND_FLAG",
  "SUSPECTS_ACTIONS_OTHER_FLAG",
  "SUSPECTS_ACTIONS_OTHER_BEHAVIOR_FLAG",
  "SUSPECTS_ACTIONS_PROXIMITY_TO_SCENE_FLAG"
)

# --- build individual-level analysis dataset ----
stops_indiv <- sqf_all %>%
  dplyr::mutate(
    # encode RS flags as binary (sqf_all.rds is pre-prep_vars encoding)
    dplyr::across(
      dplyr::all_of(RS_flags),
      ~ dplyr::if_else(.x == "Y", 1L, 0L, missing = 0L)
    ),
    # outcomes
    arrest   = dplyr::if_else(SUSPECT_ARRESTED_FLAG == "Y", 1L, 0L, missing = 0L),
    summons  = dplyr::if_else(SUMMONS_ISSUED_FLAG   == "Y", 1L, 0L, missing = 0L),
    sanction = dplyr::if_else(arrest == 1L | summons == 1L, 1L, 0L),
    # race dummies (reference = WHITE)
    black      = dplyr::if_else(SUSPECT_RACE_DESCRIPTION == "BLACK",          1L, 0L, missing = 0L),
    hisp_black = dplyr::if_else(SUSPECT_RACE_DESCRIPTION == "HISPANIC-BLACK", 1L, 0L, missing = 0L),
    hisp_white = dplyr::if_else(SUSPECT_RACE_DESCRIPTION == "HISPANIC-WHITE", 1L, 0L, missing = 0L),
    # suspect characteristics
    female = dplyr::if_else(SUSPECT_SEX == "FEMALE", 1L, 0L, missing = 0L),
    age    = as.double(SUSPECT_REPORTED_AGE),
    # identifiers for FE
    pct   = as.integer(STOP_LOCATION_PRECINCT),
    year  = as.integer(YEAR2),
    crime = as.factor(SUSPECTED_CRIME_DESCRIPTION)
  ) %>%
  dplyr::filter(
    SUSPECT_RACE_DESCRIPTION %in% c("BLACK", "HISPANIC-BLACK", "HISPANIC-WHITE", "WHITE"),
    !is.na(pct), !is.na(year), !is.na(age), !is.na(crime)
  )


# --- formula components ----
race_vars <- c("black", "hisp_black", "hisp_white")
demo_vars <- c("age", "female")

rhs_base    <- paste(c(race_vars, demo_vars), collapse = " + ")
rhs_rs      <- paste(c(race_vars, demo_vars, RS_flags), collapse = " + ")

# Model (2): RS flags + interactions with each race dummy
rs_x_black      <- paste(paste0("black:",      RS_flags), collapse = " + ")
rs_x_hisp_black <- paste(paste0("hisp_black:", RS_flags), collapse = " + ")
rs_x_hisp_white <- paste(paste0("hisp_white:", RS_flags), collapse = " + ")
rhs_rs_int <- paste(c(race_vars, demo_vars, RS_flags,
                      rs_x_black, rs_x_hisp_black, rs_x_hisp_white),
                    collapse = " + ")

fe_str <- "| pct^year + crime"


# ===========================================================================
# MODEL SET 1 — SANCTION (arrest or summons)
# ===========================================================================

# (1a) no FE
san_1a <- feols(as.formula(paste("sanction ~", rhs_rs)),
                data = stops_indiv, cluster = ~pct)

# (1b) + precinct×year FE + crime FE
san_1b <- feols(as.formula(paste("sanction ~", rhs_rs, fe_str)),
                data = stops_indiv, cluster = ~pct)

# (2a) RS flag × race interactions, no FE
san_2a <- feols(as.formula(paste("sanction ~", rhs_rs_int)),
                data = stops_indiv, cluster = ~pct)

# (2b) RS flag × race interactions + FE
san_2b <- feols(as.formula(paste("sanction ~", rhs_rs_int, fe_str)),
                data = stops_indiv, cluster = ~pct)

etable(san_1a, san_1b, san_2a, san_2b,
       title   = "OLS: Probability of Sanction",
       headers = c("(1) No FE", "(1) +FE", "(2) No FE", "(2) +FE"),
       keep    = c("black", "hisp_black", "hisp_white", "age", "female"))


# ===========================================================================
# MODEL SET 2 — ARREST
# ===========================================================================

arr_1a <- feols(as.formula(paste("arrest ~", rhs_rs)),
                data = stops_indiv, cluster = ~pct)

arr_1b <- feols(as.formula(paste("arrest ~", rhs_rs, fe_str)),
                data = stops_indiv, cluster = ~pct)

arr_2a <- feols(as.formula(paste("arrest ~", rhs_rs_int)),
                data = stops_indiv, cluster = ~pct)

arr_2b <- feols(as.formula(paste("arrest ~", rhs_rs_int, fe_str)),
                data = stops_indiv, cluster = ~pct)

etable(arr_1a, arr_1b, arr_2a, arr_2b,
       title   = "OLS: Probability of Arrest",
       headers = c("(1) No FE", "(1) +FE", "(2) No FE", "(2) +FE"),
       keep    = c("black", "hisp_black", "hisp_white", "age", "female"))


# ===========================================================================
# MODEL SET 3 — SUMMONS
# ===========================================================================

sum_1a <- feols(as.formula(paste("summons ~", rhs_rs)),
                data = stops_indiv, cluster = ~pct)

sum_1b <- feols(as.formula(paste("summons ~", rhs_rs, fe_str)),
                data = stops_indiv, cluster = ~pct)

sum_2a <- feols(as.formula(paste("summons ~", rhs_rs_int)),
                data = stops_indiv, cluster = ~pct)

sum_2b <- feols(as.formula(paste("summons ~", rhs_rs_int, fe_str)),
                data = stops_indiv, cluster = ~pct)

etable(sum_1a, sum_1b, sum_2a, sum_2b,
       title   = "OLS: Probability of Summons",
       headers = c("(1) No FE", "(1) +FE", "(2) No FE", "(2) +FE"),
       keep    = c("black", "hisp_black", "hisp_white", "age", "female"))


# ===========================================================================
# MODEL SET 4 — TWO-STEP ARREST MODEL
# ===========================================================================
#' Step 1: logit(sanction) on full sample using model (1) specification.
#'         Predicted P(sanction) is added to the arrest model on the
#'         sanctioned subsample (Step 2).

# Step 1: logistic regression for sanction probability
step1_logit <- feglm(
  as.formula(paste("sanction ~", rhs_rs, fe_str)),
  data   = stops_indiv,
  family = binomial(link = "logit"),
  cluster = ~pct
)

# feglm drops singleton FE groups; fitted() is shorter than stops_indiv.
# Recover the kept row indices and assign only to those positions.
removed  <- step1_logit$obs_selection$obsRemoved
obs_kept <- setdiff(seq_len(nrow(stops_indiv)), removed)
stops_indiv$p_sanction <- NA_real_
stops_indiv$p_sanction[obs_kept] <- as.numeric(fitted(step1_logit))

# Step 2: OLS arrest model on sanctioned stops only, conditioning on P(sanction)
rhs_step2 <- paste(c(race_vars, demo_vars, "p_sanction"), collapse = " + ")

step2_no_fe <- feols(
  as.formula(paste("arrest ~", rhs_step2)),
  data    = dplyr::filter(stops_indiv, sanction == 1L, !is.na(p_sanction)),
  cluster = ~pct
)

step2_fe <- feols(
  as.formula(paste("arrest ~", rhs_step2, fe_str)),
  data    = dplyr::filter(stops_indiv, sanction == 1L, !is.na(p_sanction)),
  cluster = ~pct
)

etable(step2_no_fe, step2_fe,
       title   = "Two-Step Arrest Model: Arrest | Sanctioned",
       headers = c("(3) No FE", "(3) +FE"))


# ===========================================================================
# MAYORAL SUBSETS — Model 1b spec (RS flags + FE) by administration
# ===========================================================================

stops_indiv <- stops_indiv |>
  dplyr::mutate(mayor = factor(dplyr::case_when(
    year <= 2013 ~ "Bloomberg",
    year <= 2021 ~ "de Blasio",
    TRUE         ~ "Adams"
  ), levels = c("Bloomberg", "de Blasio", "Adams")))

rhs_rs_local <- paste(c("black", "hisp_black", "hisp_white", "age", "female", RS_flags), collapse = " + ")

fml_san  <- as.formula(paste("sanction ~", rhs_rs_local, fe_str))
fml_arr  <- as.formula(paste("arrest ~",  rhs_rs_local, fe_str))
fml_sum  <- as.formula(paste("summons ~", rhs_rs_local, fe_str))

# --- sanction ----
san_bloomberg <- feols(fml_san, data = dplyr::filter(stops_indiv, mayor == "Bloomberg"), cluster = ~pct)
san_deblasio  <- feols(fml_san, data = dplyr::filter(stops_indiv, mayor == "de Blasio"),  cluster = ~pct)
san_adams     <- feols(fml_san, data = dplyr::filter(stops_indiv, mayor == "Adams"),       cluster = ~pct)

etable(san_bloomberg, san_deblasio, san_adams,
       title   = "Sanction Probability by Mayoral Administration",
       headers = c("Bloomberg\n(2009--2013)", "de Blasio\n(2014--2021)", "Adams\n(2022--)"),
       keep    = c("black", "hisp_black", "hisp_white", "age", "female"))

# --- arrest ----
arr_bloomberg <- feols(fml_arr, data = dplyr::filter(stops_indiv, mayor == "Bloomberg"), cluster = ~pct)
arr_deblasio  <- feols(fml_arr, data = dplyr::filter(stops_indiv, mayor == "de Blasio"),  cluster = ~pct)
arr_adams     <- feols(fml_arr, data = dplyr::filter(stops_indiv, mayor == "Adams"),       cluster = ~pct)

etable(arr_bloomberg, arr_deblasio, arr_adams,
       title   = "Arrest Probability by Mayoral Administration",
       headers = c("Bloomberg\n(2009--2013)", "de Blasio\n(2014--2021)", "Adams\n(2022--)"),
       keep    = c("black", "hisp_black", "hisp_white", "age", "female"))

# --- summons ----
sum_bloomberg <- feols(fml_sum, data = dplyr::filter(stops_indiv, mayor == "Bloomberg"), cluster = ~pct)
sum_deblasio  <- feols(fml_sum, data = dplyr::filter(stops_indiv, mayor == "de Blasio"),  cluster = ~pct)
sum_adams     <- feols(fml_sum, data = dplyr::filter(stops_indiv, mayor == "Adams"),       cluster = ~pct)

etable(sum_bloomberg, sum_deblasio, sum_adams,
       title   = "Summons Probability by Mayoral Administration",
       headers = c("Bloomberg\n(2009--2013)", "de Blasio\n(2014--2021)", "Adams\n(2022--)"),
       keep    = c("black", "hisp_black", "hisp_white", "age", "female"))

# --- Wald tests: do race coefficients differ across mayors? ----
fml_san_int <- as.formula(paste(
  "sanction ~ (black + hisp_black + hisp_white) * mayor + age + female +",
  paste(RS_flags, collapse = " + "), "| pct^year + crime"
))
fml_arr_int <- as.formula(paste(
  "arrest ~ (black + hisp_black + hisp_white) * mayor + age + female +",
  paste(RS_flags, collapse = " + "), "| pct^year + crime"
))
fml_sum_int <- as.formula(paste(
  "summons ~ (black + hisp_black + hisp_white) * mayor + age + female +",
  paste(RS_flags, collapse = " + "), "| pct^year + crime"
))

san_interact <- feols(fml_san_int, data = stops_indiv, cluster = ~pct)
arr_interact <- feols(fml_arr_int, data = stops_indiv, cluster = ~pct)
sum_interact <- feols(fml_sum_int, data = stops_indiv, cluster = ~pct)

wald(san_interact, keep = "(black|hisp).*mayor")
wald(arr_interact, keep = "(black|hisp).*mayor")
wald(sum_interact, keep = "(black|hisp).*mayor")

b_san <- coef(san_interact)
b_arr <- coef(arr_interact)
b_sum <- coef(sum_interact)

races <- c("black", "hisp_black", "hisp_white")
mayors <- c("de Blasio", "Adams")

lapply(list(sanction = b_san, arrest = b_arr, summons = b_sum), function(b) {
  data.frame(
    race      = races,
    bloomberg = b[races],
    deblasio  = b[races] + b[paste0(races, ":mayorde Blasio")],
    adams     = b[races] + b[paste0(races, ":mayorAdams")]
  )
})
