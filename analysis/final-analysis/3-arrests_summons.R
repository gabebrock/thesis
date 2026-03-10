library(fixest)
library(dplyr)

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
    crime = as.factor(SUSPECTED_CRIME_DESCRIPTION),
    hour  = as.integer(substr(STOP_FRISK_TIME, 1, 2)),
    # within-precinct location controls (NY State Plane feet, scaled to km)
    loc_x = as.numeric(STOP_LOCATION_X) / 1e3,
    loc_y = as.numeric(STOP_LOCATION_Y) / 1e3
  ) %>%
  dplyr::filter(
    SUSPECT_RACE_DESCRIPTION %in% c("BLACK", "HISPANIC-BLACK", "HISPANIC-WHITE", "WHITE"),
    !is.na(pct), !is.na(year), !is.na(age), !is.na(crime)
  )

stops_indiv <- stops_indiv %>%                                                   
  dplyr::mutate(                                                                 
    # 1. Fits Description                                                      
    RS_fits_desc   = pmax(SUSPECTS_ACTIONS_DECRIPTION_FLAG,
                          BACKROUND_CIRCUMSTANCES_VIOLENT_CRIME_SUSPECT_FLAG),
    
    # 2. Evasive/Furtive
    RS_evasive     = pmax(SUSPECTS_ACTIONS_FURTIVE_FLAG,
                          BACKROUND_CIRCUMSTANCES_FURTIVE_FLAG,
                          SUSPECTS_ACTIONS_EVASION_FLAG,
                          SUSPECTS_ACTIONS_CHANGE_DIRECTION_FLAG),
    
    # 3. Crime Location
    RS_crime_loc   = pmax(BACKROUND_CIRCUMSTANCES_RECENT_CRIME_FLAG,
                          SUSPECTS_ACTIONS_TIME_FLAG,
                          SUSPECTS_ACTIONS_IDENTIFY_CRIME_PATTERN_FLAG,
                          SUSPECTS_ACTIONS_PROXIMITY_TO_SCENE_FLAG),
    
    # 4. Casing
    RS_casing      = pmax(SUSPECTS_ACTIONS_CASING_FLAG,
                          SUSPECTS_ACTIONS_LOOKOUT_FLAG),
    
    # 5. Other
    RS_other       = pmax(SUSPECTS_ACTIONS_OTHER_FLAG,
                          SUSPECTS_ACTIONS_OTHER_BEHAVIOR_FLAG,
                          SUSPECTS_ACTIONS_INCIDENT_FLAG),
    
    # 6. Drug Transaction
    RS_drug        = SUSPECTS_ACTIONS_DRUG_TRANSACTIONS_FLAG,
    
    # 7. Suspicious Object
    RS_susp_obj    = pmax(BACKROUND_CIRCUMSTANCES_BULGE_FLAG,
                          BACKROUND_CIRCUMSTANCES_ATTIRE_FLAG,
                          BACKROUND_CIRCUMSTANCES_OTHER_SUSPECT_WEAPON_FLAG,
                          SUSPECTS_ACTIONS_CONCEALED_POSSESSION_WEAPON_FLAG,
                          SUSPECTS_ACTIONS_CLOTHING_FLAG),
    
    # 8. Criminal Appearances
    RS_crim_appear = pmax(SUSPECTS_ACTIONS_ASSOCIATION_FLAG,
                          SUSPECTS_ACTIONS_SOUND_FLAG,
                          SUSPECTS_ACTIONS_OBJECTS_FLAG,
                          
                          BACKROUND_CIRCUMSTANCES_SUSPECT_KNOWN_TO_CARRY_WEAPON_FLAG),
    
    # 9. Violent Crime
    RS_violent     = pmax(SUSPECTS_ACTIONS_VIOLENT_CRIME_FLAG,
                          BACKROUND_CIRCUMSTANCES_VIOLENT_CRIME_FLAG,
                          BACKROUND_CIRCUMSTANCES_VERBAL_THREATS_FLAG)
  )

RS_factors <- c("RS_fits_desc", "RS_evasive", "RS_crime_loc", "RS_casing",
                "RS_other", "RS_drug", "RS_susp_obj", "RS_crim_appear",
                "RS_violent")


# --- formula components ----
race_vars <- c("black", "hisp_black", "hisp_white")
demo_vars <- c("age", "female")
loc_vars  <- c("loc_x", "loc_y")

rhs_base <- paste(c(race_vars, demo_vars, loc_vars), collapse = " + ")
rhs_rs   <- paste(c(race_vars, demo_vars, RS_factors, loc_vars), collapse = " + ")

# FE progresson:
# (1) time + crime + hour FE, no RS flags
# (2) + precinct FE, RS flags included
# (3) precinct x time FE (precinct-specific time trends), RS flags included
fe_1 <- "| year + crime + hour"
fe_2 <- "| pct + year + crime + hour"
fe_3 <- "| pct^year + crime + hour"


# ===========================================================================
# MODEL SET 1 — SANCTION (arrest or summons)
# ===========================================================================

san_1 <- feols(as.formula(paste("sanction ~", rhs_base, fe_1)),
               data = stops_indiv, cluster = ~pct)
san_2 <- feols(as.formula(paste("sanction ~", rhs_rs,   fe_2)),
               data = stops_indiv, cluster = ~pct)
san_3 <- feols(as.formula(paste("sanction ~", rhs_rs,   fe_3)),
               data = stops_indiv, cluster = ~pct)

etable(san_1, san_2, san_3,
       title    = "OLS: Probability of Sanction",
       headers  = c("OLS (1)", "FE (2)", "FE (3)"),
       keep     = c("black", "hisp_black", "hisp_white"),
       se.below = FALSE,
       fitstat  = ~n,
       view     = TRUE)


# ===========================================================================
# MODEL SET 2 — ARREST
# ===========================================================================

arr_1 <- feols(as.formula(paste("arrest ~", rhs_base, fe_1)),
               data = stops_indiv, cluster = ~pct)
arr_2 <- feols(as.formula(paste("arrest ~", rhs_rs,   fe_2)),
               data = stops_indiv, cluster = ~pct)
arr_3 <- feols(as.formula(paste("arrest ~", rhs_rs,   fe_3)),
               data = stops_indiv, cluster = ~pct)

etable(arr_1, arr_2, arr_3,
       title   = "OLS: Probability of Arrest",
       headers = c("OLS (1)", "FE (2)", "FE (3)"),
       keep    = c("black", "hisp_black", "hisp_white"),
       se.below = FALSE,
       fitstat  = ~n,
       view = T)


# ===========================================================================
# MODEL SET 3 — SUMMONS
# ===========================================================================

sum_1 <- feols(as.formula(paste("summons ~", rhs_base, fe_1)),
               data = stops_indiv, cluster = ~pct)
sum_2 <- feols(as.formula(paste("summons ~", rhs_rs,   fe_2)),
               data = stops_indiv, cluster = ~pct)
sum_3 <- feols(as.formula(paste("summons ~", rhs_rs,   fe_3)),
               data = stops_indiv, cluster = ~pct)

etable(sum_1, sum_2, sum_3,
       title   = "OLS: Probability of Summons",
       headers = c("OLS (1)", "FE (2)", "FE (3)"),
       keep    = c("black", "hisp_black", "hisp_white"),
       se.below = FALSE,
       fitstat  = ~n,
       view = T)


# ===========================================================================
# MODEL SET 4 — TWO-STEP ARREST MODEL
# ===========================================================================
#' Step 1: logit(sanction) on full sample using model (1) specification.
#'         Predicted P(sanction) is added to the arrest model on the
#'         sanctioned subsample (Step 2).

# Step 1: OLS LPM of sanction (estimated across all stops)
step1_lpm <- feols(
  as.formula(paste("sanction ~", rhs_rs, fe_3)),
  data    = stops_indiv,
  cluster = ~pct
)

# feols also drops singleton FE groups; recover kept row indices
removed  <- step1_lpm$obs_selection$obsRemoved
obs_kept <- setdiff(seq_len(nrow(stops_indiv)), removed)
stops_indiv$p_sanction <- NA_real_
stops_indiv$p_sanction[obs_kept] <- as.numeric(fitted(step1_lpm))

# Step 2: OLS arrest model on sanctioned stops only, conditioning on P(sanction)
rhs_step2 <- paste(c(race_vars, demo_vars, "p_sanction"), collapse = " + ")

step2_no_fe <- feols(
  as.formula(paste("arrest ~", rhs_step2)),
  data    = dplyr::filter(stops_indiv, sanction == 1L, !is.na(p_sanction)),
  cluster = ~pct
)

step2_fe <- feols(
  as.formula(paste("arrest ~", rhs_step2, fe_3)),
  data    = dplyr::filter(stops_indiv, sanction == 1L, !is.na(p_sanction)),
  cluster = ~pct
)

etable(step2_no_fe, step2_fe,
       title   = "Two-Step Arrest Model: Arrest | Sanctioned",
       headers = c("(3) No FE", "(3) +FE"),
       keep = c("black", "hisp_black", "hisp_white"),
       se.below = FALSE,
       fitstat  = ~n,
       view = T)


# ===========================================================================
# MAYORAL SUBSETS — Model 1b spec (RS flags + FE) by administration
# ===========================================================================

stops_indiv <- stops_indiv |>
  dplyr::mutate(mayor = factor(dplyr::case_when(
    year <= 2013 ~ "Bloomberg",
    year <= 2021 ~ "de Blasio",
    TRUE         ~ "Adams"
  ), levels = c("Bloomberg", "de Blasio", "Adams")))

rhs_rs_local <- paste(c("black", "hisp_black", "hisp_white", "age", "female", RS_factors), collapse = " + ")

fe_3_mayor <- "| pct^year + crime"   # hour dropped: missing in Adams-era data

fml_san  <- as.formula(paste("sanction ~", rhs_rs_local, fe_3_mayor))
fml_arr  <- as.formula(paste("arrest ~",  rhs_rs_local, fe_3_mayor))
fml_sum  <- as.formula(paste("summons ~", rhs_rs_local, fe_3_mayor))

# --- sanction ----
dat_bloomberg <- dplyr::filter(stops_indiv, mayor == "Bloomberg")
dat_deblasio  <- dplyr::filter(stops_indiv, mayor == "de Blasio")
dat_adams     <- dplyr::filter(stops_indiv, mayor == "Adams")

san_bloomberg <- feols(fml_san, data = dat_bloomberg, cluster = ~pct)
san_deblasio  <- feols(fml_san, data = dat_deblasio,  cluster = ~pct)
san_adams     <- feols(fml_san, data = dat_adams,      cluster = ~pct)

etable(san_bloomberg, san_deblasio, san_adams,
       title   = "Sanction Probability by Mayoral Administration",
       headers = c("Bloomberg\n(2009--2013)", "de Blasio\n(2014--2021)", "Adams\n(2022--)"),
       keep    = c("black", "hisp_black", "hisp_white", "age", "female"))

# --- arrest ----
arr_bloomberg <- feols(fml_arr, data = dat_bloomberg, cluster = ~pct)
arr_deblasio  <- feols(fml_arr, data = dat_deblasio,  cluster = ~pct)
arr_adams     <- feols(fml_arr, data = dat_adams,      cluster = ~pct)

etable(arr_bloomberg, arr_deblasio, arr_adams,
       title   = "Arrest Probability by Mayoral Administration",
       headers = c("Bloomberg\n(2009--2013)", "de Blasio\n(2014--2021)", "Adams\n(2022--)"),
       keep    = c("black", "hisp_black", "hisp_white", "age", "female"))

# --- summons ----
sum_bloomberg <- feols(fml_sum, data = dat_bloomberg, cluster = ~pct)
sum_deblasio  <- feols(fml_sum, data = dat_deblasio,  cluster = ~pct)
sum_adams     <- feols(fml_sum, data = dat_adams,      cluster = ~pct)

etable(sum_bloomberg, sum_deblasio, sum_adams,
       title   = "Summons Probability by Mayoral Administration",
       headers = c("Bloomberg\n(2009--2013)", "de Blasio\n(2014--2021)", "Adams\n(2022--)"),
       keep    = c("black", "hisp_black", "hisp_white", "age", "female"))

# --- two-step arrest model by mayor ----
#' Step 1: LPM sanction on all stops within administration.
#' Step 2: OLS arrest | sanctioned, conditioning on p_sanction.
#' Uses fe_3_mayor (no hour FE) for consistency with mayoral subset models.

two_step_mayor <- function(dat) {
  rhs_s1 <- paste(c(race_vars, demo_vars, RS_factors, loc_vars), collapse = " + ")

  s1 <- feols(as.formula(paste("sanction ~", rhs_s1, fe_3_mayor)),
              data = dat, cluster = ~pct)

  removed  <- s1$obs_selection$obsRemoved
  obs_kept <- setdiff(seq_len(nrow(dat)), removed)
  dat$p_sanction <- NA_real_
  dat$p_sanction[obs_kept] <- as.numeric(fitted(s1))

  rhs_s2 <- paste(c(race_vars, demo_vars, loc_vars, "p_sanction"), collapse = " + ")

  feols(as.formula(paste("arrest ~", rhs_s2, fe_3_mayor)),
        data    = dplyr::filter(dat, sanction == 1L, !is.na(p_sanction)),
        cluster = ~pct)
}

ts_bloomberg <- two_step_mayor(dat_bloomberg)
ts_deblasio  <- two_step_mayor(dat_deblasio)
ts_adams     <- two_step_mayor(dat_adams)

etable(ts_bloomberg, ts_deblasio, ts_adams,
       title   = "Two-Step Arrest Model by Mayoral Administration",
       headers = c("Bloomberg\n(2009--2013)", "de Blasio\n(2014--2021)", "Adams\n(2022--2024)"),
       keep    = c("black", "hisp_black", "hisp_white"),
       se.below = FALSE,
       fitstat  = ~n,
       view     = TRUE)


# --- Wald tests: do race coefficients differ across mayors? ----
fml_san_int <- as.formula(paste(
  "sanction ~ (black + hisp_black + hisp_white) * mayor + age + female +",
  paste(RS_factors, collapse = " + "), fe_3
))
fml_arr_int <- as.formula(paste(
  "arrest ~ (black + hisp_black + hisp_white) * mayor + age + female +",
  paste(RS_factors, collapse = " + "), fe_3
))
fml_sum_int <- as.formula(paste(
  "summons ~ (black + hisp_black + hisp_white) * mayor + age + female +",
  paste(RS_factors, collapse = " + "), fe_3
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
