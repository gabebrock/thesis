library(fixest)
library(dplyr)

# --- load data ----
# sqf_all <- readRDS("data/data-final/nypd-stop/sqf_all.rds")

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

force_flags <- c(
  "PHYSICAL_FORCE_HANDS_SUSPECT_FLAG",
  "PHYSICAL_FORCE_WALL_SUSPECT_FLAG",
  "PHYSICAL_FORCE_GROUND_SUSPECT_FLAG",
  "PHYSICAL_FORCE_DRAW_POINT_FIREARM_FLAG",
  "PHYSICAL_FORCE_POINT_WEAPON_SUSPECT_FLAG",
  "PHYSICAL_FORCE_WEAPON_IMPACT_FLAG",
  "PHYSICAL_FORCE_HANDCUFF_SUSPECT_FLAG",
  "PHYSICAL_FORCE_OC_SPRAY_USED_FLAG",
  "PHYSICAL_FORCE_CEW_FLAG",
  "PHYSICAL_FORCE_RESTRAINT_USED_FLAG",
  "PHYSICAL_FORCE_VERBAL_INSTRUCTION_FLAG",
  "PHYSICAL_FORCE_OTHER_FLAG"
)

# --- build individual-level dataset ----
needed_cols <- c(
  RS_flags, force_flags,
  "SUSPECT_ARRESTED_FLAG", "SUMMONS_ISSUED_FLAG", "SEARCHED_FLAG", "FRISKED_FLAG",
  "SUSPECT_RACE_DESCRIPTION", "SUSPECT_SEX", "SUSPECT_REPORTED_AGE",
  "STOP_LOCATION_PRECINCT", "YEAR2", "SUSPECTED_CRIME_DESCRIPTION"
)

stops_indiv <- sqf_all %>%
  dplyr::select(dplyr::all_of(needed_cols)) %>%
  dplyr::mutate(
    # encode RS flags as binary (sqf_all.rds is pre-prep_vars encoding)
    dplyr::across(
      dplyr::all_of(RS_flags),
      ~ dplyr::if_else(.x == "Y", 1L, 0L, missing = 0L)
    ),
    # encode force flags as binary
    dplyr::across(
      dplyr::all_of(force_flags),
      ~ dplyr::if_else(.x == "Y", 1L, 0L, missing = 0L)
    ),
    # core outcomes
    arrest    = dplyr::if_else(SUSPECT_ARRESTED_FLAG == "Y", 1L, 0L, missing = 0L),
    summons   = dplyr::if_else(SUMMONS_ISSUED_FLAG   == "Y", 1L, 0L, missing = 0L),
    sanction  = dplyr::if_else(arrest == 1L | summons == 1L, 1L, 0L),
    searched  = dplyr::if_else(SEARCHED_FLAG == "Y", 1L, 0L, missing = 0L),
    any_frisk = dplyr::if_else(FRISKED_FLAG  == "Y", 1L, 0L, missing = 0L),
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

# Compute any_force separately to avoid rowSums memory spike inside mutate
stops_indiv$any_force <- as.integer(
  rowSums(stops_indiv[, force_flags]) > 0
)

# --- RS factor composites ----
stops_indiv <- stops_indiv %>%
  dplyr::mutate(
    RS_fits_desc   = pmax(SUSPECTS_ACTIONS_DECRIPTION_FLAG,
                          BACKROUND_CIRCUMSTANCES_VIOLENT_CRIME_SUSPECT_FLAG),
    RS_evasive     = pmax(SUSPECTS_ACTIONS_FURTIVE_FLAG,
                          BACKROUND_CIRCUMSTANCES_FURTIVE_FLAG,
                          SUSPECTS_ACTIONS_EVASION_FLAG,
                          SUSPECTS_ACTIONS_CHANGE_DIRECTION_FLAG),
    RS_crime_loc   = pmax(BACKROUND_CIRCUMSTANCES_RECENT_CRIME_FLAG,
                          SUSPECTS_ACTIONS_TIME_FLAG,
                          SUSPECTS_ACTIONS_IDENTIFY_CRIME_PATTERN_FLAG,
                          SUSPECTS_ACTIONS_PROXIMITY_TO_SCENE_FLAG),
    RS_casing      = pmax(SUSPECTS_ACTIONS_CASING_FLAG,
                          SUSPECTS_ACTIONS_LOOKOUT_FLAG),
    RS_other       = pmax(SUSPECTS_ACTIONS_OTHER_FLAG,
                          SUSPECTS_ACTIONS_OTHER_BEHAVIOR_FLAG,
                          SUSPECTS_ACTIONS_INCIDENT_FLAG),
    RS_drug        = SUSPECTS_ACTIONS_DRUG_TRANSACTIONS_FLAG,
    RS_susp_obj    = pmax(BACKROUND_CIRCUMSTANCES_BULGE_FLAG,
                          BACKROUND_CIRCUMSTANCES_ATTIRE_FLAG,
                          BACKROUND_CIRCUMSTANCES_OTHER_SUSPECT_WEAPON_FLAG,
                          SUSPECTS_ACTIONS_CONCEALED_POSSESSION_WEAPON_FLAG,
                          SUSPECTS_ACTIONS_CLOTHING_FLAG),
    RS_crim_appear = pmax(SUSPECTS_ACTIONS_ASSOCIATION_FLAG,
                          SUSPECTS_ACTIONS_SOUND_FLAG,
                          SUSPECTS_ACTIONS_OBJECTS_FLAG,
                          BACKROUND_CIRCUMSTANCES_SUSPECT_KNOWN_TO_CARRY_WEAPON_FLAG),
    RS_violent     = pmax(SUSPECTS_ACTIONS_VIOLENT_CRIME_FLAG,
                          BACKROUND_CIRCUMSTANCES_VIOLENT_CRIME_FLAG,
                          BACKROUND_CIRCUMSTANCES_VERBAL_THREATS_FLAG),
    # derived frisk/force outcomes
    # Extra Frisk: frisked with no RS indicator of weapon or violence
    extra_frisk  = dplyr::if_else(any_frisk == 1L & RS_susp_obj == 0L & RS_violent == 0L, 1L, 0L),
    # Unproductive Frisk: frisked with no subsequent search and no sanction
    unprod_frisk = dplyr::if_else(any_frisk == 1L & searched == 0L & sanction == 0L, 1L, 0L),
    # Extra Force: force with no RS indicator of weapon or violence
    extra_force  = dplyr::if_else(any_force == 1L & RS_susp_obj == 0L & RS_violent == 0L, 1L, 0L)
  )

# Drop raw flag columns — composites are built, raw flags no longer needed
stops_indiv <- dplyr::select(stops_indiv, -dplyr::all_of(c(RS_flags, force_flags)))

RS_factors <- c("RS_fits_desc", "RS_evasive", "RS_crime_loc", "RS_casing",
                "RS_other", "RS_drug", "RS_susp_obj", "RS_crim_appear", "RS_violent")

# Five appearance-based (subjective) RS factors used in race interactions
RS_subjective <- c("RS_evasive", "RS_crime_loc", "RS_other", "RS_susp_obj", "RS_crim_appear")


# --- formula components ----
race_vars <- c("black", "hisp_black", "hisp_white")
demo_vars <- c("age", "female")

fe_str <- "| pct^year + crime"

# Base RHS: race + demographics + all RS factors (no interactions)
rhs_base <- paste(c(race_vars, demo_vars, RS_factors), collapse = " + ")

# Interaction RHS: add race × 5 subjective RS factors
interaction_terms <- as.vector(outer(race_vars, RS_subjective, paste, sep = ":"))
rhs_interact <- paste(c(race_vars, demo_vars, RS_factors, interaction_terms), collapse = " + ")

# Variables to display in etable
keep_vars <- c("black", "hisp_black", "hisp_white", "age", "female",
               "RS_evasive", "RS_crime_loc", "RS_other", "RS_susp_obj", "RS_crim_appear")
dict_vars <- c(
  black          = "Black",
  hisp_black     = "Black Hispanic",
  hisp_white     = "White Hispanic",
  female         = "Female",
  RS_evasive     = "Evasive/Furtive",
  RS_crime_loc   = "Crime Location",
  RS_other       = "RS -- Other",
  RS_susp_obj    = "Suspicious Object",
  RS_crim_appear = "Criminal Appearances"
)


# ===========================================================================
# MODEL SET 1 — ANY FRISK
# ===========================================================================

frisk_1 <- feols(as.formula(paste("any_frisk ~", rhs_base,    fe_str)),
                 data = stops_indiv, cluster = ~pct)
frisk_2 <- feols(as.formula(paste("any_frisk ~", rhs_interact, fe_str)),
                 data = stops_indiv, cluster = ~pct)


# ===========================================================================
# MODEL SET 2 — EXTRA FRISK
# ===========================================================================

efrisk_1 <- feols(as.formula(paste("extra_frisk ~", rhs_base,    fe_str)),
                  data = stops_indiv, cluster = ~pct)
efrisk_2 <- feols(as.formula(paste("extra_frisk ~", rhs_interact, fe_str)),
                  data = stops_indiv, cluster = ~pct)


# ===========================================================================
# MODEL SET 3 — UNPRODUCTIVE FRISK
# ===========================================================================

unprod_1 <- feols(as.formula(paste("unprod_frisk ~", rhs_base,    fe_str)),
                  data = stops_indiv, cluster = ~pct)
unprod_2 <- feols(as.formula(paste("unprod_frisk ~", rhs_interact, fe_str)),
                  data = stops_indiv, cluster = ~pct)


# ===========================================================================
# MODEL SET 4 — ANY FORCE
# ===========================================================================

force_1 <- feols(as.formula(paste("any_force ~", rhs_base,    fe_str)),
                 data = stops_indiv, cluster = ~pct)
force_2 <- feols(as.formula(paste("any_force ~", rhs_interact, fe_str)),
                 data = stops_indiv, cluster = ~pct)


# ===========================================================================
# MODEL SET 5 — EXTRA FORCE
# ===========================================================================

eforce_1 <- feols(as.formula(paste("extra_force ~", rhs_base,    fe_str)),
                  data = stops_indiv, cluster = ~pct)
eforce_2 <- feols(as.formula(paste("extra_force ~", rhs_interact, fe_str)),
                  data = stops_indiv, cluster = ~pct)


# ===========================================================================
# TABLE 10 — Frisk by Suspect and Case Characteristics
# ===========================================================================

etable(
  frisk_1, frisk_2, unprod_1, unprod_2,
  title   = "OLS Regression of Frisk by Suspect and Case Characteristics",
  headers = list(
    "Any Frisk"          = 2,
    "Unproductive Frisk" = 2
  ),
  extralines = list(
    "_Sub-headers" = c("No Race-RS\\nInteractions", "With Race-RS\\nInteractions",
                       "No Race-RS\\nInteractions", "With Race-RS\\nInteractions")
  ),
  keep = keep_vars,
  dict = dict_vars,
  view = TRUE
)

# ===========================================================================
# TABLE — Force by Suspect and Case Characteristics
# ===========================================================================

etable(
  force_1, force_2, eforce_1, eforce_2,
  title   = "OLS Regression of Force by Suspect and Case Characteristics",
  headers = list(
    "Any Force"   = 2,
    "Extra Force" = 2
  ),
  extralines = list(
    "_Sub-headers" = c("No Race-RS\\nInteractions", "With Race-RS\\nInteractions",
                       "No Race-RS\\nInteractions", "With Race-RS\\nInteractions")
  ),
  keep = keep_vars,
  dict = dict_vars,
  view = TRUE
)
