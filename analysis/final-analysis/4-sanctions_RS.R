library(fixest)
library(dplyr)

#' OLS regressions of sanction (arrest or summons) on suspect characteristics
#' and RS factors, with progressive fixed effects.
#'
#' RS factors grouped into 8 composite categories. Omitted reference categories:
#'   RS_fac1 — Fits Description        (SUSPECTS_ACTIONS_DECRIPTION_FLAG)
#'   RS_fac4 — Casing                  (SUSPECTS_ACTIONS_CASING_FLAG)
#'   RS_fac6 — Violent Crime / Drug    (rs_violent, rs_drug)
#'
#' Subjective RS factors (fac2, fac3, fac5, fac7, fac8) are estimated
#' relative to the behavioral reference group.
#'
#' FE progression:
#'   (1) No FE, no RS controls
#'   (2) No FE, + RS factors
#'   (3) + Year FE
#'   (4) + Year + Precinct FE
#'   (5) + Year + Precinct + Suspected Crime FE
#'   (6) + Precinct × Year FE + Suspected Crime FE
#'
#' Assumes stops_indiv in environment from 3-arrests_summons.R
#' (RS flags already encoded as 0/1 integers, race dummies created).

# --- composite RS factor indicators ----
#' Also recode female robustly: handles both character ("FEMALE") and
#' integer (0L) encodings of SUSPECT_SEX across different sqf_all.rds vintages.
stops_rs <- stops_indiv %>%
  dplyr::mutate(
    female = dplyr::case_when(
      SUSPECT_SEX %in% c("FEMALE", 0L) ~ 1L,
      TRUE                              ~ 0L
    ),

    # RS_fac2 — Furtive Movements (subjective)
    RS_fac2 = pmax(
      BACKROUND_CIRCUMSTANCES_FURTIVE_FLAG,
      SUSPECTS_ACTIONS_FURTIVE_FLAG,
      SUSPECTS_ACTIONS_EVASION_FLAG,
      SUSPECTS_ACTIONS_CHANGE_DIRECTION_FLAG,
      SUSPECTS_ACTIONS_LOOKOUT_FLAG
    ),

    # RS_fac3 — Crime Location / Time (subjective)
    RS_fac3 = pmax(
      BACKROUND_CIRCUMSTANCES_RECENT_CRIME_FLAG,
      SUSPECTS_ACTIONS_PROXIMITY_TO_SCENE_FLAG,
      SUSPECTS_ACTIONS_INCIDENT_FLAG,
      SUSPECTS_ACTIONS_TIME_FLAG
    ),

    # RS_fac4 — Casing (behavioral, OMITTED reference)
    # RS_fac4 = SUSPECTS_ACTIONS_CASING_FLAG  — excluded from model

    # RS_fac5 — Suspicious Object (subjective)
    RS_fac5 = pmax(
      BACKROUND_CIRCUMSTANCES_BULGE_FLAG,
      BACKROUND_CIRCUMSTANCES_SUSPECT_KNOWN_TO_CARRY_WEAPON_FLAG,
      BACKROUND_CIRCUMSTANCES_OTHER_SUSPECT_WEAPON_FLAG,
      SUSPECTS_ACTIONS_OBJECTS_FLAG,
      SUSPECTS_ACTIONS_CONCEALED_POSSESSION_WEAPON_FLAG
    ),

    # RS_fac6 — Violent Crime / Drug Transaction (behavioral, OMITTED reference)
    # RS_fac6 = pmax(rs_violent, rs_drug)  — excluded from model

    # RS_fac7 — Criminal Appearance (subjective)
    RS_fac7 = pmax(
      BACKROUND_CIRCUMSTANCES_ATTIRE_FLAG,
      SUSPECTS_ACTIONS_CLOTHING_FLAG,
      SUSPECTS_ACTIONS_ASSOCIATION_FLAG
    ),

    # RS_fac8 — Other (subjective)
    RS_fac8 = pmax(
      BACKROUND_CIRCUMSTANCES_VERBAL_THREATS_FLAG,
      SUSPECTS_ACTIONS_OTHER_FLAG,
      SUSPECTS_ACTIONS_OTHER_BEHAVIOR_FLAG,
      SUSPECTS_ACTIONS_SOUND_FLAG,
      SUSPECTS_ACTIONS_IDENTIFY_CRIME_PATTERN_FLAG
    )

    # RS_fac1 — Fits Description (OMITTED reference)
    # SUSPECTS_ACTIONS_DECRIPTION_FLAG — excluded from model
  )

# --- formula components ----
race_vars <- c("black", "hisp_black", "hisp_white")
demo_vars <- c("age", "female")
rs_vars   <- c("RS_fac2", "RS_fac3", "RS_fac5", "RS_fac7", "RS_fac8")

rhs_base <- paste(c(race_vars, demo_vars), collapse = " + ")
rhs_rs   <- paste(c(race_vars, demo_vars, rs_vars), collapse = " + ")


# --- 6-model progressive FE sequence ----

m1 <- feols(as.formula(paste("sanction ~", rhs_base)),
            data = stops_rs, cluster = ~pct)

m2 <- feols(as.formula(paste("sanction ~", rhs_rs)),
            data = stops_rs, cluster = ~pct)

m3 <- feols(as.formula(paste("sanction ~", rhs_rs, "| year")),
            data = stops_rs, cluster = ~pct)

m4 <- feols(as.formula(paste("sanction ~", rhs_rs, "| year + pct")),
            data = stops_rs, cluster = ~pct)

m5 <- feols(as.formula(paste("sanction ~", rhs_rs, "| year + pct + crime")),
            data = stops_rs, cluster = ~pct)

m6 <- feols(as.formula(paste("sanction ~", rhs_rs, "| pct^year + crime")),
            data = stops_rs, cluster = ~pct)

etable(
  m1, m2, m3, m4, m5, m6,
  title   = "Table 6. OLS Regressions of Any Sanction (Arrest or Summons) by Suspect Characteristics and Reasonable Suspicion",
  headers = c("(1)", "(2)", "(3)", "(4)", "(5)", "(6)"),
  dict    = c(
    black      = "Black",
    hisp_black = "Black Hispanic",
    hisp_white = "White Hispanic",
    age        = "Age",
    female     = "Female",
    RS_fac2    = "RS_fac2",
    RS_fac3    = "RS_fac3",
    RS_fac5    = "RS_fac5",
    RS_fac7    = "RS_fac7",
    RS_fac8    = "RS_fac8"
  ),
  view = T
)
