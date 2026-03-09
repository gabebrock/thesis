library(fixest)
library(dplyr)

#' Arrest conditional on sanction — two-step selection model.
#'
#' Arrests carry greater punishment liability than summons. To estimate
#' racial bias in the arrest-vs-summons decision, we restrict the sample
#' to sanctioned stops and add p(sanction) — the predicted probability of
#' any sanction from the logit model in 3-arrests_summons.R — to control
#' for selection into the sanctioned population.
#'
#' Model structure mirrors 4-sanctions_RS.R (6 progressive FE specifications)
#' with two differences:
#'   1. Sample restricted to sanctioned stops (arrest == 1 | summons == 1)
#'   2. p_sanction added as a regressor to control for selection
#'
#' Assumes in environment from prior scripts:
#'   stops_indiv — from 3-arrests_summons.R (race dummies, outcomes, p_sanction)
#'   step1_logit — from 3-arrests_summons.R (for p_sanction row alignment)

# --- rebuild RS composites on stops_indiv (self-contained) ----
#' Avoids dependence on stops_rs from 4-sanctions_RS.R session state.
#' female recoded robustly to handle both character and integer SUSPECT_SEX.
stops_sanctioned <- stops_indiv %>%
  dplyr::mutate(
    female = dplyr::case_when(
      SUSPECT_SEX %in% c("FEMALE", 0L) ~ 1L,
      TRUE                              ~ 0L
    ),
    RS_fac2 = pmax(
      BACKROUND_CIRCUMSTANCES_FURTIVE_FLAG, SUSPECTS_ACTIONS_FURTIVE_FLAG,
      SUSPECTS_ACTIONS_EVASION_FLAG, SUSPECTS_ACTIONS_CHANGE_DIRECTION_FLAG,
      SUSPECTS_ACTIONS_LOOKOUT_FLAG
    ),
    RS_fac3 = pmax(
      BACKROUND_CIRCUMSTANCES_RECENT_CRIME_FLAG,
      SUSPECTS_ACTIONS_PROXIMITY_TO_SCENE_FLAG,
      SUSPECTS_ACTIONS_INCIDENT_FLAG, SUSPECTS_ACTIONS_TIME_FLAG
    ),
    RS_fac5 = pmax(
      BACKROUND_CIRCUMSTANCES_BULGE_FLAG,
      BACKROUND_CIRCUMSTANCES_SUSPECT_KNOWN_TO_CARRY_WEAPON_FLAG,
      BACKROUND_CIRCUMSTANCES_OTHER_SUSPECT_WEAPON_FLAG,
      SUSPECTS_ACTIONS_OBJECTS_FLAG,
      SUSPECTS_ACTIONS_CONCEALED_POSSESSION_WEAPON_FLAG
    ),
    RS_fac7 = pmax(
      BACKROUND_CIRCUMSTANCES_ATTIRE_FLAG,
      SUSPECTS_ACTIONS_CLOTHING_FLAG, SUSPECTS_ACTIONS_ASSOCIATION_FLAG
    ),
    RS_fac8 = pmax(
      BACKROUND_CIRCUMSTANCES_VERBAL_THREATS_FLAG,
      SUSPECTS_ACTIONS_OTHER_FLAG, SUSPECTS_ACTIONS_OTHER_BEHAVIOR_FLAG,
      SUSPECTS_ACTIONS_SOUND_FLAG, SUSPECTS_ACTIONS_IDENTIFY_CRIME_PATTERN_FLAG
    )
  ) %>%
  dplyr::filter(sanction == 1L, !is.na(p_sanction))

# --- formula components ----
race_vars <- c("black", "hisp_black", "hisp_white")
demo_vars <- c("age", "female")
rs_vars   <- c("RS_fac2", "RS_fac3", "RS_fac5", "RS_fac7", "RS_fac8")

rhs_base <- paste(c(race_vars, demo_vars, "p_sanction"), collapse = " + ")
rhs_rs   <- paste(c(race_vars, demo_vars, rs_vars, "p_sanction"), collapse = " + ")


# --- 6-model progressive FE sequence ----

m1 <- feols(as.formula(paste("arrest ~", rhs_base)),
            data = stops_sanctioned, cluster = ~pct)

m2 <- feols(as.formula(paste("arrest ~", rhs_rs)),
            data = stops_sanctioned, cluster = ~pct)

m3 <- feols(as.formula(paste("arrest ~", rhs_rs, "| year")),
            data = stops_sanctioned, cluster = ~pct)

m4 <- feols(as.formula(paste("arrest ~", rhs_rs, "| year + pct")),
            data = stops_sanctioned, cluster = ~pct)

m5 <- feols(as.formula(paste("arrest ~", rhs_rs, "| year + pct + crime")),
            data = stops_sanctioned, cluster = ~pct)

m6 <- feols(as.formula(paste("arrest ~", rhs_rs, "| pct^year + crime")),
            data = stops_sanctioned, cluster = ~pct)

etable(
  m1, m2, m3, m4, m5, m6,
  title   = "OLS Regressions of Arrest (Conditional on Sanction) by Suspect Characteristics and Reasonable Suspicion",
  headers = c("(1)", "(2)", "(3)", "(4)", "(5)", "(6)"),
  dict    = c(
    black       = "Black",
    hisp_black  = "Black Hispanic",
    hisp_white  = "White Hispanic",
    age         = "Age",
    female      = "Female",
    p_sanction  = "P(Sanction)",
    RS_fac2     = "RS_fac2",
    RS_fac3     = "RS_fac3",
    RS_fac5     = "RS_fac5",
    RS_fac7     = "RS_fac7",
    RS_fac8     = "RS_fac8"
  )
)
