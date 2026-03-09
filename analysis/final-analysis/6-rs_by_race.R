library(fixest)
library(dplyr)

#' Table 8. OLS Regression of Reasonable Suspicion Factors by Suspect Race/Ethnicity
#'
#' Each column is a separate OLS regression with a binary RS factor as the DV
#' and race dummies (reference = White) as predictors.
#' FE: precinct × year + suspected crime.
#' SE clustered by precinct.
#'
#' Assumes stops_indiv in environment from 3-arrests_summons.R
#' (RS flags encoded as 0/1, race dummies created).

# --- build RS factor DVs ----
stops_rs8 <- stops_indiv %>%
  dplyr::mutate(

    # re-encode directly from raw flag to guard against all-NA columns
    # (some flags are absent in post-2017 xlsx data)
    dplyr::across(
      dplyr::any_of(c(
        "SUSPECTS_ACTIONS_DECRIPTION_FLAG",
        "SUSPECTS_ACTIONS_CASING_FLAG",
        "SUSPECTS_ACTIONS_DRUG_TRANSACTIONS_FLAG",
        "BACKROUND_CIRCUMSTANCES_VIOLENT_CRIME_FLAG",
        "BACKROUND_CIRCUMSTANCES_VIOLENT_CRIME_SUSPECT_FLAG",
        "SUSPECTS_ACTIONS_VIOLENT_CRIME_FLAG"
      )),
      ~ dplyr::if_else(.x == "Y", 1L, 0L, missing = 0L)
    ),

    # (1) Fits Description
    rs_fits_desc = SUSPECTS_ACTIONS_DECRIPTION_FLAG,

    # (2) Evasive / Furtive
    rs_furtive = pmax(
      BACKROUND_CIRCUMSTANCES_FURTIVE_FLAG, SUSPECTS_ACTIONS_FURTIVE_FLAG,
      SUSPECTS_ACTIONS_EVASION_FLAG, SUSPECTS_ACTIONS_CHANGE_DIRECTION_FLAG,
      SUSPECTS_ACTIONS_LOOKOUT_FLAG
    ),

    # (3) Crime Location
    rs_crime_loc = pmax(
      BACKROUND_CIRCUMSTANCES_RECENT_CRIME_FLAG,
      SUSPECTS_ACTIONS_PROXIMITY_TO_SCENE_FLAG,
      SUSPECTS_ACTIONS_INCIDENT_FLAG, SUSPECTS_ACTIONS_TIME_FLAG
    ),

    # (4) Casing
    rs_casing = SUSPECTS_ACTIONS_CASING_FLAG,

    # (5) Other
    rs_other = pmax(
      BACKROUND_CIRCUMSTANCES_VERBAL_THREATS_FLAG,
      SUSPECTS_ACTIONS_OTHER_FLAG, SUSPECTS_ACTIONS_OTHER_BEHAVIOR_FLAG,
      SUSPECTS_ACTIONS_SOUND_FLAG, SUSPECTS_ACTIONS_IDENTIFY_CRIME_PATTERN_FLAG
    ),

    # (6) Drug Transaction
    rs_drug = SUSPECTS_ACTIONS_DRUG_TRANSACTIONS_FLAG,

    # (7) Suspicious Object
    rs_suspicious_obj = pmax(
      BACKROUND_CIRCUMSTANCES_BULGE_FLAG,
      BACKROUND_CIRCUMSTANCES_SUSPECT_KNOWN_TO_CARRY_WEAPON_FLAG,
      BACKROUND_CIRCUMSTANCES_OTHER_SUSPECT_WEAPON_FLAG,
      SUSPECTS_ACTIONS_OBJECTS_FLAG,
      SUSPECTS_ACTIONS_CONCEALED_POSSESSION_WEAPON_FLAG
    ),

    # (8) Criminal Appearances
    rs_appearance = pmax(
      BACKROUND_CIRCUMSTANCES_ATTIRE_FLAG,
      SUSPECTS_ACTIONS_CLOTHING_FLAG, SUSPECTS_ACTIONS_ASSOCIATION_FLAG
    ),

    # (9) Violent Crime
    rs_violent = pmax(
      BACKROUND_CIRCUMSTANCES_VIOLENT_CRIME_FLAG,
      BACKROUND_CIRCUMSTANCES_VIOLENT_CRIME_SUSPECT_FLAG,
      SUSPECTS_ACTIONS_VIOLENT_CRIME_FLAG
    )
  )

# --- formula ----
rhs  <- "black + hisp_black + hisp_white"
fe   <- "| pct^year + crime"
fml  <- function(dv) as.formula(paste(dv, "~", rhs, fe))

# --- 9 models ----
t8_1 <- feols(fml("rs_fits_desc"),
              data = stops_rs8 |>
                filter(!is.na(SUSPECTS_ACTIONS_DECRIPTION_FLAG)),
              cluster = ~pct)
t8_2 <- feols(fml("rs_furtive"),      data = stops_rs8, cluster = ~pct)
t8_3 <- feols(fml("rs_crime_loc"),    data = stops_rs8, cluster = ~pct)
t8_4 <- feols(fml("rs_casing"),       data = stops_rs8, cluster = ~pct)
t8_5 <- feols(fml("rs_other"),        data = stops_rs8, cluster = ~pct)
t8_6 <- feols(fml("rs_drug"),         data = stops_rs8, cluster = ~pct)
t8_7 <- feols(fml("rs_suspicious_obj"), data = stops_rs8, cluster = ~pct)
t8_8 <- feols(fml("rs_appearance"),   data = stops_rs8, cluster = ~pct)
t8_9 <- feols(fml("rs_violent"),      data = stops_rs8, cluster = ~pct)

etable(
  t8_1, t8_2, t8_3, t8_4, t8_5, t8_6, t8_7, t8_8, t8_9,
  title   = "Table 8. OLS Regression of Reasonable Suspicion Factors by Suspect Race or Ethnicity",
  headers = c("(1) Fits\nDescription", "(2) Evasive/\nFurtive",
              "(3) Crime\nLocation",   "(4) Casing",
              "(5) Other",             "(6) Drug\nTransaction",
              "(7) Suspicious\nObject","(8) Criminal\nAppearances",
              "(9) Violent\nCrime"),
  dict    = c(black      = "Black",
              hisp_black = "Black Hispanic",
              hisp_white = "White Hispanic"),
  fitstat = ~ r2 + n,
  notes   = "Regressions include fixed effects for year, precinct, suspected crime, and year * precinct interactions. Standard errors clustered by precinct."
)
