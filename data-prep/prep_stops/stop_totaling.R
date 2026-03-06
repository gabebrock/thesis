#' Create pct-year and pct-year-month stop dfs with
#' total stop counts, and counts of
#' force type totals (inc. any_force flag), RS_flag totals,
#' totals for suspect race (Black, White, Hispanic-Black, Hispanic-White),
#' WEAPON_FOUND_FLAG, OTHER_CONTRABAND_FLAG
#' stop outcomes (e.g. summons, arrest, etc.)
#'
#' saved `sqf_pct-year` and `sqf_pct-month`

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


# --- row-level any_force flag ----
sqf_all <- sqf_all %>%
  dplyr::mutate(
    any_force = dplyr::if_else(
      rowSums(dplyr::pick(dplyr::all_of(force_flags))) > 0, 1L, 0L
    )
  )

# --- shared summarize helper ----
summarize_stops <- function(df) {
  df %>%
    dplyr::summarize(
      n_stops          = dplyr::n(),

      # race / ethnicity
      n_black          = sum(SUSPECT_RACE_DESCRIPTION == "BLACK",            na.rm = TRUE),
      n_white          = sum(SUSPECT_RACE_DESCRIPTION == "WHITE",            na.rm = TRUE),
      n_hisp_black     = sum(SUSPECT_RACE_DESCRIPTION == "HISPANIC-BLACK",   na.rm = TRUE),
      n_hisp_white     = sum(SUSPECT_RACE_DESCRIPTION == "HISPANIC-WHITE",   na.rm = TRUE),

      # outcomes
      n_arrested       = sum(SUSPECT_ARRESTED_FLAG    == "Y", na.rm = TRUE),
      n_summons        = sum(SUMMONS_ISSUED_FLAG       == "Y", na.rm = TRUE),
      n_weapon         = sum(WEAPON_FOUND_FLAG         == "Y", na.rm = TRUE),
      n_contraband     = sum(OTHER_CONTRABAND_FLAG     == "Y", na.rm = TRUE),
      n_frisked        = sum(FRISKED_FLAG              == "Y", na.rm = TRUE),
      n_searched       = sum(SEARCHED_FLAG             == "Y", na.rm = TRUE),

      # force
      n_any_force      = sum(any_force, na.rm = TRUE),
      dplyr::across(dplyr::all_of(force_flags), sum, .names = "n_{.col}"),

      # reasonable suspicion
      dplyr::across(dplyr::all_of(RS_flags), sum, .names = "n_{.col}"),

      .groups = "drop"
    )
}

# --- pct × year ----
sqf_pct_year <- sqf_all %>%
  dplyr::group_by(STOP_LOCATION_PRECINCT, YEAR2) %>%
  summarize_stops()

# --- pct × year × month ----
sqf_pct_month <- sqf_all %>%
  dplyr::group_by(STOP_LOCATION_PRECINCT, YEAR2, MONTH2) %>%
  summarize_stops()

# --- save ----
saveRDS(sqf_pct_year,  "data/data-final/nypd-stop/sqf_pct_year.rds")
saveRDS(sqf_pct_month, "data/data-final/nypd-stop/sqf_pct_month.rds")
