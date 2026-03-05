
needed_vars_ols <- c(
  "STOP_FRISK_DATE",
  "YEAR2", "MONTH2", "DAY2",
  "STOP_LOCATION_PRECINCT",
  "STOP_FRISK_TIME",
  "SUSPECT_RACE_DESCRIPTION",
  "SUSPECT_REPORTED_AGE",
  "SUSPECT_SEX",
  "PHYSICAL_FORCE_CEW_FLAG",
  "PHYSICAL_FORCE_DRAW_POINT_FIREARM_FLAG",
  "PHYSICAL_FORCE_HANDCUFF_SUSPECT_FLAG",
  "PHYSICAL_FORCE_OC_SPRAY_USED_FLAG",
  "PHYSICAL_FORCE_OTHER_FLAG",
  "PHYSICAL_FORCE_RESTRAINT_USED_FLAG",
  "PHYSICAL_FORCE_WEAPON_IMPACT_FLAG",
  "SUSPECT_ARRESTED_FLAG",
  "SUMMONS_ISSUED_FLAG",
  "OTHER_CONTRABAND_FLAG",
  "WEAPON_FOUND_FLAG",
  "FIREARM_FLAG",
  "KNIFE_CUTTER_FLAG",
  "FRISKED_FLAG",
  "SEARCHED_FLAG",
  "SUSPECTS_ACTIONS_DECRIPTION_FLAG",
  "SUSPECTS_ACTIONS_IDENTIFY_CRIME_PATTERN_FLAG",
  "SUSPECTS_ACTIONS_PROXIMITY_TO_SCENE_FLAG",
  "SUSPECTS_ACTIONS_CASING_FLAG",
  "SUSPECTS_ACTIONS_LOOKOUT_FLAG",
  "SUSPECTS_ACTIONS_OTHER_FLAG",
  "OTHER_PERSON_STOPPED_FLAG",
  "SUSPECTS_ACTIONS_DRUG_TRANSACTIONS_FLAG",
  "SUSPECTS_ACTIONS_CONCEALED_POSSESSION_WEAPON_FLAG",
  "BACKROUND_CIRCUMSTANCES_SUSPECT_KNOWN_TO_CARRY_WEAPON_FLAG",
  "BACKROUND_CIRCUMSTANCES_VIOLENT_CRIME_FLAG",
  "SUSPECTED_CRIME_DESCRIPTION"
)

sqf_ols_hist <- sqf_hist %>%
  select(all_of(needed_vars_ols)) %>%
  mutate(
    YEAR2 = as.integer(YEAR2),
    MONTH2 = as.integer(MONTH2),
    DAY2 = as.integer(DAY2)
  )

sqf_ols_legacy <- sqf_legacy %>%
  select(all_of(needed_vars_ols))

sqf_all <- bind_rows(sqf_ols_hist, sqf_ols_legacy)

sqf_all <- sqf_all %>%
  tidyr::drop_na(SUSPECT_RACE_DESCRIPTION, SUSPECT_REPORTED_AGE,
                 STOP_LOCATION_PRECINCT, STOP_FRISK_DATE) %>%
  # normalize race descriptions
  mutate(SUSPECT_RACE_DESCRIPTION = dplyr::case_when(.data$SUSPECT_RACE_DESCRIPTION %in%
                                                       c("BLACK HISPANIC", "WHITE HISPANIC") ~ "HISPANIC",
                                                     TRUE ~ .data$SUSPECT_RACE_DESCRIPTION),
         SUSPECT_RACE_DESCRIPTION = dplyr::case_when(.data$SUSPECT_RACE_DESCRIPTION %in%
                                                       c("ASIAN/PAC.ISL") ~ "ASIAN",
                                                     TRUE ~ .data$SUSPECT_RACE_DESCRIPTION),
         SUSPECT_RACE_DESCRIPTION = dplyr::case_when(.data$SUSPECT_RACE_DESCRIPTION %in%
                                                       c("AMER IND", "AMERICAN INDIAN/ALASKAN N") ~ "AMERICAN INDIAN/ALASKAN NATIVE",
                                                     TRUE ~ .data$SUSPECT_RACE_DESCRIPTION),
         SUSPECT_RACE_DESCRIPTION = dplyr::case_when(.data$SUSPECT_RACE_DESCRIPTION %in%
                                                       c("MIDDLE EASTERN/SOUTHWEST") ~ "MIDDLE EASTERN/SOUTHWEST ASIAN",
                                                     TRUE ~ .data$SUSPECT_RACE_DESCRIPTION))
