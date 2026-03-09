glimpse(sqf_all)


# --- merge STOP_ID into STOP_FRISK_ID ----
#' Modern data uses STOP_FRISK_ID; legacy uses STOP_ID. Coalesce into one field.
sqf_all <- sqf_all %>%
  dplyr::mutate(STOP_FRISK_ID = dplyr::coalesce(STOP_FRISK_ID, as.double(STOP_ID_ANONY))) %>%
  dplyr::select(-STOP_ID_ANONY)


# --- fix MONTH2 / DAY2 (NA for modern xlsx rows) ----
sqf_all <- sqf_all %>%
  dplyr::mutate(
    MONTH2 = as.double(lubridate::month(STOP_FRISK_DATE)),
    DAY2   = as.double(lubridate::day(STOP_FRISK_DATE))
  )


# --- clean STOP_FRISK_TIME ----
#' xlsx files store time as a phantom datetime "1899-12-31 HH:MM:SS".
#' Legacy data stores time as HHMM integer strings (e.g. "800", "58").
#' Normalize all values to "HH:MM".
sqf_all <- sqf_all %>%
  dplyr::mutate(
    STOP_FRISK_TIME = dplyr::if_else(
      stringr::str_starts(STOP_FRISK_TIME, "1899"),
      stringr::str_sub(STOP_FRISK_TIME, 12, 16),
      stringr::str_pad(STOP_FRISK_TIME, 4, pad = "0") |>
        stringr::str_replace("(\\d{2})(\\d{2})", "\\1:\\2")
    )
  )

# --- variable groups ----

# key identifiers and demographics — present in both modern and legacy data
key_vars_qual <- c(
  "STOP_LOCATION_BORO_NAME", "STOP_LOCATION_PATROL_BORO_NAME", "SUSPECT_RACE_DESCRIPTION",
  "SUSPECT_SEX", "DEMEANOR_OF_PERSON_STOPPED"
)

unique(sqf_all$STOP_LOCATION_BORO_NAME)
# --- normalize STOP_LOCATION_BORO_NAME ----
#' Legacy data mixed in patrol boro codes and numeric strings into this field.
#' Map patrol boro codes to their borough; numeric strings → NA.
sqf_all <- sqf_all %>%
  dplyr::mutate(
    STOP_LOCATION_BORO_NAME = dplyr::case_when(
      STOP_LOCATION_BORO_NAME %in% c("PBMS", "PBMN")      ~ "MANHATTAN",
      STOP_LOCATION_BORO_NAME == "PBBX"                   ~ "BRONX",
      STOP_LOCATION_BORO_NAME %in% c("PBBS", "PBBN")      ~ "BROOKLYN",
      STOP_LOCATION_BORO_NAME == "PBSI"                   ~ "STATEN ISLAND",
      STOP_LOCATION_BORO_NAME == "STATEN IS"              ~ "STATEN ISLAND",
      stringr::str_detect(STOP_LOCATION_BORO_NAME, "^\\d+$") ~ NA_character_,
      TRUE ~ STOP_LOCATION_BORO_NAME
    )
  )

unique(sqf_all$SUSPECT_RACE_DESCRIPTION)

unique(sqf_all$SUSPECT_SEX)
# --- encode SUSPECT_SEX as binary ----
#' MALE = 1, FEMALE = 0; numeric strings (age values in wrong field) → NA
sqf_all <- sqf_all %>%
  dplyr::mutate(
    SUSPECT_SEX = dplyr::case_when(
      SUSPECT_SEX == "MALE"   ~ 1L,
      SUSPECT_SEX == "FEMALE" ~ 0L,
      TRUE                    ~ NA_integer_
    )
  )

if (all(c("demeanor_score", "demeanor_n_words", "demeanor_valence") %in% names(sqf_all))) {
  message("demeanor columns present in sqf_all, skipping demeanor_analysis.R")                                                            
  return(invisible(NULL))                                           
} else {
  message("demeanor columns not found in sqf_all, running demeanor_analysis.R")
  source("~/Projects/thesis/data-prep/prep_stops/demeanor_analysis.R", echo = FALSE)
} 

#
key_vars_quant <- c(
  "STOP_LOCATION_PRECINCT", "SUSPECT_HEIGHT", "SUSPECT_WEIGHT", "SUSPECT_REPORTED_AGE",
  "STOP_FRISK_ID", "STOP_ID", "STOP_FRISK_DATE", "STOP_FRISK_TIME"
)

unique(sqf_all$STOP_LOCATION_PRECINCT)
sqf_all <- sqf_all %>% 
  filter(STOP_LOCATION_PRECINCT <= 123)

unique(sqf_all$SUSPECT_HEIGHT)
unique(sqf_all$SUSPECT_WEIGHT)

# reasonable suspicion flags — present in both modern and legacy data
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

# --- encode RS_flags as binary, NA → 0 ----
sqf_all <- sqf_all %>%
  dplyr::mutate(dplyr::across(
    dplyr::all_of(RS_flags),
    ~ dplyr::if_else(.x == "Y", 1L, 0L, missing = 0L)
  ))


# --- mean of each RS flag by year ----
RS_flag_means <- sqf_all %>%
  dplyr::group_by(YEAR2) %>%
  dplyr::summarize(dplyr::across(dplyr::all_of(RS_flags), mean), .groups = "drop") %>%
  tidyr::pivot_longer(-YEAR2, names_to = "flag", values_to = "mean") %>%
  tidyr::pivot_wider(names_from = YEAR2, values_from = mean) %>%
  dplyr::arrange(flag)

# physical force flags
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

# --- encode force_flags as binary, NA → 0 ----
sqf_all <- sqf_all %>%
  dplyr::mutate(dplyr::across(
    dplyr::all_of(force_flags),
    ~ dplyr::if_else(.x == "Y", 1L, 0L, missing = 0L)
  ))

# --- mean of each RS flag by year ----
force_flag_means <- sqf_all %>%
  dplyr::group_by(YEAR2) %>%
  dplyr::summarize(dplyr::across(dplyr::all_of(force_flags), mean), .groups = "drop") %>%
  tidyr::pivot_longer(-YEAR2, names_to = "flag", values_to = "mean") %>%
  tidyr::pivot_wider(names_from = YEAR2, values_from = mean) %>%
  dplyr::arrange(flag)

