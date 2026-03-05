
expected_sqf_data <- c(
  "ASK_FOR_CONSENT_FLG","BACKROUND_CIRCUMSTANCES_SUSPECT_KNOWN_TO_CARRY_WEAPON_FLAG",
  "BACKROUND_CIRCUMSTANCES_VIOLENT_CRIME_FLAG", "CONSENT_GIVEN_FLG",
  "DAY2", "DEMEANOR_CODE", "DEMEANOR_OF_PERSON_STOPPED", "FIREARM_FLAG", "FRISKED_FLAG",
  "ID_CARD_IDENTIFIES_OFFICER_FLAG", "ISSUING_OFFICER_COMMAND_CODE", "ISSUING_OFFICER_RANK",
  "JURISDICTION_CODE", "JURISDICTION_DESCRIPTION", "KNIFE_CUTTER_FLAG", "LOCATION_IN_OUT_CODE",
  "MONTH2", "OBSERVED_DURATION_MINUTES", "OFFICER_EXPLAINED_STOP_FLAG", "OFFICER_IN_UNIFORM_FLAG",
  "OFFICER_NOT_EXPLAINED_STOP_DESCRIPTION", "OTHER_CONTRABAND_FLAG", "OTHER_PERSON_STOPPED_FLAG",
  "OTHER_WEAPON_FLAG", "PHYSICAL_FORCE_CEW_FLAG", "PHYSICAL_FORCE_DRAW_POINT_FIREARM_FLAG",
  "PHYSICAL_FORCE_HANDCUFF_SUSPECT_FLAG", "PHYSICAL_FORCE_OC_SPRAY_USED_FLAG",
  "PHYSICAL_FORCE_OTHER_FLAG", "PHYSICAL_FORCE_RESTRAINT_USED_FLAG",
  "PHYSICAL_FORCE_VERBAL_INSTRUCTION_FLAG", "PHYSICAL_FORCE_WEAPON_IMPACT_FLAG", "RECORD_STATUS_CODE",
  "SEARCH_BASIS_ADMISSION_FLAG", "SEARCH_BASIS_CONSENT_FLAG", "SEARCH_BASIS_HARD_OBJECT_FLAG",
  "SEARCH_BASIS_INCIDENTAL_TO_ARREST_FLAG", "SEARCH_BASIS_OTHER_FLAG", "SEARCH_BASIS_OUTLINE_FLAG",
  "SEARCHED_FLAG", "SHIELD_IDENTIFIES_OFFICER_FLAG", "STOP_DURATION_MINUTES","STOP_FRISK_DATE",
  "STOP_FRISK_ID", "STOP_FRISK_TIME", "STOP_ID", "STOP_ID_ANONY", "STOP_LOCATION_APARTMENT",
  "STOP_LOCATION_BORO_NAME", "STOP_LOCATION_FULL_ADDRESS", "STOP_LOCATION_PATROL_BORO_NAME",
  "STOP_LOCATION_PRECINCT", "STOP_LOCATION_PREMISES_NAME","STOP_LOCATION_SECTOR_CODE",
  "STOP_LOCATION_STREET_NAME", "STOP_LOCATION_X", "STOP_LOCATION_Y", "STOP_LOCATION_ZIP_CODE",
  "STOP_WAS_INITIATED", "SUMMONS_ISSUED_FLAG", "SUMMONS_OFFENSE_DESCRIPTION",
  "SUPERVISING_ACTION_CORRESPONDING_ACTIVITY_LOG_ENTRY_REVIEWED", "SUPERVISING_OFFICER_COMMAND_CODE",
  "SUPERVISING_OFFICER_RANK", "SUSPECT_ARREST_OFFENSE", "SUSPECT_ARRESTED_FLAG",
  "SUSPECT_BODY_BUILD_TYPE", "SUSPECT_EYE_COLOR", "SUSPECT_HAIR_COLOR", "SUSPECT_HEIGHT",
  "SUSPECT_OTHER_DESCRIPTION", "SUSPECT_RACE_DESCRIPTION", "SUSPECT_REPORTED_AGE", "SUSPECT_SEX",
  "SUSPECT_WEIGHT", "SUSPECTED_CRIME_DESCRIPTION", "SUSPECTS_ACTIONS_CASING_FLAG",
  "SUSPECTS_ACTIONS_DRUG_TRANSACTIONS_FLAG", "SUSPECTS_ACTIONS_IDENTIFY_CRIME_PATTERN_FLAG",
  "SUSPECTS_ACTIONS_LOOKOUT_FLAG", "SUSPECTS_ACTIONS_OTHER_FLAG",
  "SUSPECTS_ACTIONS_PROXIMITY_TO_SCENE_FLAG", "VERBAL_IDENTIFIES_OFFICER_FLAG", "WEAPON_FOUND_FLAG",
  "YEAR2"
)


read_sqf_csv <- function(path) {
  df <- read_csv(path, na = "(null)") %>% # treating (null) values as NA values
    dplyr::mutate(across(dplyr::everything(), as.character) # coerce fields to standardized variable types
    )
}

sqf_files_csv <- c(
  "data/nypd-stop/sqf-2009.csv",
  "data/nypd-stop/sqf-2010.csv",
  "data/nypd-stop/sqf-2011.csv",
  "data/nypd-stop/sqf-2012.csv",
  "data/nypd-stop/sqf-2013.csv",
  "data/nypd-stop/sqf-2014.csv",
  "data/nypd-stop/sqf-2015.csv",
  "data/nypd-stop/sqf-2016.csv"
)

# read historic sqf files to df
sqf_hist_csv <- sqf_files_csv %>%
  map_dfr(read_sqf_csv)

sqf_hist_csv <- sqf_hist_csv %>%
  rename(YEAR2 = year,
         STOP_FRISK_DATE = datestop,
         STOP_FRISK_TIME = timestop,
         RECORD_STATUS_CODE = recstat,
         STOP_FRISK_ID = ser_num,
         
         ISSUING_OFFICER_COMMAND_CODE = repcmd,
         SUPERVISING_OFFICER_COMMAND_CODE = revcmd,
         OFFICER_IN_UNIFORM_FLAG = offunif,
         OFFICER_EXPLAINED_STOP_FLAG = explnstp,
         SHIELD_IDENTIFIES_OFFICER_FLAG = offshld,
         VERBAL_IDENTIFIES_OFFICER_FLAG = offverb,
         
         OTHER_PERSON_STOPPED_FLAG = othpers,
         SUSPECT_ARRESTED_FLAG = arstmade,
         SUSPECT_ARREST_OFFENSE = arstoffn, # need to re-code entries
         SUMMONS_ISSUED_FLAG = sumissue,
         SUMMONS_OFFENSE_DESCRIPTION = sumoffen, # need to re-code entries
         
         FRISKED_FLAG = frisked,
         SEARCHED_FLAG = searched,
         OTHER_CONTRABAND_FLAG = contrabn,
         
         SEARCH_BASIS_HARD_OBJECT_FLAG = sb_hdobj,
         SEARCH_BASIS_OUTLINE_FLAG = sb_outln,
         SEARCH_BASIS_ADMISSION_FLAG = sb_admis,
         SEARCH_BASIS_OTHER_FLAG = sb_other,
         
         KNIFE_CUTTER_FLAG = knifcuti,
         OTHER_WEAPON_FLAG = othrweap,
         
         PHYSICAL_FORCE_HANDCUFF_SUSPECT_FLAG = pf_hcuff,
         PHYSICAL_FORCE_OC_SPRAY_USED_FLAG = pf_pepsp,
         PHYSICAL_FORCE_OTHER_FLAG = pf_other,
         PHYSICAL_FORCE_WEAPON_IMPACT_FLAG = pf_baton,
         
         SUSPECT_REPORTED_AGE = age,
         SUSPECT_SEX = sex,
         SUSPECT_RACE_DESCRIPTION = race,
         SUSPECT_WEIGHT = weight,
         SUSPECT_HAIR_COLOR = haircolr,
         SUSPECT_EYE_COLOR = eyecolor,
         SUSPECT_BODY_BUILD_TYPE = build,
         SUSPECT_OTHER_DESCRIPTION = othfeatr,
         
         SUSPECTED_CRIME_DESCRIPTION = crimsusp,
         SUSPECTS_ACTIONS_CASING_FLAG = cs_casng,
         SUSPECTS_ACTIONS_LOOKOUT_FLAG = cs_lkout,
         SUSPECTS_ACTIONS_DRUG_TRANSACTIONS_FLAG = cs_drgtr,
         SUSPECTS_ACTIONS_PROXIMITY_TO_SCENE_FLAG = ac_proxm,
         SUSPECTS_ACTIONS_DECRIPTION_FLAG = cs_descr,
         SUSPECTS_ACTIONS_CONCEALED_POSSESSION_WEAPON_FLAG = cs_bulge,
         SUSPECTS_ACTIONS_OTHER_FLAG = cs_other,
         BACKROUND_CIRCUMSTANCES_VIOLENT_CRIME_FLAG = rf_vcrim,
         BACKROUND_CIRCUMSTANCES_SUSPECT_KNOWN_TO_CARRY_WEAPON_FLAG = rf_knowl,
         
         STOP_LOCATION_APARTMENT = aptnum,
         STOP_LOCATION_STREET_NAME = stname,
         STOP_LOCATION_ZIP_CODE = zip,
         STOP_LOCATION_X = xcoord,
         STOP_LOCATION_Y = ycoord,
         STOP_LOCATION_SECTOR_CODE = sector,
         STOP_LOCATION_PRECINCT = addrpct
  ) %>%
  mutate(FIREARM_FLAG = if_else(pistol == "Y" | riflshot == "Y" | asltweap == "Y" | machgun == "Y", 
                                     "Y", "N", missing = "N"),
         WEAPON_FOUND_FLAG = if_else(FIREARM_FLAG == "Y" | KNIFE_CUTTER_FLAG == "Y" | OTHER_WEAPON_FLAG == "Y",
                                     "Y", "N", missing = "N"),
         PHYSICAL_FORCE_DRAW_POINT_FIREARM_FLAG = if_else(pf_drwep == "Y" | pf_ptwep == "Y",
                                                          "Y", "N", missing = "N"),
         SUSPECT_HEIGHT = paste0(ht_feet, ".", ht_inch)) # %>%

# Add any missing expected columns as NA
missing_cols <- setdiff(expected_sqf_data, names(sqf_hist_csv))
sqf_hist_csv[missing_cols] <- NA

# Reorder columns so expected ones come first (or only):
sqf_hist_csv <- sqf_hist_csv[, union(expected_sqf_data, names(sqf_hist_csv))]
  
sqf_hist_csv <- sqf_hist_csv[, expected_sqf_data]

setdiff(names(sqf_hist), names(sqf_hist_csv))


# rename variables entries to match modern schema

numeric_vars <- c(
  "STOP_ID", "STOP_FRISK_ID", "STOP_FRISK_TIME", "ISSUING_OFFICER_COMMAND_CODE", "SUPERVISING_OFFICER_COMMAND_CODE",
  "OBSERVED_DURATION_MINUTES", "STOP_DURATION_MINUTES", "SUSPECT_REPORTED_AGE", "SUSPECT_WEIGHT",
  "SUSPECT_HEIGHT",  "STOP_LOCATION_PRECINCT", "STOP_LOCATION_X", "STOP_LOCATION_Y", "STOP_LOCATION_ZIP_CODE"
)

sqf_hist_csv <- sqf_hist_csv %>% 
  mutate(
    SUSPECT_RACE_DESCRIPTION = dplyr::case_when(
      SUSPECT_RACE_DESCRIPTION == "A" ~ "ASIAN / PACIFIC ISLANDER",
      SUSPECT_RACE_DESCRIPTION == "B" ~ "BLACK",
      SUSPECT_RACE_DESCRIPTION == "W" ~ "WHITE",
      SUSPECT_RACE_DESCRIPTION == "P" ~ "HISPANIC-BLACK",
      SUSPECT_RACE_DESCRIPTION == "Q" ~ "HISPANIC-WHITE",
      SUSPECT_RACE_DESCRIPTION == "I" ~ "AMERICAN INDIAN/ALASKAN NATIVE",
      SUSPECT_RACE_DESCRIPTION %in% c("Z", "U", "") ~ NA_character_,
      TRUE ~ NA_character_),
    SUSPECT_SEX = dplyr::case_when(
      SUSPECT_SEX == "M" ~ "MALE",
      SUSPECT_SEX == "F" ~ "FEMALE",
      TRUE ~ NA_character_),
    across(all_of(numeric_vars), as.double),
    STOP_FRISK_TIME = as.character(STOP_FRISK_TIME),
    ISSUING_OFFICER_COMMAND_CODE = as.character(ISSUING_OFFICER_COMMAND_CODE),
    SUPERVISING_OFFICER_COMMAND_CODE = as.character(SUPERVISING_OFFICER_COMMAND_CODE),
    OBSERVED_DURATION_MINUTES = as.double(OBSERVED_DURATION_MINUTES),
    STOP_DURATION_MINUTES = as.double(STOP_DURATION_MINUTES),
    SUSPECT_REPORTED_AGE = as.double(SUSPECT_REPORTED_AGE),
    SUSPECT_WEIGHT = as.double(SUSPECT_WEIGHT),
    SUSPECT_HEIGHT = as.double(SUSPECT_HEIGHT),
    STOP_LOCATION_PRECINCT = as.double(STOP_LOCATION_PRECINCT),
    STOP_LOCATION_X = as.double(STOP_LOCATION_X),
    STOP_LOCATION_Y = as.double(STOP_LOCATION_Y),
    STOP_LOCATION_ZIP_CODE = as.double(STOP_LOCATION_ZIP_CODE),
    YEAR2 = as.double(YEAR2)
    )


needed_vars <- c(
  "STOP_FRISK_DATE",
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


sqf1 <- sqf_hist %>%
  select(all_of(needed_vars))

sqf2 <- sqf_hist_csv %>%
  select(all_of(needed_vars)) %>%
  mutate(STOP_FRISK_DATE = str_pad(STOP_FRISK_DATE, width = 8, pad = "0"),
         STOP_FRISK_DATE = as.Date(STOP_FRISK_DATE, format = "%m%d%Y"),
         STOP_FRISK_TIME = as.character(STOP_FRISK_TIME))

sqf_all <- bind_rows(sqf1, sqf2)

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




