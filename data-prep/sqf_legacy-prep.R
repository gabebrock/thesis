

# --- read normalized legacy sqf data ----
#' this is data recorded before the standardized format in 2017
#' the crimsusp field was normalized by /data-prep/normalize_crimesusp.R
sqf_legacy <- read_csv("data/sqf-combined-2009-2016-normalized.csv", 
                       col_types = cols(datestop = col_character(), 
                                        repcmd = col_double(), revcmd = col_double(), 
                                        dob = col_character(), xcoord = col_double(), 
                                                               ycoord = col_double()))

# --- normalize dates in legacy data ----
sqf_legacy <- sqf_legacy %>%
  # Add leading zero for single-digit months
  mutate(datestop = ifelse(nchar(datestop) == 7, 
                          paste0("0", datestop), 
                          datestop),
         dob = ifelse(nchar(dob) == 7, paste0("0", dob), dob),
         datestop = as.Date(datestop, format = "%m%d%Y"),
         dob = as.Date(dob, format = "%m%d%Y"))

# --- standardize legacy data to match new sqf format ----
# rename fields
sqf_legacy_check <- sqf_legacy %>%
  rename(YEAR2 = year,
         STOP_LOCATION_PRECINCT = pct,
         STOP_FRISK_ID = ser_num,
         STOP_FRISK_DATE = datestop,
         STOP_FRISK_TIME = timestop,
         RECORD_STATUS_CODE = recstat,
         LOCATION_IN_OUT_CODE = inout,
         JURISDICTION_DESCRIPTION = trhsloc,
         OBSERVED_DURATION_MINUTES = perobs,
         SUSPECTED_CRIME_DESCRIPTION = crimsusp,
         STOP_DURATION_MINUTES = perstop,
         OFFICER_EXPLAINED_STOP_FLAG = explnstp,
         OTHER_PERSON_STOPPED_FLAG = othpers,
         SUSPECT_ARRESTED_FLAG = arstmade,
         SUSPECT_ARREST_OFFENSE = arstoffn,
         SUMMONS_ISSUED_FLAG = sumissue,
         SUMMONS_OFFENSE_DESCRIPTION = sumoffen,
         OFFICER_IN_UNIFORM_FLAG = offunif,
         ISSUING_OFFICER_RANK = officrid,
         FRISKED_FLAG = frisked,
         SEARCHED_FLAG = searched,
         OTHER_CONTRABAND_FLAG = contrabn,
         
         # Weapon flags
         FIREARM_FLAG = pistol,
         KNIFE_CUTTER_FLAG = knifcuti,
         OTHER_WEAPON_FLAG = othrweap,
         
         # Physical force flags
         PHYSICAL_FORCE_HANDS_SUSPECT_FLAG = pf_hands,
         PHYSICAL_FORCE_WALL_SUSPECT_FLAG = pf_wall,
         PHYSICAL_FORCE_GROUND_SUSPECT_FLAG = pf_grnd,
         PHYSICAL_FORCE_DRAW_POINT_FIREARM_FLAG = pf_drwep,
         PHYSICAL_FORCE_POINT_WEAPON_SUSPECT_FLAG = pf_ptwep,
         PHYSICAL_FORCE_WEAPON_IMPACT_FLAG = pf_baton,
         PHYSICAL_FORCE_HANDCUFF_SUSPECT_FLAG = pf_hcuff,
         PHYSICAL_FORCE_OC_SPRAY_USED_FLAG = pf_pepsp,
         PHYSICAL_FORCE_OTHER_FLAG = pf_other,
         
         # Radio and action codes
         RADIO_FLAG = radio,
         ADDITIONAL_REPORT_FLAG = ac_rept,
         INVESTIGATION_FLAG = ac_inves,
         
         # Background circumstances
         BACKROUND_CIRCUMSTANCES_VIOLENT_CRIME_FLAG = rf_vcrim,
         BACKROUND_CIRCUMSTANCES_OTHER_SUSPECT_WEAPON_FLAG = rf_othsw,
         SUSPECTS_ACTIONS_PROXIMITY_TO_SCENE_FLAG = ac_proxm,
         BACKROUND_CIRCUMSTANCES_ATTIRE_FLAG = rf_attir,
         
         # Suspect actions
         SUSPECTS_ACTIONS_OBJECTS_FLAG = cs_objcs,
         SUSPECTS_ACTIONS_DECRIPTION_FLAG = cs_descr,
         SUSPECTS_ACTIONS_CASING_FLAG = cs_casng,
         SUSPECTS_ACTIONS_LOOKOUT_FLAG = cs_lkout,
         BACKROUND_CIRCUMSTANCES_VIOLENT_CRIME_SUSPECT_FLAG = rf_vcact,
         SUSPECTS_ACTIONS_CLOTHING_FLAG = cs_cloth,
         SUSPECTS_ACTIONS_DRUG_TRANSACTIONS_FLAG = cs_drgtr,
         SUSPECTS_ACTIONS_EVASION_FLAG = ac_evasv,
         SUSPECTS_ACTIONS_ASSOCIATION_FLAG = ac_assoc,
         SUSPECTS_ACTIONS_FURTIVE_FLAG = cs_furtv,
         BACKROUND_CIRCUMSTANCES_RECENT_CRIME_FLAG = rf_rfcmp,
         SUSPECTS_ACTIONS_CHANGE_DIRECTION_FLAG = ac_cgdir,
         BACKROUND_CIRCUMSTANCES_VERBAL_THREATS_FLAG = rf_verbl,
         SUSPECTS_ACTIONS_VIOLENT_CRIME_FLAG = cs_vcrim,
         SUSPECTS_ACTIONS_CONCEALED_POSSESSION_WEAPON_FLAG = cs_bulge,
         SUSPECTS_ACTIONS_OTHER_FLAG = cs_other,
         SUSPECTS_ACTIONS_INCIDENT_FLAG = ac_incid,
         SUSPECTS_ACTIONS_TIME_FLAG = ac_time,
         BACKROUND_CIRCUMSTANCES_SUSPECT_KNOWN_TO_CARRY_WEAPON_FLAG = rf_knowl,
         SUSPECTS_ACTIONS_SOUND_FLAG = ac_stsnd,
         SUSPECTS_ACTIONS_OTHER_BEHAVIOR_FLAG = ac_other,
         
         # Search basis
         SEARCH_BASIS_HARD_OBJECT_FLAG = sb_hdobj,
         SEARCH_BASIS_OUTLINE_FLAG = sb_outln,
         SEARCH_BASIS_ADMISSION_FLAG = sb_admis,
         SEARCH_BASIS_OTHER_FLAG = sb_other,
         
         # Officer identification
         ISSUING_OFFICER_COMMAND_CODE = repcmd,
         SUPERVISING_OFFICER_COMMAND_CODE = revcmd,
         BACKROUND_CIRCUMSTANCES_FURTIVE_FLAG = rf_furt,
         BACKROUND_CIRCUMSTANCES_BULGE_FLAG = rf_bulg,
         VERBAL_IDENTIFIES_OFFICER_FLAG = offverb,
         SHIELD_IDENTIFIES_OFFICER_FLAG = offshld,
         
         # Suspect demographics
         SUSPECT_SEX = sex,
         SUSPECT_RACE_DESCRIPTION = race,
         SUSPECT_REPORTED_AGE = age,
         SUSPECT_WEIGHT = weight,
         SUSPECT_HAIR_COLOR = haircolr,
         SUSPECT_EYE_COLOR = eyecolor,
         SUSPECT_BODY_BUILD_TYPE = build,
         SUSPECT_OTHER_DESCRIPTION = othfeatr,
         
         # Location details
         STOP_LOCATION_ADDRESS_TYPE = addrtyp,
         STOP_LOCATION_RESIDENCE_CODE = rescode,
         STOP_LOCATION_PREMISES_TYPE = premtype,
         STOP_LOCATION_PREMISES_NAME = premname,
         STOP_LOCATION_ADDRESS_NUMBER = addrnum,
         STOP_LOCATION_STREET_NAME = stname,
         STOP_LOCATION_STREET_INTERSECTION = stinter,
         STOP_LOCATION_CROSS_STREET = crossst,
         STOP_LOCATION_APARTMENT = aptnum,
         STOP_LOCATION_CITY = city,
         STOP_LOCATION_STATE = state,
         STOP_LOCATION_ZIP_CODE = zip,
         STOP_LOCATION_SECTOR_CODE = sector,
         STOP_LOCATION_BEAT = beat,
         STOP_LOCATION_POST = post,
         STOP_LOCATION_X = xcoord,
         STOP_LOCATION_Y = ycoord,
         
         # Additional fields
         DETAIL_TYPE = dettypcm,
         LINE_CODE = linecm,
         DETAIL_CM = detailcm,
         FORCE_USE_CODE = forceuse
  ) %>%
  mutate(
    # Create suspect height from feet and inches
    SUSPECT_HEIGHT = paste0(ht_feet, ".", ht_inch),
    # Create combined weapon flag
    WEAPON_FOUND_FLAG = if_else(FIREARM_FLAG == "Y" | KNIFE_CUTTER_FLAG == "Y" | OTHER_WEAPON_FLAG == "Y" | 
                                riflshot == "Y" | asltweap == "Y" | machgun == "Y",
                                "Y", "N", missing = "N"),
    # Create firearm flag from all weapon types
    FIREARM_FLAG = if_else(FIREARM_FLAG == "Y" | riflshot == "Y" | asltweap == "Y" | machgun == "Y",
                           "Y", "N", missing = "N")
  ) %>%
  select(-c(riflshot, asltweap, machgun, ht_feet, ht_inch))

# list of expected sqf fields
expected_sqf_fields <- c("STOP_ID", "STOP_FRISK_DATE", "STOP_FRISK_TIME", "YEAR2",
                         "MONTH2", "DAY2", "STOP_WAS_INITIATED", "ISSUING_OFFICER_RANK",
                         "ISSUING_OFFICER_COMMAND_CODE", "SUPERVISING_OFFICER_RANK", "SUPERVISING_OFFICER_COMMAND_CODE", "SUPERVISING_ACTION_CORRESPONDING_ACTIVITY_LOG_ENTRY_REVIEWED",
                         "LOCATION_IN_OUT_CODE", "JURISDICTION_CODE", "JURISDICTION_DESCRIPTION", "OBSERVED_DURATION_MINUTES",
                         "SUSPECTED_CRIME_DESCRIPTION", "STOP_DURATION_MINUTES", "OFFICER_EXPLAINED_STOP_FLAG", "OFFICER_NOT_EXPLAINED_STOP_DESCRIPTION",
                         "OTHER_PERSON_STOPPED_FLAG", "SUSPECT_ARRESTED_FLAG", "SUSPECT_ARREST_OFFENSE", "SUMMONS_ISSUED_FLAG",
                         "SUMMONS_OFFENSE_DESCRIPTION", "OFFICER_IN_UNIFORM_FLAG", "ID_CARD_IDENTIFIES_OFFICER_FLAG", "SHIELD_IDENTIFIES_OFFICER_FLAG",
                         "VERBAL_IDENTIFIES_OFFICER_FLAG", "FRISKED_FLAG", "SEARCHED_FLAG", "ASK_FOR_CONSENT_FLG",
                         "CONSENT_GIVEN_FLG", "OTHER_CONTRABAND_FLAG", "FIREARM_FLAG", "KNIFE_CUTTER_FLAG",
                         "OTHER_WEAPON_FLAG", "WEAPON_FOUND_FLAG", "PHYSICAL_FORCE_CEW_FLAG", "PHYSICAL_FORCE_DRAW_POINT_FIREARM_FLAG",
                         "PHYSICAL_FORCE_HANDCUFF_SUSPECT_FLAG", "PHYSICAL_FORCE_OC_SPRAY_USED_FLAG", "PHYSICAL_FORCE_OTHER_FLAG", "PHYSICAL_FORCE_RESTRAINT_USED_FLAG",
                         "PHYSICAL_FORCE_VERBAL_INSTRUCTION_FLAG", "PHYSICAL_FORCE_WEAPON_IMPACT_FLAG", "BACKROUND_CIRCUMSTANCES_VIOLENT_CRIME_FLAG", "BACKROUND_CIRCUMSTANCES_SUSPECT_KNOWN_TO_CARRY_WEAPON_FLAG",
                         "SUSPECTS_ACTIONS_CASING_FLAG", "SUSPECTS_ACTIONS_CONCEALED_POSSESSION_WEAPON_FLAG", "SUSPECTS_ACTIONS_DECRIPTION_FLAG", "SUSPECTS_ACTIONS_DRUG_TRANSACTIONS_FLAG",
                         "SUSPECTS_ACTIONS_IDENTIFY_CRIME_PATTERN_FLAG", "SUSPECTS_ACTIONS_LOOKOUT_FLAG", "SUSPECTS_ACTIONS_OTHER_FLAG", "SUSPECTS_ACTIONS_PROXIMITY_TO_SCENE_FLAG",
                         "SEARCH_BASIS_ADMISSION_FLAG", "SEARCH_BASIS_CONSENT_FLAG", "SEARCH_BASIS_HARD_OBJECT_FLAG", "SEARCH_BASIS_INCIDENTAL_TO_ARREST_FLAG",
                         "SEARCH_BASIS_OTHER_FLAG", "SEARCH_BASIS_OUTLINE_FLAG", "DEMEANOR_OF_PERSON_STOPPED", "SUSPECT_REPORTED_AGE",
                         "SUSPECT_SEX", "SUSPECT_RACE_DESCRIPTION", "SUSPECT_HEIGHT", "SUSPECT_WEIGHT",
                         "SUSPECT_BODY_BUILD_TYPE", "SUSPECT_EYE_COLOR", "SUSPECT_HAIR_COLOR", "SUSPECT_OTHER_DESCRIPTION",
                         "STOP_LOCATION_PRECINCT", "STOP_LOCATION_SECTOR_CODE", "STOP_LOCATION_APARTMENT", "STOP_LOCATION_FULL_ADDRESS",
                         "STOP_LOCATION_STREET_NAME", "STOP_LOCATION_X", "STOP_LOCATION_Y", "STOP_LOCATION_PATROL_BORO_NAME",
                         "STOP_LOCATION_BORO_NAME", "STOP_LOCATION_ZIP_CODE", "RECORD_STATUS_CODE", "DEMEANOR_CODE",
                         "STOP_ID_ANONY", "STOP_FRISK_ID", "STOP_LOCATION_PREMISES_NAME", "geometry")


# add any missing expected columns as NA
missing_cols <- setdiff(expected_sqf_fields, names(sqf_legacy_check))
sqf_legacy_check[missing_cols] <- NA


numeric_vars <- c(
  "STOP_ID", "STOP_FRISK_ID", "STOP_FRISK_TIME", "ISSUING_OFFICER_COMMAND_CODE", "SUPERVISING_OFFICER_COMMAND_CODE",
  "OBSERVED_DURATION_MINUTES", "STOP_DURATION_MINUTES", "SUSPECT_REPORTED_AGE", "SUSPECT_WEIGHT",
  "SUSPECT_HEIGHT",  "STOP_LOCATION_PRECINCT", "STOP_LOCATION_X", "STOP_LOCATION_Y", "STOP_LOCATION_ZIP_CODE"
)

sqf_legacy_check <- sqf_legacy_check %>% 
  mutate(
    SUSPECT_RACE_DESCRIPTION = dplyr::case_when(
      SUSPECT_RACE_DESCRIPTION == "A" ~ "ASIAN / PACIFIC ISLANDER",
      SUSPECT_RACE_DESCRIPTION == "B" ~ "BLACK",
      SUSPECT_RACE_DESCRIPTION == "W" ~ "WHITE",
      SUSPECT_RACE_DESCRIPTION %in% c("Q", "P") ~ "HISPANIC",
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

sqf_legacy_check <- sqf_legacy_check %>%
  mutate(MONTH2 = as.double(month(STOP_FRISK_DATE)),
         DAY2 = as.double(day(STOP_FRISK_DATE)))

missing_cols <- setdiff(expected_sqf_fields, names(sqf_legacy_check))
sqf_legacy_check[missing_cols] <- NA

sqf_legacy <- sqf_legacy_check