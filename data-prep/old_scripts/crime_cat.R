# filter stops with uncodable or noncriminal offenses

sqf_all %>% 
  filter(crimsusp_normalized %in% c("MISDEMEANOR", "FELONY")) %>% 
  count()

sqf_all_clean <- sqf_all

sqf_all_clean <- sqf_all_clean %>% 
  filter(!crimsusp_normalized %in% c("MISDEMEANOR", "FELONY"))

# Map to specific crime categories

sqf_all_clean <- sqf_all_clean %>%
  mutate(
    crime_desc = stringr::str_squish(toupper(crimsusp_normalized)),
    
    off_cat = case_when(
      # Murder
      crime_desc == "MURDER" ~ "Murder",
      
      # Violent / Minor violent / Sex offenses that are clearly violent
      crime_desc %in% c("ASSAULT", "MENACING", "RECKLESS ENDANGERMENT", "RAPE") ~ "Violent Crime",
      crime_desc == "FORCIBLE TOUCHING" ~ "Sex Crimes and Related",
      
      # Weapons (CPW = Criminal Possession of a Weapon)
      crime_desc == "CPW" ~ "Weapons",
      
      # Hard Drug Crime
      crime_desc == "CRIMINAL POSSESSION OF CONTROLLED SUBSTANCE" ~ "Hard Drug Crime",
      crime_desc == "CRIMINAL SALE OF CONTROLLED SUBSTANCE"      ~ "Hard Drug Crime",
      
      # Marijuana Possession / Sale
      crime_desc == "CRIMINAL POSSESSION OF MARIHUANA" ~ "Marijuana Possession",
      crime_desc == "CRIMINAL SALE OF MARIHUANA"       ~ "Marijuana Sale",
      
      # Part I Property Crime
      crime_desc %in% c("GRAND LARCENY", "BURGLARY", "GRAND LARCENY AUTO") ~ "Part I Property Crime",
      
      # Minor Property Crime
      crime_desc %in% c("PETIT LARCENY",
                        "AUTO STRIPPIG",  # typo in data
                        "AUTO STRIPPING",
                        "CRIMINAL MISCHIEF",
                        "UNAUTHORIZED USE OF A VEHICLE",
                        "THEFT OF SERVICES",
                        "CPSP"  # Criminal Possession of Stolen Property
      ) ~ "Minor Property Crime",
      
      # Fraud and Related
      crime_desc == "CRIMINAL POSSESSION OF FORGED INSTRUMENT" ~ "Fraud and Related",
      
      # Trespass
      crime_desc == "CRIMINAL TRESPASS" ~ "Trespass",
      
      # Prostitution
      crime_desc == "PROSTITUTION" ~ "Prostitution",
      
      # Terrorism
      crime_desc == "TERRORISM" ~ "Terrorism",
      
      # Quality of Life / Disorder
      crime_desc == "MAKING GRAFFITI" ~ "Quality of Life/Disorder",
      
      # Very generic or uncodable buckets
      crime_desc %in% c("OTHER", "MISDEMEANOR", "FELONY") ~ "Other",
      
      TRUE ~ "Other"   # default catch‑all
    )
  )


# Collapse to the broad 7‑category scheme

sqf_all_clean <- sqf_all_clean %>%
  mutate(
    off_cat_broad = case_when(
      off_cat == "Murder" ~ "Murder",
      
      off_cat %in% c("Violent Crime") ~ "Violent",
      
      off_cat == "Weapons" ~ "Weapons",
      
      off_cat %in% c("Hard Drug Crime", "Marijuana Possession", "Marijuana Sale") ~ "Drug",
      
      off_cat %in% c("Part I Property Crime",
                     "Minor Property Crime",
                     "Fraud and Related") ~ "Property",
      
      off_cat == "Trespass" ~ "Trespass",
      
      off_cat == "Quality of Life/Disorder" ~ "QualityOfLife",
      
      # Everything else goes to Other (Prostitution, Terrorism, Sex, very generic buckets)
      TRUE ~ "Other"
    ),
    off_cat_broad = factor(off_cat_broad,
                           levels = c("Murder", "Violent", "Weapons",
                                      "Property", "Drug", "Trespass",
                                      "QualityOfLife", "Other"))
  )

table(sqf_all_clean$off_cat_broad, sqf_all_clean$off_cat, useNA = "ifany")
