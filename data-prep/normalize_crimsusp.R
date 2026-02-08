# Normalize crimsusp field to standardized categories
library(dplyr)

# Read the sqf_all dataset
cat("Using sqf_all dataset from environment...\n")

cat(paste("Loaded", nrow(sqf_all), "rows\n"))

# Standardized crime categories
crime_categories <- c(
  "CPW",
  "GRAND LARCENY", 
  "BURGLARY",
  "ASSAULT",
  "PETIT LARCENY",
  "ROBBERY",
  "OTHER",
  "MURDER",
  "GRAND LARCENY AUTO",
  "MENACING",
  "CRIMINAL POSSESSION OF CONTROLLED SUBSTANCE",
  "RECKLESS ENDANGERMENT",
  "CPSP",
  "CRIMINAL POSSESSION OF MARIHUANA",
  "CRIMINAL TRESPASS",
  "CRIMINAL SALE OF MARIHUANA",
  "CRIMINAL SALE OF CONTROLLED SUBSTANCE",
  "CRIMINAL MISCHIEF",
  "AUTO STRIPPIG",
  "UNAUTHORIZED USE OF A VEHICLE",
  "MAKING GRAFFITI",
  "THEFT OF SERVICES",
  "TERRORISM",
  "CRIMINAL POSSESSION OF FORGED INSTRUMENT",
  "FORCIBLE TOUCHING",
  "RAPE",
  "AUTO STRIPPING",
  "PROSTITUTION",
  "MISDEMEANOR",
  "FELONY"
)

cat("Normalizing crimsusp field...\n")

# Function to normalize crimsusp values
normalize_crimsusp <- function(crimsusp) {
  if (is.na(crimsusp) || crimsusp == "" || is.null(crimsusp)) {
    return("OTHER")
  }
  
  # Convert to uppercase and trim whitespace
  crimsusp_clean <- toupper(trimws(crimsusp))
  
  # Direct mapping for most common values (covers ~85% of data)
  direct_mapping <- list(
    "FEL" = "FELONY",
    "MISD" = "MISDEMEANOR", 
    "CPW" = "CPW",
    "FELONY" = "FELONY",
    "ROBBERY" = "ROBBERY",
    "GLA" = "GRAND LARCENY AUTO",
    "BURGLARY" = "BURGLARY",
    "CPM" = "CRIMINAL POSSESSION OF MARIHUANA",
    "CRIMINAL TRESPASS" = "CRIMINAL TRESPASS",
    "CRIM TRES" = "CRIMINAL TRESPASS",
    "ASSAULT" = "ASSAULT",
    "GRAND LARCENY" = "GRAND LARCENY",
    "CPCS" = "CRIMINAL POSSESSION OF CONTROLLED SUBSTANCE",
    "BURG" = "BURGLARY",
    "MIS" = "MISDEMEANOR",
    "PETIT LARCENY" = "PETIT LARCENY",
    "MISDEMEANOR" = "MISDEMEANOR",
    "CSCS" = "CRIMINAL SALE OF CONTROLLED SUBSTANCE",
    "ROB" = "ROBBERY",
    "GRAFFITI" = "MAKING GRAFFITI",
    "F" = "FELONY",
    "CRIM TRESS" = "CRIMINAL TRESPASS",
    "M" = "MISDEMEANOR",
    "CRIMINAL MISCHIEF" = "CRIMINAL MISCHIEF",
    "MAKING GRAFFITI" = "MAKING GRAFFITI",
    "ROBB" = "ROBBERY",
    "CRIM TRESPASS" = "CRIMINAL TRESPASS",
    "CPSP" = "CPSP",
    "CT" = "CRIMINAL TRESPASS",
    "GRAND LARC" = "GRAND LARCENY",
    "C.P.W." = "CPW",
    "G/L" = "GRAND LARCENY",
    "GL" = "GRAND LARCENY",
    "TRESPASS" = "CRIMINAL TRESPASS",
    "PROSTITUTION" = "PROSTITUTION",
    "THEFT OF SERVICE" = "THEFT OF SERVICES",
    "P/L" = "PETIT LARCENY",
    "DRUG SALES" = "CRIMINAL SALE OF CONTROLLED SUBSTANCE",
    "PETIT LARC" = "PETIT LARCENY"
  )
  
  # Check direct mapping first
  if (crimsusp_clean %in% names(direct_mapping)) {
    return(direct_mapping[[crimsusp_clean]])
  }
  
  # Pattern matching for variations (covers ~10% more)
  if (grepl("CPW|C\\.P\\.W\\.|POSSESSION.*WEAPON|CRIMINAL.*WEAPON", crimsusp_clean)) {
    return("CPW")
  } else if (grepl("GRAND.*LARCENY.*AUTO|GLA|AUTO.*GRAND", crimsusp_clean)) {
    return("GRAND LARCENY AUTO")
  } else if (grepl("GRAND.*LARCENY|GRAND.*LARC|GL", crimsusp_clean)) {
    return("GRAND LARCENY")
  } else if (grepl("PETIT.*LARCENY|PETIT.*LARC|P/L|PETTY.*LARCENY", crimsusp_clean)) {
    return("PETIT LARCENY")
  } else if (grepl("BURGLARY|BURG", crimsusp_clean)) {
    return("BURGLARY")
  } else if (grepl("ASSAULT|ASLT", crimsusp_clean)) {
    return("ASSAULT")
  } else if (grepl("ROBBERY|ROB|ROBB", crimsusp_clean)) {
    return("ROBBERY")
  } else if (grepl("MURDER|HOMICIDE", crimsusp_clean)) {
    return("MURDER")
  } else if (grepl("MENACING", crimsusp_clean)) {
    return("MENACING")
  } else if (grepl("CPCS|CONTROLLED.*SUBSTANCE|POSSESSION.*CONTROLLED", crimsusp_clean)) {
    return("CRIMINAL POSSESSION OF CONTROLLED SUBSTANCE")
  } else if (grepl("RECKLESS.*ENDANGERMENT", crimsusp_clean)) {
    return("RECKLESS ENDANGERMENT")
  } else if (grepl("CPSP|POSSESSION.*STOLEN", crimsusp_clean)) {
    return("CPSP")
  } else if (grepl("CPM|MARIHUANA|MARIJUANA.*POSSESSION", crimsusp_clean)) {
    return("CRIMINAL POSSESSION OF MARIHUANA")
  } else if (grepl("CRIMINAL.*TRESPASS|TRESPASS|CRIM.*TRES|CT", crimsusp_clean)) {
    return("CRIMINAL TRESPASS")
  } else if (grepl("CSCS|SALE.*CONTROLLED|DRUG.*SALE", crimsusp_clean)) {
    return("CRIMINAL SALE OF CONTROLLED SUBSTANCE")
  } else if (grepl("CRIMINAL.*MISCHIEF|MISCHIEF", crimsusp_clean)) {
    return("CRIMINAL MISCHIEF")
  } else if (grepl("AUTO.*STRIP", crimsusp_clean)) {
    return("AUTO STRIPPING")
  } else if (grepl("UNAUTHORIZED.*VEHICLE|UUV", crimsusp_clean)) {
    return("UNAUTHORIZED USE OF A VEHICLE")
  } else if (grepl("GRAFFITI|GRAFITTI", crimsusp_clean)) {
    return("MAKING GRAFFITI")
  } else if (grepl("THEFT.*SERVICES", crimsusp_clean)) {
    return("THEFT OF SERVICES")
  } else if (grepl("TERRORISM", crimsusp_clean)) {
    return("TERRORISM")
  } else if (grepl("FORGED.*INSTRUMENT|FORGERY", crimsusp_clean)) {
    return("CRIMINAL POSSESSION OF FORGED INSTRUMENT")
  } else if (grepl("FORCIBLE.*TOUCHING", crimsusp_clean)) {
    return("FORCIBLE TOUCHING")
  } else if (grepl("RAPE|SEX.*ASSAULT", crimsusp_clean)) {
    return("RAPE")
  } else if (grepl("PROSTITUTION", crimsusp_clean)) {
    return("PROSTITUTION")
  } else if (grepl("FELONY|FEL|FEOL", crimsusp_clean)) {
    return("FELONY")
  } else if (grepl("MISDEMEANOR|MISD|MIS|M", crimsusp_clean)) {
    return("MISDEMEANOR")
  } else if (grepl("VIOLATION", crimsusp_clean)) {
    return("OTHER")
  } else if (grepl("FRAUD", crimsusp_clean)) {
    return("OTHER")
  } else if (grepl("DRIVING.*UNLICENSED|UNLICENSED", crimsusp_clean)) {
    return("OTHER")
  } else if (grepl("IMPERSONATION", crimsusp_clean)) {
    return("OTHER")
  } else if (grepl("ACCOSTING", crimsusp_clean)) {
    return("OTHER")
  } else if (grepl("PICKPOCKET", crimsusp_clean)) {
    return("GRAND LARCENY")
  } else if (grepl("COW|VOOP", crimsusp_clean)) {
    return("OTHER")
  } else {
    return("OTHER")
  }
}

# Apply normalization - append new column to sqf_all
sqf_all <- sqf_all %>%
  mutate(crimsusp_normalized = sapply(SUSPECTED_CRIME_DESCRIPTION, normalize_crimsusp))

# Show distribution of normalized categories
cat("\nDistribution of normalized crime categories:\n")
category_counts <- sqf_all %>% 
  count(crimsusp_normalized, sort = TRUE) %>%
  mutate(percentage = round(n / sum(n) * 100, 1))

print(category_counts)

# Show some examples of original vs normalized
cat("\nSample of original vs normalized values:\n")
sample_data <- sqf_all %>% 
  filter(!is.na(SUSPECTED_CRIME_DESCRIPTION)) %>%
  group_by(crimsusp_normalized) %>%
  slice_head(n = 2) %>%
  select(SUSPECTED_CRIME_DESCRIPTION, crimsusp_normalized) %>%
  arrange(crimsusp_normalized)

print(head(sample_data, 20))

cat(paste("\nAppended crimsusp_normalized column to sqf_all\n"))
