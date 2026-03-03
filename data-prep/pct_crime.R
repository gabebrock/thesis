library(readxl)

## ---- Precinct felony crime data ----

# read in NYPD precinct-level major felony crime data
pct_crime_felony <- read_excel("data/nypd-crime/major-felony-counts-precinct.xlsx")

# clean and transform felony crime data
pct_crime_felony <- pct_crime_felony %>%
  filter(CRIME != "TOTAL SEVEN MAJOR FELONY OFFENSES") %>% # filter out total row
  rename(crime_desc = CRIME, pct = PCT) %>% # rename columns to match sqf data
  mutate(pct = as.numeric(pct)) %>% # mutate precinct to numeric
  # categorize crimes into broad offense categories
  mutate(
    off_cat_broad = case_when(
      
      ## ── Murder ────────────────────────────────────────────
      crime_desc == "MURDER & NON NEGL. MANSLAUGHTER" ~ "Murder",
      
      ## ── Violent (non-murder) ──────────────────────────────
      crime_desc %in% c(
        "RAPE",
        "ROBBERY",
        "FELONY ASSAULT"
      ) ~ "Violent",
      
      ## ── Property ──────────────────────────────────────────
      crime_desc %in% c(
        "BURGLARY",
        "GRAND LARCENY",
        "GRAND LARCENY OF MOTOR VEHICLE"
      ) ~ "Property"
    ),
    
    off_cat_broad = factor(
      off_cat_broad,
      levels = c(
        "Murder", "Violent", "Weapons",
        "Property", "Drug", "Trespass",
        "QualityOfLife", "Other"
      )
    )
  )

# pivot df longer to precinct-year
pct_crime_felony <- pct_crime_felony %>%
  pivot_longer(
    cols = starts_with("20"),       # all columns that are years
    names_to = "year",              # new column for the year
    values_to = "count_fel",            # value column
    names_transform = list(year = as.integer)
  ) %>%
  arrange(pct, crime_desc, year)

# summarize total felony counts by precinct, year, and broad offense category
pct_crime_felony <- pct_crime_felony %>%
  group_by(pct, year, off_cat_broad) %>%
  summarize(total_count = sum(count_fel, na.rm = TRUE), .groups = "drop")


## ---- Precinct misdemeanor crime data ----

# read in NYPD precinct-level misdemeanor crime data
pct_crime_misdemeanor <- read_excel("data/nypd-crime/misdemeanor-counts-by-precinct.xlsx")

# clean and transform misdemeanor crime data
pct_crime_misdemeanor <- pct_crime_misdemeanor %>% 
  filter(CRIME != "TOTAL MISDEMEANOR OFFENSES") %>% # filter out total row
  rename(crime_desc = CRIME, pct = PCT) %>% # rename columns to match sqf data
  mutate(pct = as.numeric(pct)) %>% # mutate precinct to numeric
  # categorize crimes into broad offense categories
  mutate(
    off_cat_broad = case_when(
      
      ## ── Violent (non-murder) ───────────────────────────────
      crime_desc %in% c(
        "ASSAULT 3 AND RELATED OFFENSES",
        "AGGRAVATED HARASSMENT 2",
        "OFFENSES AGAINST THE PERSON (7)"
      ) ~ "Violent",
      
      ## ── Sex crimes (kept in Other per your rule) ──────────
      crime_desc %in% c(
        "MISDEMEANOR SEX CRIMES (4)"
      ) ~ "Other",
      
      ## ── Weapons ───────────────────────────────────────────
      crime_desc %in% c(
        "MISDEMEANOR DANGEROUS WEAPONS (5)"
      ) ~ "Weapons",
      
      ## ── Drug ──────────────────────────────────────────────
      crime_desc %in% c(
        "MISDEMEANOR DANGEROUS DRUGS  (1)"
      ) ~ "Drug",
      
      ## ── Property ──────────────────────────────────────────
      crime_desc %in% c(
        "PETIT LARCENY",
        "MISDEMEANOR POSSESSION OF STOLEN PROPERTY",
        "MISD. CRIMINAL MISCHIEF & RELATED OFFENSES",
        "UNAUTHORIZED USE OF A VEHICLE",
        "FRAUDS (3)"
      ) ~ "Property",
      
      ## ── Trespass ──────────────────────────────────────────
      crime_desc %in% c(
        "CRIMINAL TRESPASS"
      ) ~ "Trespass",
      
      ## ── Quality of Life / Disorder ────────────────────────
      crime_desc %in% c(
        "ADMINISTRATIVE CODE (6)",
        "OTHER MISDEMEANORS (8)",
        "VEHICLE AND TRAFFIC LAWS",
        "INTOXICATED & IMPAIRED DRIVING",
        "OFFENSES AGAINST PUBLIC ADMINISTRATION (2)"
      ) ~ "QualityOfLife"
      
    ),
    
    off_cat_broad = factor(
      off_cat_broad,
      levels = c(
        "Violent", "Weapons",
        "Property", "Drug", "Trespass",
        "QualityOfLife", "Other"
      )
    )
  )

# pivot df longer to precinct-year
pct_crime_misdemeanor <- pct_crime_misdemeanor %>%
  pivot_longer(
    cols = starts_with("20"),
    names_to = "year",
    values_to = "count_msd",
    names_transform = list(year = as.integer)
  ) %>%
  arrange(pct, crime_desc, year)

# summarize total misdemeanor counts by precinct, year, and broad offense category
pct_crime_misdemeanor <- pct_crime_misdemeanor %>%
  group_by(pct, year, off_cat_broad) %>%
  summarize(total_count = sum(count_msd, na.rm = TRUE), .groups = "drop")


## ---- Combine felony and misdemeanor data ----

# transform murder into violent for broad categories
pct_crime_felony <- pct_crime_felony %>%
  mutate(
    off_cat_broad = recode(off_cat_broad,
                           "Murder" = "Violent")
  )

# 
pct_crime <- bind_rows(
  pct_crime_felony %>% rename(count = total_count),
  pct_crime_misdemeanor %>% rename(count = total_count)
) %>%
  group_by(pct, year, off_cat_broad) %>%
  summarize(total_count = sum(count, na.rm = TRUE), .groups = "drop") %>%
  arrange(pct, year, off_cat_broad)

# pivot wider crime categories so there is a unique precinct-year in each row
pct_crime <- pct_crime %>%
  pivot_wider(names_from = off_cat_broad,
              values_from = total_count,
              names_prefix = "crime_")

saveRDS(pct_crime, file = "data/pct_crime.rds")

