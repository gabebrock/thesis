# ---- Arrest/summons conditional on sanction ----

sqf_sanctioned <- sqf_ols %>% filter(sanction == 1)

sqf_sanctioned <- sqf_sanctioned %>%
  mutate(
    arrest_only = if_else(arrest == 1 & summons == 0, 1L, 0L),
    # if you have rare instances of both arrest & summons, treat as arrest_only = 1
  )

# create data subsets for different mayoral administrations
sqf_adams_sanctioned <- sqf_sanctioned %>%
  filter(year >= 2022)

sqf_blasio_sanctioned <- sqf_sanctioned %>%
  filter(year >= 2014 & year <= 2021)

sqf_bloomberg_sanctioned <- sqf_sanctioned %>%
  filter(year <= 2013)

# Model: arrest_only ~ race + age + female + RS_factors + FE x sanction/arrest
feols(
  arrest_only ~ Black + Hisp + age + female +
    RS_furtive + RS_crimloc + RS_casing + RS_other +
    RS_drug + RS_suspobj + RS_appear + RS_violent +
    i(off_cat) | pct + year + interaction(pct, year),
  data    = sqf_sanctioned,
  cluster = "pct"
) %>%
  summary()

# Model: Bloomberg Admin. x sanction/arrest
feols(
  arrest_only ~ Black + Hisp + age + female +
    RS_furtive + RS_crimloc + RS_casing + RS_other +
    RS_drug + RS_suspobj + RS_appear + RS_violent +
    i(off_cat) | pct + year + interaction(pct, year),
  data    = sqf_bloomberg_sanctioned,
  cluster = "pct"
) %>%
  summary()

# Model: De Blasio Admin. x sanction/arrest
feols(
  arrest_only ~ Black + Hisp + age + female +
    RS_furtive + RS_crimloc + RS_casing + RS_other +
    RS_drug + RS_suspobj + RS_appear + RS_violent +
    i(off_cat) | pct + year + interaction(pct, year),
  data    = sqf_blasio_sanctioned,
  cluster = "pct"
) %>% 
  summary()

# Model: Adams Admin. x sanction/arrest
feols(
  arrest_only ~ Black + Hisp + age + female +
    RS_furtive + RS_crimloc + RS_casing + RS_other +
    RS_drug + RS_suspobj + RS_appear + RS_violent +
    i(off_cat) | pct + year + interaction(pct, year),
  data    = sqf_adams_sanctioned,
  cluster = "pct"
) %>% 
  summary()
