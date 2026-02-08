
# When Black share in a precinct rises, do stops rise more than crime would predict?

# OLS Regressions of Precinct Officer Allocations (logged) by Precinct Racial Composition and  Social and Crime Conditions, 2017 - 2023 (b, SE, p)
feols(
  log_stops ~ pct_black + pct_hisp + log_violent_rate + log_nonviolent_rate +
    pop_density + pct_18_24 + pct_public_housing + factor(year) | BoroName,
  data = pct_year_ols,
  cluster = "pct"
) %>% summary()

# create data subsets for different mayoral administrations
pct_year_ols_adams <- pct_year_ols %>%
  filter(year >= 2022)

pct_year_ols_blasio <- pct_year_ols %>%
  filter(year >= 2014 & year <= 2021)

pct_year_ols_bloomberg <- pct_year_ols %>%
  filter(year <= 2013)


feols(
  log_stops ~ pct_black + pct_hisp + log_violent_rate + log_nonviolent_rate +
    pop_density + pct_18_24 + pct_public_housing + factor(year) | BoroName,
  data = pct_year_ols_bloomberg,
  cluster = "pct"
) %>% summary()

feols(
  log_stops ~ pct_black + pct_hisp + log_violent_rate + log_nonviolent_rate +
    pop_density + pct_18_24 + pct_public_housing + factor(year) | BoroName,
  data = pct_year_ols_blasio,
  cluster = "pct"
) %>% summary()

feols(
  log_stops ~ pct_black + pct_hisp + log_violent_rate + log_nonviolent_rate +
    pop_density + pct_18_24 + pct_public_housing + factor(year) | BoroName,
  data = pct_year_ols_adams,
  cluster = "pct"
) %>% summary()


