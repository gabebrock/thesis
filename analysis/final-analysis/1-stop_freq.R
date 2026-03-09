library(fixest)
library(dplyr)
library(tidyr)
library(sf)

# --- load data ----
sqf_pct_month   <- readRDS("data/data-final/nypd-stop/sqf_pct_month.rds")
demo_pct        <- readRDS("data/data-final/census-gis/demo_pct.rds")
crime_pct_month <- readRDS("data/data-final/nyc-crime/crime_pct_month.rds")
nypd_sf         <- readRDS("data/data-final/census-gis/nypd_sf.rds")

# --- precinct areas (sq miles) ----
pct_areas <- nypd_sf %>%
  sf::st_make_valid() %>%
  dplyr::mutate(area_sqmi = as.numeric(sf::st_area(geometry)) * 3.861e-7) %>%
  sf::st_drop_geometry() %>%
  dplyr::select(pct = Precinct, area_sqmi)

# --- expand annual demo to monthly, compute density ----
demo_monthly <- demo_pct %>%
  tidyr::uncount(12) %>%
  dplyr::group_by(pct, year) %>%
  dplyr::mutate(month = dplyr::row_number()) %>%
  dplyr::ungroup() %>%
  dplyr::left_join(pct_areas, by = "pct") %>%
  dplyr::mutate(
    pop_density   = total_pop / area_sqmi,
    log_total_pop = log(total_pop + 1)
  )

# --- stop counts ----
stops <- sqf_pct_month %>%
  dplyr::rename(pct = STOP_LOCATION_PRECINCT, year = YEAR2, month = MONTH2) %>%
  dplyr::select(pct, year, month, n_stops)

# --- build panel: join stops + demo + crime ----
panel <- stops %>%
  dplyr::left_join(demo_monthly, by = c("pct", "year", "month")) %>%
  dplyr::left_join(
    crime_pct_month %>% dplyr::select(pct, boro, year, month, n, crime_Violent),
    by = c("pct", "year", "month")
  ) %>%
  dplyr::mutate(
    log_stops          = log(n_stops + 1),
    date               = as.Date(sprintf("%04d-%02d-01", year, month)),
    total_crime_rate   = log((n             + 1) / total_pop * 1000),
    violent_crime_rate = log((crime_Violent + 1) / total_pop * 1000)
  ) %>%
  # --- lag crime variables within precinct ----
  dplyr::arrange(pct, date) %>%
  dplyr::group_by(pct) %>%
  dplyr::mutate(
    lag_total_crime_rate = dplyr::lag(total_crime_rate, 1),
    delta_violent        = violent_crime_rate - dplyr::lag(violent_crime_rate, 1),
    lag_delta_violent    = dplyr::lag(delta_violent, 1)
  ) %>%
  dplyr::ungroup() %>%
  dplyr::filter(!is.na(lag_total_crime_rate), !is.na(lag_delta_violent))


# --- formula components ----
race_vars <- c("pct_black", "pct_hisp")
ses_vars  <- c("pop_density", "pct_18_24", "pct_foreign_born", "log_total_pop", "median_income")

rhs_1 <- paste(race_vars, collapse = " + ")
rhs_2 <- paste(c(race_vars, ses_vars), collapse = " + ")
rhs_3 <- paste(c(race_vars, ses_vars, "lag_total_crime_rate"), collapse = " + ")
rhs_5 <- paste(c(race_vars, ses_vars, "lag_total_crime_rate", "lag_delta_violent"), collapse = " + ")


# ===========================================================================
# SET A: precinct fixed effects
# ===========================================================================

m_pct_1 <- feols(as.formula(paste("log_stops ~", rhs_1)),            data = panel, cluster = ~boro)
m_pct_2 <- feols(as.formula(paste("log_stops ~", rhs_2)),            data = panel, cluster = ~boro)
m_pct_3 <- feols(as.formula(paste("log_stops ~", rhs_3)),            data = panel, cluster = ~boro)
m_pct_4 <- feols(as.formula(paste("log_stops ~", rhs_3, "| pct")),   data = panel, cluster = ~boro)
m_pct_5 <- feols(as.formula(paste("log_stops ~", rhs_5, "| pct")),   data = panel, cluster = ~boro)

etable(
  m_pct_1, m_pct_2, m_pct_3, m_pct_4, m_pct_5,
  title   = "Stop Frequency — Borough Fixed Effects",
  headers = c("(Base\n(Race Only)", "(1) + SES", "(2) + Crime", "(3) + Boro FE", "(4) + Change in\nViolent Crime"),
  keep    = c("pct_black", "pct_hisp"),
  dict    = c("%pct_black" = "\\% Black", "%pct_hisp" = "\\% Hispanic")
)


# ===========================================================================
# SET B: borough fixed effects
# ===========================================================================
#' Models 1–3 are identical to Set A (no FE); only models 4–5 change.

m_boro_4 <- feols(as.formula(paste("log_stops ~", rhs_3, "| boro")), data = panel, cluster = ~pct)
m_boro_5 <- feols(as.formula(paste("log_stops ~", rhs_5, "| boro")), data = panel, cluster = ~pct)

etable(
  m_pct_1, m_pct_2, m_pct_3, m_boro_4, m_boro_5,
  title   = "Stop Frequency — Borough Fixed Effects",
  headers = c("(Base\n(Race Only)", "(1) + SES", "(2) + Crime", "(3) + Boro FE", "(4) + Change in\nViolent Crime"),
  keep    = c("pct_black", "pct_hisp"),
  dict    = c(pct_black = "\\% Black", pct_hisp = "\\% Hispanic")
)


# ===========================================================================
# SET C: mayoral subsets (best spec: rhs_5 + precinct FE)
# ===========================================================================

panel <- panel |>
  dplyr::mutate(mayor = factor(dplyr::case_when(
    year <= 2013 ~ "Bloomberg",
    year <= 2021 ~ "de Blasio",
    TRUE         ~ "Adams"
  ), levels = c("Bloomberg", "de Blasio", "Adams")))

fml_best <- as.formula(paste("log_stops ~", rhs_5, "| pct"))

m_bloomberg <- feols(fml_best, data = dplyr::filter(panel, mayor == "Bloomberg"), cluster = ~boro)
m_deblasio  <- feols(fml_best, data = dplyr::filter(panel, mayor == "de Blasio"),  cluster = ~boro)
m_adams     <- feols(fml_best, data = dplyr::filter(panel, mayor == "Adams"),       cluster = ~boro)

etable(
  m_bloomberg, m_deblasio, m_adams,
  title   = "Stop Frequency by Mayoral Administration (Precinct FE, Full Controls)",
  headers = c("Bloomberg\n(2002–2013)", "de Blasio\n(2014–2021)", "Adams\n(2022–)")
)

# --- Wald test: do racial disparity coefficients differ across mayors? ----
m_interact <- feols(
  as.formula(paste0(
    "log_stops ~ (pct_black + pct_hisp) * mayor + ",
    paste(ses_vars, collapse = " + "),
    " + lag_total_crime_rate + lag_delta_violent | pct"
  )),
  data = panel, cluster = ~pct
)

# Joint test that mayor interactions on race vars = 0
wald(m_interact, keep = "pct_(black|hisp).*mayor")

coef(m_interact)[grep("pct_(black|hisp).*mayor", names(coef(m_interact)))]
#' The interpretation is: pct_black / pct_hisp = Bloomberg's effect
#' pct_black:mayorde Blasio = change from Bloomberg → de Blasio (−0.55, weaker disparity)
#' pct_black:mayorAdams = change from Bloomberg → Adams (+0.47, stronger disparity)

coefplot(m_interact, keep = "pct_(black|hisp).*mayor")

# absolute effects for each mayor
b <- coef(m_interact)
data.frame(
  mayor    = c("Bloomberg", "de Blasio", "Adams"),
  pct_black = c(b["pct_black"],
                b["pct_black"] + b["pct_black:mayorde Blasio"],
                b["pct_black"] + b["pct_black:mayorAdams"]),
  pct_hisp  = c(b["pct_hisp"],
                b["pct_hisp"]  + b["pct_hisp:mayorde Blasio"],
                b["pct_hisp"]  + b["pct_hisp:mayorAdams"])
)
