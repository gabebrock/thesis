#' Break down stop frequency in precincts to increments in the
#' concentrations of Black or Hispanic residents.
#'
#' Each model estimates effects compared to the precincts with the
#' lowest percentages of Black or Hispanic residents (0–20%, reference).
#'
#' Stop rates are disaggregated by precinct racial/ethnic composition,
#' using categorical increments in pct_black and pct_hisp: 20–40%,
#' 40–60%, and 60–100%.
#'
#' Assumes `panel` is already in the environment from 1-stop_freq.R.

# --- add race bins, reference = "0-20" ----
panel_binned <- panel %>%
  dplyr::mutate(
    black_cat = cut(
      pct_black,
      breaks = c(0, 0.20, 0.40, 0.60, 1.01),
      labels = c("0-20", "20-40", "40-60", "60-100"),
      right  = FALSE
    ),
    hisp_cat = cut(
      pct_hisp,
      breaks = c(0, 0.20, 0.40, 0.60, 1.01),
      labels = c("0-20", "20-40", "40-60", "60-100"),
      right  = FALSE
    ),
    black_cat = relevel(factor(black_cat), ref = "0-20"),
    hisp_cat  = relevel(factor(hisp_cat),  ref = "0-20")
  )

# --- formula components ----
ses_vars  <- c("pop_density", "pct_18_24", "pct_foreign_born", "log_total_pop", "median_income")

rhs_black_1 <- "black_cat"
rhs_black_2 <- paste(c("black_cat", ses_vars), collapse = " + ")
rhs_black_3 <- paste(c("black_cat", ses_vars, "lag_total_crime_rate"), collapse = " + ")
rhs_black_5 <- paste(c("black_cat", ses_vars, "lag_total_crime_rate", "lag_delta_violent"), collapse = " + ")

rhs_hisp_1 <- "hisp_cat"
rhs_hisp_2 <- paste(c("hisp_cat", ses_vars), collapse = " + ")
rhs_hisp_3 <- paste(c("hisp_cat", ses_vars, "lag_total_crime_rate"), collapse = " + ")
rhs_hisp_5 <- paste(c("hisp_cat", ses_vars, "lag_total_crime_rate", "lag_delta_violent"), collapse = " + ")


# ===========================================================================
# BLACK COMPOSITION BINS
# ===========================================================================

# Set A: precinct FE
mb_pct_1 <- feols(as.formula(paste("log_stops ~", rhs_black_1)),          data = panel_binned, cluster = ~boro)
mb_pct_2 <- feols(as.formula(paste("log_stops ~", rhs_black_2)),          data = panel_binned, cluster = ~boro)
mb_pct_3 <- feols(as.formula(paste("log_stops ~", rhs_black_3)),          data = panel_binned, cluster = ~boro)
mb_pct_4 <- feols(as.formula(paste("log_stops ~", rhs_black_3, "| pct")), data = panel_binned, cluster = ~boro)
mb_pct_5 <- feols(as.formula(paste("log_stops ~", rhs_black_5, "| pct")), data = panel_binned, cluster = ~boro)

etable(
  mb_pct_1, mb_pct_2, mb_pct_3, mb_pct_4, mb_pct_5,
  title   = "Stop Frequency by % Black Residents — Precinct Fixed Effects",
  headers = c("(1) Base", "(2) +SES", "(3) +Crime", "(4) +Pct FE", "(5) +ΔViolent")
)

# Set B: borough FE
mb_boro_4 <- feols(as.formula(paste("log_stops ~", rhs_black_3, "| boro")), data = panel_binned, cluster = ~pct)
mb_boro_5 <- feols(as.formula(paste("log_stops ~", rhs_black_5, "| boro")), data = panel_binned, cluster = ~pct)

etable(
  mb_pct_1, mb_pct_2, mb_pct_3, mb_boro_4, mb_boro_5,
  title   = "Stop Frequency by % Black Residents — Borough Fixed Effects",
  headers = c("(1) Base", "(2) +SES", "(3) +Crime", "(4) +Boro FE", "(5) +ΔViolent")
)


# ===========================================================================
# HISPANIC COMPOSITION BINS
# ===========================================================================

# Set A: precinct FE
mh_pct_1 <- feols(as.formula(paste("log_stops ~", rhs_hisp_1)),          data = panel_binned, cluster = ~boro)
mh_pct_2 <- feols(as.formula(paste("log_stops ~", rhs_hisp_2)),          data = panel_binned, cluster = ~boro)
mh_pct_3 <- feols(as.formula(paste("log_stops ~", rhs_hisp_3)),          data = panel_binned, cluster = ~boro)
mh_pct_4 <- feols(as.formula(paste("log_stops ~", rhs_hisp_3, "| pct")), data = panel_binned, cluster = ~boro)
mh_pct_5 <- feols(as.formula(paste("log_stops ~", rhs_hisp_5, "| pct")), data = panel_binned, cluster = ~boro)

etable(
  mh_pct_1, mh_pct_2, mh_pct_3, mh_pct_4, mh_pct_5,
  title   = "Stop Frequency by % Hispanic Residents — Precinct Fixed Effects",
  headers = c("(1) Base", "(2) +SES", "(3) +Crime", "(4) +Pct FE", "(5) +ΔViolent")
)

# Set B: borough FE
mh_boro_4 <- feols(as.formula(paste("log_stops ~", rhs_hisp_3, "| boro")), data = panel_binned, cluster = ~pct)
mh_boro_5 <- feols(as.formula(paste("log_stops ~", rhs_hisp_5, "| boro")), data = panel_binned, cluster = ~pct)

etable(
  mh_pct_1, mh_pct_2, mh_pct_3, mh_boro_4, mh_boro_5,
  title   = "Stop Frequency by % Hispanic Residents — Borough Fixed Effects",
  headers = c("(1) Base", "(2) +SES", "(3) +Crime", "(4) +Boro FE", "(5) +ΔViolent")
)


# ===========================================================================
# COMBINED: black_cat + hisp_cat
# ===========================================================================

#' In NYC, high-Black and high-Hispanic precincts tend to be geographically distinct 
#' (e.g., Brownsville/East New York vs. South Bronx), many interaction cells 
#' (e.g., 60-100% Black × 60-100% Hispanic) will be near-empty or empty entirely.
#' 
#' With 4 bins each, a full interaction produces 16 cells and 9 interaction 
#' coefficients which is hard to interpret and likely underpowered in sparse cells
table(panel_binned$black_cat, panel_binned$hisp_cat)

#' The sparse/empty cells confirm it — no interaction. Precincts separate cleanly 
#' along the diagonal: high-Black precincts are low-Hispanic and vice versa. 
#' The zeros in 60-100% Black × 40-60%+ Hispanic and 40-60%+ Black × 60-100% 
#' Hispanic mean an interaction would be unidentified in those cells.
#' 
#' The combined model already answers "what is the effect of Black composition,
#' holding Hispanic composition constant?"

rhs_both_1 <- "black_cat + hisp_cat"
rhs_both_2 <- paste(c("black_cat", "hisp_cat", ses_vars), collapse = " + ")
rhs_both_3 <- paste(c("black_cat", "hisp_cat", ses_vars, "lag_total_crime_rate"), collapse = " + ")
rhs_both_5 <- paste(c("black_cat", "hisp_cat", ses_vars, "lag_total_crime_rate", "lag_delta_violent"), collapse = " + ")

# Set A: precinct FE
mc_pct_1 <- feols(as.formula(paste("log_stops ~", rhs_both_1)),          data = panel_binned, cluster = ~boro)
mc_pct_2 <- feols(as.formula(paste("log_stops ~", rhs_both_2)),          data = panel_binned, cluster = ~boro)
mc_pct_3 <- feols(as.formula(paste("log_stops ~", rhs_both_3)),          data = panel_binned, cluster = ~boro)
mc_pct_4 <- feols(as.formula(paste("log_stops ~", rhs_both_3, "| pct")), data = panel_binned, cluster = ~pct)
mc_pct_5 <- feols(as.formula(paste("log_stops ~", rhs_both_5, "| pct")), data = panel_binned, cluster = ~pct)

etable(
  mc_pct_1, mc_pct_2, mc_pct_3, mc_pct_4, mc_pct_5,
  title   = "Stop Frequency by % Black & Hispanic Residents — Precinct Fixed Effects",
  headers = c("(1) Base", "(2) +SES", "(3) +Crime", "(4) +Pct FE", "(5) +Change in\nViolent")
)

etable(mc_pct_5)

# Set B: borough FE
mc_boro_4 <- feols(as.formula(paste("log_stops ~", rhs_both_3, "| boro")), data = panel_binned, cluster = ~pct)
mc_boro_5 <- feols(as.formula(paste("log_stops ~", rhs_both_5, "| boro")), data = panel_binned, cluster = ~pct)

etable(
  mc_pct_1, mc_pct_2, mc_pct_3, mc_boro_4, mc_boro_5,
  title   = "Stop Frequency by % Black & Hispanic Residents — Borough Fixed Effects",
  headers = c("(1) Base", "(2) +SES", "(3) +Crime", "(4) +Boro FE", "(5) +ΔViolent")
)

# ===========================================================================
# SET D: mayoral subsets — combined bins (best spec: rhs_both_5 + precinct FE)
# ===========================================================================

panel_binned <- panel_binned |>
  dplyr::mutate(mayor = factor(dplyr::case_when(                              
    year <= 2013 ~ "Bloomberg",
    year <= 2021 ~ "de Blasio",
    TRUE         ~ "Adams"
  ), levels = c("Bloomberg", "de Blasio", "Adams")))

fml_bins_best <- as.formula(paste("log_stops ~", rhs_both_5, "| pct"))

mc_bloomberg <- feols(fml_bins_best, data = dplyr::filter(panel_binned, mayor
                                                          == "Bloomberg"), cluster = ~boro)
mc_deblasio  <- feols(fml_bins_best, data = dplyr::filter(panel_binned, mayor
                                                          == "de Blasio"),  cluster = ~boro)
mc_adams     <- feols(fml_bins_best, data = dplyr::filter(panel_binned, mayor
                                                          == "Adams"),       cluster = ~boro)

etable(
  mc_bloomberg, mc_deblasio, mc_adams,
  title   = "Stop Frequency by Racial Composition Bins — Mayoral
  Administrations",
  headers = c("Bloomberg\n(2009--2013)", "de Blasio\n(2014--2021)",
              "Adams\n(2022--)"),
  keep    = c("black_cat", "hisp_cat"),
  dict    = c(
    "black_cat20-40"  = "20--40\\% Black",  "black_cat40-60"  = "40--60\\%
  Black",
    "black_cat60-100" = "60--100\\% Black", "hisp_cat20-40"   = "20--40\\%
  Hispanic",
    "hisp_cat40-60"   = "40--60\\% Hispanic", "hisp_cat60-100" = "60--100\\%
  Hispanic"
  )
)

# --- Wald test: do bin effects differ across mayors? ----
m_bins_interact <- feols(
  as.formula(paste0(
    "log_stops ~ (black_cat + hisp_cat) * mayor + ",
    paste(ses_vars, collapse = " + "),
    " + lag_total_crime_rate + lag_delta_violent | pct"
  )),
  data = panel_binned, cluster = ~pct
)

wald(m_bins_interact, keep = "cat.*mayor")

# Black bins only
wald(m_bins_interact, keep = "black_cat.*mayor")

# Hispanic bins only
wald(m_bins_interact, keep = "hisp_cat.*mayor")

b <- coef(m_bins_interact)

# pull Bloomberg baseline + de Blasio/Adams deviations for each bin
bins <- c("20-40", "40-60", "60-100")

data.frame(
  bin       = rep(bins, 2),
  race      = rep(c("Black", "Hispanic"), each = 3),
  bloomberg = c(b[paste0("black_cat", bins)], b[paste0("hisp_cat", bins)]),
  deblasio  = c(b[paste0("black_cat", bins)] + b[paste0("black_cat", bins,
                                                        ":mayorde Blasio")],
                b[paste0("hisp_cat",  bins)] + b[paste0("hisp_cat",  bins,
                                                        ":mayorde Blasio")]),
  adams     = c(b[paste0("black_cat", bins)] + b[paste0("black_cat", bins,
                                                        ":mayorAdams")],
                b[paste0("hisp_cat",  bins)] + b[paste0("hisp_cat",  bins,
                                                        ":mayorAdams")])
)
