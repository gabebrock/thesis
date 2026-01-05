# ============================================================================
# NYPD Stop-and-Frisk Difference-in-Differences Analysis
# Based on Brock (2026) Thesis Methodology
# ============================================================================

# Required Libraries
library(tidyverse)      # Data manipulation
library(lubridate)      # Date handling
library(MatchIt)        # Matching procedures
library(lmtest)         # Regression testing
library(sandwich)       # Robust standard errors
library(mgcv)           # GAM models
library(fixest)         # Fixed effects models
library(sf)             # Spatial data

# ============================================================================
# STEP 1: DATA PREPARATION
# ============================================================================

# Load your data
# sqf_2024 data should have: stop_id, date, precinct, lat, lon, SUSPECT_RACE_DESCRIPTION, 
# suspect_age, suspect_gender, suspect_height, force_used, frisked, searched, etc.
# Shooting data should have: incident_id, date, precinct, lat, lon, victim_race, SUSPECT_RACE_DESCRIPTION
  
  # Load sqf_2024 data
  sqf_2024 <- sqf_hist |>
    filter(YEAR2 == 2024 | YEAR2 == 2023) |>
    mutate(
      date = ymd(STOP_FRISK_DATE),
      stop_datetime = ymd_hms(paste(STOP_FRISK_DATE, STOP_FRISK_TIME)),
      # Create time of day coordinates (for matching)
      hour_angle = (hour(stop_datetime) + minute(stop_datetime)/60) * (2*pi/24),
      time_x = cos(hour_angle),
      time_y = sin(hour_angle),
      # Binary force indicators
      any_force = as.numeric(
        coalesce(PHYSICAL_FORCE_HANDCUFF_SUSPECT_FLAG, "N") == "Y" |
          coalesce(PHYSICAL_FORCE_RESTRAINT_USED_FLAG, "N") == "Y" |
          coalesce(PHYSICAL_FORCE_CEW_FLAG, "N") == "Y" |
          coalesce(PHYSICAL_FORCE_OC_SPRAY_USED_FLAG, "N") == "Y" |
          coalesce(PHYSICAL_FORCE_DRAW_POINT_FIREARM_FLAG, "N") == "Y" |
          coalesce(PHYSICAL_FORCE_OTHER_FLAG, "N") == "Y"
      ),
      # Filter to main racial groups
      race = case_when(
        SUSPECT_RACE_DESCRIPTION == "BLACK" ~ "Black",
        SUSPECT_RACE_DESCRIPTION == "WHITE" ~ "White",
        SUSPECT_RACE_DESCRIPTION == "HISPANIC" ~ "Hispanic",
        TRUE ~ NA_character_
      )
    ) %>%
    filter(!is.na(race)) %>%
    filter(race %in% c("Black", "White")) %>%
    select(date, stop_datetime, hour_angle, time_x, time_y,
           any_force, race, SUSPECT_REPORTED_AGE, SUSPECT_HEIGHT, SUSPECT_SEX,
           STOP_LOCATION_PRECINCT, STOP_LOCATION_X, STOP_LOCATION_Y)
  
  # Load shooting data
  shooting_hist

# ============================================================================
# STEP 2: IDENTIFY BOUNDARY shooting_hist
# ============================================================================

  # Convert shooting_hist to sf object
  shootings_sf <- shooting_hist %>%
    filter(year(OCCUR_DATE) == 2024 | year(OCCUR_DATE) == 2023) %>%
    drop_na(Longitude, Latitude) %>%
    st_as_sf(
      coords = c("Longitude", "Latitude"),
      crs = 4326
    ) %>%
    st_transform(2263)
  
  # Find neighboring precincts for each precinct
    # convert precinct boundaries to proper projection CRS
    nypd_sf <- nypd_sf %>%
      st_transform(2263)
    
    # make sure geometries are valid
    st_is_valid(nypd_sf) |> table()
    nypd_sf <- st_make_valid(nypd_sf)
  
  # create tibble of neighboring precincts  
  precinct_neighbors <- st_touches(nypd_sf)
  
  precinct_boundaries <- nypd_sf |>
    select(Precinct, geometry)
    
  boundary_shootings <- list()
  
  for(i in 1:nrow(shootings_sf)) {
    shooting <- shootings_sf[i,]
    assigned_precinct <- shooting$PRECINCT
    
    # Create buffer around shooting
    shooting_buffer <- st_buffer(shooting, 150) # 50 meters buffer
    
    # Find which precincts intersect the buffer
    intersecting_idx <- st_intersects(shooting_buffer, precinct_boundaries)[[1]]
    intersecting_precincts <- precinct_boundaries$Precinct[intersecting_idx]
    
    # Require at least two precincts (boundary condition)
    if (length(intersecting_precincts) < 2) next
    
    # Require that the assigned precinct is one of them
    if (!(assigned_precinct %in% intersecting_precincts)) next
    
    # Identify control precinct (the neighboring one)
    control_precinct <- intersecting_precincts[
      intersecting_precincts != assigned_precinct
    ][1]
    
    # Store boundary shooting with x/y coordinates
    coords <- st_coordinates(shooting)

    boundary_shootings[[length(boundary_shootings) + 1]] <- data.frame(
      shooting_id = shooting$INCIDENT_KEY,
      date = shooting$OCCUR_DATE,
      treated_precinct = assigned_precinct,
      control_precinct = control_precinct,
      x = coords[1],
      y = coords[2]
    )
    
  }
  
  # Combine all boundary shootings into a single dataframe
  boundary_shootings_df <- bind_rows(boundary_shootings)
  
  # Basic checks
  nrow(boundary_shootings_df)
  table(boundary_shootings_df$treated_precinct)
  
  # check uniqueness and clustering
  length(unique(boundary_shootings_df$shooting_id))
  summary(as.Date(boundary_shootings_df$date))
  with(boundary_shootings_df, table(treated_precinct, as.Date(date)))
  
  #' 39 shootings are represented more than once in `boundary_shootings_df` 
  #' likely because they intersect more than two precincts, or have 
  #' multiple source rows per incident in shooting_hist
  #' 
  #' Before you build pre/post windows and match stops,  
  #' collapse this to one row per unique shooting event.
  
    # Inspect the duplicates
    dup_ids <- boundary_shootings_df$shooting_id[duplicated(boundary_shootings_df$shooting_id)]
    length(unique(dup_ids))       # how many events have multiple rows
    boundary_shootings_df %>%
      filter(shooting_id %in% dup_ids) %>%
      arrange(shooting_id) %>%
      head(20)
    
    # Collapse to unique events
    boundary_shootings_df <- boundary_shootings_df %>%
      distinct(shooting_id, date, treated_precinct, control_precinct, x, y)
    
    nrow(boundary_shootings_df)                      
    length(unique(boundary_shootings_df$shooting_id))

  # ============================================================================
  # STEP 3: MATCHING PROCEDURE (Mahalanobis Distance) WITH PRE/POST STOPS
  # ============================================================================
  
    # type consistency checks
    sqf_2024 <- sqf_2024 %>%
      mutate(
        STOP_LOCATION_PRECINCT = as.integer(STOP_LOCATION_PRECINCT),
        SUSPECT_REPORTED_AGE = as.numeric(SUSPECT_REPORTED_AGE),
        SUSPECT_HEIGHT = as.numeric(SUSPECT_HEIGHT)
      )
    boundary_shootings_df <- boundary_shootings_df %>%
      mutate(
        treated_precinct = as.integer(treated_precinct),
        control_precinct = as.integer(control_precinct)
      )  
    
    
  all_matched <- list()
  
  for(i in 1:nrow(boundary_shootings_df)) {
    
    shooting <- boundary_shootings_df[i, ]
    
   
    shooting_date <- shooting$date
    treated_precinct <- shooting$treated_precinct
    control_precinct <- shooting$control_precinct
    
    pre_window <- 365
    post_window <- 14
  
    # PRE period (both precincts)
    pre_stops <- sqf_2024 %>%
      filter(
        STOP_LOCATION_PRECINCT %in% c(treated_precinct, control_precinct),
        date >= shooting_date - days(pre_window),
        date < shooting_date
      ) %>%
      mutate(
        period = "pre",
        treatment = as.numeric(STOP_LOCATION_PRECINCT == treated_precinct)
      )
    
    # POST period (both precincts)
    post_stops <- sqf_2024 %>%
      filter(
        STOP_LOCATION_PRECINCT %in% c(treated_precinct, control_precinct),
        date >= shooting_date,
        date <= shooting_date + days(post_window)
      ) %>%
      mutate(
        period = "post",
        treatment = as.numeric(STOP_LOCATION_PRECINCT == treated_precinct)
      )
    
    # Combine all periods
    combined <- bind_rows(pre_stops, post_stops)
    
    # Remove rows with missing or non-finite covariates
    combined <- combined %>%
      filter(
        !is.na(SUSPECT_REPORTED_AGE), is.finite(SUSPECT_REPORTED_AGE),
        !is.na(SUSPECT_HEIGHT), is.finite(SUSPECT_HEIGHT),
        !is.na(SUSPECT_SEX),
        !is.na(STOP_LOCATION_X), is.finite(STOP_LOCATION_X),
        !is.na(STOP_LOCATION_Y), is.finite(STOP_LOCATION_Y)
      )
    
    # -----------------------------
    # Matching by race
    # -----------------------------
    for(race_group in c("Black", "White")) {
      
      match_data <- combined %>% filter(race == race_group)
      if(nrow(match_data) == 0) next
      
      # Skip if no variation in treatment or sex
      if(length(unique(match_data$treatment)) < 2) next
      
      # Create binary treated indicator for MatchIt
      match_data <- match_data %>%
        mutate(treated_bin = as.numeric(treatment == 1))
      
      # Mahalanobis matching
      match_formula <- treated_bin ~
        STOP_LOCATION_X + STOP_LOCATION_Y +
        time_x + time_y +
        SUSPECT_REPORTED_AGE + SUSPECT_HEIGHT
      
      matched <- matchit(
        match_formula,
        data = match_data,
        method = "nearest",
        distance = "mahalanobis",
        replace = TRUE,
        ratio = 5
      )
      
      matched_data <- match.data(matched)
      
      # Add shooting info
      matched_data <- matched_data %>%
        mutate(
          shooting_id = shooting$shooting_id,
          shooting_date = shooting_date,
          treated_precinct = treated_precinct,
          control_precinct = control_precinct
        )
      
      all_matched[[length(all_matched) + 1]] <- matched_data
    }
  }
  
  matched_samples_df <- bind_rows(all_matched)
  
  # Basic checks
    # number of matched samples
    nrow(matched_samples_df)
    # size of treatment and control groups pre/post
    table(matched_samples_df$treatment, matched_samples_df$period)
    # size of racial groups
    table(matched_samples_df$race)
    # number of unique shooting events represented 
    length(unique(matched_samples_df$shooting_id))
      #' Not all shootings yield usable matches
      #' For ommitted shootings, either
        #' There were no stops in the pre/post windows for either precinct, or
        #' There were stops but no variation in treatment within a race group 
        #' (length(unique(match_data$treatment)) < 2), 
        #' so those race–event combinations were skipped, 
        #' and possibly the entire event if no race yielded a match.
    # identify shootings with no matched samples
    setdiff(boundary_shootings_df$shooting_id,
            unique(matched_samples_df$shooting_id))
    
    # store matched samples for further analysis
    matched_events <- unique(matched_samples_df$shooting_id)
    length(matched_events)
    
  # ============================================================================
  # STEP 4: DIFFERENCE-IN-DIFFERENCES REGRESSION
  # ============================================================================
  
  outcome <- "any_force"
  
  # Use the full matched_samples_df
  analysis_data <- matched_samples_df %>%
    mutate(
      post = as.numeric(period == "post"),
      treated = as.numeric(treatment == 1),
      did = post * treated,
      male = as.numeric(SUSPECT_SEX == "MALE")
    )
  
  # Optional: filter out very small groups for stability
  # analysis_data <- analysis_data %>%
  #  group_by(shooting_id, treated, period) %>%
  #  filter(n() >= 5) %>%
  #  ungroup()
  
  # -----------------------------
  # 1. Basic DiD
  # -----------------------------
  model_basic <- feols(
    any_force ~ did,
    data = analysis_data,
    cluster = ~STOP_LOCATION_PRECINCT
  )
  
  # -----------------------------
  # 2. DiD with controls
  # -----------------------------
  model_controls <- feols(
    any_force ~ did + SUSPECT_REPORTED_AGE + SUSPECT_HEIGHT + time_x + time_y,
    data = analysis_data,
    cluster = ~STOP_LOCATION_PRECINCT
  )
  
  # -----------------------------
  # 3. DiD with precinct fixed effects
  # -----------------------------
  model_fe <- feols(
    any_force ~ did | STOP_LOCATION_PRECINCT,
    data = analysis_data,
    cluster = ~STOP_LOCATION_PRECINCT
  )
  
  # -----------------------------
  # 4. By race
  # -----------------------------
  models_by_race <- analysis_data %>%
    group_by(race) %>%
    group_map(~ feols(any_force ~ did, data = .x, cluster = ~STOP_LOCATION_PRECINCT))
  
  # -----------------------------
  # Output results
  # -----------------------------
  list(
    basic = model_basic,
    controls = model_controls,
    fixed_effects = model_fe,
    by_race = models_by_race
  )


# ============================================================================
# STEP 5: VISUAL ANALYSIS (GAM for Trends)
# ============================================================================
  
  
  
  # Calculate days since shooting
  matched_data <- matched_data %>%
    mutate(
      days_since = as.numeric(date - min(date[period == "post"])),
      treatment_group = case_when(
        period == "post" & treatment == 1 ~ "Treated (Observed)",
        period == "post" & treatment == 0 ~ "Control (Observed)",
        period == "pre" & treatment == 1 ~ "Treated (Counterfactual)",
        period == "pre" & treatment == 0 ~ "Control (Counterfactual)"
      )
    )
  
  # Fit GAM models
  gam_treated <- gam(
    as.formula(paste0(outcome, " ~ s(days_since)")),
    data = matched_data %>% filter(treatment == 1),
    family = binomial
  )
  
  gam_control <- gam(
    as.formula(paste0(outcome, " ~ s(days_since)")),
    data = matched_data %>% filter(treatment == 0),
    family = binomial
  )
  
  # Create prediction grid
  pred_grid <- expand_grid(
    days_since = seq(min(matched_data$days_since), 
                     max(matched_data$days_since), 
                     length.out = 100)
  )
  
  pred_treated <- predict(gam_treated, newdata = pred_grid, 
                          se.fit = TRUE, type = "response")
  pred_control <- predict(gam_control, newdata = pred_grid, 
                          se.fit = TRUE, type = "response")
  
  plot_data <- bind_rows(
    pred_grid %>% mutate(
      group = "Treated",
      fit = pred_treated$fit,
      se = pred_treated$se.fit
    ),
    pred_grid %>% mutate(
      group = "Control",
      fit = pred_control$fit,
      se = pred_control$se.fit
    )
  ) %>%
    mutate(
      lower = fit - 1.96*se,
      upper = fit + 1.96*se
    )
  
  # Plot
  ggplot(plot_data, aes(x = days_since, y = fit, color = group, fill = group)) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "red") +
    geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2, color = NA) +
    geom_line(size = 1) +
    labs(
      title = "Counterfactual Trend Analysis",
      subtitle = "Use of Force Before and After Shooting Event",
      x = "Days Since Shooting",
      y = "Probability of Force Use",
      color = "Group",
      fill = "Group"
    ) +
    theme_minimal() +
    theme(legend.position = "bottom")

# ============================================================================
# STEP 6: REGRESSION DISCONTINUITY DESIGN
# ============================================================================

run_rdd_analysis <- function(matched_data, outcome = "any_force", 
                             bandwidth = 7) {
  
  # Calculate running variable (days since shooting)
  rdd_data <- matched_data %>%
    filter(period == "post") %>%
    mutate(
      days_since = as.numeric(date - min(date)),
      post_shooting = as.numeric(days_since >= 0)
    ) %>%
    filter(abs(days_since) <= bandwidth)
  
  # Local linear regression
  rdd_model <- feols(
    as.formula(paste0(
      outcome, " ~ post_shooting * days_since + suspect_age + 
      suspect_height + suspect_gender"
    )),
    data = rdd_data,
    cluster = ~precinct
  )
  
  # Plot discontinuity
  bin_data <- rdd_data %>%
    mutate(day_bin = round(days_since)) %>%
    group_by(day_bin) %>%
    summarise(
      force_rate = mean(get(outcome), na.rm = TRUE),
      n = n()
    )
  
  p <- ggplot(bin_data, aes(x = day_bin, y = force_rate)) +
    geom_vline(xintercept = 0, color = "red", linetype = "dashed") +
    geom_point(aes(size = n), alpha = 0.6) +
    geom_smooth(data = bin_data %>% filter(day_bin < 0), 
                method = "lm", se = TRUE, color = "blue") +
    geom_smooth(data = bin_data %>% filter(day_bin >= 0), 
                method = "lm", se = TRUE, color = "blue") +
    labs(
      title = "Regression Discontinuity Design",
      x = "Days Since Shooting",
      y = "Force Use Rate"
    ) +
    theme_minimal()
  
  return(list(model = rdd_model, plot = p))
}

# ============================================================================
# STEP 7: PLACEBO TEST
# ============================================================================

run_placebo_test <- function(sqf_2024, boundary_shootings_df, 
                             placebo_days_before = 14,
                             pre_period_days = 90) {
  
  # Use 2 weeks before actual shooting as placebo treatment
  placebo_date <- boundary_shootings_df$date - days(placebo_days_before)
  
  placebo_event <- boundary_shootings_df
  placebo_event$date <- placebo_date
  
  # Run same matching and analysis
  matched_placebo <- create_matched_sample(
    sqf_2024, placebo_event, 
    pre_period_days = 90
  )
  
  placebo_results <- run_did_analysis(matched_placebo)
  
  return(placebo_results)
}

# ============================================================================
# STEP 8: MAIN EXECUTION FUNCTION
# ============================================================================

run_full_analysis <- function(sqf_path, shooting_path, precinct_boundaries_path) {
  
  # Load data
  data <- load_and_prep_data(sqf_path, shooting_path)
  precinct_boundaries <- st_read(precinct_boundaries_path)
  
  # Identify boundary shooting_hist
  boundary_shootings <- identify_boundary_shootings(
    data$shooting_hist, 
    precinct_boundaries
  )
  
  cat("Found", nrow(boundary_shootings), "boundary shooting_hist\n")
  
  # Analyze each boundary shooting
  all_results <- list()
  
  for(i in 1:nrow(boundary_shootings)) {
    
    cat("Analyzing shooting", i, "of", nrow(boundary_shootings), "\n")
    
    shooting <- boundary_shootings[i,]
    
    # Create matched sample
    matched <- create_matched_sample(data$sqf_2024, shooting)
    
    # Run DiD analysis
    did_results <- run_did_analysis(matched)
    
    # Create visualizations
    trend_plot <- plot_counterfactual_trends(matched)
    
    # Run RDD
    rdd_results <- run_rdd_analysis(matched)
    
    # Placebo test
    placebo_results <- run_placebo_test(data$sqf_2024, shooting)
    
    all_results[[i]] <- list(
      shooting = shooting,
      matched_data = matched,
      did = did_results,
      trends = trend_plot,
      rdd = rdd_results,
      placebo = placebo_results
    )
  }
  
  return(all_results)
}