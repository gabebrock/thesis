


crime <- complaint_hist |>
  mutate(week = floor_date(CMPLNT_FR_DT, unit = "week", week_start = 1),
         month = floor_date(CMPLNT_FR_DT, unit = "week", week_start = 1)) 


crime |>
  group_by(ADDR_PCT_CD, LAW_CAT_CD, week) |>
  count() |>
  pivot_wider(names_from = ADDR_PCT_CD,
              values_from = n) |>
  arrange(desc(week))
  

crime |> 
  # Violent crime rate
  # Murder rate
  # Assault rate
  # Property crime rate
  # Total major crime rate
  
  
sqf2024 |>
  count()

function(sqf_data) {
  filter(STOP_FRISK_DATE %within% 
           interval(STOP_FRISK_DATE %m+% days(1), STOP_FRISK_DATE %m+% weeks(2)))
}

# calculate hit rate for stops occurring two weeks after shooting
  # shooting_date 
  # filter sqf_data to contain all shootings 
    # 1) within the predefined treatment window AND 
    # 2) 14-days after shooting_date
  # calculate hit rate for pre & post windows, by race (BWH)
    # find total number of stops
    # find total number of seizures
    # divide seizures/stops, return hit rate


hit_rates <- sqf_hist %>%
  # keep only stops in ±14 day window around shooting
  mutate(
    days_from_shooting = as.integer(STOP_FRISK_DATE - shooting_date),
    window = case_when(
      days_from_shooting >= -90 & days_from_shooting <= -1 ~ 0,
      days_from_shooting >=  1  & days_from_shooting <= 14 ~ 1,
      TRUE ~ NA
    )
  ) %>%
  filter(!is.na(window)) %>%
  filter(SUSPECT_RACE_DESCRIPTION %in% BWH) %>%
  filter(STOP_LOCATION_PRECINCT %in% nst_precincts) %>%
  group_by(window, SUSPECT_RACE_DESCRIPTION) %>%
  summarize(
    total_stops = n(),
    total_seizures = sum(FIREARM_FLAG == "Y", na.rm = TRUE),
    hit_rate = total_seizures / total_stops,
    .groups = "drop" 
  )

hit_rates

# precincts with NYPD neighborhood safety teams (NST)
nst_precincts <- c(23, 25, 26, 28, 32, 34, # Manhattan North
                   40, 41, 42, 43, 44, 46, 47, 48, 49, 52, # Bronx
                   67, 69, 71, # Brooklyn South
                   73, 75, 77, 79, 81, 83, # Brooklyn North
                   101, 103, 105, 113, # Queens South
                   114, 115, # Queens North
                   120) # Staten Island

nypd_sf |> 
  ggplot() +
  geom_sf(fill = "white", color = "black", linewidth = 0.3) +
  geom_sf(
    data = nypd_sf |> filter(Precinct %in% nst_precincts),
    aes(geometry = geometry),
    fill = "red",
    alpha = 0.5,
    color = "black",
    linewidth = 0.3
  ) +
    theme_void()

weekly_stops_2024 <- sqf_hist |>
  st_drop_geometry() |>
  filter(YEAR2 >= 2024) |>
  mutate(
    STOP_FRISK_DATE = ymd(STOP_FRISK_DATE),
    week = floor_date(STOP_FRISK_DATE, "week")) |>
  group_by(STOP_LOCATION_PRECINCT, week) |>
  count() 

weekly_stops_2024 |>
  pivot_wider(names_from = STOP_LOCATION_PRECINCT,
              values_from = n)

weekly_shots_2024 <- shooting_hist |>
  filter(year(OCCUR_DATE) >= 2024) |>
  mutate(
    week = floor_date(OCCUR_DATE, "week")
  ) |>
  group_by(PRECINCT, week) |>
  count() 

weekly_shots_2024 |>
  pivot_wider(names_from = PRECINCT,
              values_from = n)


shooting_hist |>
  filter(year(OCCUR_DATE) >= 2024) |>
  group_by(PRECINCT) |>
  count() |>
  arrange(desc(n)) |>
  print(n = Inf)


sqf_hist |>
  filter(YEAR2 >= 2024) |>
  group_by(STOP_LOCATION_PRECINCT) |>
  count() |>
  arrange(desc(n)) |>
  print(n = Inf)


    


