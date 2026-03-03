library(sf)
nyc_census2020_pct <- st_as_sf(nyc_census2020_pct)


ggplot(nyc_census2020_pct) +
  geom_sf(aes(fill = factor(precinct)), color = "white", size = 0.2) +
  theme_minimal() +
  labs(
    title = "NYC Precincts",
    fill = "Precinct"
  )


ggplot(nypd_sf) +
  geom_sf(aes(fill = factor(Precinct)), color = "white", size = 0.2) +
  theme_void() +
  labs(
    fill = "Precinct"
  ) +
  # omit legend
  theme(legend.position = "none")


nypd_sf <- nypd_sf %>%
  st_make_valid()

tracts_sf <- tracts_sf %>%
  st_make_valid()

st_crs(tracts_sf)
st_crs(nypd_sf)

nypd_sf$Precinct <- as.integer(nypd_sf$Precinct)

tracts_sf <- st_join(
  tracts_sf,
  nypd_sf %>% select(Precinct),
  join = st_intersects
)

ggplot(tracts_sf) +
  geom_sf(aes(fill = factor(Precinct)), color = "white", size = 0.2) +
  theme_void() +
  labs(
    fill = "Precinct"
  ) +
  # omit legend
  theme(legend.position = "none")


# get nyc census tract demographic data from tidycensus
age_vars <- c(
  "B01001_007E", "B01001_008E", "B01001_009E", "B01001_010E", # Male 18–24
  "B01001_031E", "B01001_032E", "B01001_033E", "B01001_034E"  # Female 18–24
)
housing_vars <- c("B25024_002E", "B25024_001E")
foreign_born_vars <- c("B05012_002E")  # Foreign-born population (total)

# Add these to your variable definitions
employment_vars <- c("B23025_007E")   # Unemployment Rate (pre-calculated)
income_vars <- c("B19013_001E")   # Median Household Income (Dollars)

get_pop <- function(year) {
  
  # retrieve population data for NYC tracts
  get_acs(
    geography = "tract",
    variables = c("B01001_001E",
                  "B03002_004E", # black alone, not hispanic/latino
                  "B03002_003E", # white alone, not hispanic/latino
                  "B03002_012E", # hispanic/latino
                  age_vars,
                  housing_vars,
                  foreign_born_vars,
                  employment_vars,
                  income_vars),
    state = "NY",
    county = c("New York", "Kings", "Queens", "Bronx", "Richmond"),
    year = year,
    geometry = TRUE
  ) %>%
    select(GEOID, variable, estimate, geometry) %>%
    pivot_wider(
      names_from = variable,
      values_from = estimate
    ) %>%
    mutate(year = year) %>% 
    rename(total_pop = "B01001_001",
           black_pop = "B03002_004", # black alone, not hispanic/latino
           white_pop = "B03002_003", # white alone, not hispanic/latino
           hisp_pop  = "B03002_012", # hispanic/latino
           foreign_born_pop = "B05012_002", # foreign-born population
           unemployment_rate = "B23025_007", # unemployment rate
           median_income = "B19013_001" # median income
           ) %>%
    mutate(age_18_24_pop = B01001_007 + B01001_008 + B01001_009 + B01001_010 +
             B01001_031 + B01001_032 + B01001_033 + B01001_034,
           pct_public_housing = B25024_002 / B25024_001,
           pct_foreign_born = foreign_born_pop / total_pop)
}

# get population data for years 2017-2024
years <- 2009:2023

# retrieve and combine population data for all years
pop_wide <- map_dfr(years, get_pop) %>%
  st_drop_geometry() %>%
  select(GEOID, year, total_pop, black_pop, white_pop, hisp_pop, age_18_24_pop, pct_public_housing, foreign_born_pop, pct_foreign_born)

# join population data to tracts_sf
pct_demo <- tracts_sf %>%
  left_join(pop_wide, by = c("GEOID" = "GEOID")) %>%
  group_by(BoroName, Precinct, year) %>%
  summarize(
    total_pop = sum(total_pop, na.rm = TRUE),
    black_pop = sum(black_pop, na.rm = TRUE),
    white_pop = sum(white_pop, na.rm = TRUE),
    hisp_pop  = sum(hisp_pop, na.rm = TRUE),
    foreign_born_pop = sum(foreign_born_pop, na.rm = TRUE),
    age_18_24_pop = sum(age_18_24_pop, na.rm = TRUE),
    pct_public_housing = mean(pct_public_housing, na.rm = TRUE),
    pct_foreign_born = sum(foreign_born_pop, na.rm = TRUE) / sum(total_pop, na.rm = TRUE)
  ) %>%
  mutate(
    pct_black = black_pop / total_pop,
    pct_hisp = hisp_pop / total_pop,
    pct_white = white_pop / total_pop,
    pct_18_24 = age_18_24_pop / total_pop
  ) %>%
  rename(pct = Precinct)

saveRDS(pct_demo, file = "data/pct_demo.rds")
  




