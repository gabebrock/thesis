# Config Files (run first)
- `config/setup.R` - Installs packages, loads libraries, reads initial SQF and crime data
- `config/gis.R` - Downloads and processes geospatial data (NYC boundaries, precincts, census tracts)
- `config/census.R` - Loads and processes census data (depends on gis.R being run first)

1. `config/load.R` - This handles the dependencies automatically in the correct order

# Data-Prep Files (run after config)
2. `data-prep/read_sqf-csv.R` - Reads and processes legacy CSV data (2009-2016)
3. `data-prep/normalize_crimsusp.R` - Normalizes crimsusp values
4. `data-prep/crime_cat.R` - Categorizes crime types for OLS models
