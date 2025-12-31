# check to see if the setup has already been run, if not, run it
# this includes the NYPD crime and SQF data and R libraries 
if (!exists(".setup_complete") || !.setup_complete) {
  source("config/setup.R")
} else {
  message("setup.R has already been run.")
}

# check to see if the GIS setup has already been run, if not, run it
if (!exists(".setup_complete_gis") || !.setup_complete_gis) {
  source("config/gis.R")
} else {
  message("gis.R has already been run.")
}

# check to see if the Census setup has already been run, if not, run it
# `census.R` depends on `gis.R` being run first
if (!exists(".setup_complete_census") || !.setup_complete_census) {
  source("config/census.R")
} else {
  message("census.R has already been run.")
}