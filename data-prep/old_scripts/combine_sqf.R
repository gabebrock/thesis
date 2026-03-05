# List of SQF files to combine
sqf_files_csv <- c(
  "data/nypd-stop/sqf-2009.csv",
  "data/nypd-stop/sqf-2010.csv",
  "data/nypd-stop/sqf-2011.csv",
  "data/nypd-stop/sqf-2012.csv",
  "data/nypd-stop/sqf-2013.csv",
  "data/nypd-stop/sqf-2014.csv",
  "data/nypd-stop/sqf-2015.csv",
  "data/nypd-stop/sqf-2016.csv"
)

# Load required library
library(dplyr)

cat("Combining SQF files...\n")

# Read and combine all CSV files
combined_data <- data.frame()

for (file_path in sqf_files_csv) {
  cat(paste("Reading", file_path, "...\n"))
  
  tryCatch({
    df <- read.csv(file_path, colClasses = "character")
    
    # Add year column if not present
    if (!"year" %in% colnames(df)) {
      year <- as.numeric(gsub(".*sqf-(\\d{4})\\.csv", "\\1", file_path))
      df$year <- year
    }
    
    cat(paste("  - Loaded", nrow(df), "rows\n"))
    
    # Combine with existing data
    if (nrow(combined_data) == 0) {
      combined_data <- df
    } else {
      combined_data <- bind_rows(combined_data, df)
    }
    
  }, error = function(e) {
    cat(paste("  - Error loading", file_path, ":", e$message, "\n"))
  })
}

# Save combined file if data was loaded
if (nrow(combined_data) > 0) {
  cat(paste("\nCombined dataset:", nrow(combined_data), "total rows\n"))
  
  output_file <- "data/nypd-stop/sqf-combined-2009-2016.csv"
  write.csv(combined_data, output_file, row.names = FALSE)
  cat(paste("Saved combined file to:", output_file, "\n"))
  
  # Show basic info
  cat(paste("\nDataset info:\n"))
  cat(paste("- Columns:", paste(colnames(combined_data), collapse = ", "), "\n"))
  if ("year" %in% colnames(combined_data)) {
    cat(paste("- Years covered:", paste(sort(unique(combined_data$year)), collapse = ", "), "\n"))
  }
} else {
  cat("No data was loaded.\n")
}
