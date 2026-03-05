 # Explore the crimsusp field to understand the data
library(dplyr)

# Read the combined dataset
cat("Reading combined SQF dataset...\n")
df <- read.csv("data/sqf-combined-2009-2016.csv", colClasses = "character")

cat(paste("Loaded", nrow(df), "rows\n"))

# Get unique crimsusp values
unique_crimsusp <- unique(df$crimsusp)
cat(paste("\nTotal unique crimsusp values:", length(unique_crimsusp), "\n"))

# Show first 100 unique values
cat("\nFirst 100 unique crimsusp values:\n")
for(i in 1:min(100, length(unique_crimsusp))) {
  cat(sprintf("%3d: '%s'\n", i, unique_crimsusp[i]))
}

# Show frequency distribution (top 50)
cat("\n\nTop 50 most frequent crimsusp values:\n")
freq_table <- df %>% 
  count(crimsusp, sort = TRUE) %>%
  head(50)

print(freq_table)

# Save unique values to a file for analysis
write.table(unique_crimsusp, "crimsusp_unique_values.txt", 
            quote = FALSE, row.names = FALSE, col.names = FALSE)
cat("\nSaved all unique values to 'crimsusp_unique_values.txt'\n")
