# LAC-9 Totals and Growth Analysis
# This script processes the previously created employment data file to calculate:
# 1. LAC-9 totals for each demographic group
# 2. Annualized growth rates for each demographic group
# Author: Luis Castellanos - Stats Team LAC
# Last modification: 2025/05/06
# -----------------------------------------------------------------------------

# Clean environment and load packages
rm(list = ls())
cat("\014")  # Clear console

# Install (if needed) and load required packages
required_pkgs <- c("readxl", "writexl", "dplyr", "purrr", "tidyr", "lubridate", "stringr")
for(pkg in required_pkgs) {
  if (!require(pkg, character.only = TRUE)) {
    install.packages(pkg, dependencies = TRUE)
    library(pkg, character.only = TRUE)
  }
}

# Define output directory path (same as the previous script)
output_dir <- "C:/Users/wb593225/OneDrive - WBG/Desktop/Luis - Private/Lablac/Semiannual report/Excel/Lablac output/01_New outputs/02 LM rates"

# Ensure output directory exists
dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

# Get the filename of the most recent absolute-worker file
today_date <- format(Sys.Date(), "%Y-%m-%d")
input_file <- file.path(output_dir, paste0(today_date, "-absolute-worker-per-group.xlsx"))

cat("=============================\n")
cat("PROCESSING LAC-9 TOTALS\n")
cat("Loading file:", input_file, "\n")
cat("=============================\n")

# Get sheet names
sheet_names <- excel_sheets(input_file)

# Filter out non-data sheets
exclude_sheets <- c("Description", "Annual Change - Total")
data_sheets <- sheet_names[!sheet_names %in% exclude_sheets]

cat("Processing", length(data_sheets), "data sheets\n")

# Initialize list to store LAC-9 results
lac9_results <- list()
group_metrics <- list()

# Process each data sheet
for(sheet_name in data_sheets) {
  cat("Processing sheet:", sheet_name, "\n")
  
  # Read the sheet
  sheet_data <- read_excel(input_file, sheet = sheet_name)
  
  # Store sheet metric name (used for growth calculations later)
  metric_name <- tolower(gsub(" ", "_", sheet_name))
  group_metrics[[sheet_name]] <- metric_name
  
  # Initialize the LAC-9 data frame
  lac9_data <- data.frame(Period = sheet_data$Period, LAC9 = NA_real_)
  
  # For each row (period)
  for(i in 1:nrow(sheet_data)) {
    period <- sheet_data$Period[i]
    
    # Get all country columns (excluding Period)
    country_cols <- names(sheet_data)[names(sheet_data) != "Period"]
    
    # Convert each value to numeric (removing commas)
    period_values <- sapply(country_cols, function(col) {
      val <- sheet_data[[col]][i]
      if(is.na(val)) return(0)
      as.numeric(gsub(",", "", val))
    })
    
    # Calculate the sum for this period
    lac9_sum <- sum(period_values, na.rm = TRUE)
    
    # Store in the LAC-9 data frame
    lac9_data$LAC9[i] <- lac9_sum
  }
  
  # Format LAC-9 values with commas
  lac9_data$LAC9 <- format(round(lac9_data$LAC9), big.mark = ",")
  
  # Store the results
  lac9_results[[sheet_name]] <- lac9_data
}

# Calculate annualized growth rates for each demographic group
cat("\n=============================\n")
cat("CALCULATING GROWTH RATES\n")
cat("=============================\n")

# Initialize growth rates dataframe
growth_rates <- data.frame(
  Group = character(),
  `2016-Q2 to 2019-Q2` = numeric(),
  `2023-Q2 to 2024-Q2` = numeric(),
  stringsAsFactors = FALSE
)

# Process each demographic group
for(sheet_name in names(lac9_results)) {
  cat("Calculating growth rates for:", sheet_name, "\n")
  
  # Get LAC-9 data for this group
  lac9_data <- lac9_results[[sheet_name]]
  
  # Extract periods and convert values to numeric
  periods <- lac9_data$Period
  values <- as.numeric(gsub(",", "", lac9_data$LAC9))
  
  # Create a data frame for easier manipulation
  period_data <- data.frame(Period = periods, Value = values)
  
  # Extract the data for target years
  value_2016 <- period_data$Value[period_data$Period == "2016-Q2"]
  value_2019 <- period_data$Value[period_data$Period == "2019-Q2"]
  value_2023 <- period_data$Value[period_data$Period == "2023-Q2"]
  value_2024 <- period_data$Value[period_data$Period == "2024-Q2"]
  
  # Calculate growth rates
  growth_2016_2019 <- NA
  growth_2023_2024 <- NA
  
  # 2016-Q2 to 2019-Q2 (3 years)
  if(length(value_2016) > 0 && length(value_2019) > 0 && 
     !is.na(value_2016) && !is.na(value_2019) && value_2016 > 0) {
    growth_2016_2019 <- ((value_2019 / value_2016) ^ (1/3) - 1) * 100
  }
  
  # 2023-Q2 to 2024-Q2 (1 year)
  if(length(value_2023) > 0 && length(value_2024) > 0 && 
     !is.na(value_2023) && !is.na(value_2024) && value_2023 > 0) {
    growth_2023_2024 <- ((value_2024 / value_2023) - 1) * 100
  }
  
  # Add to growth rates dataframe
  growth_rates <- rbind(growth_rates, data.frame(
    Group = sheet_name,
    `2016-Q2 to 2019-Q2` = growth_2016_2019,
    `2023-Q2 to 2024-Q2` = growth_2023_2024,
    stringsAsFactors = FALSE
  ))
}

# Format growth rates as percentages
growth_rates$`2016-Q2 to 2019-Q2` <- sprintf("%.2f%%", growth_rates$`2016-Q2 to 2019-Q2`)
growth_rates$`2023-Q2 to 2024-Q2` <- sprintf("%.2f%%", growth_rates$`2023-Q2 to 2024-Q2`)

# Create description sheet for the new Excel file
create_description_sheet <- function() {
  description_df <- data.frame(
    Sheet = c(
      # Worker count sheets
      names(lac9_results),
      
      # Growth rate sheet
      "LAC-9 Growth Rates",
      
      "Description"
    ),
    Description = c(
      # Worker count descriptions
      rep("Total number of employed workers for LAC-9 (sum of all 9 countries)", length(lac9_results)),
      
      # Growth rate description
      "Annualized percentage change in employed workers for LAC-9 between periods (2016-Q2 to 2019-Q2 and 2023-Q2 to 2024-Q2)",
      
      "This sheet contains descriptions of each sheet in this workbook"
    ),
    stringsAsFactors = FALSE
  )
  
  return(description_df)
}

# Create description sheet
description_df <- create_description_sheet()

# Create the output Excel file with current date
output_file <- file.path(output_dir, paste0(today_date, "-LAC9-totals-and-growth.xlsx"))

cat("\n=============================\n")
cat("CREATING OUTPUT FILE\n")
cat("Output file:", output_file, "\n")
cat("=============================\n")

# Prepare sheets for Excel
sheets <- c(
  # LAC-9 total sheets
  lac9_results,
  
  # Add growth rates sheet
  list("LAC-9 Growth Rates" = growth_rates),
  
  # Add description sheet
  list("Description" = description_df)
)

# Save Excel file
write_xlsx(sheets, path = output_file)
cat("✅ Analysis saved to:", output_file, "\n")

cat("\n=============================\n")
cat("ANALYSIS COMPLETE\n")
cat("=============================\n")

# Clean up environment to free memory
rm(list = ls())
gc()
