# Regional Aggregates for Labor Market Rates and Earnings
# This script creates regional aggregates (LAC-9 and LAC-7) for employment rates and labor earnings by calculating weighted averages across Latin American countries, using working-age population weights for employment metrics and worker weights for income metrics.
# This code takes the output from the previous script (08_2025-03-04_employment rate-groups-V3.R)
# Author: Luis Alberto Castañeda
# Date: 2025-06-05
# -----------------------------------------------------------------------------

# ==============================================================================
# DATALIBWEB & NOCOHH NOTICE
# ==============================================================================
# NOTE: Uers should retrieve data directly from datalibweb:
#
# IMPORTANT: Results, might change due to:
#   1. Updates in the LABLAC datasets
#   2. The use of the nocohh option
# For MEX, using nocohh is critical so that labor statistics are consistent
# with those reported by the ILO.
# ==============================================================================

# Clean environment and load packages
rm(list = ls())
cat("\014")  # Clear console

# Regional Aggregates for Labor Market Rates and Earnings
# Creates LAC-9 and LAC-7 aggregates using population and worker weights

# Load required packages
library(readxl)  # For reading Excel files
library(writexl) # For writing Excel files
library(dplyr)   # For data manipulation
library(tidyr)   # For reshaping data

# Define file paths
input_file <- "<<DATALIBWEB_PATH_OR_GENERIC_URL>>"  # Input file
output_dir <- "<<DATALIBWEB_PATH_OR_GENERIC_URL>>"

# Define country lists
lac9_countries <- c("Bolivia", "Brazil", "Chile", "Colombia", "Costa Rica", "Mexico", "Peru", "Uruguay", "Argentina")
lac7_countries <- c("Bolivia", "Brazil", "Costa Rica", "Mexico", "Peru", "Uruguay", "Argentina")

# Print status
cat("Creating regional aggregates for labor market indicators\n")
cat("Input file:", input_file, "\n")

# Get sheet names from the Excel file
sheets <- excel_sheets(input_file)
employment_sheets <- sheets[grepl("Employment$", sheets)]
income_sheets <- sheets[grepl("Mean Income$", sheets)]
worker_weight_sheet <- "Workers weight"
wap_weight_sheet <- "WAP weight"

cat("Found", length(employment_sheets), "employment sheets\n")
cat("Found", length(income_sheets), "income sheets\n")

# Function to read a sheet and convert values to numeric
read_sheet <- function(file_path, sheet_name) {
  df <- read_excel(file_path, sheet = sheet_name)
  
  # Identify period column (assuming it's the first column)
  period_col_name <- names(df)[1]
  
  # Convert all non-Period columns to numeric
  for (col_name in names(df)[-1]) {
    if (!is.numeric(df[[col_name]])) {
      # For character columns, remove any non-numeric chars except decimal and negative
      if (is.character(df[[col_name]])) {
        df[[col_name]] <- gsub("[^0-9\\.-]", "", df[[col_name]])
      }
      # Convert to numeric
      df[[col_name]] <- as.numeric(df[[col_name]])
    }
  }
  
  return(df)
}

# Function to calculate weighted aggregate for a demographic group
calculate_aggregate <- function(data_sheet, weight_sheet, countries, sheet_label) {
  # Identify period column
  period_col_name <- names(data_sheet)[1]
  
  # Keep only relevant countries that exist in both sheets
  available_countries <- intersect(names(data_sheet), countries)
  available_countries <- intersect(available_countries, names(weight_sheet))
  
  if (length(available_countries) == 0) {
    warning("No countries from the list are available in both data and weight sheets")
    return(data.frame(Period = data_sheet[[period_col_name]], !!sheet_label := NA))
  }
  
  # Initialize result vector
  periods <- data_sheet[[period_col_name]]
  result <- numeric(length(periods))
  
  # Calculate weighted average for each period
  for (i in 1:length(periods)) {
    current_period <- periods[i]
    
    # Extract data and weights for this period
    period_data <- as.numeric(data_sheet[i, available_countries])
    period_weights <- as.numeric(weight_sheet[i, available_countries])
    
    # Check for missing data
    valid_indices <- !is.na(period_data) & !is.na(period_weights)
    if (sum(valid_indices) == 0) {
      result[i] <- NA
      next
    }
    
    # Calculate weighted average for this period
    weighted_sum <- sum(period_data[valid_indices] * period_weights[valid_indices], na.rm = TRUE)
    total_weight <- sum(period_weights[valid_indices], na.rm = TRUE)
    
    if (total_weight > 0) {
      result[i] <- weighted_sum / total_weight
    } else {
      result[i] <- NA
    }
  }
  
  # Create result dataframe
  result_df <- data.frame(
    Period = periods,
    Value = result
  )
  names(result_df)[2] <- sheet_label
  
  return(result_df)
}

# Read weight sheets
cat("Reading weight sheets...\n")
wap_weights <- read_sheet(input_file, wap_weight_sheet)
worker_weights <- read_sheet(input_file, worker_weight_sheet)

# Initialize results dataframes with period column
period_col_name <- names(wap_weights)[1]
employment_results <- data.frame(wap_weights[period_col_name])
names(employment_results)[1] <- period_col_name

income_results <- data.frame(worker_weights[period_col_name])
names(income_results)[1] <- period_col_name

# Process employment sheets for LAC-9 (using WAP weights)
cat("Calculating LAC-9 employment rate aggregates...\n")
for (sheet_name in employment_sheets) {
  # Create a clean label for the aggregate column
  clean_label <- gsub(" Employment$", "", sheet_name)
  agg_label <- paste0("LAC9_", clean_label)
  
  # Read data sheet
  sheet_data <- read_sheet(input_file, sheet_name)
  
  # Calculate LAC-9 aggregate
  agg_result <- calculate_aggregate(
    sheet_data, 
    wap_weights, 
    lac9_countries, 
    agg_label
  )
  
  # Add to results
  employment_results <- employment_results %>%
    left_join(agg_result, by = period_col_name)
  
  cat("  Processed:", sheet_name, "\n")
}

# Process income sheets for LAC-7 (using worker weights)
cat("Calculating LAC-7 earnings aggregates...\n")
for (sheet_name in income_sheets) {
  # Create a clean label for the aggregate column
  clean_label <- gsub(" Mean Income$", "", sheet_name)
  agg_label <- paste0("LAC7_", clean_label)
  
  # Read data sheet
  sheet_data <- read_sheet(input_file, sheet_name)
  
  # Calculate LAC-7 aggregate
  agg_result <- calculate_aggregate(
    sheet_data, 
    worker_weights, 
    lac7_countries, 
    agg_label
  )
  
  # Add to results
  income_results <- income_results %>%
    left_join(agg_result, by = period_col_name)
  
  cat("  Processed:", sheet_name, "\n")
}

# Create output filename with current date
today_date <- format(Sys.Date(), "%Y-%m-%d")
output_file <- file.path(output_dir, paste0(today_date, "-groups-earnings.xlsx"))

# Write to Excel
write_xlsx(
  list(
    "LAC9 Employment" = employment_results,
    "LAC7 Earnings" = income_results
  ),
  path = output_file
)

cat("\n✅ Regional aggregates saved to:", output_file, "\n")
