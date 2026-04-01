#===============================================
# Multi-Country Labor Income Analysis (2016-2024)
#===============================================

# This script analyzes labor income and wage data for multiple Latin American countries,
# comparing specific periods for each country with various filtering conditions
# and calculating annualized growth rates.
# Author: Luis Castellanos
# Last modified: 2025-05-06

# Clear workspace and memory
rm(list = ls())
gc()

# Load necessary libraries
if (!require("haven")) install.packages("haven", dependencies = TRUE)
if (!require("writexl")) install.packages("writexl", dependencies = TRUE)
if (!require("dplyr")) install.packages("dplyr", dependencies = TRUE)

library(haven)
library(writexl)
library(dplyr)

# Set directory and output paths
data_dir <- "C:/Users/wb593225/OneDrive - WBG/Desktop/Luis - Private/Lablac/Semiannual report/Lablac data"
output_dir <- "C:/Users/wb593225/OneDrive - WBG/Desktop/Luis - Private/Lablac/Semiannual report/Excel/Lablac output/01_New outputs/06 new wage ila annualized"
output_file <- paste0(output_dir, "/", Sys.Date(), "-annualized-ila-wage-multi-country.xlsx")

# Create output directory if it doesn't exist
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

# Define country-period mappings
country_periods <- list(
  "arg" = c("2016_q02", "2024_q02"),
  "bol" = c("2016_q02", "2024_q02"),
  "bra" = c("2016_q02", "2024_q02"),
  "chl" = c("2016_q04", "2023_q04"),
  "col" = c("2021_q04", "2023_q04"),
  "cri" = c("2017_q04", "2024_q04"),
  "dom" = c("2017_q02", "2024_q02"),
  "ecu" = c("2021_q02", "2024_q02"),
  "mex" = c("2016_q02", "2024_q02"),
  "pry" = c("2022_q02", "2024_q02"),
  "per" = c("2016_q02", "2024_q02"),
  "slv" = c("2016_q02", "2023_q02"),
  "ury" = c("2022_q02", "2024_q02")
)

# List all .dta files in the directory
files <- list.files(path = data_dir, pattern = "\\.dta$", full.names = TRUE)

# Find relevant files for each country and period
selected_files <- list()

for (country in names(country_periods)) {
  cat("\nFinding files for", country, "\n")
  
  for (period in country_periods[[country]]) {
    # Create pattern to match files for this country and period
    pattern <- paste0("LABLAC_", country, ".*", period)
    matching_files <- files[grep(pattern, files, ignore.case = TRUE)]
    
    if (length(matching_files) > 0) {
      if (length(matching_files) > 1) {
        cat("  Multiple files found for", country, "in period", period, "- using first one\n")
      }
      
      file_path <- matching_files[1]
      selected_files[[paste(country, period, sep = "_")]] <- file_path
      cat("  Selected for", period, ":", basename(file_path), "\n")
    } else {
      cat("⚠️ No file found for", country, "in period", period, "\n")
    }
  }
}

cat("\nTotal files to process:", length(selected_files), "\n")

# Initialize results storage for each sheet
sheet1_results <- data.frame() # ila_ppp17 with missing as 0
sheet2_results <- data.frame() # ila_ppp17 excluding missing
sheet3_results <- data.frame() # wage_ppp17 with missing as 0
sheet4_results <- data.frame() # wage_ppp17 excluding missing
sheet5_results <- data.frame() # Reference method - no filters
sheet6_results <- data.frame() # Reference method with cohi filter

# Process each file
for (file_key in names(selected_files)) {
  file <- selected_files[[file_key]]
  
  # Extract country and period from the file key
  parts <- strsplit(file_key, "_")[[1]]
  country <- parts[1]
  period <- paste(parts[2], parts[3], sep = "_")
  
  cat("\nProcessing file for", country, "period:", period, "\n")
  cat("File:", basename(file), "\n")
  
  # Read data and handle errors
  data <- tryCatch(read_dta(file), error = function(e) {
    cat("Error reading file:", basename(file), "\n")
    return(NULL)
  })
  if (is.null(data)) next
  
  # Check for required variables
  required_vars <- c("pais_ocaux", "ocupado", "cohi", "asal", "ila_ppp17", "pondera", "edad")
  
  # For Peru, use ilaho_ppp17 instead of wage_ppp17
  wage_var <- if(country == "per") "ilaho_ppp17" else "wage_ppp17"
  required_vars <- c(required_vars, wage_var)
  
  # Check for urbano if needed for Peru or Paraguay
  if (country %in% c("per", "pry")) {
    required_vars <- c(required_vars, "urbano")
  }
  
  missing_vars <- required_vars[!required_vars %in% colnames(data)]
  
  if (length(missing_vars) > 0) {
    cat("Skipping file due to missing required variables:", paste(missing_vars, collapse=", "), "\n")
    next
  }
  
  # Apply urban filter for Peru and Paraguay
  if (country %in% c("per", "pry")) {
    data <- data %>% filter(urbano == 1)
    cat("Applied urban filter for", country, "\n")
  }
  
  # Filter for workers over 14 years old for all analyses
  data <- data %>% filter(edad > 14)
  
  # 1. Sheet 1: Average ila_ppp17 (missing as 0) for ocupado=1 and cohi=1
  sheet1_data <- data %>%
    filter(ocupado == 1, cohi == 1) %>%
    mutate(ila_ppp17_filled = ifelse(is.na(ila_ppp17), 0, ila_ppp17))
  
  mean_ila_with_zeros <- weighted.mean(sheet1_data$ila_ppp17_filled, sheet1_data$pondera, na.rm = TRUE)
  
  sheet1_results <- bind_rows(sheet1_results,
                              data.frame(
                                country = country,
                                period = period,
                                mean_total_labor_income_per_worker = mean_ila_with_zeros
                              ))
  
  # 2. Sheet 2: Average ila_ppp17 (excluding missing) for ocupado=1 and cohi=1
  sheet2_data <- data %>%
    filter(ocupado == 1, cohi == 1, !is.na(ila_ppp17))
  
  mean_ila_no_zeros <- weighted.mean(sheet2_data$ila_ppp17, sheet2_data$pondera, na.rm = TRUE)
  
  sheet2_results <- bind_rows(sheet2_results,
                              data.frame(
                                country = country,
                                period = period,
                                reported_mean_total_labor_income = mean_ila_no_zeros
                              ))
  
  # 3. Sheet 3: Average wage_ppp17 (or ilaho_ppp17 for Peru) with missing as 0
  sheet3_data <- data %>%
    filter(asal == 1, cohi == 1) %>%
    mutate(wage_var_filled = ifelse(is.na(.data[[wage_var]]), 0, .data[[wage_var]]))
  
  mean_wage_with_zeros <- weighted.mean(sheet3_data$wage_var_filled, sheet3_data$pondera, na.rm = TRUE)
  
  sheet3_results <- bind_rows(sheet3_results,
                              data.frame(
                                country = country,
                                period = period,
                                mean_wage_per_worker = mean_wage_with_zeros
                              ))
  
  # 4. Sheet 4: Average wage_ppp17 (or ilaho_ppp17 for Peru) excluding missing
  sheet4_data <- data %>%
    filter(asal == 1, cohi == 1, !is.na(.data[[wage_var]]))
  
  mean_wage_no_zeros <- weighted.mean(sheet4_data[[wage_var]], sheet4_data$pondera, na.rm = TRUE)
  
  sheet4_results <- bind_rows(sheet4_results,
                              data.frame(
                                country = country,
                                period = period,
                                reported_mean_wage = mean_wage_no_zeros
                              ))
  
  # 5. Sheet 5: Reference method - Calculate weighted means without filtering (except edad > 14)
  mean_ila_ref <- sum(data$ila_ppp17 * data$pondera, na.rm = TRUE) / sum(data$pondera, na.rm = TRUE)
  
  sheet5_results <- bind_rows(sheet5_results,
                              data.frame(
                                country = country,
                                period = period,
                                reference_mean_labor_income = mean_ila_ref
                              ))
  
  # 6. Sheet 6: Reference method with cohi filter
  data_cohi <- data %>% filter(cohi == 1)
  mean_ila_ref_cohi <- sum(data_cohi$ila_ppp17 * data_cohi$pondera, na.rm = TRUE) / sum(data_cohi$pondera, na.rm = TRUE)
  
  sheet6_results <- bind_rows(sheet6_results,
                              data.frame(
                                country = country,
                                period = period,
                                reference_mean_labor_income_cohi = mean_ila_ref_cohi
                              ))
  
  cat("Processed:", country, period, "- Records:", nrow(data), "\n")
}

# Function to calculate time span between two periods (in years)
calculate_time_span <- function(start_period, end_period) {
  # Parse years and quarters
  start_year <- as.numeric(substr(start_period, 1, 4))
  start_quarter <- as.numeric(substr(start_period, nchar(start_period), nchar(start_period)))
  
  end_year <- as.numeric(substr(end_period, 1, 4))
  end_quarter <- as.numeric(substr(end_period, nchar(end_period), nchar(end_period)))
  
  # Calculate time span in years
  years_diff <- end_year - start_year
  quarters_diff <- end_quarter - start_quarter
  
  return(years_diff + quarters_diff / 4)
}

# Calculate annualized growth rates
calculate_annualized_growth <- function(data, value_col, country_val, periods) {
  # Get start and end periods
  start_period <- periods[1]
  end_period <- periods[2]
  
  # Get start value
  start_data <- data %>% 
    filter(country == country_val, period == start_period)
  
  if (nrow(start_data) == 0) {
    return(list(growth = NA, start_value = NA, end_value = NA))
  }
  
  start_value <- start_data[[value_col]]
  
  # Get end value
  end_data <- data %>% 
    filter(country == country_val, period == end_period)
  
  if (nrow(end_data) == 0) {
    return(list(growth = NA, start_value = NA, end_value = NA))
  }
  
  end_value <- end_data[[value_col]]
  
  # Calculate time span
  time_span <- calculate_time_span(start_period, end_period)
  
  # Calculate growth rate
  if (start_value > 0 && time_span > 0) {
    growth_rate <- ((end_value / start_value) ^ (1/time_span)) - 1
    return(list(
      growth = growth_rate * 100,  # Convert to percentage
      start_value = start_value,
      end_value = end_value
    ))
  } else {
    return(list(growth = NA, start_value = start_value, end_value = end_value))
  }
}

# Create growth results
growth_results <- data.frame()

for (country in names(country_periods)) {
  # Get periods for this country
  periods <- country_periods[[country]]
  period_text <- paste(periods[1], "to", periods[2])
  
  # Calculate growth rates for each metric with values for debugging
  labor_0s_result <- calculate_annualized_growth(sheet1_results, "mean_total_labor_income_per_worker", country, periods)
  labor_rep_result <- calculate_annualized_growth(sheet2_results, "reported_mean_total_labor_income", country, periods)
  wage_0s_result <- calculate_annualized_growth(sheet3_results, "mean_wage_per_worker", country, periods)
  wage_rep_result <- calculate_annualized_growth(sheet4_results, "reported_mean_wage", country, periods)
  ref_labor_result <- calculate_annualized_growth(sheet5_results, "reference_mean_labor_income", country, periods)
  ref_labor_cohi_result <- calculate_annualized_growth(sheet6_results, "reference_mean_labor_income_cohi", country, periods)
  
  # Calculate time span
  time_span <- calculate_time_span(periods[1], periods[2])
  
  # Add results to dataframe
  growth_results <- bind_rows(growth_results,
                              data.frame(
                                country = country,
                                periods = period_text,
                                time_span_years = time_span,
                                total_labor_inc_0s = labor_0s_result$growth,
                                reported_labor_inc = labor_rep_result$growth,
                                wage_0s = wage_0s_result$growth,
                                reported_wage = wage_rep_result$growth,
                                ref_labor_inc = ref_labor_result$growth,
                                ref_labor_inc_cohi = ref_labor_cohi_result$growth
                              ))
}

# Create description sheet explaining each calculation
peru_note <- "NOTE: For Peru, 'ilaho_ppp17' is used instead of 'wage_ppp17' in all wage calculations (sheets 'wage_0s' and 'wage_rep')."

descriptions <- data.frame(
  sheet_name = c("labor_0s", "labor_rep", "wage_0s", "wage_rep", "ref_labor", "ref_labor_cohi", "growth", "desc"),
  description = c(
    "Mean total labor income per worker (ila_ppp17) with missing values as 0, filtered for ocupado=1, cohi=1, edad>14",
    "Reported mean total labor income (ila_ppp17) excluding missing values, filtered for ocupado=1, cohi=1, edad>14",
    paste("Mean wage per worker (wage_ppp17) with missing values as 0, filtered for asal=1, cohi=1, edad>14.", peru_note),
    paste("Reported mean wage (wage_ppp17) excluding missing values, filtered for asal=1, cohi=1, edad>14.", peru_note),
    "Reference calculation of labor income (ila_ppp17) using sum(var*pondera)/sum(pondera), filtered for edad>14",
    "Reference calculation of labor income (ila_ppp17) using sum(var*pondera)/sum(pondera), filtered for cohi=1, edad>14",
    "Annualized growth rates (%) between start and end periods for each country across all metrics",
    "This sheet - descriptions of all calculations and notes"
  )
)

# Add additional notes to the description
urban_note <- "Urban filter (urbano==1) is applied for Peru and Paraguay data."
age_note <- "All calculations are limited to individuals above 14 years old (edad>14)."

additional_notes <- data.frame(
  sheet_name = c("Note 1", "Note 2", "Note 3"),
  description = c(
    peru_note,
    urban_note,
    age_note
  )
)

descriptions <- bind_rows(descriptions, additional_notes)

# Create Excel file with all sheets using shorter names
write_xlsx(
  list(
    "labor_0s" = sheet1_results,
    "labor_rep" = sheet2_results,
    "wage_0s" = sheet3_results,
    "wage_rep" = sheet4_results,
    "ref_labor" = sheet5_results,
    "ref_labor_cohi" = sheet6_results,
    "growth" = growth_results,
    "desc" = descriptions
  ),
  path = output_file
)

cat("\nAnalysis complete. Results saved to:", output_file, "\n")
