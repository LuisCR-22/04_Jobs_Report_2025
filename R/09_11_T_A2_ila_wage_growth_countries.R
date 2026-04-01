#===============================================
# Streamlined Labor Income Analysis - Reported Values Only
#===============================================

# This script analyzes reported labor income and wage data for multiple Latin American countries,
# comparing specific periods for each country and calculating annualized growth rates.
# Author: Luis Castellanos - Stats Team (lcasterodr@worldbank.org)
# Last modified: 2025-05-06

# ==============================================================================
# DATALIBWEB & NOCOHH NOTICE
# ==============================================================================
# NOTE: please retrieve data directly from datalibweb 
# 
# IMPORTANT: The results, might change due to updates in the LABLAC datasets and the 
# use of the nocohh option. For MEX, using the nocohh datasets is critical 
# so that the labor statistics results are consistent with those reported by the ILO.
# ==============================================================================
# -----------------------------------------------------------------------------


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
data_dir <- "<<DATALIBWEB_PATH_OR_GENERIC_URL>>"
output_dir <- "<<DATALIBWEB_PATH_OR_GENERIC_URL>>"
output_file <- paste0(output_dir, "/", Sys.Date(), "-reported-earnings-multi-country-V2.xlsx")

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

# Initialize results storage
labor_income_results <- data.frame() # reported labor income
wage_results <- data.frame() # reported wage

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
  
  # 1. Reported labor income: Average ila_ppp17 (excluding missing) for ocupado=1 and cohi=1
  labor_data <- data %>%
    filter(ocupado == 1, cohi == 1, !is.na(ila_ppp17))
  
  mean_labor_income <- weighted.mean(labor_data$ila_ppp17, labor_data$pondera, na.rm = TRUE)
  
  labor_income_results <- bind_rows(labor_income_results,
                                    data.frame(
                                      country = country,
                                      period = period,
                                      reported_labor_income = mean_labor_income,
                                      n_observations = nrow(labor_data)
                                    ))
  
  # 2. Reported wage: Average wage_ppp17 (or ilaho_ppp17 for Peru) excluding missing
  wage_data <- data %>%
    filter(asal == 1, cohi == 1, !is.na(.data[[wage_var]]))
  
  mean_wage <- weighted.mean(wage_data[[wage_var]], wage_data$pondera, na.rm = TRUE)
  
  wage_results <- bind_rows(wage_results,
                            data.frame(
                              country = country,
                              period = period,
                              reported_wage = mean_wage,
                              n_observations = nrow(wage_data),
                              wage_variable = wage_var
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
    return(list(growth = NA, start_value = NA, end_value = NA, n_obs_start = NA, n_obs_end = NA))
  }
  
  start_value <- start_data[[value_col]]
  n_obs_start <- start_data$n_observations
  
  # Get end value
  end_data <- data %>% 
    filter(country == country_val, period == end_period)
  
  if (nrow(end_data) == 0) {
    return(list(growth = NA, start_value = NA, end_value = NA, n_obs_start = n_obs_start, n_obs_end = NA))
  }
  
  end_value <- end_data[[value_col]]
  n_obs_end <- end_data$n_observations
  
  # Calculate time span
  time_span <- calculate_time_span(start_period, end_period)
  
  # Calculate growth rate
  if (start_value > 0 && time_span > 0) {
    growth_rate <- ((end_value / start_value) ^ (1/time_span)) - 1
    return(list(
      growth = growth_rate * 100,  # Convert to percentage
      start_value = start_value,
      end_value = end_value,
      n_obs_start = n_obs_start,
      n_obs_end = n_obs_end
    ))
  } else {
    return(list(
      growth = NA, 
      start_value = start_value, 
      end_value = end_value,
      n_obs_start = n_obs_start,
      n_obs_end = n_obs_end
    ))
  }
}

# Create summary results with both values and growth rates
summary_results <- data.frame()

for (country in names(country_periods)) {
  # Get periods for this country
  periods <- country_periods[[country]]
  period_text <- paste(periods[1], "to", periods[2])
  
  # Calculate growth rates for each metric
  labor_result <- calculate_annualized_growth(labor_income_results, "reported_labor_income", country, periods)
  wage_result <- calculate_annualized_growth(wage_results, "reported_wage", country, periods)
  
  # Get wage variable name (to note if Peru is using different variable)
  wage_var_name <- wage_results %>% 
    filter(country == country, period == periods[1]) %>% 
    pull(wage_variable)
  if (length(wage_var_name) == 0) wage_var_name <- NA
  
  # Calculate time span
  time_span <- calculate_time_span(periods[1], periods[2])
  
  # Add results to dataframe
  summary_results <- bind_rows(summary_results,
                               data.frame(
                                 country = country,
                                 periods = period_text,
                                 time_span_years = time_span,
                                 
                                 # Labor income values
                                 labor_start = labor_result$start_value,
                                 labor_end = labor_result$end_value,
                                 labor_growth = labor_result$growth,
                                 labor_obs_start = labor_result$n_obs_start,
                                 labor_obs_end = labor_result$n_obs_end,
                                 
                                 # Wage values
                                 wage_start = wage_result$start_value,
                                 wage_end = wage_result$end_value,
                                 wage_growth = wage_result$growth,
                                 wage_obs_start = wage_result$n_obs_start,
                                 wage_obs_end = wage_result$n_obs_end,
                                 wage_var = wage_var_name
                               ))
}

# Create a more compact growth results table
growth_results <- summary_results %>%
  select(country, periods, time_span_years, labor_growth, wage_growth)

# Create description sheet explaining each calculation
peru_note <- "NOTE: For Peru, 'ilaho_ppp17' is used instead of 'wage_ppp17' in all wage calculations."

descriptions <- data.frame(
  sheet_name = c("labor_income", "wage", "summary", "growth", "desc"),
  description = c(
    "Reported mean total labor income (ila_ppp17) excluding missing values, filtered for ocupado=1, cohi=1, edad>14",
    paste("Reported mean wage (wage_ppp17) excluding missing values, filtered for asal=1, cohi=1, edad>14.", peru_note),
    "Summary table with initial values, end values, growth rates, and observation counts for both metrics",
    "Compact table showing only annualized growth rates (%) between start and end periods",
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
    "labor_income" = labor_income_results,
    "wage" = wage_results,
    "summary" = summary_results,
    "growth" = growth_results,
    "desc" = descriptions
  ),
  path = output_file
)

cat("\nAnalysis complete. Results saved to:", output_file, "\n")