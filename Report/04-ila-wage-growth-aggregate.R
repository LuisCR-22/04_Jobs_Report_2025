#===============================================
# Streamlined Labor Income Analysis with LAC Aggregate
#===============================================

# This script analyzes reported labor income and wage data for multiple Latin American countries,
# and calculates a LAC regional aggregate using country weights.
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
output_file <- paste0(output_dir, "/", Sys.Date(), "-reported-earnings-lac-aggregate.xlsx")

# Create output directory if it doesn't exist
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

# Define country weights for LAC aggregate
weights_2016_q02 <- c(
  "arg" = 6.1, "bol" = 2.5, "bra" = 48.4, "chl" = 4.3, "col" = 11.8, 
  "cri" = 1.0, "dom" = 0.0, "ecu" = 0.0, "mex" = 18.5, "per" = 2.5, 
  "pry" = 0.0, "slv" = 1.4, "ury" = 0.9
)

weights_2024_q02 <- c(
  "arg" = 6.2, "bol" = 3.2, "bra" = 47.1, "chl" = 4.4, "col" = 10.6, 
  "cri" = 0.9, "dom" = 2.3, "ecu" = 3.8, "mex" = 17.0, "per" = 2.5, 
  "pry" = 1.3, "slv" = 0.0, "ury" = 0.8
)

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

# Map periods for regional aggregate calculation
period_mapping <- list(
  "start" = c("2016_q02", "2016_q04"),  # Consider both Q2 and Q4 for start period
  "end" = c("2024_q02", "2023_q04", "2024_q04")  # Consider alternative end periods
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

# Function to adjust weights for available countries
adjust_weights <- function(weights, available_countries) {
  # Create a new vector with only available countries
  adjusted_weights <- weights[available_countries]
  
  # Replace NA values with 0
  adjusted_weights[is.na(adjusted_weights)] <- 0
  
  # Normalize to sum to 100
  sum_weights <- sum(adjusted_weights)
  if (sum_weights > 0) {
    adjusted_weights <- adjusted_weights / sum_weights * 100
  }
  
  return(adjusted_weights)
}

# Identify countries with both start and end periods for labor income
labor_countries_with_data <- data.frame()

for (country in unique(labor_income_results$country)) {
  # Get periods available for this country
  country_periods <- labor_income_results$period[labor_income_results$country == country]
  
  # Find if country has a start period
  start_matches <- intersect(country_periods, period_mapping$start)
  if (length(start_matches) > 0) {
    start_period <- start_matches[1]  # Use first match if multiple
    
    # Find if country has an end period
    end_matches <- intersect(country_periods, period_mapping$end)
    if (length(end_matches) > 0) {
      end_period <- end_matches[1]  # Use first match if multiple
      
      # Record this country's data
      labor_countries_with_data <- bind_rows(
        labor_countries_with_data,
        data.frame(
          country = country,
          start_period = start_period,
          end_period = end_period
        )
      )
    }
  }
}

# Do the same for wage data
wage_countries_with_data <- data.frame()

for (country in unique(wage_results$country)) {
  # Get periods available for this country
  country_periods <- wage_results$period[wage_results$country == country]
  
  # Find if country has a start period
  start_matches <- intersect(country_periods, period_mapping$start)
  if (length(start_matches) > 0) {
    start_period <- start_matches[1]  # Use first match if multiple
    
    # Find if country has an end period
    end_matches <- intersect(country_periods, period_mapping$end)
    if (length(end_matches) > 0) {
      end_period <- end_matches[1]  # Use first match if multiple
      
      # Record this country's data
      wage_countries_with_data <- bind_rows(
        wage_countries_with_data,
        data.frame(
          country = country,
          start_period = start_period,
          end_period = end_period
        )
      )
    }
  }
}

# Adjust weights for labor income calculation
labor_weights_start <- adjust_weights(weights_2016_q02, labor_countries_with_data$country)
labor_weights_end <- adjust_weights(weights_2024_q02, labor_countries_with_data$country)

# Create a weights table for labor income
labor_weights_table <- data.frame(
  country = labor_countries_with_data$country,
  start_period = labor_countries_with_data$start_period,
  end_period = labor_countries_with_data$end_period,
  original_weight_start = sapply(labor_countries_with_data$country, function(c) {
    if (c %in% names(weights_2016_q02)) weights_2016_q02[c] else 0
  }),
  original_weight_end = sapply(labor_countries_with_data$country, function(c) {
    if (c %in% names(weights_2024_q02)) weights_2024_q02[c] else 0
  }),
  adjusted_weight_start = unname(labor_weights_start),
  adjusted_weight_end = unname(labor_weights_end)
)

# Compute weighted average for labor income
lac_labor_start <- 0
lac_labor_end <- 0

for (i in 1:nrow(labor_countries_with_data)) {
  country <- labor_countries_with_data$country[i]
  start_period <- labor_countries_with_data$start_period[i]
  end_period <- labor_countries_with_data$end_period[i]
  
  # Get country values
  country_labor_start <- labor_income_results$reported_labor_income[
    labor_income_results$country == country & labor_income_results$period == start_period
  ]
  
  country_labor_end <- labor_income_results$reported_labor_income[
    labor_income_results$country == country & labor_income_results$period == end_period
  ]
  
  # Add weighted values
  lac_labor_start <- lac_labor_start + (country_labor_start * labor_weights_table$adjusted_weight_start[i] / 100)
  lac_labor_end <- lac_labor_end + (country_labor_end * labor_weights_table$adjusted_weight_end[i] / 100)
}

# Adjust weights for wage calculation
wage_weights_start <- adjust_weights(weights_2016_q02, wage_countries_with_data$country)
wage_weights_end <- adjust_weights(weights_2024_q02, wage_countries_with_data$country)

# Create a weights table for wage
wage_weights_table <- data.frame(
  country = wage_countries_with_data$country,
  start_period = wage_countries_with_data$start_period,
  end_period = wage_countries_with_data$end_period,
  original_weight_start = sapply(wage_countries_with_data$country, function(c) {
    if (c %in% names(weights_2016_q02)) weights_2016_q02[c] else 0
  }),
  original_weight_end = sapply(wage_countries_with_data$country, function(c) {
    if (c %in% names(weights_2024_q02)) weights_2024_q02[c] else 0
  }),
  adjusted_weight_start = unname(wage_weights_start),
  adjusted_weight_end = unname(wage_weights_end)
)

# Compute weighted average for wage
lac_wage_start <- 0
lac_wage_end <- 0

for (i in 1:nrow(wage_countries_with_data)) {
  country <- wage_countries_with_data$country[i]
  start_period <- wage_countries_with_data$start_period[i]
  end_period <- wage_countries_with_data$end_period[i]
  
  # Get country values
  country_wage_start <- wage_results$reported_wage[
    wage_results$country == country & wage_results$period == start_period
  ]
  
  country_wage_end <- wage_results$reported_wage[
    wage_results$country == country & wage_results$period == end_period
  ]
  
  # Add weighted values
  lac_wage_start <- lac_wage_start + (country_wage_start * wage_weights_table$adjusted_weight_start[i] / 100)
  lac_wage_end <- lac_wage_end + (country_wage_end * wage_weights_table$adjusted_weight_end[i] / 100)
}

# Calculate time span (reference period is 2016_q02 to 2024_q02)
lac_time_span <- calculate_time_span("2016_q02", "2024_q02")

# Calculate growth rates
lac_labor_growth <- ((lac_labor_end / lac_labor_start) ^ (1/lac_time_span)) - 1
lac_wage_growth <- ((lac_wage_end / lac_wage_start) ^ (1/lac_time_span)) - 1

# Calculate annualized growth rates for individual countries
growth_results <- data.frame()

for (country in unique(labor_income_results$country)) {
  # Get the country's periods from the original mapping
  if (!(country %in% names(country_periods))) next
  
  country_start_period <- NULL
  country_end_period <- NULL
  
  # Check if country has both periods
  country_data <- labor_income_results %>% filter(country == !!country)
  
  # Find the relevant periods
  if (country_periods[[country]][1] %in% country_data$period) {
    country_start_period <- country_periods[[country]][1]
  } else if (country == "chl" && "2016_q04" %in% country_data$period) {
    # Special case for Chile
    country_start_period <- "2016_q04"
  }
  
  if (country_periods[[country]][2] %in% country_data$period) {
    country_end_period <- country_periods[[country]][2]
  } else if (country == "chl" && "2023_q04" %in% country_data$period) {
    # Special case for Chile
    country_end_period <- "2023_q04"
  }
  
  if (is.null(country_start_period) || is.null(country_end_period)) {
    next  # Skip if missing either period
  }
  
  # Get labor values
  labor_start <- labor_income_results$reported_labor_income[
    labor_income_results$country == country & labor_income_results$period == country_start_period
  ]
  
  labor_end <- labor_income_results$reported_labor_income[
    labor_income_results$country == country & labor_income_results$period == country_end_period
  ]
  
  # Get wage values
  wage_data <- wage_results %>% filter(country == !!country)
  
  wage_start <- NULL
  wage_end <- NULL
  
  if (country_start_period %in% wage_data$period) {
    wage_start <- wage_data$reported_wage[wage_data$period == country_start_period]
  }
  
  if (country_end_period %in% wage_data$period) {
    wage_end <- wage_data$reported_wage[wage_data$period == country_end_period]
  }
  
  # Calculate time span
  time_span <- calculate_time_span(country_start_period, country_end_period)
  
  # Calculate growth rates
  labor_growth <- ifelse(
    !is.null(labor_start) && !is.null(labor_end) && labor_start > 0,
    ((labor_end / labor_start) ^ (1/time_span) - 1) * 100,
    NA
  )
  
  wage_growth <- ifelse(
    !is.null(wage_start) && !is.null(wage_end) && wage_start > 0,
    ((wage_end / wage_start) ^ (1/time_span) - 1) * 100,
    NA
  )
  
  # Add to results
  growth_results <- bind_rows(
    growth_results,
    data.frame(
      country = country,
      periods = paste(country_start_period, "to", country_end_period),
      time_span_years = time_span,
      labor_growth = labor_growth,
      wage_growth = wage_growth
    )
  )
}

# Add LAC aggregate to growth results
growth_results <- bind_rows(
  growth_results,
  data.frame(
    country = "LAC-Aggregate",
    periods = "2016_q02 to 2024_q02 (weighted)",
    time_span_years = lac_time_span,
    labor_growth = lac_labor_growth * 100,
    wage_growth = lac_wage_growth * 100
  )
)

# Create a clean, non-repetitive summary table
summary_results <- data.frame(
  country = character(),
  period = character(),
  labor_income = numeric(),
  wage = numeric(),
  labor_obs = numeric(),
  wage_obs = numeric(),
  wage_var = character()
)

# Add country-level data
for (country in unique(c(labor_income_results$country, "LAC-Aggregate"))) {
  if (country == "LAC-Aggregate") {
    # Add the start period aggregate
    summary_results <- bind_rows(
      summary_results,
      data.frame(
        country = "LAC-Aggregate",
        period = "2016_q02 (weighted)",
        labor_income = lac_labor_start,
        wage = lac_wage_start,
        labor_obs = NA,
        wage_obs = NA,
        wage_var = NA
      )
    )
    
    # Add the end period aggregate
    summary_results <- bind_rows(
      summary_results,
      data.frame(
        country = "LAC-Aggregate",
        period = "2024_q02 (weighted)",
        labor_income = lac_labor_end,
        wage = lac_wage_end,
        labor_obs = NA,
        wage_obs = NA,
        wage_var = NA
      )
    )
  } else {
    # Get all periods for this country
    country_periods <- unique(labor_income_results$period[labor_income_results$country == country])
    
    for (period in country_periods) {
      # Get labor income data
      labor_data <- labor_income_results %>% 
        filter(country == !!country, period == !!period)
      
      labor_income <- if(nrow(labor_data) > 0) labor_data$reported_labor_income[1] else NA
      labor_obs <- if(nrow(labor_data) > 0) labor_data$n_observations[1] else NA
      
      # Get wage data
      wage_data <- wage_results %>% 
        filter(country == !!country, period == !!period)
      
      wage <- if(nrow(wage_data) > 0) wage_data$reported_wage[1] else NA
      wage_obs <- if(nrow(wage_data) > 0) wage_data$n_observations[1] else NA
      wage_var <- if(nrow(wage_data) > 0) wage_data$wage_variable[1] else NA
      
      # Add to summary
      summary_results <- bind_rows(
        summary_results,
        data.frame(
          country = country,
          period = period,
          labor_income = labor_income,
          wage = wage,
          labor_obs = labor_obs,
          wage_obs = wage_obs,
          wage_var = wage_var
        )
      )
    }
  }
}

# Create description sheet explaining each calculation
peru_note <- "NOTE: For Peru, 'ilaho_ppp17' is used instead of 'wage_ppp17' in all wage calculations."
lac_note <- "LAC-Aggregate values are calculated as weighted averages of countries with both start and end period data. Original weights are adjusted to only include available countries and normalized to sum to 100%."

descriptions <- data.frame(
  sheet_name = c(
    "summary", "growth", "labor_weights", "wage_weights", "labor_detail", "wage_detail", "desc"
  ),
  description = c(
    paste("Summary table with labor income and wage values by country and period.", lac_note),
    "Annualized growth rates (%) between start and end periods for each country and the LAC aggregate",
    "Countries and weights used for calculating the LAC-Aggregate labor income, showing both original and adjusted weights",
    "Countries and weights used for calculating the LAC-Aggregate wage, showing both original and adjusted weights",
    "Detailed labor income data including all observations and periods",
    paste("Detailed wage data including all observations and periods.", peru_note),
    "This sheet - descriptions of all calculations and notes"
  )
)

# Add additional notes to the description
urban_note <- "Urban filter (urbano==1) is applied for Peru and Paraguay data."
age_note <- "All calculations are limited to individuals above 14 years old (edad>14)."

additional_notes <- data.frame(
  sheet_name = c("Note 1", "Note 2", "Note 3", "Note 4"),
  description = c(
    peru_note,
    urban_note,
    age_note,
    lac_note
  )
)

descriptions <- bind_rows(descriptions, additional_notes)

# Create Excel file with all sheets using meaningful names
write_xlsx(
  list(
    "summary" = summary_results,
    "growth" = growth_results,
    "labor_weights" = labor_weights_table,
    "wage_weights" = wage_weights_table,
    "labor_detail" = labor_income_results,
    "wage_detail" = wage_results,
    "desc" = descriptions
  ),
  path = output_file
)

cat("\nAnalysis complete. Results saved to:", output_file, "\n")
