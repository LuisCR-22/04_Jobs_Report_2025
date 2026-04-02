# Revised Gini coefficient calculator for LAC countries
# This script processes LABLAC datasets to calculate Gini coefficients for wage and income variables.
# Focuses on specific country-period combinations with modified filters and aggregation
# Author: Luis Castellanos - Stats Team (lcasterodr@worldbank.org)
# Last modified: 2025-05-06

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
# -----------------------------------------------------------------------------


rm(list = ls())  # Clear the workspace
gc()  # Garbage collection

# Install required packages if not already installed
if (!require("haven")) install.packages("haven", dependencies = TRUE)
if (!require("dplyr")) install.packages("dplyr", dependencies = TRUE)
if (!require("writexl")) install.packages("writexl", dependencies = TRUE)
if (!require("ineq")) install.packages("ineq", dependencies = TRUE)
if (!require("tidyr")) install.packages("tidyr", dependencies = TRUE)
if (!require("stringr")) install.packages("stringr", dependencies = TRUE)

library(haven)      # Read Stata (.dta) files
library(dplyr)      # Data manipulation
library(writexl)    # Write output to Excel
library(ineq)       # Compute unweighted Gini
library(tidyr)      # For data reshaping
library(stringr)    # For string manipulation

# Define the data directory
data_dir <- "<<DATALIBWEB_PATH_OR_GENERIC_URL>>"

# Define country-specific period pairs
country_periods <- list(
  "arg" = c("2016-2", "2024-2"),
  "bol" = c("2016-2", "2024-2"),
  "bra" = c("2016-2", "2024-2"),
  "chl" = c("2016-4", "2023-4"),
  "col" = c("2021-2", "2023-2"),
  "cri" = c("2016-2", "2024-2"),
  "dom" = c("2017-2", "2024-2"),
  "ecu" = c("2021-2", "2024-2"),
  "slv" = c("2016-2", "2023-2"),
  "mex" = c("2016-2", "2024-2"),
  "per" = c("2016-2", "2024-2"),
  "pry" = c("2022-2", "2024-2"),
  "ury" = c("2021-2", "2024-2")
)

# Define LAC-7 countries
lac7_countries <- c("arg", "bol", "bra", "cri", "mex", "per", "ury")

# Create an empty list to store all file paths that match our criteria
selected_files <- list()

# List all .dta files
all_files <- list.files(path = data_dir, pattern = "^LABLAC_.*\\.dta$", full.names = TRUE)

# For each country, find the matching files for the specified periods
for (country in names(country_periods)) {
  periods <- country_periods[[country]]
  
  for (period in periods) {
    # Extract year and quarter
    year <- substr(period, 1, 4)
    quarter <- substr(period, 6, 6)
    
    # Format quarter with 'q' prefix and zero-padding
    quarter_formatted <- paste0("q", sprintf("%02d", as.numeric(quarter)))
    
    # Find matching files - pattern matches LABLAC_country_year_quarter_ALL.dta format
    matching_pattern <- paste0("LABLAC_", country, "_", year, "_", quarter_formatted, "_ALL\\.dta$")
    matching_files <- all_files[grep(matching_pattern, all_files, ignore.case = TRUE)]
    
    if (length(matching_files) > 0) {
      selected_files <- c(selected_files, matching_files)
    } else {
      cat("⚠️ Warning: No matching file found for", country, "period", period, "\n")
    }
  }
}

# Print selected files
cat("Selected", length(selected_files), "files for processing:\n")
for (file in selected_files) {
  cat("  -", basename(file), "\n")
}

# Define the earnings variables for analysis
earnings_vars <- c("wage_ppp17", "ila_ppp17")

# Function for weighted Gini calculation
weighted_gini <- function(values, weights) {
  valid <- !is.na(values) & !is.na(weights) & values > 0  # Remove NAs and non-positive values
  if (sum(valid) > 1) {
    values <- values[valid]
    weights <- weights[valid]
    # Sort values and weights
    sorted_indices <- order(values)
    values <- values[sorted_indices]
    weights <- weights[sorted_indices]
    # Compute cumulative shares
    cumulative_weights <- cumsum(weights) / sum(weights)
    cumulative_values <- cumsum(values * weights) / sum(values * weights)
    # Calculate weighted Gini coefficient
    gini <- 1 - sum((cumulative_values[-length(cumulative_values)] + cumulative_values[-1]) *
                     diff(c(0, cumulative_weights)))
    return(gini)
  } else {
    return(NA)  # Return NA if insufficient data
  }
}

# Function for unweighted Gini calculation using the ineq package
unweighted_gini <- function(values) {
  valid <- !is.na(values) & values > 0  # Remove NAs and non-positive values
  if (sum(valid) > 1) {
    return(ineq::Gini(values[valid]))
  } else {
    return(NA)
  }
}

# Initialize data frames for storing results
results <- list(
  weighted = data.frame(
    country = character(),
    period = character(),
    wage_gini = numeric(),
    ila_gini = numeric(),
    total_weight = numeric(),
    stringsAsFactors = FALSE
  ),
  unweighted = data.frame(
    country = character(),
    period = character(),
    wage_gini = numeric(),
    ila_gini = numeric(),
    total_weight = numeric(),
    stringsAsFactors = FALSE
  ),
  worker_counts = data.frame(
    country = character(),
    period = character(),
    worker_count = numeric(),
    stringsAsFactors = FALSE
  )
)

# Process each dataset file one by one
if (length(selected_files) == 0) {
  cat("⚠️ No matching datasets found. Exiting script. ⚠️\n")
} else {
  # First loop: Process all files and collect data
  for (file_path in selected_files) {
    cat("Processing file:", basename(file_path), "\n")
    
    # Extract country and period info from filename using string splitting
    file_name <- basename(file_path)
    # Remove .dta extension and split by underscore
    parts <- strsplit(gsub("\\.dta$", "", file_name), "_")[[1]]
    
    if (length(parts) >= 4) {
      country <- parts[2]
      year <- parts[3]
      quarter_match <- gsub("q", "", parts[4])
      period <- paste0(year, "-", as.numeric(quarter_match))
      
      # Read dataset with error handling
      data <- tryCatch(read_dta(file_path), error = function(e) {
        cat("⚠️ Error reading file:", basename(file_path), "- Skipping. ⚠️\n")
        return(NULL)
      })
      
      if (!is.null(data)) {
        # Apply basic filters:
        # - edad > 14 (age > 14)
        # - asal == 1 (salaried workers)
        # - cohi == 1 (head of household)
        data <- data %>% 
          filter(edad > 14, asal == 1, cohi == 1)
        
        # Apply urban filter only for Peru and Paraguay
        if (country %in% c("per", "pry")) {
          data <- data %>% filter(urbano == 1)
        }
        
        # Check if we have enough observations after filtering
        if (nrow(data) > 0) {
          # Calculate total worker weight for this country-period
          worker_count <- data %>%
            filter(ocupado == 1) %>%
            summarize(total_weight = sum(pondera, na.rm = TRUE)) %>%
            pull(total_weight)
          
          # Store worker count
          results$worker_counts <- bind_rows(
            results$worker_counts,
            data.frame(
              country = country,
              period = period,
              worker_count = worker_count
            )
          )
          
          # Process each earnings variable
          wage_gini_wgt <- NA
          ila_gini_wgt <- NA
          wage_gini_unwgt <- NA
          ila_gini_unwgt <- NA
          
          for (earnings_var in earnings_vars) {
            # Special case for Peru: use ilaho_ppp17 instead of wage_ppp17
            actual_var <- earnings_var
            if (country == "per" && earnings_var == "wage_ppp17" && "ilaho_ppp17" %in% colnames(data)) {
              actual_var <- "ilaho_ppp17"
              cat("Note: Using ilaho_ppp17 instead of wage_ppp17 for Peru\n")
            }
            
            if (actual_var %in% colnames(data)) {
              # Filter for non-missing values in the earnings variable
              filtered_data <- data %>% filter(!is.na(.data[[actual_var]]))
              
              # Calculate weighted Gini
              wgt_gini <- weighted_gini(filtered_data[[actual_var]], filtered_data$pondera)
              
              # Calculate unweighted Gini
              unwgt_gini <- unweighted_gini(filtered_data[[actual_var]])
              
              # Store results
              if (earnings_var == "wage_ppp17") {  # Note: we use the original variable name for storage
                wage_gini_wgt <- wgt_gini
                wage_gini_unwgt <- unwgt_gini
              } else if (earnings_var == "ila_ppp17") {
                ila_gini_wgt <- wgt_gini
                ila_gini_unwgt <- unwgt_gini
              }
            } else {
              cat("⚠️ Warning: Variable", actual_var, "not found in dataset for", country, period, "\n")
            }
          }
          
          # Add weighted results
          results$weighted <- bind_rows(
            results$weighted,
            data.frame(
              country = country,
              period = period,
              wage_gini = wage_gini_wgt,
              ila_gini = ila_gini_wgt,
              total_weight = worker_count
            )
          )
          
          # Add unweighted results
          results$unweighted <- bind_rows(
            results$unweighted,
            data.frame(
              country = country,
              period = period,
              wage_gini = wage_gini_unwgt,
              ila_gini = ila_gini_unwgt,
              total_weight = worker_count
            )
          )
        } else {
          cat("⚠️ Warning: No observations after filtering for", country, period, "\n")
        }
      }
    } else {
      cat("⚠️ Warning: Could not extract country or period from filename:", file_name, "\n")
    }
  } # End of file processing loop
  
  # After processing all files, create output sheets and save Excel
  
  # Create Sheet 1: Country Weighted Ginis
  # Reshape for easier Excel output
  sheet1 <- results$weighted %>%
    arrange(country, period) %>%
    group_by(country) %>%
    summarize(
      first_period = first(period),
      wage_gini_first = first(wage_gini),
      last_period = last(period),
      wage_gini_last = last(wage_gini),
      ila_gini_first = first(ila_gini),
      ila_gini_last = last(ila_gini)
    ) %>%
    select(country, 
           first_period, wage_gini_first, 
           last_period, wage_gini_last,
           first_period, ila_gini_first,
           last_period, ila_gini_last)
  
  # Create Sheet 2: LAC-7 Weighted Aggregate Ginis
  # Group by period and calculate weights for LAC-7 countries
  worker_counts_by_period <- results$worker_counts %>%
    filter(country %in% lac7_countries) %>%
    group_by(period) %>%
    mutate(weight_proportion = worker_count / sum(worker_count, na.rm = TRUE)) %>%
    ungroup()
  
  # Calculate LAC-7 aggregated weighted Ginis
  lac7_weighted <- results$weighted %>%
    filter(country %in% lac7_countries) %>%
    left_join(worker_counts_by_period, by = c("country", "period")) %>%
    group_by(period) %>%
    summarize(
      wage_gini = sum(wage_gini * weight_proportion, na.rm = TRUE),
      ila_gini = sum(ila_gini * weight_proportion, na.rm = TRUE)
    ) %>%
    ungroup()
  
  # Reshape for Excel output
  sheet2 <- data.frame(
    aggregate = "LAC-7",
    first_period = lac7_weighted$period[1],
    wage_gini_first = lac7_weighted$wage_gini[1],
    last_period = lac7_weighted$period[2],
    wage_gini_last = lac7_weighted$wage_gini[2],
    first_period_ila = lac7_weighted$period[1],
    ila_gini_first = lac7_weighted$ila_gini[1],
    last_period_ila = lac7_weighted$period[2],
    ila_gini_last = lac7_weighted$ila_gini[2]
  )
  
  # Create Sheet 3: Country Unweighted Ginis
  sheet3 <- results$unweighted %>%
    arrange(country, period) %>%
    group_by(country) %>%
    summarize(
      first_period = first(period),
      wage_gini_first = first(wage_gini),
      last_period = last(period),
      wage_gini_last = last(wage_gini),
      ila_gini_first = first(ila_gini),
      ila_gini_last = last(ila_gini)
    ) %>%
    select(country, 
           first_period, wage_gini_first, 
           last_period, wage_gini_last,
           first_period, ila_gini_first,
           last_period, ila_gini_last)
  
  # Create Sheet 4: LAC-7 Unweighted Aggregate Ginis
  lac7_unweighted <- results$unweighted %>%
    filter(country %in% lac7_countries) %>%
    left_join(worker_counts_by_period, by = c("country", "period")) %>%
    group_by(period) %>%
    summarize(
      wage_gini = sum(wage_gini * weight_proportion, na.rm = TRUE),
      ila_gini = sum(ila_gini * weight_proportion, na.rm = TRUE)
    ) %>%
    ungroup()
  
  # Reshape for Excel output
  sheet4 <- data.frame(
    aggregate = "LAC-7",
    first_period = lac7_unweighted$period[1],
    wage_gini_first = lac7_unweighted$wage_gini[1],
    last_period = lac7_unweighted$period[2],
    wage_gini_last = lac7_unweighted$wage_gini[2],
    first_period_ila = lac7_unweighted$period[1],
    ila_gini_first = lac7_unweighted$ila_gini[1],
    last_period_ila = lac7_unweighted$period[2],
    ila_gini_last = lac7_unweighted$ila_gini[2]
  )
  
  # Print the weights used for LAC-7 aggregation
  cat("\nWeights used for LAC-7 aggregation:\n")
  weights_output <- worker_counts_by_period %>%
    select(period, country, worker_count, weight_proportion) %>%
    arrange(period, country)
  print(weights_output)
  
  # Create Sheet 5: Description
  sheet5 <- data.frame(
    Sheet = c("Sheet 1", "Sheet 2", "Sheet 3", "Sheet 4", "Sheet 5"),
    Description = c(
      "Country Weighted Ginis: Presents the weighted Gini coefficients for wage_ppp17 and ila_ppp17 for each country for the first and second time periods.",
      "LAC-7 Weighted Aggregate Ginis: Aggregate weighted Gini for LAC-7 countries (Argentina, Bolivia, Brazil, Costa Rica, Mexico, Peru, Uruguay), weighted by the proportion of workers in each country.",
      "Country Unweighted Ginis: Presents the unweighted Gini coefficients for wage_ppp17 and ila_ppp17 for each country for the first and second time periods.",
      "LAC-7 Unweighted Aggregate Ginis: Aggregate unweighted Gini for LAC-7 countries, weighted by the proportion of workers in each country.",
      "Description: Summary explanation of the content and calculations in each sheet."
    ),
    Calculation_Details = c(
      "Filters applied: 'edad' > 14, 'asal' = 1, 'cohi' = 1. Urban filter ('urbano' = 1) applied only for Peru and Paraguay.",
      "LAC-7 weighted Gini calculated as the weighted average of country Ginis using the proportion of workers in each country relative to the total for all 7 countries.",
      "Same filters as Sheet 1, but Gini coefficients calculated without using population weights for income values.",
      "LAC-7 unweighted Gini calculated as the weighted average of country unweighted Ginis using the proportion of workers in each country relative to the total for all 7 countries.",
      "N/A"
    )
  )
  
  # Define output file path
  output_dir <- "<<DATALIBWEB_PATH_OR_GENERIC_URL>>"
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  output_file <- file.path(output_dir, paste0(Sys.Date(), "-Selected-Quarters-Gini-wage-ila-V2.xlsx"))
  
  # Prepare the list of sheets for the Excel file
  sheets_list <- list(
    "Sheet1_Country_Weighted_Ginis" = sheet1,
    "Sheet2_LAC7_Weighted_Ginis" = sheet2,
    "Sheet3_Country_Unweighted_Ginis" = sheet3,
    "Sheet4_LAC7_Unweighted_Ginis" = sheet4,
    "Sheet5_Description" = sheet5,
    "Sheet6_LAC7_Weights" = weights_output
  )
  
  # Save results into an Excel file
  write_xlsx(sheets_list, path = output_file)
  
  cat("✅ Gini estimates saved to:", output_file, "\n")
}
