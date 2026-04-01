# Modified Gini coefficient calculator for Latin American labor statistics
# Processes files by quarter to create a time series of inequality measures in long format

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

# Set to TRUE to test only with Argentina 2016 files
TEST_MODE <- TRUE

# Define the data directory
data_dir <- "C:/Users/wb593225/OneDrive - WBG/Desktop/Luis - Private/Lablac/Semiannual report/Lablac data/No cohh"

# Create mapping for country codes to full names
country_mapping <- c(
  "arg" = "Argentina",
  "bol" = "Bolivia",
  "bra" = "Brazil",
  "chl" = "Chile",
  "col" = "Colombia",
  "cri" = "Costa Rica",
  "dom" = "Dominican Republic",
  "ecu" = "Ecuador",
  "gtm" = "Guatemala",
  "mex" = "Mexico",
  "per" = "Peru",
  "pry" = "Paraguay",
  "slv" = "El Salvador",
  "ury" = "Uruguay"
)

# Function to list and filter files for processing
list_files_to_process <- function(test_mode = FALSE) {
  # List all .dta files in the directory
  all_files <- list.files(path = data_dir, pattern = "^LABLAC_.*\\.dta$", full.names = TRUE)
  
  if(test_mode) {
    # For test mode, only process Argentina 2016 files
    files_to_process <- grep("LABLAC_arg_2016_q\\d{2}", basename(all_files), value = TRUE)
    files_to_process <- file.path(data_dir, files_to_process)
  } else {
    # For full processing, include all country files between 2016 and 2024
    files_to_process <- grep("LABLAC_[a-z]{3}_(20(1[6-9]|2[0-4]))_q\\d{2}", basename(all_files), value = TRUE)
    files_to_process <- file.path(data_dir, files_to_process)
  }
  
  return(files_to_process)
}

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

# Initialize data frame for storing results in long format
results <- data.frame(
  Period = character(),
  Indicator = character(),
  Country = character(),
  Value = numeric(),
  stringsAsFactors = FALSE
)

# Get files to process
files_to_process <- list_files_to_process(TEST_MODE)

# Print selected files
cat("Selected", length(files_to_process), "files for processing:\n")
for (file in files_to_process) {
  cat("  -", basename(file), "\n")
}

# Process each dataset file one by one
if (length(files_to_process) == 0) {
  cat("⚠️ No matching datasets found. Exiting script. ⚠️\n")
} else {
  for (file_path in files_to_process) {
    cat("Processing file:", basename(file_path), "\n")
    
    # Extract country and period info from filename
    file_name <- basename(file_path)
    # Extract parts using regex pattern matching
    parts <- str_match(file_name, "LABLAC_([a-z]{3})_(\\d{4})_q(\\d{2})")[1,]
    
    if (length(parts) >= 4) {
      country_code <- parts[2]
      year <- parts[3]
      quarter <- as.numeric(parts[4])
      
      # Format the period as YYYY-QX
      period <- paste0(year, "-Q", quarter)
      
      # Read dataset with error handling
      data <- tryCatch(read_dta(file_path), error = function(e) {
        cat("⚠️ Error reading file:", basename(file_path), "- Skipping. ⚠️\n")
        return(NULL)
      })
      
      if (!is.null(data)) {
        # Base filters for all calculations: edad > 14 and cohi == 1
        data <- data %>% filter(edad > 14, cohi == 1)
        
        # Apply urban filter only for Peru and Paraguay
        if (country_code %in% c("per", "pry")) {
          data <- data %>% filter(urbano == 1)
        }
        
        # Check if we have enough observations after filtering
        if (nrow(data) > 0) {
          # Process wage gini (with asal == 1 filter)
          wage_var <- "wage_ppp17"
          # Special case for Peru: use ilaho_ppp17 instead of wage_ppp17
          if (country_code == "per" && "ilaho_ppp17" %in% colnames(data)) {
            wage_var <- "ilaho_ppp17"
          }
          
          if (wage_var %in% colnames(data)) {
            # Filter for wage workers (asal == 1)
            wage_data <- data %>% filter(asal == 1, !is.na(.data[[wage_var]]))
            
            if (nrow(wage_data) > 0) {
              # Calculate weighted wage gini
              wage_gini_weighted <- weighted_gini(wage_data[[wage_var]], wage_data$pondera)
              
              # Calculate unweighted wage gini
              wage_gini_unweighted <- unweighted_gini(wage_data[[wage_var]])
              
              # Add weighted wage gini to results
              results <- bind_rows(
                results,
                data.frame(
                  Period = period,
                  Indicator = "Wage gini - weighted",
                  Country = country_mapping[country_code],
                  Value = wage_gini_weighted
                )
              )
              
              # Add unweighted wage gini to results
              results <- bind_rows(
                results,
                data.frame(
                  Period = period,
                  Indicator = "Wage gini - unweighted",
                  Country = country_mapping[country_code],
                  Value = wage_gini_unweighted
                )
              )
            } else {
              cat("⚠️ Warning: No valid wage observations after filtering for", country_code, period, "\n")
            }
          } else {
            cat("⚠️ Warning: Variable", wage_var, "not found in dataset for", country_code, period, "\n")
          }
          
          # Process labor income gini (no asal filter)
          if ("ila_ppp17" %in% colnames(data)) {
            # Filter for non-missing values in labor income
            income_data <- data %>% filter(!is.na(ila_ppp17))
            
            if (nrow(income_data) > 0) {
              # Calculate weighted labor income gini
              income_gini_weighted <- weighted_gini(income_data$ila_ppp17, income_data$pondera)
              
              # Calculate unweighted labor income gini
              income_gini_unweighted <- unweighted_gini(income_data$ila_ppp17)
              
              # Add weighted labor income gini to results
              results <- bind_rows(
                results,
                data.frame(
                  Period = period,
                  Indicator = "Labor income gini - weighted",
                  Country = country_mapping[country_code],
                  Value = income_gini_weighted
                )
              )
              
              # Add unweighted labor income gini to results
              results <- bind_rows(
                results,
                data.frame(
                  Period = period,
                  Indicator = "Labor income gini - unweighted",
                  Country = country_mapping[country_code],
                  Value = income_gini_unweighted
                )
              )
            } else {
              cat("⚠️ Warning: No valid labor income observations after filtering for", country_code, period, "\n")
            }
          } else {
            cat("⚠️ Warning: Variable ila_ppp17 not found in dataset for", country_code, period, "\n")
          }
        } else {
          cat("⚠️ Warning: No observations after basic filtering for", country_code, period, "\n")
        }
      }
    } else {
      cat("⚠️ Warning: Could not extract country or period from filename:", file_name, "\n")
    }
  } # End of file processing loop
  
  # Add Order column (sequential number for each Country-Indicator combination)
  if (nrow(results) > 0) {
    results <- results %>%
      group_by(Country, Indicator) %>%
      arrange(Period) %>%
      mutate(Order = row_number()) %>%
      ungroup()
    
    # Create description sheet
    description <- data.frame(
      Section = c("Overview", "Indicators", "Filters", "Country Specific", "Structure"),
      Description = c(
        "This dataset contains Gini coefficients for wage and labor income across Latin American countries.",
        "Four indicators are calculated: 1) Wage gini - weighted, 2) Wage gini - unweighted, 3) Labor income gini - weighted, 4) Labor income gini - unweighted.",
        "Basic filters for all measures: Working age (edad > 14) and Heads of household (cohi == 1). Wage ginis additionally filter for wage workers (asal == 1).",
        "Peru and Paraguay data are filtered for urban areas only (urbano == 1). For Peru, ilaho_ppp17 is used instead of wage_ppp17 for wage calculations.",
        "Data is presented in long format with Period (YYYY-QX), Indicator name, Country name, Value, and Order (sequential row count for each Country-Indicator combination)."
      )
    )
    
    # Define output file path
    output_dir <- "C:/Users/wb593225/OneDrive - WBG/Desktop/Luis - Private/Lablac/Semiannual report/Excel/Lablac output/01_New outputs/15 wage gini"
    dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
    
    # Create filename with test mode indicator
    test_suffix <- if(TEST_MODE) "-ARG2016-TEST" else ""
    output_file <- file.path(output_dir, paste0(Sys.Date(), "-Quarterly-Gini-Series", test_suffix, ".xlsx"))
    
    # Prepare the list of sheets for the Excel file
    sheets_list <- list(
      "Gini_Series" = results,
      "Description" = description
    )
    
    # Save results into an Excel file
    write_xlsx(sheets_list, path = output_file)
    
    cat("✅ Gini estimates saved to:", output_file, "\n")
    
    # Display summary information
    cat("\nProcessing complete.\n")
    cat("Total rows:", nrow(results), "\n")
    cat("Countries:", paste(unique(results$Country), collapse=", "), "\n")
    cat("Indicators:", paste(unique(results$Indicator), collapse=", "), "\n")
    cat("Periods:", paste(sort(unique(results$Period)), collapse=", "), "\n")
  } else {
    cat("\nNo results were generated.\n")
  }
}
