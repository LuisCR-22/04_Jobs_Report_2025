# Informality Series Analysis for LAC countries
# This script processes LABLAC datasets to create a time series of informality measures

# -----------------------------------------------------------------------------
# Clean environment and load packages
# -----------------------------------------------------------------------------
rm(list = ls())
cat("\014")  # Clear console

# Install (if needed) and load required packages
required_pkgs <- c("haven", "dplyr", "tidyr", "writexl", "stringr", "lubridate", "readxl")
for(pkg in required_pkgs) {
  if (!require(pkg, character.only = TRUE)) {
    install.packages(pkg, dependencies = TRUE)
    library(pkg, character.only = TRUE)
  }
}

# -----------------------------------------------------------------------------
# Define directories and parameters
# -----------------------------------------------------------------------------
# Input and output paths
data_dir <- "C:/Users/wb593225/OneDrive - WBG/Desktop/Luis - Private/Lablac/Semiannual report/Lablac data/No cohh"
output_dir <- "C:/Users/wb593225/WBG/LAC Team for Statistical Development - WB Group - Documents/General/FY2025/Semiannual_Report/May/excel/Outputs/13 Dashboard data"

# Create output directory if it doesn't exist
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}

# Define current date for output file naming
current_date <- format(Sys.Date(), "%Y-%m-%d")
output_file <- file.path(output_dir, paste0("03_", current_date, "-informality-series-V3.xlsx"))

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
  "ury" = "Uruguay",
  "lac" = "LAC"
)

# -----------------------------------------------------------------------------
# Define function to list and filter files for processing
# -----------------------------------------------------------------------------
# For the initial test version, we'll only process Argentina 2016 files
list_files_to_process <- function(test_mode = FALSE) {
  # List all .dta files in the directory
  all_files <- list.files(path = data_dir, pattern = "\\.dta$", full.names = TRUE)
  
  if(test_mode) {
    # For test mode, only process Argentina 2016 files
    files_to_process <- grep("LABLAC_arg_2016_q\\d{2}", basename(all_files), value = TRUE)
    files_to_process <- file.path(data_dir, files_to_process)
  } else {
    # For full processing, include all countries between 2016 Q1 and 2024 Q4
    files_to_process <- grep("LABLAC_[a-z]{3}_(20(1[6-9]|2[0-4]))_q\\d{2}", basename(all_files), value = TRUE)
    files_to_process <- file.path(data_dir, files_to_process)
  }
  
  return(files_to_process)
}

# -----------------------------------------------------------------------------
# Define function to process a single file
# -----------------------------------------------------------------------------
process_file <- function(file_path) {
  # Extract file information from the filename
  filename <- basename(file_path)
  parts <- str_match(filename, "LABLAC_([a-z]{3})_(\\d{4})_q(\\d{2})_.*\\.dta$")
  
  if (is.na(parts[1])) {
    cat("⚠️ Warning: Filename doesn't match the expected pattern:", filename, "\n")
    return(NULL)
  }
  
  country_code <- parts[2]
  year <- as.numeric(parts[3])
  quarter <- as.numeric(parts[4])
  
  # Format the period as YYYY-QX
  period <- paste0(year, "-Q", quarter)
  
  cat("Processing:", country_code, period, "\n")
  
  # Read dataset
  dat <- tryCatch({
    read_dta(file_path)
  }, error = function(e) {
    cat("⚠️ Error reading file:", file_path, " - ", e$message, "\n")
    return(NULL)
  })
  
  if(is.null(dat)) return(NULL)
  
  # Check if required variables exist
  required_vars <- c("ocupado", "pais_ocaux", "pondera", "edad", "hombre", "asal")
  
  # For productive informality we need categ_lab
  # For legal informality, we need different variables depending on the country
  if(country_code %in% c("mex", "per")) {
    legal_var <- "dsegsale"  # Mexico and Peru use health insurance
  } else {
    legal_var <- "djubila"   # Other countries use pension
  }
  
  # Check for missing variables
  missing_required <- required_vars[!required_vars %in% names(dat)]
  missing_productive <- !("categ_lab" %in% names(dat))
  missing_legal <- !(legal_var %in% names(dat))
  
  if(length(missing_required) > 0) {
    cat("⚠️ Warning: Missing required variables in", file_path, ":", paste(missing_required, collapse=", "), "\n")
    return(NULL)
  }
  
  # Initialize results list for all indicators
  results <- list()
  
  # Filter for employed adults (15+)
  employed_adults <- dat %>% filter(ocupado == 1, edad > 14)
  
  # -------------------------------------------------------------------------
  # Calculate Wage Employment (as defined in Code 1)
  # -------------------------------------------------------------------------
  # All workers
  results <- add_wage_employment(results, employed_adults, country_code, period, "Total")
  
  # Men only
  men_employed <- employed_adults %>% filter(hombre == 1)
  results <- add_wage_employment(results, men_employed, country_code, period, "Men")
  
  # Women only
  women_employed <- employed_adults %>% filter(hombre == 0)
  results <- add_wage_employment(results, women_employed, country_code, period, "Women")
  
  # -------------------------------------------------------------------------
  # Calculate Productive Informality (for ALL workers - no asal filter)
  # -------------------------------------------------------------------------
  if(!missing_productive) {
    # All workers
    results <- add_productive_informality(results, employed_adults, country_code, period, "Total")
    
    # Men only
    men_employed <- employed_adults %>% filter(hombre == 1)
    results <- add_productive_informality(results, men_employed, country_code, period, "Men")
    
    # Women only
    women_employed <- employed_adults %>% filter(hombre == 0)
    results <- add_productive_informality(results, women_employed, country_code, period, "Women")
  }
  
  # -------------------------------------------------------------------------
  # Calculate Legal Informality (using different variables by country)
  # -------------------------------------------------------------------------
  if(!missing_legal) {
    # All workers
    results <- add_legal_informality(results, employed_adults, country_code, period, "Total", legal_var)
    
    # Men only
    men_employed <- employed_adults %>% filter(hombre == 1)
    results <- add_legal_informality(results, men_employed, country_code, period, "Men", legal_var)
    
    # Women only
    women_employed <- employed_adults %>% filter(hombre == 0)
    results <- add_legal_informality(results, women_employed, country_code, period, "Women", legal_var)
  }
  
  # Convert results list to data frame
  if(length(results) > 0) {
    results_df <- do.call(rbind, results)
    return(as.data.frame(results_df))
  } else {
    return(NULL)
  }
}

# Function to calculate wage employment and add to results
add_wage_employment <- function(results, data, country_code, period, group) {
  # Filter for workers with valid asal
  workers <- data %>% filter(!is.na(asal))
  
  if(nrow(workers) > 0) {
    # Calculate weighted proportion of wage workers (asal=1)
    weighted_sum_wage <- sum(workers$pondera[workers$asal == 1], na.rm = TRUE)
    weighted_total <- sum(workers$pondera, na.rm = TRUE)
    
    proportion <- if(weighted_total > 0) weighted_sum_wage / weighted_total else NA
    
    # Add to results
    indicator <- paste("Wage Employment -", group)
    results[[length(results) + 1]] <- data.frame(
      Period = period,
      Indicator = indicator,
      Country = country_code,
      Value = proportion
    )
  }
  
  return(results)
}

# Function to calculate productive informality and add to results
# MODIFIED: Removed the asal filter as requested
add_productive_informality <- function(results, data, country_code, period, group) {
  # Filter for workers with valid categ_lab (no asal filter)
  workers <- data %>% filter(!is.na(categ_lab))
  
  if(nrow(workers) > 0) {
    # Calculate weighted proportion of informal workers (categ_lab=2)
    weighted_sum_informal <- sum(workers$pondera[workers$categ_lab == 2], na.rm = TRUE)
    weighted_total <- sum(workers$pondera, na.rm = TRUE)
    
    proportion <- if(weighted_total > 0) weighted_sum_informal / weighted_total else NA
    
    # Add to results
    indicator <- paste("Productive Informality -", group)
    results[[length(results) + 1]] <- data.frame(
      Period = period,
      Indicator = indicator,
      Country = country_code,
      Value = proportion
    )
  }
  
  return(results)
}

# Function to calculate legal informality and add to results
add_legal_informality <- function(results, data, country_code, period, group, legal_var) {
  # Filter for wage workers (asal=1) with valid legal variable
  wage_workers <- data %>% filter(asal == 1, !is.na(.data[[legal_var]]))
  
  if(nrow(wage_workers) > 0) {
    # Calculate weighted proportion of workers without coverage (legal_var=0)
    weighted_sum_no_coverage <- sum(wage_workers$pondera[wage_workers[[legal_var]] == 0], na.rm = TRUE)
    weighted_total <- sum(wage_workers$pondera, na.rm = TRUE)
    
    proportion <- if(weighted_total > 0) weighted_sum_no_coverage / weighted_total else NA
    
    # Add to results
    indicator <- paste("Legal Informality -", group)
    results[[length(results) + 1]] <- data.frame(
      Period = period,
      Indicator = indicator,
      Country = country_code,
      Value = proportion
    )
  }
  
  return(results)
}

# -----------------------------------------------------------------------------
# Main processing function
# -----------------------------------------------------------------------------
process_all_files <- function(test_mode = FALSE) {
  # Get files to process
  files_to_process <- list_files_to_process(test_mode)
  
  if(length(files_to_process) == 0) {
    cat("No files found for processing.\n")
    return(NULL)
  }
  
  cat("Found", length(files_to_process), "files to process.\n")
  
  # Initialize dataframe to store all results
  all_results <- data.frame()
  
  # Process each file
  for(file in files_to_process) {
    file_results <- process_file(file)
    
    if(!is.null(file_results)) {
      all_results <- bind_rows(all_results, file_results)
    }
    
    # Clean up memory
    rm(file_results)
    gc()
  }
  
  # Map country codes to full names
  all_results$Country <- sapply(all_results$Country, function(code) {
    if(code %in% names(country_mapping)) {
      return(country_mapping[code])
    } else {
      return(code)  # Keep original if no mapping exists
    }
  })
  
  # Add Order column (sequential number for each Country-Indicator combination)
  all_results <- all_results %>%
    group_by(Country, Indicator) %>%
    arrange(Period) %>%
    mutate(Order = row_number()) %>%
    ungroup()
  
  return(all_results)
}

# -----------------------------------------------------------------------------
# Run the processing and save results
# -----------------------------------------------------------------------------
# Process files (in test mode, only Argentina 2016)
results <- process_all_files()

if(!is.null(results) && nrow(results) > 0) {
  # Display summary information
  cat("\nProcessing complete.\n")
  cat("Total observations:", nrow(results), "\n")
  cat("Countries:", paste(unique(results$Country), collapse=", "), "\n")
  cat("Indicators:", paste(unique(results$Indicator), collapse=", "), "\n")
  cat("Periods:", paste(sort(unique(results$Period)), collapse=", "), "\n")
  
  # Display values for each indicator and period for Argentina
  cat("\nResults for Argentina:\n")
  arg_results <- results %>% 
    filter(Country == "Argentina") %>%
    arrange(Indicator, Period)
  
  for(i in 1:nrow(arg_results)) {
    cat(sprintf("%s, %s: %.2f%%\n", 
                arg_results$Indicator[i], 
                arg_results$Period[i], 
                arg_results$Value[i] * 100))
  }
  
  # Save to Excel
  write_xlsx(results, output_file)
  cat("\nResults saved to:", output_file, "\n")
} else {
  cat("\nNo results were generated.\n")
}
