# Colombia Youth Labor Market Analysis (2016-2024)
# Calculates youth unemployment and NEET rates by gender
# Author: Simplified version for Colombia analysis
# Date: 2025-06-16
# -----------------------------------------------------------------------------

# Clean environment and load packages
rm(list = ls())
cat("\014")  # Clear console

# Install (if needed) and load required packages
required_pkgs <- c("haven", "dplyr", "writexl")
for(pkg in required_pkgs) {
  if (!require(pkg, character.only = TRUE)) {
    install.packages(pkg, dependencies = TRUE)
    library(pkg, character.only = TRUE)
  }
}

# Define data directory path
data_dir <- "C:/Users/wb593225/OneDrive - WBG/Desktop/Luis - Private/Lablac/Semiannual report/Lablac data/No cohh"

# Define output directory path
output_dir <- "C:/Users/wb593225/WBG/LAC Team for Statistical Development - WB Group - Documentos/General/FY2025/Semiannual_Report/May/excel/Outputs/17 BBL Col"

# Ensure output directory exists
dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

# Function to calculate weighted mean with error handling
calculate_weighted_mean <- function(x, w) {
  valid <- !is.na(x) & !is.na(w)
  if(sum(valid) == 0) return(NA)
  return(weighted.mean(x[valid], w[valid], na.rm = TRUE))
}

# Function to process a Colombia dataset and calculate youth indicators
process_colombia_dataset <- function(file) {
  cat("Processing file:", basename(file), "\n")
  
  # Extract period information from filename
  period_match <- regexpr("\\d{4}_q\\d{2}", basename(file))
  if (period_match == -1) {
    cat("⚠️ Cannot identify period from filename:", basename(file), "\n")
    return(NULL)
  }
  period_str <- substr(basename(file), period_match, period_match + attr(period_match, "match.length") - 1)
  year <- as.numeric(substr(period_str, 1, 4))
  quarter <- as.numeric(substr(period_str, 7, 8))
  period_label <- sprintf("%d-Q%d", year, quarter)
  
  # Double-check year is within range (2016-2024)
  if(is.na(year) || year < 2016 || year > 2024) {
    cat("⚠️ Skipping file outside 2016-2024 range:", basename(file), "(Year:", year, ")\n")
    return(NULL)
  }
  
  # Read the data file
  dat <- tryCatch({
    read_dta(file)
  }, error = function(e) {
    cat("⚠️ Error reading file:", file, "-", e$message, "\n")
    return(NULL)
  })
  
  if(is.null(dat)) return(NULL)
  
  # Check for required variables
  required_vars <- c("pondera", "ocupado", "edad", "pea", "desocupa", "asiste", "hombre")
  if(!all(required_vars %in% names(dat))) {
    missing_vars <- required_vars[!required_vars %in% names(dat)]
    cat("⚠️ Missing required variables in", basename(file), ":", paste(missing_vars, collapse=", "), "\n")
    return(NULL)
  }
  
  # Apply base filters: edad > 14 and not cohi == 1
  dat <- dat %>% filter(edad > 14)
  
  if(nrow(dat) == 0) {
    cat("⚠️ No observations after applying base filters in", basename(file), "\n")
    return(NULL)
  }
  
  # Create gender indicators based on "hombre" variable
  # hombre = 1 means male, hombre = 0 (or other values) means female
  dat$male <- ifelse(!is.na(dat$hombre) & dat$hombre == 1, 1, 0)
  dat$female <- ifelse(is.na(dat$hombre) | dat$hombre != 1, 1, 0)
  
  # Create youth filter (15-24 years old)
  youth_filter <- dat$edad >= 15 & dat$edad <= 24
  
  # Youth labor force and unemployment filters
  youth_labor_force_filter <- youth_filter & !is.na(dat$pea) & dat$pea == 1
  youth_unemployed_filter <- youth_filter & !is.na(dat$desocupa) & dat$desocupa == 1 & 
                            !is.na(dat$pea) & dat$pea == 1
  
  # Youth NEET filter (not employed and not studying)
  youth_neet_filter <- youth_filter & 
    (is.na(dat$ocupado) | dat$ocupado != 1) & 
    (is.na(dat$asiste) | dat$asiste != 1)
  
  # Calculate totals
  # Youth totals
  youth_total <- sum(dat$pondera[youth_filter], na.rm = TRUE)
  youth_male_total <- sum(dat$pondera[youth_filter & dat$male == 1], na.rm = TRUE)
  youth_female_total <- sum(dat$pondera[youth_filter & dat$female == 1], na.rm = TRUE)
  
  # Youth labor force totals
  youth_lf_total <- sum(dat$pondera[youth_labor_force_filter], na.rm = TRUE)
  youth_lf_male_total <- sum(dat$pondera[youth_labor_force_filter & dat$male == 1], na.rm = TRUE)
  youth_lf_female_total <- sum(dat$pondera[youth_labor_force_filter & dat$female == 1], na.rm = TRUE)
  
  # Youth unemployed totals
  youth_unemp_total <- sum(dat$pondera[youth_unemployed_filter], na.rm = TRUE)
  youth_unemp_male_total <- sum(dat$pondera[youth_unemployed_filter & dat$male == 1], na.rm = TRUE)
  youth_unemp_female_total <- sum(dat$pondera[youth_unemployed_filter & dat$female == 1], na.rm = TRUE)
  
  # Youth NEET totals
  youth_neet_total <- sum(dat$pondera[youth_neet_filter], na.rm = TRUE)
  youth_neet_male_total <- sum(dat$pondera[youth_neet_filter & dat$male == 1], na.rm = TRUE)
  youth_neet_female_total <- sum(dat$pondera[youth_neet_filter & dat$female == 1], na.rm = TRUE)
  
  # Calculate rates
  youth_unemployment_total <- (youth_unemp_total / youth_lf_total) * 100
  youth_unemployment_male <- (youth_unemp_male_total / youth_lf_male_total) * 100
  youth_unemployment_female <- (youth_unemp_female_total / youth_lf_female_total) * 100
  
  neet_rate_total <- (youth_neet_total / youth_total) * 100
  neet_rate_male <- (youth_neet_male_total / youth_male_total) * 100
  neet_rate_female <- (youth_neet_female_total / youth_female_total) * 100
  
  # Return results
  return(data.frame(
    period = period_label,
    year = year,
    quarter = quarter,
    youth_unemployment_total = youth_unemployment_total,
    youth_unemployment_male = youth_unemployment_male,
    youth_unemployment_female = youth_unemployment_female,
    neet_rate_total = neet_rate_total,
    neet_rate_male = neet_rate_male,
    neet_rate_female = neet_rate_female,
    stringsAsFactors = FALSE
  ))
}

# Process all Colombia files (2016-2024 only)
process_colombia_data <- function() {
  cat("\n=============================\n")
  cat("PROCESSING COLOMBIA DATA (2016-2024)\n")
  cat("=============================\n")
  
  # Create file pattern for Colombia files
  file_pattern <- "LABLAC_col.*\\.dta$"
  
  # List files matching the pattern
  files <- list.files(path = data_dir, pattern = file_pattern, full.names = TRUE)
  
  if(length(files) == 0) {
    cat("⚠️ No Colombia files found. Skipping analysis.\n")
    return(NULL)
  }
  
  cat("Found", length(files), "total Colombia files\n")
  
  # Filter files to only include 2016-2024
  valid_files <- c()
  for(file in files) {
    # Extract year from filename
    period_match <- regexpr("\\d{4}_q\\d{2}", basename(file))
    if (period_match != -1) {
      period_str <- substr(basename(file), period_match, period_match + attr(period_match, "match.length") - 1)
      year <- as.numeric(substr(period_str, 1, 4))
      
      # Only include files from 2016-2024
      if(!is.na(year) && year >= 2016 && year <= 2024) {
        valid_files <- c(valid_files, file)
        cat("✓ Valid file found:", basename(file), "(Year:", year, ")\n")
      } else {
        cat("✗ Skipping file (outside 2016-2024):", basename(file), "(Year:", year, ")\n")
      }
    } else {
      cat("✗ Skipping file (cannot extract year):", basename(file), "\n")
    }
  }
  
  if(length(valid_files) == 0) {
    cat("⚠️ No Colombia files found for period 2016-2024. Skipping analysis.\n")
    return(NULL)
  }
  
  cat("Processing", length(valid_files), "Colombia files from 2016-2024\n")
  
  # Process each valid file
  all_results <- list()
  
  for (file in valid_files) {
    result <- process_colombia_dataset(file)
    
    if(!is.null(result)) {
      all_results[[length(all_results) + 1]] <- result
      cat("✅ Successfully processed", basename(file), "for period", result$period, "\n")
    } else {
      cat("⚠️ Failed to process", basename(file), "\n")
    }
  }
  
  # Combine all results into a single dataframe
  if(length(all_results) > 0) {
    combined_results <- do.call(rbind, all_results)
    
    # Sort by year and quarter
    combined_results <- combined_results %>%
      arrange(year, quarter)
    
    return(combined_results)
  } else {
    cat("⚠️ No results to combine\n")
    return(NULL)
  }
}

# Main execution
cat("Starting Colombia Youth Labor Market Analysis (2016-2024)...\n")

# Process Colombia data
colombia_results <- process_colombia_data()

if(!is.null(colombia_results)) {
  
  # Create formatted results for Excel export
  # Round values to 2 decimal places
  colombia_results_formatted <- colombia_results %>%
    mutate(
      youth_unemployment_total = round(youth_unemployment_total, 2),
      youth_unemployment_male = round(youth_unemployment_male, 2),
      youth_unemployment_female = round(youth_unemployment_female, 2),
      neet_rate_total = round(neet_rate_total, 2),
      neet_rate_male = round(neet_rate_male, 2),
      neet_rate_female = round(neet_rate_female, 2)
    ) %>%
    select(period, year, quarter, everything())
  
  # Create description sheet
  description_df <- data.frame(
    Indicator = c(
      "youth_unemployment_total",
      "youth_unemployment_male", 
      "youth_unemployment_female",
      "neet_rate_total",
      "neet_rate_male",
      "neet_rate_female"
    ),
    Description = c(
      "Youth Unemployment Rate (Total): Percentage of economically active population aged 15-24 that is unemployed",
      "Youth Unemployment Rate (Male): Percentage of economically active male population aged 15-24 that is unemployed",
      "Youth Unemployment Rate (Female): Percentage of economically active female population aged 15-24 that is unemployed",
      "NEET Rate (Total): Percentage of youth aged 15-24 that are Not in Education, Employment or Training",
      "NEET Rate (Male): Percentage of male youth aged 15-24 that are Not in Education, Employment or Training",
      "NEET Rate (Female): Percentage of female youth aged 15-24 that are Not in Education, Employment or Training"
    ),
    stringsAsFactors = FALSE
  )
  
  # Prepare sheets for Excel
  sheets <- list(
    "Colombia_Youth_Indicators" = colombia_results_formatted,
    "Description" = description_df
  )
  
  # Create output filename with timestamp
  output_file <- file.path(output_dir, "01_Youth_unemployment_NEET_Col.xlsx")
  
  # Save Excel file
  write_xlsx(sheets, path = output_file)
  cat("✅ Analysis saved to:", output_file, "\n")
  
  # Print summary
  cat("\n=============================\n")
  cat("SUMMARY STATISTICS (2016-2024)\n")
  cat("=============================\n")
  cat("Periods processed:", nrow(colombia_results_formatted), "\n")
  cat("Date range:", min(colombia_results_formatted$year), "-", max(colombia_results_formatted$year), "\n")
  cat("Quarters available:", paste(sort(unique(colombia_results_formatted$quarter)), collapse = ", "), "\n")
  cat("Latest youth unemployment (total):", tail(colombia_results_formatted$youth_unemployment_total, 1), "%\n")
  cat("Latest NEET rate (total):", tail(colombia_results_formatted$neet_rate_total, 1), "%\n")
  
} else {
  cat("❌ No data processed. Please check file paths and data availability.\n")
}

cat("\n=============================\n")
cat("ANALYSIS COMPLETE\n")
cat("=============================\n")

# Clean up environment
gc()
