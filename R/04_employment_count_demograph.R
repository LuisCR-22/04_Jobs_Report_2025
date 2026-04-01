# Employment Count Analysis by Demographic Groups
# This script processes quarterly labor market data from 9 Latin American countries to analyze employment counts across demographic groups (gender, age, skill level), comparing data from 2016, 2019, 2023, and 2024 (Q2) and calculating growth rates between periods.
# Author: Luis Castellanos - Stats Team (lcasterodr@worldbank.org)
# Date: 2025-05-06
# -----------------------------------------------------------------------------

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


# Clean environment and load packages
rm(list = ls())
cat("\014")  # Clear console

# Install (if needed) and load required packages
required_pkgs <- c("haven", "dplyr", "writexl", "stringr", "purrr", "tidyr", "lubridate")
for(pkg in required_pkgs) {
  if (!require(pkg, character.only = TRUE)) {
    install.packages(pkg, dependencies = TRUE)
    library(pkg, character.only = TRUE)
  }
}

# Define data directory path (same as original)
data_dir <- "<<DATALIBWEB_PATH_OR_GENERIC_URL>>"

# Define output directory path
output_dir <- "<<DATALIBWEB_PATH_OR_GENERIC_URL>>"

# Ensure output directory exists
dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

# Define country mappings
country_names <- c(
  arg = "Argentina",
  bol = "Bolivia",
  bra = "Brazil",
  chl = "Chile",
  col = "Colombia",
  cri = "Costa Rica",
  mex = "Mexico",
  per = "Peru",
  ury = "Uruguay"
)

# Define the target years and quarter (only Q2)
target_years <- c(2016, 2019, 2023, 2024)
target_quarter <- "02"  # Only Q2

# Function to determine skill level based on nivel variable
skill_level <- function(nivel) {
  if(is.na(nivel)) return(NA_character_)
  if(nivel %in% c(0,1,2,3)) return("Low") 
  if(nivel %in% c(4,5)) return("Middle")
  if(nivel == 6) return("High")
  return(NA_character_)
}

# Function to calculate total employed workers
calculate_employed_workers <- function(data, filter_condition) {
  filtered_data <- data[filter_condition, ]
  if(nrow(filtered_data) == 0) return(NA)
  
  # Sum weights for employed people
  employed_total <- sum(filtered_data$pondera[filtered_data$ocupado == 1], na.rm = TRUE)
  return(employed_total)
}

# Function to process a dataset and calculate employed workers for different groups
process_dataset <- function(file, country_code) {
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
  
  # Read the data file
  dat <- tryCatch({
    read_dta(file)
  }, error = function(e) {
    cat("⚠️ Error reading file:", file, "-", e$message, "\n")
    return(NULL)
  })
  
  if(is.null(dat)) return(NULL)
  
  # Check for required variables
  required_vars <- c("pondera", "ocupado", "edad", "hombre", "nivel")
  if(!all(required_vars %in% names(dat))) {
    missing_vars <- required_vars[!required_vars %in% names(dat)]
    cat("⚠️ Missing required variables in", basename(file), ":", paste(missing_vars, collapse=", "), "\n")
    return(NULL)
  }
  
  # Apply base filter: edad > 14
  dat <- dat %>% filter(edad > 14)
  
  if(nrow(dat) == 0) {
    cat("⚠️ No observations after applying base filters in", basename(file), "\n")
    return(NULL)
  }
  
  # Add skill level to dataset
  dat$skill <- sapply(dat$nivel, skill_level)
  
  # Calculate all employed worker counts at once
  
  # 1. Overall employed workers
  total_employed <- calculate_employed_workers(dat, rep(TRUE, nrow(dat)))
  
  # 2. Employed workers by gender
  male_employed <- calculate_employed_workers(dat, dat$hombre == 1)
  female_employed <- calculate_employed_workers(dat, dat$hombre == 0)
  
  # 3. Employed workers by age groups
  age_15_24_employed <- calculate_employed_workers(dat, dat$edad >= 15 & dat$edad <= 24)
  age_25_44_employed <- calculate_employed_workers(dat, dat$edad >= 25 & dat$edad <= 44)
  age_45_54_employed <- calculate_employed_workers(dat, dat$edad >= 45 & dat$edad <= 54)
  age_55_64_employed <- calculate_employed_workers(dat, dat$edad >= 55 & dat$edad <= 64)
  age_65plus_employed <- calculate_employed_workers(dat, dat$edad >= 65)
  
  # Additional age groups
  age_25_54_employed <- calculate_employed_workers(dat, dat$edad >= 25 & dat$edad <= 54)
  
  # 4. Employed workers by skill level
  low_skill_employed <- calculate_employed_workers(dat, dat$skill == "Low")
  middle_skill_employed <- calculate_employed_workers(dat, dat$skill == "Middle")
  high_skill_employed <- calculate_employed_workers(dat, dat$skill == "High")
  
  # Return results as a list
  return(list(
    country_code = country_code,
    country_name = country_names[country_code],
    period = period_label,
    year = year,
    quarter = quarter,
    
    # All employed worker counts
    total_employed = total_employed,
    male_employed = male_employed,
    female_employed = female_employed,
    
    # Age groups - employed workers
    age_15_24_employed = age_15_24_employed,
    age_25_44_employed = age_25_44_employed,
    age_45_54_employed = age_45_54_employed,
    age_55_64_employed = age_55_64_employed,
    age_65plus_employed = age_65plus_employed,
    age_25_54_employed = age_25_54_employed,
    
    # Skill levels - employed workers
    low_skill_employed = low_skill_employed,
    middle_skill_employed = middle_skill_employed,
    high_skill_employed = high_skill_employed
  ))
}

# Process all countries for target years and Q2
process_country_data <- function() {
  all_results <- list()
  
  for (country_code in names(country_names)) {
    cat("\n=============================\n")
    cat(paste0("PROCESSING ", toupper(country_code), " DATA\n"))
    cat("=============================\n")
    
    # Build patterns for target years and Q2
    target_patterns <- c()
    for (year in target_years) {
      # Create pattern for this country, year, and Q2 combination
      target_patterns <- c(target_patterns, sprintf("LABLAC_%s_%d_q%s", country_code, year, target_quarter))
    }
    
    # List all files in directory
    all_files <- list.files(path = data_dir, pattern = "\\.dta$", full.names = TRUE)
    
    # Filter files by target patterns
    files <- c()
    for (pattern in target_patterns) {
      matching_files <- all_files[grepl(pattern, all_files)]
      files <- c(files, matching_files)
    }
    
    if(length(files) == 0) {
      cat(sprintf("⚠️ No %s files found for target years and Q2. Skipping analysis.\n", toupper(country_code)))
      next
    }
    
    cat("Found", length(files), "files for", toupper(country_code), "in target years and Q2\n")
    
    # Process each file
    for (file in files) {
      result <- process_dataset(file, country_code)
      
      if(!is.null(result)) {
        # Store results by period and country
        period_key <- result$period
        
        # Initialize period in all_results if not exists
        if(!(period_key %in% names(all_results))) {
          all_results[[period_key]] <- list()
        }
        
        # Add country data to this period
        all_results[[period_key]][[country_code]] <- result
        cat("✅ Successfully processed", toupper(country_code), "for period", period_key, "\n")
      }
    }
    
    # Clean up to free memory
    gc()
  }
  
  # Sort results by period
  sorted_period_keys <- sort(names(all_results))
  sorted_results <- list()
  for (period_key in sorted_period_keys) {
    sorted_results[[period_key]] <- all_results[[period_key]]
  }
  
  # Return the sorted results
  return(sorted_results)
}

# Create formatted dataframe for Excel with full country names as headers and periods as rows
create_formatted_results <- function(all_results, metric) {
  # If no results, return empty dataframe
  if (length(all_results) == 0) {
    return(data.frame(Period = character(), stringsAsFactors = FALSE))
  }
  
  # Get all unique countries across all periods
  all_countries <- unique(unlist(lapply(all_results, names)))
  
  # Create dataframe with Period column
  result_df <- data.frame(Period = names(all_results), stringsAsFactors = FALSE)
  
  # Initialize columns for all countries (to handle missing countries in some periods)
  for (country_code in all_countries) {
    country_name <- country_names[country_code]
    result_df[[country_name]] <- NA
  }
  
  # Fill in the data for each period and country
  for (period_idx in 1:length(all_results)) {
    period_key <- names(all_results)[period_idx]
    period_data <- all_results[[period_key]]
    
    # Add data for each country in this period
    for (country_code in names(period_data)) {
      country_data <- period_data[[country_code]]
      country_name <- country_data$country_name
      
      # Check if the value exists
      if (!is.null(country_data[[metric]]) && !is.na(country_data[[metric]])) {
        # Format as number with comma separators
        value <- format(round(country_data[[metric]]), big.mark = ",")
        
        # Add value to the dataframe
        result_df[period_idx, country_name] <- value
      }
    }
  }
  
  return(result_df)
}

# Create annual change sheet
create_annual_change_sheet <- function(all_results, metric) {
  # If no results, return empty dataframe
  if (length(all_results) == 0) {
    return(data.frame(Period = character(), stringsAsFactors = FALSE))
  }
  
  # Get all unique countries across all periods
  all_countries <- unique(unlist(lapply(all_results, names)))
  
  # Create dataframe for annual changes
  result_df <- data.frame(
    Period = c("2016-Q2 to 2019-Q2", "2023-Q2 to 2024-Q2"),
    stringsAsFactors = FALSE
  )
  
  # Initialize columns for all countries
  for (country_code in all_countries) {
    country_name <- country_names[country_code]
    result_df[[country_name]] <- NA
  }
  
  # Extract the period keys that match our target years
  period_keys <- names(all_results)
  
  # Find relevant periods
  period_2016 <- period_keys[grepl("2016-Q2", period_keys)]
  period_2019 <- period_keys[grepl("2019-Q2", period_keys)]
  period_2023 <- period_keys[grepl("2023-Q2", period_keys)]
  period_2024 <- period_keys[grepl("2024-Q2", period_keys)]
  
  # Calculate changes for each country
  for (country_code in all_countries) {
    country_name <- country_names[country_code]
    
    # 2016-Q2 to 2019-Q2 (3 years)
    if (length(period_2016) > 0 && length(period_2019) > 0) {
      if (country_code %in% names(all_results[[period_2016]]) && 
          country_code %in% names(all_results[[period_2019]])) {
        
        value_2016 <- all_results[[period_2016]][[country_code]][[metric]]
        value_2019 <- all_results[[period_2019]][[country_code]][[metric]]
        
        if (!is.null(value_2016) && !is.na(value_2016) && 
            !is.null(value_2019) && !is.na(value_2019) && 
            value_2016 > 0) {
          
          # Calculate annualized change (compound annual growth rate)
          annual_change <- ((value_2019 / value_2016) ^ (1/3) - 1) * 100
          
          # Format as percentage with 2 decimal places
          result_df[1, country_name] <- sprintf("%.2f%%", annual_change)
        }
      }
    }
    
    # 2023-Q2 to 2024-Q2 (1 year)
    if (length(period_2023) > 0 && length(period_2024) > 0) {
      if (country_code %in% names(all_results[[period_2023]]) && 
          country_code %in% names(all_results[[period_2024]])) {
        
        value_2023 <- all_results[[period_2023]][[country_code]][[metric]]
        value_2024 <- all_results[[period_2024]][[country_code]][[metric]]
        
        if (!is.null(value_2023) && !is.na(value_2023) && 
            !is.null(value_2024) && !is.na(value_2024) && 
            value_2023 > 0) {
          
          # Calculate annual change (direct percentage change)
          annual_change <- ((value_2024 / value_2023) - 1) * 100
          
          # Format as percentage with 2 decimal places
          result_df[2, country_name] <- sprintf("%.2f%%", annual_change)
        }
      }
    }
  }
  
  return(result_df)
}

# Create description sheet for the Excel file
create_description_sheet <- function() {
  description_df <- data.frame(
    Sheet = c(
      # Employed workers sheets
      "Total Employed",
      "Male Employed",
      "Female Employed",
      "Ages 15-24 Employed",
      "Ages 25-44 Employed",
      "Ages 45-54 Employed",
      "Ages 55-64 Employed",
      "Ages 65+ Employed",
      "Ages 25-54 Employed",
      "Low Skill Employed",
      "Middle Skill Employed",
      "High Skill Employed",
      
      # Annual change sheet
      "Annual Change - Total",
      
      "Description"
    ),
    Description = c(
      # Employed workers descriptions
      "Total number of employed workers (ocupado=1) aged 15+ (weighted by pondera)",
      "Total number of employed male workers aged 15+ (weighted by pondera)",
      "Total number of employed female workers aged 15+ (weighted by pondera)",
      "Total number of employed workers aged 15-24 (weighted by pondera)",
      "Total number of employed workers aged 25-44 (weighted by pondera)",
      "Total number of employed workers aged 45-54 (weighted by pondera)",
      "Total number of employed workers aged 55-64 (weighted by pondera)",
      "Total number of employed workers aged 65+ (weighted by pondera)",
      "Total number of employed prime-age workers (25-54) (weighted by pondera)",
      "Total number of employed low skill workers (nivel 0-3) (weighted by pondera)",
      "Total number of employed middle skill workers (nivel 4-5) (weighted by pondera)",
      "Total number of employed high skill workers (nivel 6) (weighted by pondera)",
      
      # Annual change description
      "Annualized percentage change in total employed workers between periods (2016-Q2 to 2019-Q2 and 2023-Q2 to 2024-Q2)",
      
      "This sheet contains descriptions of each sheet in this workbook"
    ),
    stringsAsFactors = FALSE
  )
  
  return(description_df)
}

# Main execution

# Process target countries for target years and Q2
cat("\n=============================\n")
cat("STARTING ANALYSIS\n")
cat("Target Years: ", paste(target_years, collapse=", "), "\n")
cat("Target Quarter: Q", target_quarter, "\n")
cat("=============================\n")

all_period_results <- process_country_data()

cat("\n=============================\n")
cat("CREATING OUTPUT SHEETS\n")
cat("=============================\n")

# Create formatted results for each metric
formatted_results <- list(
  # Employed workers
  total_employed = create_formatted_results(all_period_results, "total_employed"),
  male_employed = create_formatted_results(all_period_results, "male_employed"),
  female_employed = create_formatted_results(all_period_results, "female_employed"),
  age_15_24_employed = create_formatted_results(all_period_results, "age_15_24_employed"),
  age_25_44_employed = create_formatted_results(all_period_results, "age_25_44_employed"),
  age_45_54_employed = create_formatted_results(all_period_results, "age_45_54_employed"),
  age_55_64_employed = create_formatted_results(all_period_results, "age_55_64_employed"),
  age_65plus_employed = create_formatted_results(all_period_results, "age_65plus_employed"),
  age_25_54_employed = create_formatted_results(all_period_results, "age_25_54_employed"),
  low_skill_employed = create_formatted_results(all_period_results, "low_skill_employed"),
  middle_skill_employed = create_formatted_results(all_period_results, "middle_skill_employed"),
  high_skill_employed = create_formatted_results(all_period_results, "high_skill_employed")
)

# Create annual change sheet
annual_change_total <- create_annual_change_sheet(all_period_results, "total_employed")

# Create description sheet
description_df <- create_description_sheet()

# Create the output Excel file with current date
today_date <- format(Sys.Date(), "%Y-%m-%d")
output_file <- file.path(output_dir, paste0(today_date, "-absolute-worker-per-group.xlsx"))

cat("Creating Excel file:", output_file, "\n")

# Prepare sheets for Excel
sheets <- list(
  # Employed workers sheets
  "Total Employed" = formatted_results$total_employed,
  "Male Employed" = formatted_results$male_employed,
  "Female Employed" = formatted_results$female_employed,
  "Ages 15-24 Employed" = formatted_results$age_15_24_employed,
  "Ages 25-44 Employed" = formatted_results$age_25_44_employed,
  "Ages 45-54 Employed" = formatted_results$age_45_54_employed,
  "Ages 55-64 Employed" = formatted_results$age_55_64_employed,
  "Ages 65+ Employed" = formatted_results$age_65plus_employed,
  "Ages 25-54 Employed" = formatted_results$age_25_54_employed,
  "Low Skill Employed" = formatted_results$low_skill_employed,
  "Middle Skill Employed" = formatted_results$middle_skill_employed,
  "High Skill Employed" = formatted_results$high_skill_employed,
  
  # Annual change sheet
  "Annual Change - Total" = annual_change_total,
  
  "Description" = description_df
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
