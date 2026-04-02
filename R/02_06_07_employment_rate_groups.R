# Employment Analysis by Demographic Groups
# This script processes quarterly labor market data from 9 Latin American countries to analyze employment rates and mean labor incomes across demographic groups (gender, age, skill level), comparing data from 2016, 2019, 2023, and 2024.
# Author: Luis Castellanos
# Last modification: 2025/05/06
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

# Define the target years
target_years <- c(2016, 2019, 2023, 2024)

# Function to determine skill level based on nivel variable
skill_level <- function(nivel) {
  if(is.na(nivel)) return(NA_character_)
  if(nivel %in% c(0,1,2,3)) return("Low") 
  if(nivel %in% c(4,5)) return("Middle")
  if(nivel == 6) return("High")
  return(NA_character_)
}

# Function to calculate employment rate
calculate_employment_rate <- function(data, filter_condition) {
  filtered_data <- data[filter_condition, ]
  if(nrow(filtered_data) == 0) return(NA)
  
  population_total <- sum(filtered_data$pondera, na.rm = TRUE)
  if(population_total == 0) return(NA)
  
  employed_total <- sum(filtered_data$pondera[filtered_data$ocupado == 1], na.rm = TRUE)
  return((employed_total / population_total) * 100)
}

# Function to calculate mean income
calculate_mean_income <- function(data, filter_condition) {
  # Base filters: employed, coherent income, has income value
  base_filter <- data$ocupado == 1 & data$cohi == 1 & !is.na(data$ila_ppp17)
  
  # Combine with specific filter for this group
  combined_filter <- base_filter & filter_condition
  
  filtered_data <- data[combined_filter, ]
  if(nrow(filtered_data) == 0) return(NA)
  
  # Calculate weighted mean
  income_total <- sum(filtered_data$pondera * filtered_data$ila_ppp17, na.rm = TRUE)
  population_total <- sum(filtered_data$pondera, na.rm = TRUE)
  
  if(population_total == 0) return(NA)
  return(income_total / population_total)
}

# Function to process a dataset and calculate employment rates for different groups
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
  required_vars <- c("pondera", "ocupado", "edad", "hombre", "nivel", "ila_ppp17", "cohi")
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
  
  # Calculate all employment rates at once
  
  # 1. Overall employment rate
  total_emp_rate <- calculate_employment_rate(dat, rep(TRUE, nrow(dat)))
  
  # 2. Employment rate by gender
  male_emp_rate <- calculate_employment_rate(dat, dat$hombre == 1)
  female_emp_rate <- calculate_employment_rate(dat, dat$hombre == 0)
  
  # 3. Employment rates by age groups
  age_15_24_emp_rate <- calculate_employment_rate(dat, dat$edad >= 15 & dat$edad <= 24)
  age_25_44_emp_rate <- calculate_employment_rate(dat, dat$edad >= 25 & dat$edad <= 44)
  age_45_54_emp_rate <- calculate_employment_rate(dat, dat$edad >= 45 & dat$edad <= 54)
  age_55_64_emp_rate <- calculate_employment_rate(dat, dat$edad >= 55 & dat$edad <= 64)
  age_65plus_emp_rate <- calculate_employment_rate(dat, dat$edad >= 65)
  
  # Additional age groups
  age_25_54_emp_rate <- calculate_employment_rate(dat, dat$edad >= 25 & dat$edad <= 54)
  age_55_64_emp_rate <- calculate_employment_rate(dat, dat$edad >= 55 & dat$edad <= 64)
  
  # 4. Employment rates by skill level
  low_skill_emp_rate <- calculate_employment_rate(dat, dat$skill == "Low")
  middle_skill_emp_rate <- calculate_employment_rate(dat, dat$skill == "Middle")
  high_skill_emp_rate <- calculate_employment_rate(dat, dat$skill == "High")
  
  # Calculate mean incomes for all groups
  
  # 1. Overall mean income
  total_mean_income <- calculate_mean_income(dat, rep(TRUE, nrow(dat)))
  
  # 2. Mean income by gender
  male_mean_income <- calculate_mean_income(dat, dat$hombre == 1)
  female_mean_income <- calculate_mean_income(dat, dat$hombre == 0)
  
  # 3. Mean income by age groups
  age_15_24_mean_income <- calculate_mean_income(dat, dat$edad >= 15 & dat$edad <= 24)
  age_25_44_mean_income <- calculate_mean_income(dat, dat$edad >= 25 & dat$edad <= 44)
  age_45_54_mean_income <- calculate_mean_income(dat, dat$edad >= 45 & dat$edad <= 54)
  age_55_64_mean_income <- calculate_mean_income(dat, dat$edad >= 55 & dat$edad <= 64)
  age_65plus_mean_income <- calculate_mean_income(dat, dat$edad >= 65)
  
  # Additional age groups
  age_25_54_mean_income <- calculate_mean_income(dat, dat$edad >= 25 & dat$edad <= 54)
  
  # 4. Mean income by skill level
  low_skill_mean_income <- calculate_mean_income(dat, dat$skill == "Low")
  middle_skill_mean_income <- calculate_mean_income(dat, dat$skill == "Middle")
  high_skill_mean_income <- calculate_mean_income(dat, dat$skill == "High")
  
  # Return results as a list
  return(list(
    country_code = country_code,
    country_name = country_names[country_code],
    period = period_label,
    year = year,
    quarter = quarter,
    
    # All employment rates
    total_emp_rate = total_emp_rate,
    male_emp_rate = male_emp_rate,
    female_emp_rate = female_emp_rate,
    
    # Age groups - employment
    age_15_24_emp_rate = age_15_24_emp_rate,
    age_25_44_emp_rate = age_25_44_emp_rate,
    age_45_54_emp_rate = age_45_54_emp_rate,
    age_55_64_emp_rate = age_55_64_emp_rate,
    age_65plus_emp_rate = age_65plus_emp_rate,
    age_25_54_emp_rate = age_25_54_emp_rate,
    
    # Skill levels - employment
    low_skill_emp_rate = low_skill_emp_rate,
    middle_skill_emp_rate = middle_skill_emp_rate,
    high_skill_emp_rate = high_skill_emp_rate,
    
    # Mean income - overall and by gender
    total_mean_income = total_mean_income,
    male_mean_income = male_mean_income,
    female_mean_income = female_mean_income,
    
    # Mean income by age groups
    age_15_24_mean_income = age_15_24_mean_income,
    age_25_44_mean_income = age_25_44_mean_income,
    age_45_54_mean_income = age_45_54_mean_income,
    age_55_64_mean_income = age_55_64_mean_income,
    age_65plus_mean_income = age_65plus_mean_income,
    age_25_54_mean_income = age_25_54_mean_income,
    
    # Mean income by skill levels
    low_skill_mean_income = low_skill_mean_income,
    middle_skill_mean_income = middle_skill_mean_income,
    high_skill_mean_income = high_skill_mean_income
  ))
}

# Process all countries (initially just Argentina) for target years
process_country_data <- function() {
  all_results <- list()
  
  # Process Argentina only for now
  for (country_code in names(country_names)) {
    cat("\n=============================\n")
    cat(paste0("PROCESSING ", toupper(country_code), " DATA\n"))
    cat("=============================\n")
    
    # Build patterns for target years
    target_patterns <- c()
    for (year in target_years) {
      # Create pattern for this country and year combination
      target_patterns <- c(target_patterns, sprintf("LABLAC_%s_%d_q", country_code, year))
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
      cat(sprintf("⚠️ No %s files found for target years. Skipping analysis.\n", toupper(country_code)))
      next
    }
    
    cat("Found", length(files), "files for", toupper(country_code), "in target years\n")
    
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
        # Format based on whether it's employment rate (percentage) or income
        if (grepl("_emp_rate$", metric)) {
          # Format as percentage with 2 decimal places
          value <- sprintf("%.2f", country_data[[metric]])
        } else {
          # Format as currency with 2 decimal places
          value <- sprintf("%.2f", country_data[[metric]])
        }
        
        # Add value to the dataframe
        result_df[period_idx, country_name] <- value
      }
    }
  }
  
  return(result_df)
}

# Create description sheet for the Excel file
create_description_sheet <- function() {
  description_df <- data.frame(
    Sheet = c(
      # Employment sheets
      "Total Employment",
      "Male Employment",
      "Female Employment",
      "Ages 15-24 Employment",
      "Ages 25-44 Employment",
      "Ages 45-54 Employment",
      "Ages 55-64 Employment",
      "Ages 65+ Employment",
      "Ages 25-54 Employment",
      "Low Skill Employment",
      "Middle Skill Employment",
      "High Skill Employment",
      
      # Income sheets
      "Total Mean Income",
      "Male Mean Income",
      "Female Mean Income",
      "Ages 15-24 Mean Income",
      "Ages 25-44 Mean Income",
      "Ages 45-54 Mean Income",
      "Ages 55-64 Mean Income", 
      "Ages 65+ Mean Income",
      "Ages 25-54 Mean Income",
      "Low Skill Mean Income",
      "Middle Skill Mean Income",
      "High Skill Mean Income",
      
      "Description"
    ),
    Description = c(
      # Employment descriptions
      "Employment Rate: Percentage of working age population (15+) that is employed (ocupado=1)",
      "Employment Rate for Males: Percentage of male working age population (15+) that is employed",
      "Employment Rate for Females: Percentage of female working age population (15+) that is employed",
      "Employment Rate for Ages 15-24: Percentage of population aged 15-24 that is employed",
      "Employment Rate for Ages 25-44: Percentage of population aged 25-44 that is employed",
      "Employment Rate for Ages 45-54: Percentage of population aged 45-54 that is employed",
      "Employment Rate for Ages 55-64: Percentage of population aged 55-64 that is employed",
      "Employment Rate for Ages 65+: Percentage of population aged 65 and older that is employed",
      "Employment Rate for Ages 25-54 (Prime Age): Percentage of prime age population that is employed",
      "Employment Rate for Low Skill: Percentage of low skill population (nivel 0-3) that is employed",
      "Employment Rate for Middle Skill: Percentage of middle skill population (nivel 4-5) that is employed",
      "Employment Rate for High Skill: Percentage of high skill population (nivel 6) that is employed",
      
      # Income descriptions
      "Mean Labor Income: Average labor income (ila_ppp17) for employed workers with coherent income (cohi=1)",
      "Mean Labor Income for Males: Average labor income for employed male workers with coherent income",
      "Mean Labor Income for Females: Average labor income for employed female workers with coherent income",
      "Mean Labor Income for Ages 15-24: Average labor income for employed workers aged 15-24 with coherent income",
      "Mean Labor Income for Ages 25-44: Average labor income for employed workers aged 25-44 with coherent income",
      "Mean Labor Income for Ages 45-54: Average labor income for employed workers aged 45-54 with coherent income",
      "Mean Labor Income for Ages 55-64: Average labor income for employed workers aged 55-64 with coherent income",
      "Mean Labor Income for Ages 65+: Average labor income for employed workers aged 65+ with coherent income",
      "Mean Labor Income for Ages 25-54: Average labor income for employed prime-age workers with coherent income",
      "Mean Labor Income for Low Skill: Average labor income for employed low skill workers with coherent income",
      "Mean Labor Income for Middle Skill: Average labor income for employed middle skill workers with coherent income",
      "Mean Labor Income for High Skill: Average labor income for employed high skill workers with coherent income",
      
      "This sheet contains descriptions of each sheet in this workbook"
    ),
    stringsAsFactors = FALSE
  )
  
  return(description_df)
}

# Main execution

# Process target countries (initially just Argentina) for target years
all_period_results <- process_country_data()

# Create formatted results for each metric
formatted_results <- list(
  # Employment rates
  total_employment = create_formatted_results(all_period_results, "total_emp_rate"),
  male_employment = create_formatted_results(all_period_results, "male_emp_rate"),
  female_employment = create_formatted_results(all_period_results, "female_emp_rate"),
  age_15_24_employment = create_formatted_results(all_period_results, "age_15_24_emp_rate"),
  age_25_44_employment = create_formatted_results(all_period_results, "age_25_44_emp_rate"),
  age_45_54_employment = create_formatted_results(all_period_results, "age_45_54_emp_rate"),
  age_55_64_employment = create_formatted_results(all_period_results, "age_55_64_emp_rate"),
  age_65plus_employment = create_formatted_results(all_period_results, "age_65plus_emp_rate"),
  age_25_54_employment = create_formatted_results(all_period_results, "age_25_54_emp_rate"),
  low_skill_employment = create_formatted_results(all_period_results, "low_skill_emp_rate"),
  middle_skill_employment = create_formatted_results(all_period_results, "middle_skill_emp_rate"),
  high_skill_employment = create_formatted_results(all_period_results, "high_skill_emp_rate"),
  
  # Mean income
  total_mean_income = create_formatted_results(all_period_results, "total_mean_income"),
  male_mean_income = create_formatted_results(all_period_results, "male_mean_income"),
  female_mean_income = create_formatted_results(all_period_results, "female_mean_income"),
  age_15_24_mean_income = create_formatted_results(all_period_results, "age_15_24_mean_income"),
  age_25_44_mean_income = create_formatted_results(all_period_results, "age_25_44_mean_income"),
  age_45_54_mean_income = create_formatted_results(all_period_results, "age_45_54_mean_income"),
  age_55_64_mean_income = create_formatted_results(all_period_results, "age_55_64_mean_income"),
  age_65plus_mean_income = create_formatted_results(all_period_results, "age_65plus_mean_income"),
  age_25_54_mean_income = create_formatted_results(all_period_results, "age_25_54_mean_income"),
  low_skill_mean_income = create_formatted_results(all_period_results, "low_skill_mean_income"),
  middle_skill_mean_income = create_formatted_results(all_period_results, "middle_skill_mean_income"),
  high_skill_mean_income = create_formatted_results(all_period_results, "high_skill_mean_income")
)

# Create description sheet
description_df <- create_description_sheet()

# Create the output Excel file with current date
today_date <- format(Sys.Date(), "%Y-%m-%d")
output_file <- file.path(output_dir, paste0(today_date, "-employment-by-groups-V2-ALL.xlsx"))

# Prepare sheets for Excel
sheets <- list(
  # Employment rate sheets
  "Total Employment" = formatted_results$total_employment,
  "Male Employment" = formatted_results$male_employment,
  "Female Employment" = formatted_results$female_employment,
  "Ages 15-24 Employment" = formatted_results$age_15_24_employment,
  "Ages 25-44 Employment" = formatted_results$age_25_44_employment,
  "Ages 45-54 Employment" = formatted_results$age_45_54_employment,
  "Ages 55-64 Employment" = formatted_results$age_55_64_employment,
  "Ages 65+ Employment" = formatted_results$age_65plus_employment,
  "Ages 25-54 Employment" = formatted_results$age_25_54_employment,
  "Low Skill Employment" = formatted_results$low_skill_employment,
  "Middle Skill Employment" = formatted_results$middle_skill_employment,
  "High Skill Employment" = formatted_results$high_skill_employment,
  
  # Mean income sheets
  "Total Mean Income" = formatted_results$total_mean_income,
  "Male Mean Income" = formatted_results$male_mean_income,
  "Female Mean Income" = formatted_results$female_mean_income,
  "Ages 15-24 Mean Income" = formatted_results$age_15_24_mean_income,
  "Ages 25-44 Mean Income" = formatted_results$age_25_44_mean_income,
  "Ages 45-54 Mean Income" = formatted_results$age_45_54_mean_income,
  "Ages 55-64 Mean Income" = formatted_results$age_55_64_mean_income,
  "Ages 65+ Mean Income" = formatted_results$age_65plus_mean_income,
  "Ages 25-54 Mean Income" = formatted_results$age_25_54_mean_income,
  "Low Skill Mean Income" = formatted_results$low_skill_mean_income,
  "Middle Skill Mean Income" = formatted_results$middle_skill_mean_income,
  "High Skill Mean Income" = formatted_results$high_skill_mean_income,
  
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
