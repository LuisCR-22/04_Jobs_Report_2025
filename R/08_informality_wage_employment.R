# Changes in informality and wage employment in LAC.
# Author: Luis Castellanos - Stats Team (lcasterodr@worldbank.org)
# Last Modification: 2025-04-03
# -----------------------------------------------------------------------------
# Clean environment and load packages
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
# -----------------------------------------------------------------------------


rm(list = ls())
cat("\014")  # Clear console

# Install (if needed) and load required packages
required_pkgs <- c("haven", "dplyr", "tidyr", "writexl", "stringr", "lubridate", "openxlsx")
for(pkg in required_pkgs) {
  if (!require(pkg, character.only = TRUE)) {
    install.packages(pkg, dependencies = TRUE)
    library(pkg, character.only = TRUE)
  }
}

# -----------------------------------------------------------------------------
# Define directories
# -----------------------------------------------------------------------------
# Updated input path as requested
data_dir <- "<<DATALIBWEB_PATH_OR_GENERIC_URL>>"
output_dir <- "<<DATALIBWEB_PATH_OR_GENERIC_URL>>"

# -----------------------------------------------------------------------------
# Define which files to process based on country-specific periods
# -----------------------------------------------------------------------------
# Function to check if a file should be processed based on our criteria
should_process_file <- function(filename) {
  # Parse the filename to extract components
  parts <- str_match(filename, "LABLAC_([a-z]{3})_(\\d{4})_q(\\d{2})_.*\\.dta$")
  
  if (is.na(parts[1])) {
    return(FALSE)  # Filename doesn't match the expected pattern
  }
  
  country <- parts[2]
  year <- as.numeric(parts[3])
  quarter <- as.numeric(parts[4])
  
  # Country-specific processing criteria
  
  # Colombia: use 2021 Q2 instead of 2016 Q2
  if (country == "col") {
    if ((year == 2021 && quarter == 2) || 
        (year == 2024 && quarter == 2)) {
      return(TRUE)
    }
  }
  # Dominican Republic: use 2017 Q2 instead of 2016 Q2
  else if (country == "dom") {
    if ((year == 2017 && quarter == 2) || 
        (year == 2024 && quarter == 2)) {
      return(TRUE)
    }
  }
  # Paraguay: use 2022 Q2 instead of 2016 Q2/2017 Q2
  else if (country == "pry") {
    if ((year == 2022 && quarter == 2) || 
        (year == 2024 && quarter == 2)) {
      return(TRUE)
    }
  }
  # Brazil and Guatemala: use 2016 Q4 instead of 2016 Q2
  else if (country %in% c("bra", "gtm")) {
    if (year == 2016 && quarter == 4) {
      return(TRUE)
    }
    # Guatemala: use 2022 Q4 instead of 2024 Q2
    if (country == "gtm" && year == 2022 && quarter == 4) {
      return(TRUE)
    }
    # Brazil: still use 2024 Q2
    if (country == "bra" && year == 2024 && quarter == 2) {
      return(TRUE)
    }
  }
  # El Salvador: use 2023 Q2 instead of 2024 Q2
  else if (country == "slv") {
    if ((year == 2016 && quarter == 2) || 
        (year == 2023 && quarter == 2)) {
      return(TRUE)
    }
  }
  # Peru: use 2022 Q2 instead of 2024 Q2
  else if (country == "per") {
    if ((year == 2016 && quarter == 2) || 
        (year == 2022 && quarter == 2)) {
      return(TRUE)
    }
  }
  # Ecuador: use 2021 Q2 instead of 2016 Q2
  else if (country == "ecu") {
    if ((year == 2021 && quarter == 2) || 
        (year == 2024 && quarter == 2)) {
      return(TRUE)
    }
  }
  # Default case for other countries (Argentina, Bolivia, Chile, Costa Rica, Mexico, Uruguay)
  else {
    if ((year == 2016 && quarter == 2) || 
        (year == 2024 && quarter == 2)) {
      return(TRUE)
    }
  }
  
  return(FALSE)
}

# List all .dta files in the directory
all_files <- list.files(path = data_dir, pattern = "\\.dta$", full.names = TRUE)

# Filter files based on our criteria
files_to_process <- all_files[sapply(basename(all_files), should_process_file)]

if(length(files_to_process) == 0) {
  stop("No files matching the specified criteria were found.")
}

# Group files by country for reporting
country_files <- list()
for(file in files_to_process) {
  parts <- str_match(basename(file), "LABLAC_([a-z]{3})_(\\d{4})_q(\\d{2})_.*\\.dta$")
  country <- parts[2]
  
  if(!(country %in% names(country_files))) {
    country_files[[country]] <- 0
  }
  country_files[[country]] <- country_files[[country]] + 1
}

cat("Found", length(files_to_process), "files to process across", length(country_files), "countries.\n")
for(country in names(country_files)) {
  cat("- ", toupper(country), ": ", country_files[[country]], " files\n", sep="")
}

# -----------------------------------------------------------------------------
# Define a function to process one dataset file
# -----------------------------------------------------------------------------
process_file <- function(file_path) {
  # Extract file information from the filename
  filename <- basename(file_path)
  parts <- str_match(filename, "LABLAC_([a-z]{3})_(\\d{4})_q(\\d{2})_.*\\.dta$")
  
  if (is.na(parts[1])) {
    cat("⚠️ Warning: Filename doesn't match the expected pattern:", filename, "\n")
    return(NULL)
  }
  
  country <- parts[2]
  year <- as.numeric(parts[3])
  quarter <- as.numeric(parts[4])
  period <- paste(year, sprintf("q%02d", quarter), sep = "_")
  
  cat("Processing:", country, period, "\n")
  
  # Read dataset
  dat <- tryCatch({
    read_dta(file_path)
  }, error = function(e) {
    cat("⚠️ Error reading file:", file_path, " - ", e$message, "\n")
    return(NULL)
  })
  
  if(is.null(dat)) return(NULL)
  
  # Check if required variables exist
  required_vars <- c("ocupado", "pais_ocaux", "pondera", "edad", "hombre")
  target_vars <- c("categ_lab", "djubila", "dsegsale", "asal")
  
  missing_required <- required_vars[!required_vars %in% names(dat)]
  missing_target <- target_vars[!target_vars %in% names(dat)]
  
  if(length(missing_required) > 0) {
    cat("⚠️ Warning: Missing required variables in", file_path, ":", paste(missing_required, collapse=", "), "\n")
    return(NULL)
  }
  
  if(length(missing_target) == length(target_vars)) {
    cat("⚠️ Warning: None of the target variables found in", file_path, "\n")
    return(NULL)
  }
  
  # Filtering: ocupado == 1 and edad > 14
  # Note: No cohi filter as per requirement
  employed_adults <- dat %>% filter(ocupado == 1, edad > 14)
  all_adults <- dat %>% filter(edad > 14)
  
  # Initialize results list
  results <- list()
  
  # Calculate proportions for all observations
  results[["all"]] <- calculate_proportions(employed_adults, all_adults, country, period, target_vars)
  
  # Calculate proportions for men (hombre == 1)
  if("hombre" %in% names(dat)) {
    men_employed <- employed_adults %>% filter(hombre == 1)
    men_all <- all_adults %>% filter(hombre == 1)
    results[["men"]] <- calculate_proportions(men_employed, men_all, country, period, target_vars)
    
    # Calculate proportions for women (hombre == 0)
    women_employed <- employed_adults %>% filter(hombre == 0)
    women_all <- all_adults %>% filter(hombre == 0)
    results[["women"]] <- calculate_proportions(women_employed, women_all, country, period, target_vars)
  }
  
  return(results)
}

# Function to calculate proportions for target variables
calculate_proportions <- function(employed_data, all_adults_data, country, period, target_vars) {
  result <- data.frame(country = country, period = period)
  
  # Process original variables (only for employed)
  for(var in c("categ_lab", "djubila", "dsegsale")) {
    if(var %in% names(employed_data)) {
      # Calculate weighted proportion of 1s
      valid_obs <- employed_data %>% filter(!is.na(.data[[var]]))
      if(nrow(valid_obs) > 0) {
        weighted_sum_ones <- sum(valid_obs$pondera[valid_obs[[var]] == 1], na.rm = TRUE)
        weighted_total <- sum(valid_obs$pondera, na.rm = TRUE)
        proportion <- if(weighted_total > 0) weighted_sum_ones / weighted_total else NA
        result[[var]] <- proportion
      } else {
        result[[var]] <- NA
      }
    } else {
      result[[var]] <- NA
    }
  }
  
  # Calculate wage employment (asal=1) as proportion of all workers
  if("asal" %in% names(employed_data)) {
    valid_obs <- employed_data %>% filter(!is.na(asal))
    if(nrow(valid_obs) > 0) {
      weighted_sum_ones <- sum(valid_obs$pondera[valid_obs$asal == 1], na.rm = TRUE)
      weighted_total <- sum(valid_obs$pondera, na.rm = TRUE)
      proportion <- if(weighted_total > 0) weighted_sum_ones / weighted_total else NA
      result[["wage_emp"]] <- proportion
    } else {
      result[["wage_emp"]] <- NA
    }
  } else {
    result[["wage_emp"]] <- NA
  }
  
  # Calculate alternative wage employment (asal=1) as proportion of all 15+ population
  if("asal" %in% names(employed_data)) {
    # Calculate wage workers (employed with asal=1)
    valid_emp_obs <- employed_data %>% filter(!is.na(asal))
    if(nrow(valid_emp_obs) > 0) {
      weighted_sum_wage_workers <- sum(valid_emp_obs$pondera[valid_emp_obs$asal == 1], na.rm = TRUE)
      
      # Get total 15+ population
      weighted_total_adults <- sum(all_adults_data$pondera, na.rm = TRUE)
      
      proportion <- if(weighted_total_adults > 0) weighted_sum_wage_workers / weighted_total_adults else NA
      result[["wage_emp_alt"]] <- proportion
    } else {
      result[["wage_emp_alt"]] <- NA
    }
  } else {
    result[["wage_emp_alt"]] <- NA
  }
  
  # Calculate productive informality alt - wage workers (asal=1) who are in informal sector (categ_lab=2)
  if("asal" %in% names(employed_data) && "categ_lab" %in% names(employed_data)) {
    # Find wage workers who are in informal sector
    valid_obs <- employed_data %>% filter(!is.na(asal) & !is.na(categ_lab) & asal == 1)
    
    if(nrow(valid_obs) > 0) {
      # Sum weights for those with categ_lab=2 (informal sector)
      weighted_sum_informal <- sum(valid_obs$pondera[valid_obs$categ_lab == 2], na.rm = TRUE)
      
      # Total wage workers
      weighted_total_wage_workers <- sum(valid_obs$pondera, na.rm = TRUE)
      
      proportion <- if(weighted_total_wage_workers > 0) weighted_sum_informal / weighted_total_wage_workers else NA
      result[["prod_informal_alt"]] <- proportion
    } else {
      result[["prod_informal_alt"]] <- NA
    }
  } else {
    result[["prod_informal_alt"]] <- NA
  }
  
  # Calculate legal informality alt - wage workers (asal=1) without pension coverage (djubila=0)
  if("asal" %in% names(employed_data) && "djubila" %in% names(employed_data)) {
    # Find wage workers 
    valid_obs <- employed_data %>% filter(!is.na(asal) & !is.na(djubila) & asal == 1)
    
    if(nrow(valid_obs) > 0) {
      # Sum weights for those with djubila=0 (no pension coverage)
      weighted_sum_no_pension <- sum(valid_obs$pondera[valid_obs$djubila == 0], na.rm = TRUE)
      
      # Total wage workers
      weighted_total_wage_workers <- sum(valid_obs$pondera, na.rm = TRUE)
      
      proportion <- if(weighted_total_wage_workers > 0) weighted_sum_no_pension / weighted_total_wage_workers else NA
      result[["legal_informal_alt"]] <- proportion
    } else {
      result[["legal_informal_alt"]] <- NA
    }
  } else {
    result[["legal_informal_alt"]] <- NA
  }
  
  # Calculate health informality alt - wage workers (asal=1) without health insurance (dsegsale=0)
  if("asal" %in% names(employed_data) && "dsegsale" %in% names(employed_data)) {
    # Find wage workers 
    valid_obs <- employed_data %>% filter(!is.na(asal) & !is.na(dsegsale) & asal == 1)
    
    if(nrow(valid_obs) > 0) {
      # Sum weights for those with dsegsale=0 (no health insurance)
      weighted_sum_no_health <- sum(valid_obs$pondera[valid_obs$dsegsale == 0], na.rm = TRUE)
      
      # Total wage workers
      weighted_total_wage_workers <- sum(valid_obs$pondera, na.rm = TRUE)
      
      proportion <- if(weighted_total_wage_workers > 0) weighted_sum_no_health / weighted_total_wage_workers else NA
      result[["health_informal_alt"]] <- proportion
    } else {
      result[["health_informal_alt"]] <- NA
    }
  } else {
    result[["health_informal_alt"]] <- NA
  }
  
  return(result)
}

# -----------------------------------------------------------------------------
# Process all files and combine results
# -----------------------------------------------------------------------------
all_results <- list(
  all = list(categ_lab = data.frame(), djubila = data.frame(), dsegsale = data.frame(), 
             wage_emp = data.frame(), wage_emp_alt = data.frame(), prod_informal_alt = data.frame(),
             legal_informal_alt = data.frame(), health_informal_alt = data.frame()),
  men = list(categ_lab = data.frame(), djubila = data.frame(), dsegsale = data.frame(), 
             wage_emp = data.frame(), wage_emp_alt = data.frame(), prod_informal_alt = data.frame(),
             legal_informal_alt = data.frame(), health_informal_alt = data.frame()),
  women = list(categ_lab = data.frame(), djubila = data.frame(), dsegsale = data.frame(), 
               wage_emp = data.frame(), wage_emp_alt = data.frame(), prod_informal_alt = data.frame(),
               legal_informal_alt = data.frame(), health_informal_alt = data.frame())
)

for(file in files_to_process) {
  # Process the file
  file_results <- process_file(file)
  
  if(!is.null(file_results)) {
    # Append results to the appropriate dataframes
    for(group in c("all", "men", "women")) {
      if(!is.null(file_results[[group]])) {
        for(var in c("categ_lab", "djubila", "dsegsale", "wage_emp", "wage_emp_alt", 
                    "prod_informal_alt", "legal_informal_alt", "health_informal_alt")) {
          if(!is.na(file_results[[group]][[var]])) {
            new_row <- data.frame(
              country = file_results[[group]]$country,
              period = file_results[[group]]$period,
              proportion = file_results[[group]][[var]]
            )
            all_results[[group]][[var]] <- bind_rows(all_results[[group]][[var]], new_row)
          }
        }
      }
    }
  }
  
  # Clean up to optimize memory
  rm(file_results)
  gc()
}

# -----------------------------------------------------------------------------
# Reshape the data for Excel output
# -----------------------------------------------------------------------------
reshape_for_excel <- function(data) {
  if(nrow(data) == 0) return(data.frame())
  
  # Create a template with all possible periods to ensure consistency
  all_periods <- unique(data$period)
  all_countries <- unique(data$country)
  
  # Sort periods chronologically
  all_periods <- sort(all_periods)
  
  # Create a wide format dataframe with periods as rows and countries as columns
  result <- data.frame(period = all_periods)
  
  for(country_code in all_countries) {
    # Use country_code to avoid variable shadowing with the column name
    country_data <- data %>% filter(country == country_code)
    result[[country_code]] <- NA
    
    for(i in 1:nrow(result)) {
      period_match <- country_data %>% filter(period == result$period[i])
      if(nrow(period_match) > 0) {
        result[i, country_code] <- period_match$proportion[1]
      }
    }
  }
  
  return(result)
}

# Prepare Excel sheets
excel_sheets <- list()

for(group in c("all", "men", "women")) {
  for(var in c("categ_lab", "djubila", "dsegsale", "wage_emp", "wage_emp_alt", 
               "prod_informal_alt", "legal_informal_alt", "health_informal_alt")) {
    sheet_name <- paste0(group, "_", var)
    excel_sheets[[sheet_name]] <- reshape_for_excel(all_results[[group]][[var]])
  }
}

# -----------------------------------------------------------------------------
# Write output Excel file
# -----------------------------------------------------------------------------
today_date <- format(Sys.Date(), "%Y-%m-%d")
output_file <- file.path(output_dir, paste0(today_date, "-informality-all-countries-V2.xlsx"))

write_xlsx(excel_sheets, path = output_file)
cat("✅ Informality data saved to:", output_file, "\n")

# -----------------------------------------------------------------------------
# Define function to get appropriate periods for each country
# -----------------------------------------------------------------------------
get_country_periods <- function(country) {
  # Default periods
  start_period <- "2016_q02"
  end_period <- "2024_q02"
  
  # Country-specific adjustments
  if (country == "dom") {
    # Dominican Republic: use 2017 Q2 instead of 2016 Q2
    start_period <- "2017_q02"
  } else if (country == "pry") {
    # Paraguay: use 2022 Q2 instead of 2016 Q2/2017 Q2
    start_period <- "2022_q02"
  } else if (country == "col") {
    # Colombia: use 2021 Q2 instead of 2016 Q2
    start_period <- "2021_q02"
  } else if (country == "bra" || country == "gtm") {
    # Brazil and Guatemala: use 2016 Q4 instead of 2016 Q2
    start_period <- "2016_q04"
    
    # Guatemala: use 2022 Q4 instead of 2024 Q2
    if (country == "gtm") {
      end_period <- "2022_q04"
    }
  } else if (country == "slv") {
    # El Salvador: use 2023 Q2 instead of 2024 Q2
    end_period <- "2023_q02"
  } else if (country == "per") {
    # Peru: use 2022 Q2 instead of 2024 Q2
    end_period <- "2022_q02"
  } else if (country == "ecu") {
    # Ecuador: use 2021 Q2 instead of 2016 Q2
    start_period <- "2021_q02"
  }
  
  return(list(start = start_period, end = end_period))
}

# -----------------------------------------------------------------------------
# Create analysis workbook with formatted results
# -----------------------------------------------------------------------------
# Process the exported data to calculate changes between periods
process_informality_analysis <- function() {
  # Define the path to the raw data Excel file
  input_file <- output_file
  analysis_output_file <- file.path(output_dir, paste0(today_date, "-informality-analysis-report-V2.xlsx"))
  
  # Get sheet names
  sheet_names <- excel_sheets %>% names()
  
  # Create a workbook for analysis results
  wb <- createWorkbook()
  
  # Define styles
  headerStyle <- createStyle(fontSize = 12, textDecoration = "bold", halign = "center",
                             border = "bottom", borderStyle = "medium")
  numStyle <- createStyle(numFmt = "0.0")
  changeStyle <- createStyle(numFmt = "0.0")
  posChangeStyle <- createStyle(fontColour = "#006100", bgFill = "#C6EFCE")
  negChangeStyle <- createStyle(fontColour = "#9C0006", bgFill = "#FFC7CE")
  
  # Define variable types
  var_types <- c("categ_lab", "djubila", "dsegsale", "wage_emp", "wage_emp_alt", 
                "prod_informal_alt", "legal_informal_alt", "health_informal_alt")
  var_labels <- list(
    categ_lab = "Informal Employment",
    djubila = "No Pension Coverage",
    dsegsale = "No Health Insurance",
    wage_emp = "Wage Employment (% of workers)",
    wage_emp_alt = "Wage Employment (% of 15+ population)",
    prod_informal_alt = "Productive Informality (% of wage workers)",
    legal_informal_alt = "Legal Informality (% of wage workers without pension)",
    health_informal_alt = "Health Informality (% of wage workers without health insurance)"
  )
  
  # Create list to store country period information for the report
  country_periods <- list()
  
  # Add sheets for each variable type
  for (var_type in var_types) {
    sheet_name <- paste0("Analysis_", var_type)
    addWorksheet(wb, sheet_name)
    
    # Get the raw data sheet for "all" group
    df_all <- excel_sheets[[paste0("all_", var_type)]]
    
    if (nrow(df_all) == 0) {
      cat("No data for", var_type, "\n")
      next
    }
    
    # Get all countries in the data
    all_countries <- setdiff(names(df_all), "period")
    
    # Initialize result dataframe
    result <- data.frame(Country = all_countries, stringsAsFactors = FALSE)
    
    # For each country
    for (country in all_countries) {
      # Get appropriate periods for this country
      periods <- get_country_periods(country)
      start_period <- periods$start
      end_period <- periods$end
      
      # Store period information for reporting
      if (!country %in% names(country_periods)) {
        country_periods[[country]] <- periods
      }
      
      # For each demographic group (all, men, women)
      for (group in c("all", "men", "women")) {
        # Get sheet data
        df <- excel_sheets[[paste0(group, "_", var_type)]]
        
        if (nrow(df) > 0 && country %in% names(df)) {
          # Check if both start and end periods exist in the data
          has_start <- start_period %in% df$period
          has_end <- end_period %in% df$period
          
          # Get values for start and end periods
          start_value <- if (has_start) df[df$period == start_period, country] else NA
          end_value <- if (has_end) df[df$period == end_period, country] else NA
          
          # For wage employment and the *_informal_alt variables, use the values directly
          # For categ_lab, djubila, dsegsale, convert to informality measure (1 - formality)
          if (var_type %in% c("wage_emp", "wage_emp_alt", "prod_informal_alt", "legal_informal_alt", "health_informal_alt")) {
            # Keep values as they are (no inversion) - these are already informality measures or should remain as is
          } else {
            if (!is.na(start_value)) start_value <- 1 - start_value
            if (!is.na(end_value)) end_value <- 1 - end_value
          }
          
          # Calculate change (percentage points)
          change <- if (!is.na(start_value) && !is.na(end_value)) {
            (end_value - start_value) * 100
          } else {
            NA
          }
          
          # Add to result dataframe
          col_prefix <- paste0(group, "_")
          result[result$Country == country, paste0(col_prefix, "start")] <- if (!is.na(start_value)) start_value * 100 else NA
          result[result$Country == country, paste0(col_prefix, "end")] <- if (!is.na(end_value)) end_value * 100 else NA
          result[result$Country == country, paste0(col_prefix, "change")] <- change
          
          # Add period information for this country
          result[result$Country == country, "start_period"] <- start_period
          result[result$Country == country, "end_period"] <- end_period
        }
      }
    }
    
    # Calculate LAC simple average
    if (nrow(result) > 0) {
      # Add a row for LAC average
      lac_row <- data.frame(
        Country = "LAC_Average",
        all_start = mean(result$all_start, na.rm = TRUE),
        all_end = mean(result$all_end, na.rm = TRUE),
        all_change = mean(result$all_change, na.rm = TRUE),
        men_start = mean(result$men_start, na.rm = TRUE),
        men_end = mean(result$men_end, na.rm = TRUE),
        men_change = mean(result$men_change, na.rm = TRUE),
        women_start = mean(result$women_start, na.rm = TRUE),
        women_end = mean(result$women_end, na.rm = TRUE),
        women_change = mean(result$women_change, na.rm = TRUE),
        start_period = "Various",
        end_period = "Various"
      )
      
      # Add LAC row to the results
      result <- rbind(result, lac_row)
    }
    
    # Create better column names
    if (ncol(result) > 1) {  # Ensure we have data
      # Define new column names
      new_colnames <- c("Country", 
                        paste0("Total_", var_labels[[var_type]], "_Start"), 
                        paste0("Total_", var_labels[[var_type]], "_End"), 
                        paste0("Total_Change_(pp)"),
                        paste0("Men_", var_labels[[var_type]], "_Start"), 
                        paste0("Men_", var_labels[[var_type]], "_End"), 
                        paste0("Men_Change_(pp)"),
                        paste0("Women_", var_labels[[var_type]], "_Start"), 
                        paste0("Women_", var_labels[[var_type]], "_End"), 
                        paste0("Women_Change_(pp)"),
                        "Start_Period", "End_Period")
      
      # Set column names
      colnames(result) <- new_colnames
      
      # Write data to worksheet
      writeData(wb, sheet_name, result, startRow = 1, startCol = 1)
      
      # Apply header style
      addStyle(wb, sheet_name, headerStyle, rows = 1, cols = 1:ncol(result))
      
      # Apply number format to numeric columns
      numeric_cols <- 2:10  # Columns with numeric data
      addStyle(wb, sheet_name, numStyle, rows = 2:(nrow(result) + 1), 
               cols = numeric_cols, gridExpand = TRUE)
      
      # Apply conditional formatting for change columns (cols 4, 7, 10)
      change_cols <- c(4, 7, 10)
      
      # For wage employment, increases are positive (green), decreases are negative (red)
      if (var_type %in% c("wage_emp", "wage_emp_alt")) {
        conditionalFormatting(wb, sheet_name, cols = change_cols, 
                              rows = 2:(nrow(result) + 1),
                              rule = ">0", style = posChangeStyle)
        conditionalFormatting(wb, sheet_name, cols = change_cols, 
                              rows = 2:(nrow(result) + 1),
                              rule = "<0", style = negChangeStyle)
      } else {
        # For informality indicators, decreases are positive (green), increases are negative (red)
        conditionalFormatting(wb, sheet_name, cols = change_cols, 
                              rows = 2:(nrow(result) + 1),
                              rule = "<0", style = posChangeStyle)
        conditionalFormatting(wb, sheet_name, cols = change_cols, 
                              rows = 2:(nrow(result) + 1),
                              rule = ">0", style = negChangeStyle)
      }
      
      # Add notes
      explanation_note1 <- "How to interpret this table:"
      explanation_note2 <- if (var_type %in% c("wage_emp", "wage_emp_alt")) {
        paste("- For", var_labels[[var_type]], ": Higher values represent higher formality (more wage employment). Green indicates an increase (improvement).")
      } else {
        paste("- For", var_labels[[var_type]], ": Higher values represent higher informality. Green indicates a decrease (improvement).")
      }
      
      notes <- c(
        explanation_note1,
        explanation_note2,
        "- Country-specific periods are used based on data availability (see Start_Period and End_Period columns).",
        "- LAC_Average is a simple unweighted average across all countries with available data."
      )
      
      # Add calculation explanation
      calc_notes <- if (var_type %in% c("wage_emp", "wage_emp_alt")) {
        c(
          "Calculation method:",
          paste("- ", var_labels[[var_type]], "is calculated directly as the proportion of people with asal=1")
        )
      } else {
        c(
          "Calculation method:",
          paste("- ", var_labels[[var_type]], "is calculated as (100% - formality%), where formality is the proportion of people with", var_type, "=1")
        )
      }
      
      all_notes <- c(notes, "", calc_notes)
      
      for (i in 1:length(all_notes)) {
        writeData(wb, sheet_name, all_notes[i], startRow = nrow(result) + 3 + i, startCol = 1)
      }
      
      # Adjust column widths
      setColWidths(wb, sheet_name, cols = 1:ncol(result), widths = "auto")
    }
  }
  
  # Add a comprehensive description sheet
  addWorksheet(wb, "Description")
  
  # 1. Variable descriptions
  var_descriptions <- data.frame(
    Sheet = c(
      "Analysis_categ_lab",
      "Analysis_djubila",
      "Analysis_dsegsale",
      "Analysis_wage_emp",
      "Analysis_wage_emp_alt",
      "Analysis_prod_informal_alt",
      "Analysis_legal_informal_alt",
      "Analysis_health_informal_alt"
    ),
    Description = c(
      "Informal Employment: Calculated as (100% - formal employment%). Formal employment is identified by categ_lab=1 in the dataset. This shows the percentage of workers who don't have formal employment status. Calculated for ALL workers, not just wage workers.",
      "No Pension Coverage: Calculated as (100% - pension coverage%). Pension coverage is identified by djubila=1 in the dataset. This shows the percentage of workers without pension benefits. Calculated for ALL workers, not just wage workers.",
      "No Health Insurance: Calculated as (100% - health insurance coverage%). Health insurance is identified by dsegsale=1 in the dataset. This shows the percentage of workers without health insurance. Calculated for ALL workers, not just wage workers.",
      "Wage Employment (% of workers): Direct measure of the percentage of employed workers (ocupado=1) who are wage employees (asal=1). This is NOT an informality measure.",
      "Alternative Wage Employment (% of 15+ population): Direct measure of the percentage of working age population (15+) who are wage employees (asal=1). This is NOT an informality measure.",
      "Productive Informality (% of wage workers): Percentage of wage employees (asal=1) who work in the informal sector (categ_lab=2). This measures informality among wage workers only.",
      "Legal Informality (% of wage workers without pension): Percentage of wage employees (asal=1) who don't have pension coverage (djubila=0). This measures legal informality specifically among wage workers.",
      "Health Informality (% of wage workers without health insurance): Percentage of wage employees (asal=1) who don't have health insurance (dsegsale=0). This measures health benefit informality specifically among wage workers."
    )
  )
  
  # 2. Create country-period mapping
  country_period_data <- data.frame(
    Country = names(country_periods),
    Start_Period = sapply(country_periods, function(x) x$start),
    End_Period = sapply(country_periods, function(x) x$end),
    stringsAsFactors = FALSE
  )
  
  # 3. Create sheet structure explanation
  sheet_structure <- data.frame(
    Column = c(
      "Country", 
      "Total_*_Start", "Total_*_End", "Total_Change_(pp)",
      "Men_*_Start", "Men_*_End", "Men_Change_(pp)",
      "Women_*_Start", "Women_*_End", "Women_Change_(pp)",
      "Start_Period", "End_Period"
    ),
    Description = c(
      "Three-letter country code",
      "Value for the total population at the start period",
      "Value for the total population at the end period",
      "Change in percentage points between start and end periods for total population",
      "Value for men at the start period",
      "Value for men at the end period",
      "Change in percentage points between start and end periods for men",
      "Value for women at the start period",
      "Value for women at the end period",
      "Change in percentage points between start and end periods for women",
      "The specific start period used for this country",
      "The specific end period used for this country"
    )
  )
  
  # 4. Create methodology explanation
  methodology <- data.frame(
    Topic = c(
      "Data Source",
      "Population",
      "Informality Calculation",
      "Wage Employment",
      "Demographic Breakdown",
      "Time Periods",
      "LAC Average"
    ),
    Description = c(
      "Lablac harmonized household surveys (without household head filter - 'No cohh' folder)",
      "Working age individuals (15+). For most metrics, only employed individuals (ocupado=1) are considered.",
      "Informality measures (categ_lab, djubila, dsegsale) are calculated by inverting formality indicators: Informality = 100% - Formality%",
      "Two measures: (1) % of workers who are wage employees, (2) % of working age population who are wage employees",
      "Each indicator is calculated for the total population, men only, and women only",
      "Country-specific periods are used based on data availability",
      "Simple unweighted average of all countries with available data"
    )
  )
  
  # Write content to the description sheet
  # First: Title and introduction
  writeData(wb, "Description", "INFORMALITY ANALYSIS REPORT - DESCRIPTION", startRow = 1, startCol = 1)
  writeData(wb, "Description", "This workbook contains an analysis of labor market informality measures and wage employment indicators across Latin American countries.", startRow = 2, startCol = 1)
  
  # Section 1: Variables
  writeData(wb, "Description", "1. INDICATORS EXPLAINED", startRow = 4, startCol = 1)
  writeData(wb, "Description", var_descriptions, startRow = 5, startCol = 1)
  
  # Section 2: Country periods
  writeData(wb, "Description", "2. COUNTRY-SPECIFIC PERIODS", startRow = 5 + nrow(var_descriptions) + 2, startCol = 1)
  writeData(wb, "Description", country_period_data, startRow = 5 + nrow(var_descriptions) + 3, startCol = 1)
  
  # Section 3: Sheet structure
  writeData(wb, "Description", "3. COLUMN DESCRIPTIONS", startRow = 5 + nrow(var_descriptions) + nrow(country_period_data) + 5, startCol = 1)
  writeData(wb, "Description", sheet_structure, startRow = 5 + nrow(var_descriptions) + nrow(country_period_data) + 6, startCol = 1)
  
  # Section 4: Methodology
  writeData(wb, "Description", "4. METHODOLOGY", startRow = 5 + nrow(var_descriptions) + nrow(country_period_data) + nrow(sheet_structure) + 8, startCol = 1)
  writeData(wb, "Description", methodology, startRow = 5 + nrow(var_descriptions) + nrow(country_period_data) + nrow(sheet_structure) + 9, startCol = 1)
  
  # Apply styles to the Description sheet
  addStyle(wb, "Description", createStyle(fontSize = 14, textDecoration = "bold"), rows = 1, cols = 1)
  
  # Add style to section headers
  section_header_rows <- c(4, 
                           5 + nrow(var_descriptions) + 2, 
                           5 + nrow(var_descriptions) + nrow(country_period_data) + 5, 
                           5 + nrow(var_descriptions) + nrow(country_period_data) + nrow(sheet_structure) + 8)
  
  for (row in section_header_rows) {
    addStyle(wb, "Description", createStyle(fontSize = 12, textDecoration = "bold"), rows = row, cols = 1)
  }
  
  # Add style to table headers
  table_header_rows <- c(5, 
                         5 + nrow(var_descriptions) + 3, 
                         5 + nrow(var_descriptions) + nrow(country_period_data) + 6, 
                         5 + nrow(var_descriptions) + nrow(country_period_data) + nrow(sheet_structure) + 9)
  
  for (row in table_header_rows) {
    addStyle(wb, "Description", headerStyle, rows = row, cols = 1:2)
  }
  
  # Adjust Description sheet column widths
  setColWidths(wb, "Description", cols = 1, widths = 25)
  setColWidths(wb, "Description", cols = 2, widths = 100)
  
  # Save the workbook
  saveWorkbook(wb, analysis_output_file, overwrite = TRUE)
  cat("Analysis complete. Results saved to:", analysis_output_file, "\n")
}

# Run the analysis
process_informality_analysis()

# Clean up environment
rm(list = ls())
gc()
