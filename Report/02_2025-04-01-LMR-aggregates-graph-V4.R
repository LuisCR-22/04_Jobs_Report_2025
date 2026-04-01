# Multi-Country Labor Market Aggregate Analysis - With Imputation
# This script generates weighted regional labor market aggregates for Latin American country groups by imputing missing values in country-level indicators, calculating population-weighted averages for employment metrics and worker-weighted averages for income metrics, producing time series analysis with special handling for data limitations such as NEET calculations requiring different country compositions.
# Author: Luis Castellanos
# Last modification: 2025/05/06
# -----------------------------------------------------------------------------

# Clean environment and load packages
rm(list = ls())
cat("\014")  # Clear console

# Install (if needed) and load required packages
required_pkgs <- c("readxl", "writexl", "dplyr", "stringr", "purrr", "tidyr", "openxlsx", "zoo")
for(pkg in required_pkgs) {
  if (!require(pkg, character.only = TRUE)) {
    install.packages(pkg, dependencies = TRUE)
    library(pkg, character.only = TRUE)
  }
}

# Define output directory path
output_dir <- "C:/Users/wb593225/OneDrive - WBG/Desktop/Luis - Private/Lablac/Semiannual report/Excel/Lablac output/01_New outputs/02 LM rates"

# Define input file
input_file <- file.path(output_dir, "2025-04-02-LMR-country-nocohh-V4.xlsx")
cat("Using input file:", basename(input_file), "\n")

# Define output files
imputed_file <- file.path(output_dir, "2025-04-02-LMR-country-nocohh-imputed-V4.xlsx")
aggregate_file <- file.path(output_dir, "2025-04-02-LMR-LACaggregate-nocohh-V4.xlsx")

# Normalize country names in the group definitions (replace spaces with underscores)
normalize_country_name <- function(name) {
  return(gsub(" ", "_", name))
}

# Normalize all country group names
LAC_10 <- sapply(c("Argentina", "Bolivia", "Brazil", "Chile", "Colombia", "Costa Rica", 
                   "Dominican Republic", "Mexico", "Peru", "Uruguay"), normalize_country_name)
LAC_9 <- sapply(c("Argentina", "Bolivia", "Brazil", "Chile", "Colombia", "Costa Rica", 
                  "Mexico", "Peru", "Uruguay"), normalize_country_name)
LAC_8 <- sapply(c("Argentina", "Bolivia", "Brazil", "Chile", "Costa Rica", 
                  "Mexico", "Peru", "Uruguay"), normalize_country_name)
LAC_7 <- sapply(c("Argentina", "Bolivia", "Brazil", "Costa Rica", 
                  "Mexico", "Peru", "Uruguay"), normalize_country_name)

# Define special country groupings for NEET calculations (excluding Peru, Chile, and Bolivia)
LAC_10_NEET <- sapply(c("Argentina", "Brazil", "Colombia", "Costa Rica", 
                        "Dominican Republic", "Mexico", "Uruguay"), normalize_country_name)
LAC_9_NEET <- sapply(c("Argentina", "Brazil", "Colombia", "Costa Rica", 
                       "Mexico", "Uruguay"), normalize_country_name)
LAC_8_NEET <- sapply(c("Argentina", "Brazil", "Costa Rica", 
                       "Mexico", "Uruguay"), normalize_country_name)

# Function to read a sheet from the Excel file and convert text values to numeric
read_indicator_sheet <- function(file, sheet_name) {
  # Read the sheet
  df <- read_excel(file, sheet = sheet_name)
  
  # Get the period column
  period_col <- df$Period
  
  # Create a mapping of normalized column names to original names
  col_names <- names(df)
  
  # Create a lookup table for country names with spaces
  country_lookup <- list()
  for (col in col_names) {
    if (col != "Period") {
      # Store mapping of normalized name to original name
      normalized_name <- gsub(" ", "_", col)
      country_lookup[[normalized_name]] <- col
    }
  }
  
  # Rename columns to normalized versions (replace spaces with underscores)
  names(df) <- sapply(names(df), function(x) gsub(" ", "_", x))
  
  # Convert all other columns to numeric, handling potential errors
  df <- df %>% select(-Period)
  df <- as.data.frame(lapply(df, function(x) {
    # Try to convert to numeric, set as NA if error
    tryCatch({
      as.numeric(as.character(x))
    }, error = function(e) {
      NA
    }, warning = function(w) {
      as.numeric(as.character(x))
    })
  }))
  
  # Add the period column back
  df <- cbind(Period = period_col, df)
  
  # Return both the dataframe with normalized columns and the lookup table
  return(list(
    data = df,
    lookup = country_lookup
  ))
}

# Function to impute missing values
impute_missing_values <- function(df) {
  # Create a copy of the dataframe to store imputation status
  impute_status <- data.frame(Period = df$Period)
  
  # Process each column except Period
  for (col in names(df)[names(df) != "Period"]) {
    # Get the column values
    values <- df[[col]]
    
    # Create status column (0 = original, 1 = imputed)
    impute_status[[col]] <- 0
    
    # Skip if all values are NA
    if (all(is.na(values))) {
      next
    }
    
    # Use na.approx from zoo package for linear interpolation
    # Only impute values between valid observations (na.approx does this by default)
    imputed_values <- zoo::na.approx(values, na.rm = FALSE)
    
    # Mark which values were imputed
    impute_status[[col]][is.na(values) & !is.na(imputed_values)] <- 1
    
    # Update the dataframe with imputed values
    df[[col]] <- imputed_values
  }
  
  return(list(
    imputed_data = df,
    impute_status = impute_status
  ))
}

# Function to create Excel with colored imputed cells
create_imputed_excel <- function(input_file, output_file) {
  # Get list of sheet names
  sheets <- excel_sheets(input_file)
  
  # Create a new workbook
  wb <- createWorkbook()
  
  # Create a red fill style for imputed values
  imputed_style <- createStyle(fontColour = "red")
  
  # Process each sheet
  for (sheet_name in sheets) {
    cat("Processing sheet for imputation:", sheet_name, "\n")
    
    # Special handling for Description sheet - just copy it as is
    if (sheet_name == "Description") {
      description_df <- read_excel(input_file, sheet = sheet_name)
      addWorksheet(wb, sheet_name)
      writeData(wb, sheet_name, description_df)
      next
    }
    
    # For data sheets, process with imputation
    sheet_result <- read_indicator_sheet(input_file, sheet_name)
    df <- sheet_result$data
    
    # Impute missing values
    impute_result <- impute_missing_values(df)
    imputed_df <- impute_result$imputed_data
    impute_status <- impute_result$impute_status
    
    # Denormalize country names back to original
    imputed_df_original <- denormalize_country_names(imputed_df, sheet_result$lookup)
    
    # Add sheet to workbook
    addWorksheet(wb, sheet_name)
    writeData(wb, sheet_name, imputed_df_original)
    
    # Apply formatting to imputed cells
    for (col_idx in 2:ncol(imputed_df_original)) {
      col_name <- names(imputed_df_original)[col_idx]
      normalized_col_name <- gsub(" ", "_", col_name)
      
      # Only proceed if the column exists in impute_status
      if (normalized_col_name %in% names(impute_status)) {
        for (row_idx in 1:nrow(imputed_df_original)) {
          if (impute_status[[normalized_col_name]][row_idx] == 1) {
            # Apply style to imputed cells
            addStyle(wb, sheet_name, style = imputed_style, 
                     rows = row_idx + 1, cols = col_idx)  # +1 because of header row
          }
        }
      }
    }
  }
  
  # Add description sheet
  addWorksheet(wb, "Imputation Description")
  imputation_desc <- data.frame(
    Topic = c("Imputation Method", "Visual Indicator"),
    Description = c(
      "Linear interpolation was used to impute missing values. This method assumes that changes between available data points are approximately linear, which is often reasonable for time series economic data. Only missing values between two valid observations were imputed (no extrapolation).",
      "Imputed values are displayed in red font to distinguish them from original values."
    )
  )
  writeData(wb, "Imputation Description", imputation_desc)
  
  # Save the workbook
  saveWorkbook(wb, output_file, overwrite = TRUE)
  cat("✅ Imputed data saved to:", output_file, "\n")
  
  return(output_file)
}

# Function to denormalize country names back to their original form
denormalize_country_names <- function(df, lookup) {
  # Create a copy
  result_df <- df
  
  # Get all column names except "Period"
  col_names <- names(df)
  col_names <- col_names[col_names != "Period"]
  
  # Create new dataframe with just the Period column
  new_df <- data.frame(Period = df$Period)
  
  # Process each column, converting normalized names back to original ones
  for (col in col_names) {
    # Check if this is a normalized name we have in our lookup
    if (col %in% names(lookup)) {
      # Use the original name
      original_name <- lookup[[col]]
      new_df[[original_name]] <- df[[col]]
    } else {
      # Keep as is
      new_df[[col]] <- df[[col]]
    }
  }
  
  return(new_df)
}

# Function to calculate weighted average for a group of countries for all periods
calculate_group_weighted_avg <- function(indicator_df, weight_df, group_countries) {
  # Ensure Period column is identical in both dataframes
  if (!identical(indicator_df$Period, weight_df$Period)) {
    stop("Period columns in indicator and weight dataframes do not match")
  }
  
  # Number of periods
  n_periods <- nrow(indicator_df)
  
  # Initialize result vector
  weighted_avgs <- numeric(n_periods)
  
  # Process each period
  for (p in 1:n_periods) {
    # Identify available countries in the group (those in the data)
    available_countries <- intersect(group_countries, names(indicator_df)[-1])
    
    if (length(available_countries) == 0) {
      weighted_avgs[p] <- NA  # Return NA if no countries from the group are available
      next
    }
    
    # Extract indicator values and weights for available countries for this period
    indicator_values <- numeric(length(available_countries))
    weights <- numeric(length(available_countries))
    
    for (i in seq_along(available_countries)) {
      indicator_values[i] <- indicator_df[p, available_countries[i]]
      weights[i] <- weight_df[p, available_countries[i]]
    }
    
    # Check for NAs, zeros, or negative values in indicators or weights
    valid <- !is.na(indicator_values) & !is.na(weights) & weights > 0
    
    if (sum(valid) == 0) {
      weighted_avgs[p] <- NA  # Return NA if no valid data
      next
    }
    
    # Rescale weights to sum to 1
    rescaled_weights <- weights[valid] / sum(weights[valid])
    valid_indicators <- indicator_values[valid]
    
    # Calculate weighted average
    weighted_avgs[p] <- sum(valid_indicators * rescaled_weights)
  }
  
  return(weighted_avgs)
}

# Function to calculate rescaled weights for a group of countries for all periods
calculate_rescaled_weights <- function(weight_df, group_countries) {
  # Number of periods
  n_periods <- nrow(weight_df)
  
  # Initialize result dataframe with all periods
  rescaled_df <- data.frame(Period = weight_df$Period)
  
  # First, initialize all columns for all group countries (even if not in data)
  for (country in group_countries) {
    rescaled_df[[country]] <- NA
  }
  
  # Identify countries that exist in both the group definition and the data
  available_countries <- intersect(group_countries, names(weight_df)[-1])
  
  if (length(available_countries) == 0) {
    return(rescaled_df)  # Return dataframe with NAs if no countries from group are in data
  }
  
  # Process each period
  for (p in 1:n_periods) {
    # Extract weights for available countries for this period
    weights <- numeric(length(available_countries))
    for (i in seq_along(available_countries)) {
      weights[i] <- weight_df[p, available_countries[i]]
    }
    
    # Check for valid weights
    valid <- !is.na(weights) & weights > 0
    valid_countries <- available_countries[valid]
    valid_weights <- weights[valid]
    
    if (sum(valid) == 0) {
      # If no valid weights for this period, keep NAs for all countries
      next
    } else {
      # Rescale weights to sum to 1
      rescaled_weights <- valid_weights / sum(valid_weights)
      
      # Add rescaled weights to dataframe
      for (i in seq_along(valid_countries)) {
        rescaled_df[p, valid_countries[i]] <- rescaled_weights[i]
      }
    }
  }
  
  return(rescaled_df)
}

# Format numeric values to have consistent decimal places
format_numeric <- function(df, decimals = 2) {
  # Identify numeric columns
  numeric_cols <- sapply(df, function(x) is.numeric(x) || all(grepl("^\\s*\\d+\\.?\\d*\\s*$|^\\s*$", na.omit(x))))
  numeric_cols["Period"] <- FALSE  # Ensure Period is not treated as numeric
  
  # Format numeric columns
  for (col in names(df)[numeric_cols]) {
    df[[col]] <- sprintf(paste0("%.", decimals, "f"), as.numeric(df[[col]]))
  }
  
  return(df)
}

# Process all indicators for aggregation
process_all_indicators <- function(input_file) {
  # List of labor market indicators to process (using WAP weights)
  labor_market_indicators <- c("LFP", "Employment", "Unemployment", "Youth Unempl", 
                               "NEET", "Alt NEET")
  
  # List of income/wage indicators to process (using Workers weights)
  income_wage_indicators <- c("ILA Mean", "ILA P20", "ILA Median", "ILA P80",
                              "Wage Mean", "Wage P20", "Wage Median", "Wage P80")
  
  # Read weight sheets
  wap_weights_result <- read_indicator_sheet(input_file, "WAP weight")
  workers_weights_result <- read_indicator_sheet(input_file, "Workers weight")
  
  # Extract data and lookup tables
  wap_weights <- wap_weights_result$data
  workers_weights <- workers_weights_result$data
  country_lookup <- wap_weights_result$lookup
  
  # Initialize results dataframe with periods
  all_indicators <- data.frame(Period = wap_weights$Period)
  
  # Process labor market indicators
  for (indicator in labor_market_indicators) {
    cat("Processing:", indicator, "\n")
    
    # Read indicator sheet
    indicator_result <- read_indicator_sheet(input_file, indicator)
    indicator_df <- indicator_result$data
    
    # Use special country groups for NEET and Alt NEET indicators
    if (indicator %in% c("NEET", "Alt NEET")) {
      # For NEET indicators, exclude Peru, Chile, and Bolivia
      lac_10_avg <- calculate_group_weighted_avg(indicator_df, wap_weights, LAC_10_NEET)
      lac_9_avg <- calculate_group_weighted_avg(indicator_df, wap_weights, LAC_9_NEET)
      lac_8_avg <- calculate_group_weighted_avg(indicator_df, wap_weights, LAC_8_NEET)
      
      # Add note to the indicator name
      indicator_name <- paste0(indicator, " (excl. PE,CL,BO)")
    } else {
      # For other indicators, use regular country groups
      lac_10_avg <- calculate_group_weighted_avg(indicator_df, wap_weights, LAC_10)
      lac_9_avg <- calculate_group_weighted_avg(indicator_df, wap_weights, LAC_9)
      lac_8_avg <- calculate_group_weighted_avg(indicator_df, wap_weights, LAC_8)
      
      indicator_name <- indicator
    }
    
    # Add to results
    all_indicators[[paste0(indicator_name, " (LAC-10)")]] <- lac_10_avg
    all_indicators[[paste0(indicator_name, " (LAC-9)")]] <- lac_9_avg
    all_indicators[[paste0(indicator_name, " (LAC-8)")]] <- lac_8_avg
  }
  
  # Process income/wage indicators
  for (indicator in income_wage_indicators) {
    cat("Processing:", indicator, "\n")
    
    # Read indicator sheet
    indicator_result <- read_indicator_sheet(input_file, indicator)
    indicator_df <- indicator_result$data
    
    # Calculate aggregate for LAC-7 group
    lac_7_avg <- calculate_group_weighted_avg(indicator_df, workers_weights, LAC_7)
    
    # Add to results
    all_indicators[[paste0(indicator, " (LAC-7)")]] <- lac_7_avg
  }
  
  # Calculate rescaled weights for each group for each period
  rescaled_weights <- list()
  rescaled_weights[["LAC-10 Weights"]] <- denormalize_country_names(
    calculate_rescaled_weights(wap_weights, LAC_10), country_lookup)
  rescaled_weights[["LAC-9 Weights"]] <- denormalize_country_names(
    calculate_rescaled_weights(wap_weights, LAC_9), country_lookup)
  rescaled_weights[["LAC-8 Weights"]] <- denormalize_country_names(
    calculate_rescaled_weights(wap_weights, LAC_8), country_lookup)
  rescaled_weights[["LAC-7 Weights"]] <- denormalize_country_names(
    calculate_rescaled_weights(workers_weights, LAC_7), country_lookup)
  
  # Add special weights for NEET calculations
  rescaled_weights[["LAC-10 NEET Weights"]] <- denormalize_country_names(
    calculate_rescaled_weights(wap_weights, LAC_10_NEET), country_lookup)
  rescaled_weights[["LAC-9 NEET Weights"]] <- denormalize_country_names(
    calculate_rescaled_weights(wap_weights, LAC_9_NEET), country_lookup)
  rescaled_weights[["LAC-8 NEET Weights"]] <- denormalize_country_names(
    calculate_rescaled_weights(wap_weights, LAC_8_NEET), country_lookup)
  
  # Create description dataframe
  description_df <- data.frame(
    Sheet = c(
      "Aggregate Indicators",
      "LAC-10 Weights",
      "LAC-9 Weights",
      "LAC-8 Weights",
      "LAC-7 Weights",
      "LAC-10 NEET Weights",
      "LAC-9 NEET Weights",
      "LAC-8 NEET Weights",
      "Description"
    ),
    Description = c(
      "Aggregate indicators for different country groups across all time periods. Labor market indicators calculated for LAC-10, LAC-9, and LAC-8 using WAP weights. Income and wage indicators calculated for LAC-7 using Worker weights. IMPORTANT: NEET and Alt NEET rates exclude Peru, Chile, and Bolivia because Peru doesn't have the 'asiste' variable, and Bolivia and Chile only have it since 2019 and 2021, respectively.",
      "Rescaled WAP weights for LAC-10 countries (Argentina, Bolivia, Brazil, Chile, Colombia, Costa Rica, Dominican Republic, Mexico, Peru, Uruguay) for each time period.",
      "Rescaled WAP weights for LAC-9 countries (Argentina, Bolivia, Brazil, Chile, Colombia, Costa Rica, Mexico, Peru, Uruguay) for each time period.",
      "Rescaled WAP weights for LAC-8 countries (Argentina, Bolivia, Brazil, Chile, Costa Rica, Mexico, Peru, Uruguay) for each time period.",
      "Rescaled Workers weights for LAC-7 countries (Argentina, Bolivia, Brazil, Costa Rica, Mexico, Peru, Uruguay) for each time period.",
      "Rescaled WAP weights for LAC-10 NEET calculations (Argentina, Brazil, Colombia, Costa Rica, Dominican Republic, Mexico, Uruguay) - excluding Peru, Chile, and Bolivia.",
      "Rescaled WAP weights for LAC-9 NEET calculations (Argentina, Brazil, Colombia, Costa Rica, Mexico, Uruguay) - excluding Peru, Chile, and Bolivia.",
      "Rescaled WAP weights for LAC-8 NEET calculations (Argentina, Brazil, Costa Rica, Mexico, Uruguay) - excluding Peru, Chile, and Bolivia.",
      "Description of the content of each sheet in this workbook."
    ),
    stringsAsFactors = FALSE
  )
  
  # Add information about imputation
  imputation_df <- data.frame(
    Sheet = "Imputation Information",
    Description = paste(
      "This file was generated from imputed data. Missing values in the original dataset",
      "were imputed using linear interpolation, which assumes that changes between available",
      "data points are approximately linear, which is often reasonable for time series economic data.",
      "Only missing values between two valid observations were imputed (no extrapolation was performed)."
    ),
    stringsAsFactors = FALSE
  )
  
  description_df <- rbind(description_df, imputation_df)
  
  return(list(
    all_indicators = all_indicators,
    rescaled_weights = rescaled_weights,
    description = description_df
  ))
}

# Main execution

# Step 1: Create Excel with imputed values
cat("\n===== STEP 1: Creating Excel with imputed values =====\n")
create_imputed_excel(input_file, imputed_file)

# Step 2: Process all indicators using the imputed data
cat("\n===== STEP 2: Calculating aggregates using imputed data =====\n")
results <- process_all_indicators(imputed_file)

# Format all dataframes
results$all_indicators <- format_numeric(results$all_indicators)
for (sheet_name in names(results$rescaled_weights)) {
  results$rescaled_weights[[sheet_name]] <- format_numeric(results$rescaled_weights[[sheet_name]], 4)
}

# Prepare sheets for Excel
sheets <- c(
  list("Aggregate Indicators" = results$all_indicators),
  results$rescaled_weights,
  list("Description" = results$description)
)

# Save Excel file with aggregates
write_xlsx(sheets, path = aggregate_file)
cat("✅ Aggregation results saved to:", aggregate_file, "\n")

cat("\n=============================\n")
cat("ANALYSIS COMPLETE\n")
cat("=============================\n")

# Clean up environment to free memory
rm(list = ls())
gc()
