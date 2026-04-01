# Multi-Country Labor Market Rates Analysis (All Periods)
# This script generates a comprehensive labor market analysis for Latin American countries by processing household survey data to calculate employment rates, labor force participation, unemployment, NEET rates, and income distributions across multiple time periods, creating standardized cross-country comparisons for regional analysis.
# Author - Luis Castellanos - Stats Team
# Date: 2025-05-05
# -----------------------------------------------------------------------------

# Clean environment and load packages
rm(list = ls())
cat("\014")  # Clear console

# Install (if needed) and load required packages
required_pkgs <- c("haven", "dplyr", "writexl", "stats", "stringr", "purrr", "tidyr", "Hmisc")
for(pkg in required_pkgs) {
  if (!require(pkg, character.only = TRUE)) {
    install.packages(pkg, dependencies = TRUE)
    library(pkg, character.only = TRUE)
  }
}

# Define data directory path
data_dir <- "C:/Users/wb593225/OneDrive - WBG/Desktop/Luis - Private/Lablac/Semiannual report/Lablac data/No cohh"

# Define output directory path
output_dir <- "C:/Users/wb593225/OneDrive - WBG/Desktop/Luis - Private/Lablac/Semiannual report/Excel/Lablac output/01_New outputs/02 LM rates"

# Ensure output directory exists
dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

# Create "Dta Waste" subfolder for DTA files
dta_dir <- file.path(output_dir, "Dta Waste")
dir.create(dta_dir, showWarnings = FALSE, recursive = TRUE)

# Define country mappings (3-letter code to full name)
country_names <- c(
  arg = "Argentina",
  bol = "Bolivia",
  bra = "Brazil",
  chl = "Chile",
  col = "Colombia",
  cri = "Costa Rica",
  dom = "Dominican Republic",
  ecu = "Ecuador",
  mex = "Mexico",
  pry = "Paraguay",
  per = "Peru",
  slv = "El Salvador",
  ury = "Uruguay"
)

# Function to calculate weighted quantiles
weighted_quantile <- function(x, w, probs) {
  if (length(x) == 0 || all(is.na(x))) return(NA)
  
  # Remove NA values
  valid <- !is.na(x) & !is.na(w)
  if (sum(valid) == 0) return(NA)
  
  x <- x[valid]
  w <- w[valid]
  
  # Normalize weights
  w <- w / sum(w)
  
  # Sort data and weights
  ord <- order(x)
  x <- x[ord]
  w <- w[ord]
  
  # Calculate cumulative weights
  cumw <- cumsum(w)
  
  # Find quantiles
  result <- numeric(length(probs))
  for (i in seq_along(probs)) {
    if (probs[i] <= 0) {
      result[i] <- x[1]
    } else if (probs[i] >= 1) {
      result[i] <- x[length(x)]
    } else {
      # Find the position
      pos <- findInterval(probs[i], cumw)
      if (pos < length(x)) {
        # Interpolate if needed
        if (cumw[pos] == probs[i]) {
          result[i] <- x[pos]
        } else {
          result[i] <- x[pos + 1]
        }
      } else {
        result[i] <- x[length(x)]
      }
    }
  }
  
  return(result)
}

# Function to calculate weighted mean with improved error handling
calculate_weighted_mean <- function(x, w) {
  valid <- !is.na(x) & !is.na(w)
  if(sum(valid) == 0) return(NA)
  return(weighted.mean(x[valid], w[valid], na.rm = TRUE))
}

# Function to process a dataset and calculate labor market indicators
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
  required_vars <- c("pondera", "ocupado", "edad", "pea", "desocupa", "asiste", "ila_ppp17", "wage_ppp17")
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
  
  # Create filters for different populations of interest
  # Using basic subsetting for efficiency
  wap_filter <- !is.na(dat$pondera)  # All 15+ (already filtered above)
  labor_force_filter <- !is.na(dat$pea) & dat$pea == 1
  employed_filter <- !is.na(dat$ocupado) & dat$ocupado == 1
  unemployed_filter <- !is.na(dat$desocupa) & dat$desocupa == 1 & labor_force_filter
  
  youth_filter <- dat$edad >= 15 & dat$edad <= 24
  youth_labor_force_filter <- youth_filter & labor_force_filter
  youth_unemployed_filter <- youth_filter & unemployed_filter
  
  youth_neet_filter <- youth_filter & 
    (is.na(dat$ocupado) | dat$ocupado != 1) & 
    (is.na(dat$asiste) | dat$asiste != 1)
  
  young_adult_filter <- dat$edad >= 15 & dat$edad <= 29
  young_adult_neet_filter <- young_adult_filter & 
    (is.na(dat$ocupado) | dat$ocupado != 1) & 
    (is.na(dat$asiste) | dat$asiste != 1)
  
  # Calculate weighted sums for different groups
  wap_total <- sum(dat$pondera[wap_filter], na.rm = TRUE)
  labor_force_total <- sum(dat$pondera[labor_force_filter], na.rm = TRUE)
  employed_total <- sum(dat$pondera[employed_filter], na.rm = TRUE)
  unemployed_total <- sum(dat$pondera[unemployed_filter], na.rm = TRUE)
  
  youth_total <- sum(dat$pondera[youth_filter], na.rm = TRUE)
  youth_labor_force_total <- sum(dat$pondera[youth_labor_force_filter], na.rm = TRUE)
  youth_unemployed_total <- sum(dat$pondera[youth_unemployed_filter], na.rm = TRUE)
  youth_neet_total <- sum(dat$pondera[youth_neet_filter], na.rm = TRUE)
  
  young_adult_total <- sum(dat$pondera[young_adult_filter], na.rm = TRUE)
  young_adult_neet_total <- sum(dat$pondera[young_adult_neet_filter], na.rm = TRUE)
  
  # Calculate rates
  lfp_rate <- (labor_force_total / wap_total) * 100
  employment_rate <- (employed_total / wap_total) * 100
  unemployment_rate <- (unemployed_total / labor_force_total) * 100
  youth_unemployment_rate <- (youth_unemployed_total / youth_labor_force_total) * 100
  neet_rate <- (youth_neet_total / youth_total) * 100
  alt_neet_rate <- (young_adult_neet_total / young_adult_total) * 100
  
  # Filter for income calculations (employed & non-missing income)
  income_filter <- employed_filter & !is.na(dat$ila_ppp17)
  wage_filter <- employed_filter & !is.na(dat$wage_ppp17)
  
  # Calculate income statistics
  ila_mean <- calculate_weighted_mean(dat$ila_ppp17[income_filter], dat$pondera[income_filter])
  
  # Calculate income percentiles
  ila_p20 <- weighted_quantile(dat$ila_ppp17[income_filter], dat$pondera[income_filter], 0.20)
  ila_median <- weighted_quantile(dat$ila_ppp17[income_filter], dat$pondera[income_filter], 0.50)
  ila_p80 <- weighted_quantile(dat$ila_ppp17[income_filter], dat$pondera[income_filter], 0.80)
  
  # Calculate wage statistics
  wage_mean <- calculate_weighted_mean(dat$wage_ppp17[wage_filter], dat$pondera[wage_filter])
  
  # Calculate wage percentiles
  wage_p20 <- weighted_quantile(dat$wage_ppp17[wage_filter], dat$pondera[wage_filter], 0.20)
  wage_median <- weighted_quantile(dat$wage_ppp17[wage_filter], dat$pondera[wage_filter], 0.50)
  wage_p80 <- weighted_quantile(dat$wage_ppp17[wage_filter], dat$pondera[wage_filter], 0.80)
  
  # Return results
  return(list(
    country_code = country_code,
    country_name = country_names[country_code],
    period = period_label,
    year = year,
    quarter = quarter,
    lfp_rate = lfp_rate,
    employment_rate = employment_rate,
    unemployment_rate = unemployment_rate,
    youth_unemployment_rate = youth_unemployment_rate,
    neet_rate = neet_rate,
    alt_neet_rate = alt_neet_rate,
    workers = employed_total,
    plus15_population = wap_total,
    # Income statistics
    ila_mean = ila_mean,
    ila_p20 = ila_p20,
    ila_median = ila_median,
    ila_p80 = ila_p80,
    # Wage statistics
    wage_mean = wage_mean,
    wage_p20 = wage_p20,
    wage_median = wage_median,
    wage_p80 = wage_p80
  ))
}

# Process all countries and all time periods
process_all_countries <- function() {
  all_results <- list()
  
  # Process each country
  for (country_code in names(country_names)) {
    cat("\n=============================\n")
    cat(paste0("PROCESSING ", toupper(country_code), " DATA\n"))
    cat("=============================\n")
    
    # Create file pattern for all periods (not just 2016 Q2)
    file_pattern <- sprintf("LABLAC_%s.*\\.dta$", country_code)
    
    # List files matching the pattern
    files <- list.files(path = data_dir, pattern = file_pattern, full.names = TRUE)
    
    if(length(files) == 0) {
      cat(sprintf("⚠️ No %s files found. Skipping analysis.\n", toupper(country_code)))
      next
    }
    
    cat("Found", length(files), "files for", toupper(country_code), "\n")
    
    # Process each file (each represents a different period)
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
      } else {
        cat("⚠️ Failed to process", basename(file), "\n")
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

# Calculate weights for each period
calculate_weights <- function(period_results) {
  # Calculate totals for weights in this period
  total_workers <- sum(sapply(period_results, function(x) x$workers), na.rm = TRUE)
  total_plus15 <- sum(sapply(period_results, function(x) x$plus15_population), na.rm = TRUE)
  
  # Add weights to each country's results
  for (country_code in names(period_results)) {
    period_results[[country_code]]$workers_weight <- 
      period_results[[country_code]]$workers / total_workers * 100
    
    period_results[[country_code]]$wap_weight <- 
      period_results[[country_code]]$plus15_population / total_plus15 * 100
  }
  
  return(period_results)
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
      
      # Format the value based on metric type
      if (metric %in% c("workers", "plus15_population")) {
        # Format large numbers with commas
        value <- format(round(country_data[[metric]]), big.mark = ",")
      } else {
        # Format percentages or monetary values with 2 decimal places
        value <- sprintf("%.2f", country_data[[metric]])
      }
      
      # Add value to the dataframe
      result_df[period_idx, country_name] <- value
    }
  }
  
  return(result_df)
}

# Function to export dataframe as DTA file
export_as_dta <- function(df, sheet_name) {
  # Convert formatted values back to numeric for DTA
  numeric_df <- df
  
  # Convert all columns except Period to numeric
  for (col in names(numeric_df)) {
    if (col != "Period") {
      # Remove commas from numbers and convert to numeric
      numeric_df[[col]] <- as.numeric(gsub(",", "", numeric_df[[col]]))
    }
  }
  
  # Create a safe filename
  safe_name <- gsub(" ", "_", sheet_name)
  dta_file <- file.path(dta_dir, paste0(safe_name, ".dta"))
  
  # Export as DTA
  write_dta(numeric_df, dta_file)
  cat("✅ Saved DTA file:", dta_file, "\n")
}

# Main execution

# Process all countries and all time periods
all_period_results <- process_all_countries()

# Calculate weights for each period
for (period_key in names(all_period_results)) {
  all_period_results[[period_key]] <- calculate_weights(all_period_results[[period_key]])
}

# Create formatted results for each metric
formatted_results <- list(
  lfp = create_formatted_results(all_period_results, "lfp_rate"),
  employment = create_formatted_results(all_period_results, "employment_rate"),
  unemployment = create_formatted_results(all_period_results, "unemployment_rate"),
  youth_unemployment = create_formatted_results(all_period_results, "youth_unemployment_rate"),
  neet = create_formatted_results(all_period_results, "neet_rate"),
  alt_neet = create_formatted_results(all_period_results, "alt_neet_rate"),
  workers = create_formatted_results(all_period_results, "workers"),
  plus15 = create_formatted_results(all_period_results, "plus15_population"),
  workers_weight = create_formatted_results(all_period_results, "workers_weight"),
  wap_weight = create_formatted_results(all_period_results, "wap_weight"),
  # Income statistics
  ila_mean = create_formatted_results(all_period_results, "ila_mean"),
  ila_p20 = create_formatted_results(all_period_results, "ila_p20"),
  ila_median = create_formatted_results(all_period_results, "ila_median"),
  ila_p80 = create_formatted_results(all_period_results, "ila_p80"),
  # Wage statistics
  wage_mean = create_formatted_results(all_period_results, "wage_mean"),
  wage_p20 = create_formatted_results(all_period_results, "wage_p20"),
  wage_median = create_formatted_results(all_period_results, "wage_median"),
  wage_p80 = create_formatted_results(all_period_results, "wage_p80")
)

# Create description sheet
description_df <- data.frame(
  Sheet = c(
    "LFP", "Employment", "Unemployment", "Youth Unempl", "NEET", "Alt NEET", 
    "Workers", "Plus15", "Workers weight", "WAP weight",
    "ILA Mean", "ILA P20", "ILA Median", "ILA P80",
    "Wage Mean", "Wage P20", "Wage Median", "Wage P80",
    "Description"
  ),
  Description = c(
    "Labor Force Participation Rate: Percentage of working age population (15+) that is economically active (pea=1)",
    "Employment Rate: Percentage of working age population (15+) that is employed (ocupado=1)",
    "Unemployment Rate: Percentage of economically active population that is unemployed (desocupa=1)",
    "Youth Unemployment Rate: Percentage of economically active population aged 15-24 that is unemployed",
    "NEET Rate: Percentage of youth aged 15-24 that are Not in Education, Employment or Training (not working and not studying)",
    "Alternative NEET Rate: Percentage of young adults aged 15-29 that are Not in Education, Employment or Training",
    "Total Workers: Weighted sum of employed individuals (ocupado=1)",
    "15+ Population: Weighted sum of individuals aged 15+",
    "Workers Weight: Country's share in the total workers across all countries in the sample",
    "Working Age Population Weight: Country's share in the total 15+ population across all countries in the sample",
    "Mean Labor Income (ila_ppp17): Weighted mean of labor income for employed individuals with non-missing income",
    "Labor Income 20th Percentile: 20th percentile of ila_ppp17 for employed individuals with non-missing income",
    "Labor Income Median: Median (50th percentile) of ila_ppp17 for employed individuals with non-missing income",
    "Labor Income 80th Percentile: 80th percentile of ila_ppp17 for employed individuals with non-missing income",
    "Mean Wage (wage_ppp17): Weighted mean of wage income for employed individuals with non-missing wage",
    "Wage 20th Percentile: 20th percentile of wage_ppp17 for employed individuals with non-missing wage",
    "Wage Median: Median (50th percentile) of wage_ppp17 for employed individuals with non-missing wage",
    "Wage 80th Percentile: 80th percentile of wage_ppp17 for employed individuals with non-missing wage",
    "This sheet contains descriptions of each sheet in this workbook"
  ),
  stringsAsFactors = FALSE
)

# Create the output Excel file
today_date <- format(Sys.Date(), "%Y-%m-%d")
output_file <- file.path(output_dir, paste0(today_date, "-LMR-country-nocohh-V4.xlsx"))

# Prepare sheets for Excel
sheets <- list(
  "LFP" = formatted_results$lfp,
  "Employment" = formatted_results$employment,
  "Unemployment" = formatted_results$unemployment,
  "Youth Unempl" = formatted_results$youth_unemployment,
  "NEET" = formatted_results$neet,
  "Alt NEET" = formatted_results$alt_neet,
  "Workers" = formatted_results$workers,
  "Plus15" = formatted_results$plus15,
  "Workers weight" = formatted_results$workers_weight,
  "WAP weight" = formatted_results$wap_weight,
  "ILA Mean" = formatted_results$ila_mean,
  "ILA P20" = formatted_results$ila_p20,
  "ILA Median" = formatted_results$ila_median,
  "ILA P80" = formatted_results$ila_p80,
  "Wage Mean" = formatted_results$wage_mean,
  "Wage P20" = formatted_results$wage_p20,
  "Wage Median" = formatted_results$wage_median,
  "Wage P80" = formatted_results$wage_p80,
  "Description" = description_df
)

# Save Excel file
write_xlsx(sheets, path = output_file)
cat("✅ Analysis saved to:", output_file, "\n")

# # Export each sheet as DTA file
# cat("\n=============================\n")
# cat("EXPORTING DTA FILES\n")
# cat("=============================\n")
# 
# for (sheet_name in names(sheets)) {
#   if (sheet_name != "Description") {  # Skip description sheet
#     export_as_dta(sheets[[sheet_name]], sheet_name)
#   }
# }

cat("\n=============================\n")
cat("ANALYSIS COMPLETE\n")
cat("=============================\n")

# Clean up environment to free memory
rm(list = ls())
gc()
