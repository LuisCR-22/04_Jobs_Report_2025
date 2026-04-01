# Income Decile Analysis for Latin American Countries - Dashboard Format V2
# Includes missing value analysis and excess missing imputation for 2020
# Author: Luis Castellanos - Stats Team
# Last modification: 2025-06-06
# This code was a fail try to modify the output for 2020 and is inefficient. Needs to be reviewed

rm(list = ls())  # Clear the workspace
gc()  # Garbage collection

# Install required packages if not already installed
if (!require("haven")) install.packages("haven", dependencies = TRUE)
if (!require("dplyr")) install.packages("dplyr", dependencies = TRUE)
if (!require("writexl")) install.packages("writexl", dependencies = TRUE)
if (!require("tidyr")) install.packages("tidyr", dependencies = TRUE)
if (!require("stringr")) install.packages("stringr", dependencies = TRUE)
if (!require("ineq")) install.packages("ineq", dependencies = TRUE)

library(haven)      # Read Stata (.dta) files
library(dplyr)      # Data manipulation
library(writexl)    # Write output to Excel
library(tidyr)      # For data reshaping
library(stringr)    # For string manipulation
library(ineq)       # For unweighted Gini calculation

# Set to TRUE to process only Argentina 2016 for testing
TEST_MODE <- FALSE

# Define the data directory
data_dir <- "C:/Users/wb593225/OneDrive - WBG/Desktop/Luis - Private/Lablac/Semiannual report/Lablac data/No cohh"

# Create mapping for country codes to full names (Guatemala removed)
country_mapping <- c(
  "arg" = "Argentina",
  "bol" = "Bolivia",
  "bra" = "Brazil",
  "chl" = "Chile",
  "col" = "Colombia",
  "cri" = "Costa Rica",
  "dom" = "Dominican Republic",
  "ecu" = "Ecuador",
  "mex" = "Mexico",
  "per" = "Peru (Lima and Callao)",  # Updated name
  "pry" = "Paraguay",
  "slv" = "El Salvador",
  "ury" = "Uruguay"
)

# Spanish translations for countries (Guatemala removed, Peru updated)
country_spanish <- c(
  "Argentina" = "Argentina",
  "Bolivia" = "Bolivia", 
  "Brazil" = "Brasil",
  "Chile" = "Chile",
  "Colombia" = "Colombia",
  "Costa Rica" = "Costa Rica",
  "Dominican Republic" = "República Dominicana",
  "Ecuador" = "Ecuador",
  "Mexico" = "México",
  "Peru (Lima and Callao)" = "Perú (Lima y Callao)",
  "Paraguay" = "Paraguay",
  "El Salvador" = "El Salvador",
  "Uruguay" = "Uruguay"
)

# Define country-specific data availability rules
get_data_availability_rules <- function() {
  list(
    # Chile: only Q4 data available
    "chl" = list(allowed_quarters = 4),
    
    # Colombia: exclude 2020
    "col" = list(excluded_years = 2020),
    
    # Ecuador: only 2021-2024
    "ecu" = list(allowed_years = 2021:2024),
    
    # Paraguay: exclude 2020-2021
    "pry" = list(excluded_years = 2020:2021),
    
    # Uruguay: exclude 2020-2021
    "ury" = list(excluded_years = 2020:2021)
  )
}

# Function to check if data should be available for a country-period
should_have_data <- function(country_code, year, quarter) {
  rules <- get_data_availability_rules()
  
  if (!country_code %in% names(rules)) {
    return(TRUE)  # No restrictions
  }
  
  rule <- rules[[country_code]]
  
  # Check quarter restrictions
  if ("allowed_quarters" %in% names(rule)) {
    if (!quarter %in% rule$allowed_quarters) {
      return(FALSE)
    }
  }
  
  # Check year exclusions
  if ("excluded_years" %in% names(rule)) {
    if (year %in% rule$excluded_years) {
      return(FALSE)
    }
  }
  
  # Check year restrictions
  if ("allowed_years" %in% names(rule)) {
    if (!year %in% rule$allowed_years) {
      return(FALSE)
    }
  }
  
  return(TRUE)
}

# Function to create complete grid of all possible combinations
create_complete_grid <- function(test_mode = FALSE) {
  if (test_mode) {
    # For test mode: only Argentina 2016
    countries <- "arg"
    years <- 2016
    quarters <- 1:4
  } else {
    # Full processing: all countries 2016-2024
    countries <- names(country_mapping)
    years <- 2016:2024
    quarters <- 1:4
  }
  
  # Create all combinations
  grid <- expand.grid(
    country_code = countries,
    year = years,
    quarter = quarters,
    stringsAsFactors = FALSE
  )
  
  # Add derived fields
  grid$period <- paste0(grid$year, "-Q", grid$quarter)
  grid$country_name <- country_mapping[grid$country_code]
  
  return(grid)
}

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

# Function to calculate missing value proportion
calculate_missing_proportion <- function(data, value_var) {
  if (nrow(data) == 0) return(NA)
  
  total_obs <- nrow(data)
  missing_obs <- sum(is.na(data[[value_var]]))
  return(missing_obs / total_obs)
}

# Function to calculate expected missing values for 2020
calculate_expected_missing_2020 <- function(missing_data, country_name, quarter) {
  # Get 2019 and 2021 data for this country-quarter
  data_2019 <- missing_data %>% 
    filter(Country == country_name, Quarter == quarter, Year == 2019)
  data_2021 <- missing_data %>% 
    filter(Country == country_name, Quarter == quarter, Year == 2021)
  
  if (nrow(data_2019) > 0 && nrow(data_2021) > 0) {
    # Simple average of 2019 and 2021
    return((data_2019$Missing_Proportion + data_2021$Missing_Proportion) / 2)
  } else if (nrow(data_2019) > 0) {
    # Only 2019 available
    return(data_2019$Missing_Proportion)
  } else if (nrow(data_2021) > 0) {
    # Only 2021 available
    return(data_2021$Missing_Proportion)
  } else {
    # Neither available, use overall country average excluding 2020
    country_avg <- missing_data %>%
      filter(Country == country_name, Year != 2020) %>%
      summarize(avg_missing = mean(Missing_Proportion, na.rm = TRUE)) %>%
      pull(avg_missing)
    
    return(ifelse(is.na(country_avg), 0, country_avg))
  }
}

# Function to impute zeros for excess missing values
impute_excess_missing <- function(data, value_var, excess_proportion) {
  if (is.na(excess_proportion) || excess_proportion <= 0) {
    return(data)  # No excess missing to impute
  }
  
  # Get observations with missing values
  missing_obs <- which(is.na(data[[value_var]]))
  
  if (length(missing_obs) == 0) {
    return(data)  # No missing values to impute
  }
  
  # Calculate how many to impute (excess proportion of total observations)
  total_obs <- nrow(data)
  n_to_impute <- round(excess_proportion * total_obs)
  
  # Don't impute more than available missing values
  n_to_impute <- min(n_to_impute, length(missing_obs))
  
  if (n_to_impute > 0) {
    # Randomly select which missing values to impute as zero
    set.seed(12345)  # For reproducibility
    impute_indices <- sample(missing_obs, n_to_impute)
    data[[value_var]][impute_indices] <- 0
  }
  
  return(data)
}

# Function to calculate weighted mean within each decile (ORIGINAL METHOD)
calculate_decile_means_original <- function(data, value_var, weight_var) {
  # Check if we have enough data points
  if (nrow(data) < 10) {
    return(NULL)  # Not enough data to calculate deciles
  }
  
  # Filter valid data (excludes missing income)
  valid_data <- data %>%
    filter(!is.na(.data[[value_var]]), !is.na(.data[[weight_var]]), .data[[value_var]] > 0)
  
  if (nrow(valid_data) < 10) {
    return(NULL)  # Not enough valid data
  }
  
  # Create a new dataframe with just the variables we need
  decile_data <- data.frame(
    value = valid_data[[value_var]],
    weight = valid_data[[weight_var]]
  )
  
  # Sort by value
  decile_data <- decile_data[order(decile_data$value),]
  
  # Calculate cumulative weights
  decile_data$cum_weight <- cumsum(decile_data$weight) / sum(decile_data$weight)
  
  # Assign deciles (1-10)
  decile_data$decile <- findInterval(decile_data$cum_weight, seq(0.1, 0.9, by=0.1)) + 1
  
  # Calculate weighted mean within each decile
  decile_means <- decile_data %>%
    group_by(decile) %>%
    summarize(
      mean_value = weighted.mean(value, weight, na.rm = TRUE),
      .groups = 'drop'
    )
  
  # Make sure we have all 10 deciles
  if(nrow(decile_means) < 10) {
    missing_deciles <- setdiff(1:10, decile_means$decile)
    for(d in missing_deciles) {
      decile_means <- bind_rows(
        decile_means,
        data.frame(decile = d, mean_value = NA)
      )
    }
    decile_means <- decile_means[order(decile_means$decile),]
  }
  
  return(decile_means)
}

# Function for unweighted Gini calculation
unweighted_gini <- function(values) {
  valid <- !is.na(values) & values > 0  # Remove NAs and non-positive values
  if (sum(valid) > 1) {
    return(ineq::Gini(values[valid]))
  } else {
    return(NA)
  }
}

# Function to create Spanish translations
translate_to_spanish <- function(indicator_complete, indicator, category) {
  # Indicator translations
  indicator_esp <- case_when(
    indicator == "Labor Income Gini" ~ "Gini del ingreso laboral",
    indicator == "Mean Labor Income by Decile" ~ "Ingreso laboral promedio por decil",
    indicator == "Mean Labor Income by Decile (excess imputed)" ~ "Ingreso laboral promedio por decil (exceso imputado)",
    indicator == "Mean Wage by Decile" ~ "Salario promedio por decil",
    indicator == "Wage Gini" ~ "Gini del salario",
    TRUE ~ indicator
  )
  
  # Category translations
  category_esp <- case_when(
    category == "Total" ~ "Total",
    category == "Decile 1" ~ "Decil 1",
    category == "Decile 2" ~ "Decil 2", 
    category == "Decile 3" ~ "Decil 3",
    category == "Decile 4" ~ "Decil 4",
    category == "Decile 5" ~ "Decil 5",
    category == "Decile 6" ~ "Decil 6",
    category == "Decile 7" ~ "Decil 7",
    category == "Decile 8" ~ "Decil 8",
    category == "Decile 9" ~ "Decil 9",
    category == "Decile 10" ~ "Decil 10",
    TRUE ~ category
  )
  
  # Indicator Complete translations
  indicator_complete_esp <- case_when(
    indicator_complete == "Wage Gini" ~ "Gini del salario",
    indicator_complete == "Labor Income Gini" ~ "Gini del ingreso laboral",
    str_detect(indicator_complete, "Mean Wage - Decile") ~ str_replace(indicator_complete, "Mean Wage - Decile", "Salario promedio - Decil"),
    str_detect(indicator_complete, "Mean Labor Income - Decile \\d+$") ~ str_replace(indicator_complete, "Mean Labor Income - Decile", "Ingreso laboral promedio - Decil"),
    str_detect(indicator_complete, "Mean Labor Income - Decile \\d+ \\(excess imputed\\)") ~ str_replace(indicator_complete, "Mean Labor Income - Decile (\\d+) \\(excess imputed\\)", "Ingreso laboral promedio - Decil \\1 (exceso imputado)"),
    TRUE ~ indicator_complete
  )
  
  return(list(
    indicator_esp = indicator_esp,
    category_esp = category_esp,
    indicator_complete_esp = indicator_complete_esp
  ))
}

# Function to add empty rows for all indicators
add_empty_indicators <- function(period, country_name) {
  # Define all possible indicators
  wage_indicators <- data.frame(
    Indicator_Complete = c(paste0("Mean Wage - Decile ", 1:10), "Wage Gini"),
    Indicator = c(rep("Mean Wage by Decile", 10), "Wage Gini"),
    Category = c(paste0("Decile ", 1:10), "Total")
  )
  
  labor_income_indicators <- data.frame(
    Indicator_Complete = c(paste0("Mean Labor Income - Decile ", 1:10), 
                           paste0("Mean Labor Income - Decile ", 1:10, " (excess imputed)"),
                           "Labor Income Gini"),
    Indicator = c(rep("Mean Labor Income by Decile", 10),
                  rep("Mean Labor Income by Decile (excess imputed)", 10),
                  "Labor Income Gini"),
    Category = c(paste0("Decile ", 1:10), paste0("Decile ", 1:10), "Total")
  )
  
  all_indicators <- bind_rows(wage_indicators, labor_income_indicators)
  
  # Create empty results for this period-country
  empty_results <- data.frame(
    Period = period,
    Country = country_name,
    Indicator_Complete = all_indicators$Indicator_Complete,
    Value = NA_real_,
    Indicator = all_indicators$Indicator,
    Category = all_indicators$Category,
    stringsAsFactors = FALSE
  )
  
  return(empty_results)
}

# Initialize missing value tracking
missing_analysis <- data.frame(
  Country = character(),
  Year = numeric(),
  Quarter = numeric(),
  Period = character(),
  Missing_Proportion = numeric(),
  stringsAsFactors = FALSE
)

# Create complete grid first
complete_grid <- create_complete_grid(TEST_MODE)

# Initialize results with all possible combinations
results <- data.frame()

for (i in 1:nrow(complete_grid)) {
  row <- complete_grid[i, ]
  empty_indicators <- add_empty_indicators(row$period, row$country_name)
  results <- bind_rows(results, empty_indicators)
}

# Get files to process
files_to_process <- list_files_to_process(TEST_MODE)

# Print selected files
cat("Selected", length(files_to_process), "files for processing:\n")
for (file in files_to_process) {
  cat("  -", basename(file), "\n")
}

# FIRST PASS: Calculate missing value proportions
cat("\n=== FIRST PASS: Calculating missing value proportions ===\n")

if (length(files_to_process) > 0) {
  for (file_path in files_to_process) {
    cat("Analyzing missing values in:", basename(file_path), "\n")
    
    # Extract country and period info from filename
    file_name <- basename(file_path)
    parts <- str_match(file_name, "LABLAC_([a-z]{3})_(\\d{4})_q(\\d{2})")[1,]
    
    if (length(parts) >= 4) {
      country_code <- parts[2]
      year <- as.numeric(parts[3])
      quarter <- as.numeric(parts[4])
      
      # Skip if country not in our mapping
      if (!country_code %in% names(country_mapping)) {
        next
      }
      
      period <- paste0(year, "-Q", quarter)
      country_name <- country_mapping[country_code]
      
      # Read dataset
      data <- tryCatch(read_dta(file_path), error = function(e) NULL)
      
      if (!is.null(data)) {
        # Apply same filters as main analysis
        data <- data %>% filter(edad > 14, cohi == 1)
        
        if (country_code %in% c("per", "pry")) {
          data <- data %>% filter(urbano == 1)
        }
        
        if (nrow(data) > 0 && "ila_ppp17" %in% colnames(data)) {
          missing_prop <- calculate_missing_proportion(data, "ila_ppp17")
          
          missing_analysis <- bind_rows(
            missing_analysis,
            data.frame(
              Country = country_name,
              Year = year,
              Quarter = quarter,
              Period = period,
              Missing_Proportion = missing_prop
            )
          )
        }
      }
    }
  }
}

# Calculate expected missing values for 2020 and excess missing
cat("\n=== Calculating expected and excess missing values for 2020 ===\n")

excess_missing_data <- data.frame(
  Country = character(),
  Year = numeric(),
  Quarter = numeric(),
  Period = character(),
  Observed_Missing = numeric(),
  Expected_Missing = numeric(),
  Excess_Missing = numeric(),
  stringsAsFactors = FALSE
)

for (country_name in unique(missing_analysis$Country)) {
  for (quarter in 1:4) {
    # Calculate expected missing for 2020
    expected_missing <- calculate_expected_missing_2020(missing_analysis, country_name, quarter)
    
    # Get observed missing for 2020
    observed_2020 <- missing_analysis %>%
      filter(Country == country_name, Year == 2020, Quarter == quarter)
    
    if (nrow(observed_2020) > 0) {
      observed_missing <- observed_2020$Missing_Proportion
      excess_missing <- pmax(0, observed_missing - expected_missing)  # Don't allow negative excess
      
      excess_missing_data <- bind_rows(
        excess_missing_data,
        data.frame(
          Country = country_name,
          Year = 2020,
          Quarter = quarter,
          Period = paste0("2020-Q", quarter),
          Observed_Missing = observed_missing,
          Expected_Missing = expected_missing,
          Excess_Missing = excess_missing
        )
      )
      
      cat("Country:", country_name, "Q", quarter, "- Observed:", round(observed_missing, 3), 
          "Expected:", round(expected_missing, 3), "Excess:", round(excess_missing, 3), "\n")
    }
  }
}

# SECOND PASS: Process data and calculate indicators
cat("\n=== SECOND PASS: Processing data and calculating indicators ===\n")

if (length(files_to_process) > 0) {
  for (file_path in files_to_process) {
    cat("Processing file:", basename(file_path), "\n")
    
    # Extract country and period info from filename
    file_name <- basename(file_path)
    parts <- str_match(file_name, "LABLAC_([a-z]{3})_(\\d{4})_q(\\d{2})")[1,]
    
    if (length(parts) >= 4) {
      country_code <- parts[2]
      year <- as.numeric(parts[3])
      quarter <- as.numeric(parts[4])
      
      # Skip if country not in our mapping
      if (!country_code %in% names(country_mapping)) {
        cat("Skipping file for excluded country:", country_code, "\n")
        next
      }
      
      period <- paste0(year, "-Q", quarter)
      country_name <- country_mapping[country_code]
      
      # Check if this country-period should have data
      if (!should_have_data(country_code, year, quarter)) {
        cat("Skipping", country_code, period, "due to data availability rules\n")
        next
      }
      
      # Read dataset with error handling
      data <- tryCatch(read_dta(file_path), error = function(e) {
        cat("⚠️ Error reading file:", basename(file_path), "- Keeping blank values. ⚠️\n")
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
          
          # Process wage data (with asal == 1 filter)
          wage_var <- "wage_ppp17"
          # Special case for Peru: use ilaho_ppp17 instead of wage_ppp17
          if (country_code == "per" && "ilaho_ppp17" %in% colnames(data)) {
            wage_var <- "ilaho_ppp17"
            cat("Note: Using ilaho_ppp17 instead of wage_ppp17 for Peru\n")
          }
          
          if (wage_var %in% colnames(data)) {
            # Filter for wage workers (asal == 1)
            wage_data <- data %>% filter(asal == 1)
            
            # Calculate wage decile means - ORIGINAL METHOD ONLY
            wage_deciles <- calculate_decile_means_original(wage_data, wage_var, "pondera")
            
            if (!is.null(wage_deciles)) {
              # Update wage decile means in results
              for (j in 1:nrow(wage_deciles)) {
                decile_num <- wage_deciles$decile[j]
                indicator_complete <- paste0("Mean Wage - Decile ", decile_num)
                
                # Find and update the corresponding row
                update_idx <- which(results$Period == period & 
                                      results$Country == country_name & 
                                      results$Indicator_Complete == indicator_complete)
                
                if (length(update_idx) > 0) {
                  results$Value[update_idx] <- wage_deciles$mean_value[j]
                }
              }
            }
            
            # Calculate unweighted wage Gini
            wage_gini_unweighted <- unweighted_gini(wage_data[[wage_var]])
            
            # Update wage Gini in results
            update_idx <- which(results$Period == period & 
                                  results$Country == country_name & 
                                  results$Indicator_Complete == "Wage Gini")
            
            if (length(update_idx) > 0) {
              results$Value[update_idx] <- wage_gini_unweighted
            }
          }
          
          # Process labor income data (no asal filter)
          if ("ila_ppp17" %in% colnames(data)) {
            
            # Calculate income decile means - ORIGINAL METHOD
            income_deciles_orig <- calculate_decile_means_original(data, "ila_ppp17", "pondera")
            
            if (!is.null(income_deciles_orig)) {
              # Update original income decile means in results
              for (j in 1:nrow(income_deciles_orig)) {
                decile_num <- income_deciles_orig$decile[j]
                indicator_complete <- paste0("Mean Labor Income - Decile ", decile_num)
                
                update_idx <- which(results$Period == period & 
                                      results$Country == country_name & 
                                      results$Indicator_Complete == indicator_complete)
                
                if (length(update_idx) > 0) {
                  results$Value[update_idx] <- income_deciles_orig$mean_value[j]
                }
              }
            }
            
            # Calculate income decile means - EXCESS IMPUTED METHOD
            # For 2020, impute zeros for excess missing values; otherwise use original method
            if (year == 2020) {
              # Get excess missing proportion for this country-quarter
              excess_data <- excess_missing_data %>%
                filter(Country == country_name, Quarter == quarter)
              
              if (nrow(excess_data) > 0) {
                excess_prop <- excess_data$Excess_Missing
                cat("Imputing excess missing for", country_name, period, "- Excess:", round(excess_prop, 3), "\n")
                
                # Create modified data with imputed zeros
                data_imputed <- impute_excess_missing(data, "ila_ppp17", excess_prop)
                
                # Calculate deciles with imputed data
                income_deciles_imputed <- calculate_decile_means_original(data_imputed, "ila_ppp17", "pondera")
              } else {
                # No excess missing data available, use original method
                income_deciles_imputed <- income_deciles_orig
              }
            } else {
              # Non-2020 periods: use original method
              income_deciles_imputed <- income_deciles_orig
            }
            
            if (!is.null(income_deciles_imputed)) {
              # Update imputed income decile means in results
              for (j in 1:nrow(income_deciles_imputed)) {
                decile_num <- income_deciles_imputed$decile[j]
                indicator_complete <- paste0("Mean Labor Income - Decile ", decile_num, " (excess imputed)")
                
                update_idx <- which(results$Period == period & 
                                      results$Country == country_name & 
                                      results$Indicator_Complete == indicator_complete)
                
                if (length(update_idx) > 0) {
                  results$Value[update_idx] <- income_deciles_imputed$mean_value[j]
                }
              }
            }
            
            # Calculate unweighted labor income Gini
            income_gini_unweighted <- unweighted_gini(data$ila_ppp17)
            
            # Update labor income Gini in results
            update_idx <- which(results$Period == period & 
                                  results$Country == country_name & 
                                  results$Indicator_Complete == "Labor Income Gini")
            
            if (length(update_idx) > 0) {
              results$Value[update_idx] <- income_gini_unweighted
            }
          }
        }
      }
    }
  }
}

# Add Spanish translations
if (nrow(results) > 0) {
  # Add Spanish columns
  results$Pais <- country_spanish[results$Country]
  
  # Apply translations
  spanish_translations <- mapply(translate_to_spanish, 
                                 results$Indicator_Complete, 
                                 results$Indicator, 
                                 results$Category, 
                                 SIMPLIFY = FALSE)
  
  results$Indicator_Complete_ESP <- sapply(spanish_translations, function(x) x$indicator_complete_esp)
  results$Indicator_ESP <- sapply(spanish_translations, function(x) x$indicator_esp)
  results$Category_ESP <- sapply(spanish_translations, function(x) x$category_esp)
  
  # Reorder columns to match desired structure
  results <- results %>%
    select(Period, Country, Indicator_Complete, Value, Indicator, Category,
           Pais, Indicator_Complete_ESP, Indicator_ESP, Category_ESP)
  
  # Round values for better presentation
  results <- results %>%
    mutate(
      Value = case_when(
        # Gini coefficients: 4 decimal places
        str_detect(Indicator, "Gini") ~ round(Value, 4),
        # Income/wage means: 1 decimal place
        TRUE ~ round(Value, 1)
      )
    )
  
  # Create description sheet
  description <- data.frame(
    Section = c("Overview", "Indicators", "Missing Value Analysis", "Data Availability Rules", "Methods", "Filters", "Country Specific", "Structure"),
    Description = c(
      "This dataset contains wage and labor income analysis by decile and Gini coefficients with missing value bias correction for 2020.",
      "Wage: 10 deciles + unweighted Gini. Labor Income: 10 deciles (original) + 10 deciles (excess imputed) + unweighted Gini. Total: 32 indicators per country-period.",
      "For 2020, calculates expected missing values based on 2019 and 2021 trends. Excess missing values (observed - expected) are imputed as zeros in the second labor income series.",
      "Chile: Q4 only. Colombia: 2020 excluded. Ecuador: 2021-2024 only. Paraguay/Uruguay: 2020-2021 excluded. Missing periods show blank values for dashboard compatibility.",
      "Original method excludes workers with missing income. Excess imputed method addresses pandemic bias by imputing zeros for excess missing values in 2020.",
      "Basic filters: Working age (edad > 14) and Heads of household (cohi == 1). Wage indicators additionally filter for wage workers (asal == 1).",
      "Peru and Paraguay: urban areas only (urbano == 1). Peru: ilaho_ppp17 used instead of wage_ppp17 for wage calculations. Guatemala excluded from analysis.",
      "Columns: Period, Country, Indicator_Complete, Value, Indicator, Category + Spanish translations. Values rounded: Gini (4 decimals), means (1 decimal)."
    )
  )
  
  # Define output file paths
  output_dir <- "C:/Users/wb593225/WBG/LAC Team for Statistical Development - WB Group - Documents/General/FY2025/Semiannual_Report/May/excel/Outputs/13 Dashboard data"
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  
  # Main output file
  output_file <- file.path(output_dir, "21_Deciles_gini_Tableau_format-V2.xlsx")
  
  # Prepare the list of sheets for the Excel file
  sheets_list <- list(
    "Data" = results,
    "Description" = description
  )
  
  # Save results into an Excel file
  write_xlsx(sheets_list, path = output_file)
  
  cat("✅ Main results saved to:", output_file, "\n")
  
  # Prepare missing values analysis file
  missing_summary <- missing_analysis %>%
    arrange(Country, Year, Quarter) %>%
    mutate(Missing_Percentage = round(Missing_Proportion * 100, 2)) %>%
    select(Country, Period, Year, Quarter, Missing_Percentage)
  
  excess_summary <- excess_missing_data %>%
    mutate(
      Observed_Percentage = round(Observed_Missing * 100, 2),
      Expected_Percentage = round(Expected_Missing * 100, 2),
      Excess_Percentage = round(Excess_Missing * 100, 2)
    ) %>%
    select(Country, Period, Year, Quarter, Observed_Percentage, Expected_Percentage, Excess_Percentage)
  
  missing_description <- data.frame(
    Sheet = c("Missing_Values_Series", "Excess_Missing_2020", "Description"),
    Description = c(
      "Time series of missing value proportions in labor income variable (ila_ppp17) by country-period. Shows how missing values increased during pandemic.",
      "Analysis of 2020 missing values: observed vs expected (based on 2019 and 2021 trends) and calculated excess missing values used for imputation.",
      "Missing value analysis methodology: Expected 2020 values calculated as average of 2019 and 2021 for same quarter. Excess missing = max(0, observed - expected)."
    )
  )
  
  # Missing values output file
  missing_file <- file.path(output_dir, "21_Excess_missings.xlsx")
  
  missing_sheets_list <- list(
    "Missing_Values_Series" = missing_summary,
    "Excess_Missing_2020" = excess_summary,
    "Description" = missing_description
  )
  
  # Save missing values analysis
  write_xlsx(missing_sheets_list, path = missing_file)
  
  cat("✅ Missing values analysis saved to:", missing_file, "\n")
  
  # Display summary information
  cat("\nProcessing complete.\n")
  cat("Total rows:", nrow(results), "\n")
  cat("Countries:", paste(unique(results$Country), collapse=", "), "\n")
  cat("Indicators:", length(unique(results$Indicator)), "unique indicators\n")
  cat("Periods:", paste(sort(unique(results$Period)), collapse=", "), "\n")
  
  # Show how many rows have data vs blank
  data_summary <- results %>%
    summarize(
      total_rows = n(),
      rows_with_data = sum(!is.na(Value)),
      rows_with_blank = sum(is.na(Value)),
      pct_with_data = round(100 * rows_with_data / total_rows, 1)
    )
  
  cat("\nData completeness:\n")
  cat("Total rows:", data_summary$total_rows, "\n")
  cat("Rows with data:", data_summary$rows_with_data, "\n") 
  cat("Rows with blank values:", data_summary$rows_with_blank, "\n")
  cat("Percentage with data:", data_summary$pct_with_data, "%\n")
  
  # Show missing value analysis summary
  cat("\nMissing value analysis summary:\n")
  cat("Countries analyzed:", length(unique(missing_analysis$Country)), "\n")
  cat("Periods with missing data:", nrow(missing_analysis), "\n")
  cat("2020 periods with excess missing:", nrow(excess_missing_data), "\n")
  
  if (nrow(excess_missing_data) > 0) {
    avg_excess <- mean(excess_missing_data$Excess_Missing, na.rm = TRUE)
    cat("Average excess missing in 2020:", round(avg_excess * 100, 2), "%\n")
  }
  
} else {
  cat("\nNo results were generated.\n")
}
