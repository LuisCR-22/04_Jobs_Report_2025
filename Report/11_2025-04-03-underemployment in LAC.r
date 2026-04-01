# Underemployment Analysis for Latin American Countries.
# This script processes household survey data to analyze underemployment in Latin American countries, calculating both standard (% of employed) and alternative (% of population 15+) underemployment rates, and creating aggregated metrics for LAC-9 and LAC-10 country groups.
# Author: Luis Castellanos
# Last modified: 2025/05/06
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

# Define data directory path
data_dir <- "C:/Users/wb593225/OneDrive - WBG/Desktop/Luis - Private/Lablac/Semiannual report/Lablac data/No cohh"

# Define output directory path
output_dir <- "C:/Users/wb593225/OneDrive - WBG/Desktop/Luis - Private/Lablac/Semiannual report/Excel/Lablac output/01_New outputs/14 undermeployment"

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
  dom = "Dominican Republic",
  ecu = "Ecuador",
  gtm = "Guatemala",
  mex = "Mexico",
  per = "Peru",
  pry = "Paraguay",
  slv = "El Salvador",
  ury = "Uruguay"
)

# Define LAC-9 and LAC-10 country codes
lac9_countries <- c("arg", "bol", "bra", "chl", "col", "cri", "mex", "per", "ury")
lac10_countries <- c("arg", "bol", "bra", "chl", "col", "cri", "dom", "mex", "per", "ury")

# Function to process a dataset and calculate employment and underemployment
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
  required_vars <- c("pondera", "ocupado", "edad", "deseo_emp")
  if(!all(required_vars %in% names(dat))) {
    missing_vars <- required_vars[!required_vars %in% names(dat)]
    cat("⚠️ Missing required variables in", basename(file), ":", paste(missing_vars, collapse=", "), "\n")
    return(NULL)
  }
  
  # Calculate total population 15+ (edad > 14)
  total_pop_15plus <- sum(dat$pondera[dat$edad > 14], na.rm = TRUE)
  
  # Apply base filter: edad > 14 and ocupado == 1
  employed_data <- dat %>% filter(edad > 14, ocupado == 1)
  
  if(nrow(employed_data) == 0) {
    cat("⚠️ No employed observations after applying filters in", basename(file), "\n")
    return(NULL)
  }
  
  # Calculate total employed (weighted)
  total_employed <- sum(employed_data$pondera, na.rm = TRUE)
  
  # Calculate total underemployed (weighted)
  # Filter for deseo_emp == 1 (wants another job)
  underemployed_data <- employed_data %>% filter(deseo_emp == 1)
  total_underemployed <- sum(underemployed_data$pondera, na.rm = TRUE)
  
  # Calculate underemployment rate (as % of employed)
  underemployment_rate <- (total_underemployed / total_employed) * 100
  
  # Calculate alternative underemployment rate (as % of population 15+)
  alt_underemployment_rate <- (total_underemployed / total_pop_15plus) * 100
  
  # Return results as a list
  return(list(
    country_code = country_code,
    country_name = country_names[country_code],
    period = period_label,
    year = year,
    quarter = quarter,
    total_pop_15plus = total_pop_15plus,
    total_employed = total_employed,
    total_underemployed = total_underemployed,
    underemployment_rate = underemployment_rate,
    alt_underemployment_rate = alt_underemployment_rate
  ))
}

# Function to process all countries with specified files
process_country_data <- function() {
  all_results <- list()
  
  # Define file patterns for each country
  file_patterns <- list()
  
  # For most countries, use 2024 files
  default_countries <- c("arg", "bol", "bra", "chl", "col", "cri", "dom", "ecu", "mex", "per", "pry", "ury")
  for (country_code in default_countries) {
    file_patterns[[country_code]] <- sprintf("LABLAC_%s_2024_q", country_code)
  }
  
  # Special case for Guatemala: 2022_q04
  file_patterns[["gtm"]] <- "LABLAC_gtm_2022_q04"
  
  # Special case for El Salvador: 2023_q02 and 2023_q04
  file_patterns[["slv"]] <- c("LABLAC_slv_2023_q02", "LABLAC_slv_2023_q04")
  
  # Process each country
  for (country_code in names(country_names)) {
    cat("\n=============================\n")
    cat(paste0("PROCESSING ", toupper(country_code), " DATA\n"))
    cat("=============================\n")
    
    # Skip if no pattern defined for this country
    if (!(country_code %in% names(file_patterns))) {
      cat(sprintf("⚠️ No file patterns defined for %s. Skipping analysis.\n", toupper(country_code)))
      next
    }
    
    # Get patterns for this country
    country_patterns <- file_patterns[[country_code]]
    
    # List all files in directory
    all_files <- list.files(path = data_dir, pattern = "\\.dta$", full.names = TRUE)
    
    # Filter files by patterns
    files <- c()
    for (pattern in country_patterns) {
      matching_files <- all_files[grepl(pattern, all_files)]
      files <- c(files, matching_files)
    }
    
    if(length(files) == 0) {
      cat(sprintf("⚠️ No %s files found matching specified patterns. Skipping analysis.\n", toupper(country_code)))
      next
    }
    
    cat("Found", length(files), "files for", toupper(country_code), "\n")
    
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

# Create formatted dataframe for total population 15+
create_pop_15plus_results <- function(all_results) {
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
      
      # Add total population 15+ value
      if (!is.null(country_data$total_pop_15plus) && !is.na(country_data$total_pop_15plus)) {
        # Format with thousands separator
        value <- format(round(country_data$total_pop_15plus), big.mark = ",")
        
        # Add value to the dataframe
        result_df[period_idx, country_name] <- value
      }
    }
  }
  
  return(result_df)
}

# Create formatted dataframe for total employed people
create_employed_results <- function(all_results) {
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
      
      # Add total employed value
      if (!is.null(country_data$total_employed) && !is.na(country_data$total_employed)) {
        # Format with thousands separator
        value <- format(round(country_data$total_employed), big.mark = ",")
        
        # Add value to the dataframe
        result_df[period_idx, country_name] <- value
      }
    }
  }
  
  return(result_df)
}

# Create formatted dataframe for total underemployed people
create_underemployed_results <- function(all_results) {
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
      
      # Add total underemployed value
      if (!is.null(country_data$total_underemployed) && !is.na(country_data$total_underemployed)) {
        # Format with thousands separator
        value <- format(round(country_data$total_underemployed), big.mark = ",")
        
        # Add value to the dataframe
        result_df[period_idx, country_name] <- value
      }
    }
  }
  
  return(result_df)
}

# Create formatted dataframe for underemployment rates (as % of employed)
create_rate_results <- function(all_results) {
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
      
      # Add underemployment rate
      if (!is.null(country_data$underemployment_rate) && !is.na(country_data$underemployment_rate)) {
        # Format as percentage with 2 decimal places
        value <- sprintf("%.2f%%", country_data$underemployment_rate)
        
        # Add value to the dataframe
        result_df[period_idx, country_name] <- value
      }
    }
  }
  
  return(result_df)
}

# Create formatted dataframe for alternative underemployment rates (as % of pop 15+)
create_alt_rate_results <- function(all_results) {
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
      
      # Add alternative underemployment rate
      if (!is.null(country_data$alt_underemployment_rate) && !is.na(country_data$alt_underemployment_rate)) {
        # Format as percentage with 2 decimal places
        value <- sprintf("%.2f%%", country_data$alt_underemployment_rate)
        
        # Add value to the dataframe
        result_df[period_idx, country_name] <- value
      }
    }
  }
  
  return(result_df)
}

# Create aggregated results for LAC-9 and LAC-10
create_aggregated_results <- function(all_results) {
  # Initialize results dataframe
  result_df <- data.frame(
    Period = character(),
    Group = character(),
    Total_Pop_15plus = numeric(),
    Total_Employed = numeric(),
    Total_Underemployed = numeric(),
    Underemployment_Rate = numeric(),
    Alt_Underemployment_Rate = numeric(),
    stringsAsFactors = FALSE
  )
  
  # Process each period
  for (period_key in names(all_results)) {
    period_data <- all_results[[period_key]]
    
    # Calculate LAC-9 totals
    lac9_pop_15plus <- 0
    lac9_employed <- 0
    lac9_underemployed <- 0
    lac9_countries_found <- c()
    
    # Calculate LAC-10 totals
    lac10_pop_15plus <- 0
    lac10_employed <- 0
    lac10_underemployed <- 0
    lac10_countries_found <- c()
    
    # Sum totals for each group
    for (country_code in names(period_data)) {
      country_data <- period_data[[country_code]]
      
      if (country_code %in% lac9_countries) {
        lac9_pop_15plus <- lac9_pop_15plus + country_data$total_pop_15plus
        lac9_employed <- lac9_employed + country_data$total_employed
        lac9_underemployed <- lac9_underemployed + country_data$total_underemployed
        lac9_countries_found <- c(lac9_countries_found, country_code)
      }
      
      if (country_code %in% lac10_countries) {
        lac10_pop_15plus <- lac10_pop_15plus + country_data$total_pop_15plus
        lac10_employed <- lac10_employed + country_data$total_employed
        lac10_underemployed <- lac10_underemployed + country_data$total_underemployed
        lac10_countries_found <- c(lac10_countries_found, country_code)
      }
    }
    
    # Calculate rates
    lac9_rate <- ifelse(lac9_employed > 0, (lac9_underemployed / lac9_employed) * 100, NA)
    lac10_rate <- ifelse(lac10_employed > 0, (lac10_underemployed / lac10_employed) * 100, NA)
    
    # Calculate alternative rates
    lac9_alt_rate <- ifelse(lac9_pop_15plus > 0, (lac9_underemployed / lac9_pop_15plus) * 100, NA)
    lac10_alt_rate <- ifelse(lac10_pop_15plus > 0, (lac10_underemployed / lac10_pop_15plus) * 100, NA)
    
    # Only add LAC-9 if we have at least some of the countries
    if (length(lac9_countries_found) > 0) {
      result_df <- rbind(result_df, data.frame(
        Period = period_key,
        Group = paste0("LAC-9 (", length(lac9_countries_found), "/9 countries)"),
        Total_Pop_15plus = lac9_pop_15plus,
        Total_Employed = lac9_employed,
        Total_Underemployed = lac9_underemployed,
        Underemployment_Rate = lac9_rate,
        Alt_Underemployment_Rate = lac9_alt_rate,
        stringsAsFactors = FALSE
      ))
    }
    
    # Only add LAC-10 if we have at least some of the countries
    if (length(lac10_countries_found) > 0) {
      result_df <- rbind(result_df, data.frame(
        Period = period_key,
        Group = paste0("LAC-10 (", length(lac10_countries_found), "/10 countries)"),
        Total_Pop_15plus = lac10_pop_15plus,
        Total_Employed = lac10_employed,
        Total_Underemployed = lac10_underemployed,
        Underemployment_Rate = lac10_rate,
        Alt_Underemployment_Rate = lac10_alt_rate,
        stringsAsFactors = FALSE
      ))
    }
  }
  
  # Format the numeric columns
  result_df$Total_Pop_15plus_Formatted <- format(round(result_df$Total_Pop_15plus), big.mark = ",")
  result_df$Total_Employed_Formatted <- format(round(result_df$Total_Employed), big.mark = ",")
  result_df$Total_Underemployed_Formatted <- format(round(result_df$Total_Underemployed), big.mark = ",")
  result_df$Underemployment_Rate_Formatted <- sprintf("%.2f%%", result_df$Underemployment_Rate)
  result_df$Alt_Underemployment_Rate_Formatted <- sprintf("%.2f%%", result_df$Alt_Underemployment_Rate)
  
  # Select and order columns for display
  display_df <- result_df[, c("Period", "Group", "Total_Pop_15plus_Formatted", "Total_Employed_Formatted", 
                             "Total_Underemployed_Formatted", "Underemployment_Rate_Formatted", 
                             "Alt_Underemployment_Rate_Formatted")]
  colnames(display_df) <- c("Period", "Group", "Total Population 15+", "Total Employed", 
                           "Total Underemployed", "Underemployment Rate (% of Employed)", 
                           "Underemployment Rate (% of Population 15+)")
  
  return(display_df)
}

# Create aggregated results specifically for alternative underemployment rates
create_alt_aggregated_results <- function(all_results) {
  # Initialize results dataframe
  result_df <- data.frame(
    Period = character(),
    Group = character(),
    Total_Pop_15plus = numeric(),
    Total_Underemployed = numeric(),
    Alt_Underemployment_Rate = numeric(),
    stringsAsFactors = FALSE
  )
  
  # Process each period
  for (period_key in names(all_results)) {
    period_data <- all_results[[period_key]]
    
    # Calculate LAC-9 totals
    lac9_pop_15plus <- 0
    lac9_underemployed <- 0
    lac9_countries_found <- c()
    
    # Calculate LAC-10 totals
    lac10_pop_15plus <- 0
    lac10_underemployed <- 0
    lac10_countries_found <- c()
    
    # Sum totals for each group
    for (country_code in names(period_data)) {
      country_data <- period_data[[country_code]]
      
      if (country_code %in% lac9_countries) {
        lac9_pop_15plus <- lac9_pop_15plus + country_data$total_pop_15plus
        lac9_underemployed <- lac9_underemployed + country_data$total_underemployed
        lac9_countries_found <- c(lac9_countries_found, country_code)
      }
      
      if (country_code %in% lac10_countries) {
        lac10_pop_15plus <- lac10_pop_15plus + country_data$total_pop_15plus
        lac10_underemployed <- lac10_underemployed + country_data$total_underemployed
        lac10_countries_found <- c(lac10_countries_found, country_code)
      }
    }
    
    # Calculate alternative rates
    lac9_alt_rate <- ifelse(lac9_pop_15plus > 0, (lac9_underemployed / lac9_pop_15plus) * 100, NA)
    lac10_alt_rate <- ifelse(lac10_pop_15plus > 0, (lac10_underemployed / lac10_pop_15plus) * 100, NA)
    
    # Only add LAC-9 if we have at least some of the countries
    if (length(lac9_countries_found) > 0) {
      result_df <- rbind(result_df, data.frame(
        Period = period_key,
        Group = paste0("LAC-9 (", length(lac9_countries_found), "/9 countries)"),
        Total_Pop_15plus = lac9_pop_15plus,
        Total_Underemployed = lac9_underemployed,
        Alt_Underemployment_Rate = lac9_alt_rate,
        stringsAsFactors = FALSE
      ))
    }
    
    # Only add LAC-10 if we have at least some of the countries
    if (length(lac10_countries_found) > 0) {
      result_df <- rbind(result_df, data.frame(
        Period = period_key,
        Group = paste0("LAC-10 (", length(lac10_countries_found), "/10 countries)"),
        Total_Pop_15plus = lac10_pop_15plus,
        Total_Underemployed = lac10_underemployed,
        Alt_Underemployment_Rate = lac10_alt_rate,
        stringsAsFactors = FALSE
      ))
    }
  }
  
  # Format the numeric columns
  result_df$Total_Pop_15plus_Formatted <- format(round(result_df$Total_Pop_15plus), big.mark = ",")
  result_df$Total_Underemployed_Formatted <- format(round(result_df$Total_Underemployed), big.mark = ",")
  result_df$Alt_Underemployment_Rate_Formatted <- sprintf("%.2f%%", result_df$Alt_Underemployment_Rate)
  
  # Select and order columns for display
  display_df <- result_df[, c("Period", "Group", "Total_Pop_15plus_Formatted", 
                             "Total_Underemployed_Formatted", "Alt_Underemployment_Rate_Formatted")]
  colnames(display_df) <- c("Period", "Group", "Total Population 15+", 
                           "Total Underemployed", "Underemployment Rate (% of Population 15+)")
  
  return(display_df)
}

# Create description sheet for the Excel file
create_description_sheet <- function() {
  description_df <- data.frame(
    Sheet = c(
      "Total Population 15+",
      "Total Employed",
      "Total Underemployed",
      "Underemployment Rate (% of Emp)",
      "Underemployment Rate (% of 15+)",
      "LAC Aggregates",
      "LAC Aggregates (Alt Rate)",
      "Description"
    ),
    Description = c(
      paste0(
        "Total population 15+: Weighted sum of individuals aged 15+ (edad>14).",
        " This represents the total working age population in each country for the respective period."
      ),
      paste0(
        "Total employed: Weighted sum of individuals aged 15+ who are employed (ocupado=1).",
        " This represents the total employed population in each country for the respective period."
      ),
      paste0(
        "Total underemployed: Weighted sum of employed individuals aged 15+ who desire a second job (deseo_emp=1).",
        " This represents the subset of employed workers who wish to work more hours or have an additional job."
      ),
      paste0(
        "Underemployment Rate (% of Employed): Percentage of employed population that is underemployed (deseo_emp=1),",
        " calculated as (Total Underemployed / Total Employed) * 100."
      ),
      paste0(
        "Underemployment Rate (% of Population 15+): Alternative calculation of underemployment rate,",
        " using the total working age population (15+) as denominator instead of just the employed population.",
        " Calculated as (Total Underemployed / Total Population 15+) * 100."
      ),
      paste0(
        "LAC Aggregates: Total population 15+, total employed, total underemployed, and both underemployment rates for LAC-9",
        " (Argentina, Bolivia, Brazil, Chile, Colombia, Costa Rica, Mexico, Peru, and Uruguay)",
        " and LAC-10 (LAC-9 plus Dominican Republic)."
      ),
      paste0(
        "LAC Aggregates (Alt Rate): This sheet focuses specifically on the alternative underemployment rate calculation",
        " (underemployed as percentage of population 15+) for LAC-9 and LAC-10 groups."
      ),
      paste0(
        "Methodology: This analysis uses household survey data from the LABLAC database.",
        " The analysis focuses on the working age population (15+) using employment status (ocupado=1)",
        " and desire for additional employment (deseo_emp=1) as key indicators.",
        " All calculations use appropriate survey weights (pondera) to ensure representativeness.",
        " Two underemployment measures are provided: (1) the standard measure showing underemployed as a percentage",
        " of the employed population, and (2) an alternative measure showing underemployed as a percentage",
        " of the total working age population (15+)."
      )
    ),
    stringsAsFactors = FALSE
  )
  
  return(description_df)
}

# Main execution

# Process all countries with specified files
all_period_results <- process_country_data()

# Create formatted results for each metric
formatted_results <- list(
  total_pop_15plus = create_pop_15plus_results(all_period_results),
  total_employed = create_employed_results(all_period_results),
  total_underemployed = create_underemployed_results(all_period_results),
  underemployment_rate = create_rate_results(all_period_results),
  alt_underemployment_rate = create_alt_rate_results(all_period_results),
  lac_aggregates = create_aggregated_results(all_period_results),
  lac_alt_aggregates = create_alt_aggregated_results(all_period_results)
)

# Create description sheet
description_df <- create_description_sheet()

# Create the output Excel file with current date and V2 designation
today_date <- format(Sys.Date(), "%Y-%m-%d")
output_file <- file.path(output_dir, paste0(today_date, "-recent-underemployment-V2.xlsx"))

# Prepare sheets for Excel
sheets <- list(
  "Total Population 15+" = formatted_results$total_pop_15plus,
  "Total Employed" = formatted_results$total_employed,
  "Total Underemployed" = formatted_results$total_underemployed,
  "Underemployment Rate (% of Emp)" = formatted_results$underemployment_rate,
  "Underemployment Rate (% of 15+)" = formatted_results$alt_underemployment_rate,
  "LAC Aggregates" = formatted_results$lac_aggregates,
  "LAC Aggregates (Alt Rate)" = formatted_results$lac_alt_aggregates,
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
