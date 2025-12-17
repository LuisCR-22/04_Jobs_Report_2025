#########################################
# Gini Coefficient Calculation - Replication for Brief (May 2025)
# Author: Luis Castellanos - lcastellanosrodr@worldbank.org
# Project: Regional Jobs Update - LAC
# Stats Team - Poverty and Equity Global Practice
# Date created: 2025-03-15
# Last modification: 2025-12-16
# Purpose: Calculate weighted and unweighted Gini coefficients for wage inequality
#          for selected countries, comparing circa 2016 vs circa 2022
#########################################

# Setup
rm(list = ls())
gc()

# Load required libraries
required_packages <- c("haven", "writexl", "dplyr", "ineq", "data.table", "matrixStats", "WDI")
for (pkg in required_packages) {
  if (!require(pkg, character.only = TRUE)) {
    install.packages(pkg, dependencies = TRUE)
    library(pkg, character.only = TRUE)
  }
}

# Define paths
input_path <- #"C:/Users/.../Raw data/GLD harmonization"
output_path <- #"C:/Users/.../Output/01_GLD"
ppp_path <- #"C:/Users/.../Input/Dta/PPP_17_Database_PIP.dta"

# Create output directory if it doesn't exist
dir.create(output_path, recursive = TRUE, showWarnings = FALSE)

# Define countries to process
countries_to_process <- c("ARM", "BGD", "ETH", "GEO", "GMB", "IDN", "IND", "LKA", 
                          "MNG", "PAK", "PHL", "RWA", "THA", "TUN", "TUR", "TZA", 
                          "ZAF", "ZMB")

#########################################
# Gini calculation functions
#########################################

weighted_gini <- function(values, weights) {
  valid <- !is.na(values) & !is.na(weights) & values > 0
  if (sum(valid) > 1) {
    values <- values[valid]
    weights <- weights[valid]
    sorted_indices <- order(values)
    values <- values[sorted_indices]
    weights <- weights[sorted_indices]
    cumulative_weights <- cumsum(weights) / sum(weights)
    cumulative_values <- cumsum(values * weights) / sum(values * weights)
    gini <- 1 - sum((cumulative_values[-length(cumulative_values)] + cumulative_values[-1]) *
                      diff(c(0, cumulative_weights)))
    return(gini)
  } else {
    return(NA_real_)
  }
}

unweighted_gini <- function(values) {
  valid <- !is.na(values) & values > 0
  if (sum(valid) > 1) {
    return(ineq::Gini(values[valid]))
  } else {
    return(NA_real_)
  }
}

#########################################
# Load CPI and PPP data
#########################################

message("Loading CPI and PPP data...")

# Load CPI data with error handling
tryCatch({
  message("  Downloading CPI data from World Bank WDI...")
  CPI_raw <- WDI(country = "all", indicator = "FP.CPI.TOTL", start = 2010, end = 2024)
  
  message("  Processing CPI data...")
  CPI_WDI <- CPI_raw %>% 
    rename(countryname = country, countrycode = iso3c, CPI_2010 = FP.CPI.TOTL) %>% 
    filter(!is.na(CPI_2010)) %>%
    group_by(countrycode) %>% 
    mutate(
      value_2017 = ifelse(any(year == 2017), CPI_2010[year == 2017], NA),
      value_2017 = ifelse(is.na(value_2017), CPI_2010[which.min(abs(year - 2017))], value_2017),
      CPI_2017 = round(CPI_2010 / value_2017, 10)
    ) %>% 
    ungroup() %>% 
    select(countrycode, year, CPI_2017) %>%
    arrange(countrycode, year)
  
  message("  CPI data loaded: ", nrow(CPI_WDI), " observations")
  
}, error = function(e) {
  stop("Error loading CPI data from WDI: ", e$message, 
       "\nPlease check your internet connection or try again later.")
})

# Load PPP data
tryCatch({
  message("  Loading PPP data...")
  ppp_data <- read_dta(ppp_path) %>%
    rename(PPP_2017 = value) %>%
    select(country_code, PPP_2017) %>%
    group_by(country_code) %>%
    summarise(PPP_2017 = mean(PPP_2017, na.rm = TRUE), .groups = 'drop') %>%
    rename(countrycode = country_code)
  
  message("  PPP data loaded: ", nrow(ppp_data), " countries")
  
}, error = function(e) {
  stop("Error loading PPP data: ", e$message)
})

# Create country name mapping
tryCatch({
  message("  Creating country name mapping...")
  country_mapping_raw <- WDI(country = "all", indicator = "SP.POP.TOTL", start = 2024, end = 2024)
  
  country_mapping <- country_mapping_raw %>%
    select(iso3c, country) %>%
    rename(countrycode = iso3c, countryname = country) %>%
    distinct() %>%
    filter(!is.na(countrycode))
  
  message("  Country mapping created: ", nrow(country_mapping), " countries")
  
}, error = function(e) {
  warning("Could not create country mapping: ", e$message)
  # Create minimal mapping with country codes
  country_mapping <- data.frame(
    countrycode = countries_to_process,
    countryname = countries_to_process,
    stringsAsFactors = FALSE
  )
})

message("Data loaded successfully.\n")

#########################################
# Process countries and calculate Gini
#########################################

country_gini_results <- data.frame()
start_time <- Sys.time()

for (i in seq_along(countries_to_process)) {
  country <- countries_to_process[i]
  message(sprintf("[%d/%d] Processing country: %s", i, length(countries_to_process), country))
  
  tryCatch({
    # List all .dta files for the current country
    country_files <- list.files(path = input_path, 
                                pattern = paste0("^", country, ".*\\.dta$"), 
                                full.names = TRUE)
    
    if (length(country_files) == 0) {
      message("  No files found for ", country)
      next
    }
    
    # Filter files for years >= 2014
    filtered_files <- character(0)
    for (file in country_files) {
      filename <- basename(file)
      year_match <- regexpr("_([0-9]{4})(_|\\.|$)", filename)
      if (year_match > 0) {
        file_year <- as.numeric(substr(filename, year_match + 1, year_match + 4))
        if (!is.na(file_year) && file_year >= 2014) {
          filtered_files <- c(filtered_files, file)
        }
      } else {
        filtered_files <- c(filtered_files, file)
      }
    }
    
    if (length(filtered_files) == 0) {
      message("  No files >= 2014 for ", country)
      next
    }
    
    message("  Found ", length(filtered_files), " files to process")
    
    # Process files
    processed_list <- list()
    for (file in filtered_files) {
      tryCatch({
        dat <- read_dta(file)
        dat <- as.data.table(dat)
        
        if (!all(c("wage_no_compen", "unitwage") %in% names(dat))) {
          next
        }
        
        vars_to_keep <- c("countrycode", "year", "wage_no_compen", "unitwage", "weight")
        available_vars <- intersect(vars_to_keep, names(dat))
        dat <- dat[, ..available_vars, with = FALSE]
        
        if (!"weight" %in% names(dat)) {
          dat[, weight := 1]
        }
        
        dat <- merge(dat, CPI_WDI, by = c("countrycode", "year"), all.x = TRUE)
        
        # Convert to monthly wage
        dat[, wage_no_compen_trans := fifelse(
          unitwage == 1, wage_no_compen * 21.67,
          fifelse(unitwage == 2, wage_no_compen * 4.33,
                  fifelse(unitwage == 3, wage_no_compen * 2.17,
                          fifelse(unitwage == 5, wage_no_compen,
                                  fifelse(unitwage == 6, wage_no_compen / 3,
                                          fifelse(unitwage == 7, wage_no_compen / 6,
                                                  fifelse(unitwage == 8, wage_no_compen / 12, NA_real_)))))))]
        
        dat[, wage_deflated := ifelse(!is.na(CPI_2017) & CPI_2017 != 0, 
                                     wage_no_compen_trans / CPI_2017, NA_real_)]
        
        dat <- merge(dat, ppp_data, by = "countrycode", all.x = TRUE)
        dat[, Monthly_wage_ppp17 := wage_deflated / PPP_2017]
        dat <- dat[year >= 2014]
        
        if (nrow(dat) > 0) {
          processed_list[[file]] <- dat
        }
        
      }, error = function(e) {
        message("  Error in file: ", basename(file), " - ", e$message)
      })
    }
    
    if (length(processed_list) == 0) {
      message("  No valid data for ", country)
      next
    }
    
    # Combine all files
    final_data <- rbindlist(processed_list, use.names = TRUE, fill = TRUE)
    final_data <- final_data %>% filter(!is.na(Monthly_wage_ppp17))
    
    if (nrow(final_data) == 0) {
      message("  No valid wage data for ", country)
      next
    }
    
    message("  Total observations: ", nrow(final_data))
    
    # Get country name
    country_name <- country_mapping$countryname[country_mapping$countrycode == country]
    if (length(country_name) == 0) country_name <- country
    
    # Calculate Gini for each year
    year_results <- data.frame()
    for (yr in unique(final_data$year)) {
      year_data <- final_data[year == yr]
      if (nrow(year_data) < 10) next
      
      gini_wgt <- weighted_gini(year_data$Monthly_wage_ppp17, year_data$weight)
      gini_unwgt <- unweighted_gini(year_data$Monthly_wage_ppp17)
      mean_wage <- weighted.mean(year_data$Monthly_wage_ppp17, year_data$weight, na.rm = TRUE)
      
      year_results <- rbind(year_results, data.frame(
        year = yr,
        gini_weighted = gini_wgt,
        gini_unweighted = gini_unwgt,
        mean_wage = mean_wage,
        observations = nrow(year_data)
      ))
    }
    
    if (nrow(year_results) == 0) {
      message("  No years with sufficient data for ", country)
      next
    }
    
    message("  Years available: ", paste(year_results$year, collapse = ", "))
    
    # Find years closest to 2016 and 2022
    available_years <- year_results$year
    year_2016 <- available_years[which.min(abs(available_years - 2016))]
    year_2022 <- available_years[which.min(abs(available_years - 2022))]
    
    data_2016 <- year_results[year_results$year == year_2016, ]
    data_2022 <- year_results[year_results$year == year_2022, ]
    
    # Combine into single row
    country_row <- data.frame(
      countrycode = country,
      countryname = country_name[1],
      year_circa_2016 = if(nrow(data_2016) > 0) data_2016$year else NA,
      gini_weighted_2016 = if(nrow(data_2016) > 0) data_2016$gini_weighted else NA,
      gini_unweighted_2016 = if(nrow(data_2016) > 0) data_2016$gini_unweighted else NA,
      mean_wage_2016 = if(nrow(data_2016) > 0) data_2016$mean_wage else NA,
      observations_2016 = if(nrow(data_2016) > 0) data_2016$observations else NA,
      year_circa_2022 = if(nrow(data_2022) > 0) data_2022$year else NA,
      gini_weighted_2022 = if(nrow(data_2022) > 0) data_2022$gini_weighted else NA,
      gini_unweighted_2022 = if(nrow(data_2022) > 0) data_2022$gini_unweighted else NA,
      mean_wage_2022 = if(nrow(data_2022) > 0) data_2022$mean_wage else NA,
      observations_2022 = if(nrow(data_2022) > 0) data_2022$observations else NA,
      stringsAsFactors = FALSE
    )
    
    country_gini_results <- rbind(country_gini_results, country_row)
    message("  Success - circa 2016: ", year_2016, " | circa 2022: ", year_2022, "\n")
    
  }, error = function(e) {
    message("  ERROR processing ", country, ": ", e$message, "\n")
  })
}

#########################################
# Save results
#########################################

if (nrow(country_gini_results) > 0) {
  output_file <- file.path(output_path, "02_replication_gini_may_2025.xlsx")
  write_xlsx(country_gini_results, path = output_file)
  
  elapsed_time <- difftime(Sys.time(), start_time, units = "mins")
  
  message("==========================================================")
  message("Processing complete in ", round(elapsed_time, 2), " minutes")
  message("Calculated Gini for ", nrow(country_gini_results), " countries")
  message("Results saved to: ", output_file)
  message("==========================================================")
} else {
  message("==========================================================")
  message("ERROR: No countries were successfully processed")
  message("Please check the input path and data files")
  message("==========================================================")
}
