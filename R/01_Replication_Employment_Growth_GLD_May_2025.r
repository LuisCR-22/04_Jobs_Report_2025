######################################### Employment Growth Analysis - Selected Countries
# Author: Luis Castellanos - Stats Team (lcasterodr@worldbank.org)
# Project: Regional Jobs Update - LAC
# Stats Team - Poverty and Equity Global Practice
# Date created: 2025-03-15
# Last modification: 2025-12-16
# Purpose: Calculate employment growth circa 2016-2022 for three age groups:
#          - 15+ years
#          - 15-64 years  
#          - All ages (including child labor)
######################################### Clean workspace
rm(list = ls())
gc()
# 
# use of the nocohh option. For MEX, using the nocohh datasets is critical 

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

# Load required libraries
required_packages <- c("haven", "dplyr", "data.table", "openxlsx")
for (pkg in required_packages) {
  if (!require(pkg, character.only = TRUE)) {
    install.packages(pkg, dependencies = TRUE)
    library(pkg, character.only = TRUE)
  }
}

# Define paths
input_path <- #"C:/Users/.../Raw data/GLD harmonization"
output_path <- #"C:/Users/.../Output/01_GLD"
output_file <- file.path(output_path, "01_replication_job_creation_may_2025.xlsx")

# Create output directory
dir.create(output_path, recursive = TRUE, showWarnings = FALSE)

# Define countries to process
countries <- c("ARM", "BGD", "ETH", "GEO", "GMB", "IDN", "IND", "LKA", 
               "MNG", "PAK", "PHL", "RWA", "THA", "TUN", "TUR", "TZA", 
               "ZAF", "ZMB")

# Function to calculate annualized growth rate
calculate_growth <- function(value1, value2, year1, year2) {
  if (year1 >= year2 || is.na(value1) || is.na(value2) || value1 <= 0 || value2 <= 0) {
    return(NA_real_)
  }
  return(((value2 / value1)^(1/(year2 - year1)) - 1) * 100)
}

# Function to identify age variable
identify_age_var <- function(data) {
  age_vars <- c("age", "edad", "age_years", "ageyr")
  for (var in age_vars) {
    if (var %in% names(data)) return(var)
  }
  return(NULL)
}

# Initialize results storage
results_15plus <- data.frame()
results_15to64 <- data.frame()
results_all_ages <- data.frame()

# Start processing
start_time <- Sys.time()
message("Starting processing of ", length(countries), " countries...")

# Process each country
for (i in seq_along(countries)) {
  country <- countries[i]
  message(sprintf("[%d/%d] Processing %s...", i, length(countries), country))
  
  tryCatch({
    # Get all files for this country
    country_files <- list.files(
      path = input_path,
      pattern = paste0("^", country, ".*\\.dta$"),
      full.names = TRUE
    )
    
    if (length(country_files) == 0) {
      message("  No files found for ", country)
      next
    }
    
    # Process and combine all files for this country
    all_data <- list()
    
    for (file in country_files) {
      dat <- read_dta(file) %>% as.data.table()
      
      # Check required variables
      if (!all(c("year", "lstatus") %in% names(dat))) next
      
      # Identify age variable
      age_var <- identify_age_var(dat)
      if (is.null(age_var)) next
      
      # Keep only essential variables
      keep_vars <- c("countrycode", "year", "lstatus", "weight", age_var)
      dat <- dat[, intersect(keep_vars, names(dat)), with = FALSE]
      
      # Create weight if missing
      if (!"weight" %in% names(dat)) dat[, weight := 1]
      
      # Filter for years >= 2008 and employed only
      dat <- dat[year >= 2008 & lstatus == 1]
      
      if (nrow(dat) > 0) {
        # Standardize age variable name
        setnames(dat, age_var, "age")
        all_data[[file]] <- dat
      }
    }
    
    if (length(all_data) == 0) {
      message("  No valid data for ", country)
      next
    }
    
    # Combine all files
    combined <- rbindlist(all_data, use.names = TRUE, fill = TRUE)
    
    # Calculate employment for three age groups
    
    # 1. Employment 15+
    emp_15plus <- combined[age > 14] %>%
      group_by(countrycode, year) %>%
      summarise(employed = sum(weight, na.rm = TRUE), .groups = 'drop')
    
    # 2. Employment 15-64
    emp_15to64 <- combined[age >= 15 & age <= 64] %>%
      group_by(countrycode, year) %>%
      summarise(employed = sum(weight, na.rm = TRUE), .groups = 'drop')
    
    # 3. Employment all ages
    emp_all <- combined %>%
      group_by(countrycode, year) %>%
      summarise(employed = sum(weight, na.rm = TRUE), .groups = 'drop')
    
    # Function to calculate growth for a dataset
    calc_period_growth <- function(data, age_group) {
      if (nrow(data) < 2) return(NULL)
      
      years <- sort(unique(data$year))
      year_2016 <- years[which.min(abs(years - 2016))]
      year_2022 <- years[which.min(abs(years - 2022))]
      
      if (year_2016 >= year_2022) return(NULL)
      
      emp_2016 <- data[data$year == year_2016, "employed"][[1]]
      emp_2022 <- data[data$year == year_2022, "employed"][[1]]
      
      growth <- calculate_growth(emp_2016, emp_2022, year_2016, year_2022)
      
      data.frame(
        countrycode = country,
        age_group = age_group,
        start_year = year_2016,
        `Start Employment` = emp_2016,
        end_year = year_2022,
        `End Employment` = emp_2022,
        `Annualized growth` = growth,
        check.names = FALSE
      )
    }
    
    # Calculate growth for each age group
    result_15plus <- calc_period_growth(emp_15plus, "15+")
    result_15to64 <- calc_period_growth(emp_15to64, "15-64")
    result_all <- calc_period_growth(emp_all, "All ages")
    
    # Store results
    if (!is.null(result_15plus)) results_15plus <- rbind(results_15plus, result_15plus)
    if (!is.null(result_15to64)) results_15to64 <- rbind(results_15to64, result_15to64)
    if (!is.null(result_all)) results_all_ages <- rbind(results_all_ages, result_all)
    
    message("  ✓ Completed")
    
  }, error = function(e) {
    message("  ✗ Error: ", e$message)
  })
}

# Create Excel workbook
message("\nCreating Excel workbook...")

wb <- createWorkbook()

# Add sheets
addWorksheet(wb, "Employment 15+")
addWorksheet(wb, "Employment 15-64")
addWorksheet(wb, "Employment All Ages")

# Write data
writeData(wb, "Employment 15+", results_15plus)
writeData(wb, "Employment 15-64", results_15to64)
writeData(wb, "Employment All Ages", results_all_ages)

# Format sheets
header_style <- createStyle(textDecoration = "bold", fgFill = "#E0E0E0")
number_style <- createStyle(numFmt = "#,##0")
growth_style <- createStyle(numFmt = "0.00")

for (sheet in c("Employment 15+", "Employment 15-64", "Employment All Ages")) {
  # Header style
  addStyle(wb, sheet, header_style, rows = 1, cols = 1:7, gridExpand = TRUE)
  
  # Number formatting for employment columns
  if (sheet == "Employment 15+") data <- results_15plus
  if (sheet == "Employment 15-64") data <- results_15to64
  if (sheet == "Employment All Ages") data <- results_all_ages
  
  if (nrow(data) > 0) {
    # Format Start Employment column (column 4)
    addStyle(wb, sheet, number_style, 
             rows = 2:(nrow(data) + 1), cols = 4, gridExpand = TRUE)
    # Format End Employment column (column 6)
    addStyle(wb, sheet, number_style, 
             rows = 2:(nrow(data) + 1), cols = 6, gridExpand = TRUE)
    # Format Annualized growth column (column 7) as number, not percentage
    addStyle(wb, sheet, growth_style, 
             rows = 2:(nrow(data) + 1), cols = 7, gridExpand = TRUE)
  }
  
  setColWidths(wb, sheet, cols = 1:7, widths = "auto")
}

# Save workbook
saveWorkbook(wb, output_file, overwrite = TRUE)

# Final report
elapsed <- difftime(Sys.time(), start_time, units = "mins")
message("\n==========================================================")
message(sprintf("Analysis complete in %.1f minutes", as.numeric(elapsed)))
message(sprintf("Processed %d countries", length(countries)))
message(sprintf("Results saved to: %s", output_file))
message("==========================================================")