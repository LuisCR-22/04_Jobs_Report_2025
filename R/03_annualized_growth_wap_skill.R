# Annualized growth in working age population and working population by skill level
# This script analyzes skill level trends in Latin American countries by processing labor market data to calculate working-age population and employed workers by skill level (low, middle, high), computing annualized growth rates between 2016-2024, and generating regional aggregates for LAC-8 and LAC-7 country groups.
# Author: Luis Castellanos - Stats Team (lcasterodr@worldbank.org)
# Last modification: 2025-06-05

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

required_pkgs <- c("haven", "dplyr", "writexl", "data.table", "lubridate", "tidyr")
for(pkg in required_pkgs) {
  if (!require(pkg, character.only = TRUE)) {
    install.packages(pkg, dependencies = TRUE)
    library(pkg, character.only = TRUE)
  }
}

# Define input and output paths
input_path <- "<<DATALIBWEB_PATH_OR_GENERIC_URL>>"
output_path <- "<<DATALIBWEB_PATH_OR_GENERIC_URL>>"
current_date <- format(Sys.Date(), "%Y-%m-%d")
output_file <- file.path(output_path, paste0(current_date, "-skill-level-analysis-V2.xlsx"))

# Country configurations - using the same periods as in the original script
country_configs <- list(
  arg = list(start_year = 2016, start_quarter = 2, end_year = 2024, end_quarter = 2),
  bol = list(start_year = 2016, start_quarter = 2, end_year = 2024, end_quarter = 2),
  bra = list(start_year = 2016, start_quarter = 2, end_year = 2024, end_quarter = 2),
  chl = list(start_year = 2016, start_quarter = 2, end_year = 2024, end_quarter = 2),
  col = list(start_year = 2021, start_quarter = 4, end_year = 2023, end_quarter = 4),
  cri = list(start_year = 2016, start_quarter = 2, end_year = 2024, end_quarter = 4),
  dom = list(start_year = 2017, start_quarter = 2, end_year = 2024, end_quarter = 2),
  ecu = list(start_year = 2021, start_quarter = 2, end_year = 2024, end_quarter = 2),
  mex = list(start_year = 2016, start_quarter = 2, end_year = 2024, end_quarter = 2),
  pry = list(start_year = 2022, start_quarter = 2, end_year = 2024, end_quarter = 2),
  per = list(start_year = 2016, start_quarter = 2, end_year = 2024, end_quarter = 2),
  slv = list(start_year = 2016, start_quarter = 2, end_year = 2023, end_quarter = 2),
  ury = list(start_year = 2022, start_quarter = 2, end_year = 2024, end_quarter = 2)
)

# LAC-8 and LAC-7 countries for aggregate calculation
lac8_countries <- c("arg", "bol", "bra", "chl", "cri", "mex", "per", "ury")
lac7_countries <- c("arg", "bol", "bra", "chl", "cri", "mex", "per") # Same as LAC-8 but excluding Uruguay

# Define skill level classification function (as specified in requirements)
skill_level <- function(nivel) {
  if(is.na(nivel)) return(NA_character_)
  if(nivel %in% c(0,1,2,3)) return("Low") 
  if(nivel %in% c(4,5)) return("Middle")
  if(nivel == 6) return("High")
  return(NA_character_)
}

# Function to find data file based on country, year, and quarter
find_data_file <- function(country, year, quarter) {
  # Format quarter with leading zero if needed
  quarter_formatted <- sprintf("q%02d", quarter)
  
  # Create pattern to match the file
  pattern <- paste0("LABLAC_", country, ".*", year, "_", quarter_formatted, ".*\\.dta$")
  
  # Find matching files
  matching_files <- list.files(path = input_path, pattern = pattern, full.names = TRUE)
  
  if (length(matching_files) == 0) {
    return(NULL)
  } else {
    return(matching_files[1])  # Return the first match
  }
}

# Function to load and process a single dataset efficiently
process_single_dataset <- function(country, year, quarter) {
  file_path <- find_data_file(country, year, quarter)
  
  if (is.null(file_path)) {
    cat(paste0("WARNING: No data file found for ", toupper(country), " ", year, "-Q", quarter, "\n"))
    return(NULL)
  }
  
  cat(paste0("Loading ", toupper(country), " ", year, "-Q", quarter, " data...\n"))
  
  tryCatch({
    # Load the data once
    data <- read_dta(file_path)
    cat("Successfully loaded file with", nrow(data), "rows and", ncol(data), "columns\n")
    
    # Check if nivel variable exists
    if (!("nivel" %in% names(data))) {
      cat("Variable nivel doesn't exist in the dataset for", toupper(country), "\n")
      return(NULL)
    }
    
    # Create period identifier
    period <- paste0(year, "Q", quarter)
    
    # Process working age population (edad > 14)
    cat("Processing working age population...\n")
    working_age_data <- data %>% 
      filter(edad > 14) %>%
      mutate(skill = sapply(nivel, skill_level)) %>%
      group_by(skill) %>%
      summarise(total = sum(pondera, na.rm = TRUE)) %>%
      mutate(period = period)
    
    cat("After filtering:", nrow(data[data$edad > 14,]), "working age individuals\n")
    
    # Process working population (edad > 14 and ocupado == 1)
    cat("Processing working population...\n")
    working_data <- data %>% 
      filter(edad > 14, ocupado == 1) %>%
      mutate(skill = sapply(nivel, skill_level)) %>%
      group_by(skill) %>%
      summarise(total = sum(pondera, na.rm = TRUE)) %>%
      mutate(period = period)
    
    cat("After filtering:", nrow(data[data$edad > 14 & data$ocupado == 1,]), "working individuals\n")
    
    # Clear the full dataset from memory to save RAM
    rm(data)
    gc()
    
    # Return the processed summaries
    return(list(
      working_age = working_age_data,
      working = working_data
    ))
    
  }, error = function(e) {
    cat("Error loading file:", e$message, "\n")
    return(NULL)
  })
}

# Function to process data and calculate weighted sums by skill level efficiently
process_data_by_skill <- function() {
  # Initialize results lists
  all_working_age_results <- list()
  all_working_results <- list()
  
  # Process each country
  for (country in names(country_configs)) {
    config <- country_configs[[country]]
    
    cat("\n=== PROCESSING", toupper(country), "===\n")
    
    # Process start period
    start_results <- process_single_dataset(country, config$start_year, config$start_quarter)
    
    # Process end period
    end_results <- process_single_dataset(country, config$end_year, config$end_quarter)
    
    # Skip if any dataset processing failed
    if (is.null(start_results) || is.null(end_results)) {
      cat("Skipping", toupper(country), "due to missing or error in data\n")
      all_working_age_results[[country]] <- NULL
      all_working_results[[country]] <- NULL
      next
    }
    
    # Store the combined results for this country
    all_working_age_results[[country]] <- rbind(
      start_results$working_age,
      end_results$working_age
    )
    
    all_working_results[[country]] <- rbind(
      start_results$working,
      end_results$working
    )
    
    # Clean up to save memory
    rm(start_results, end_results)
    gc()
  }
  
  return(list(
    working_age = all_working_age_results,
    working = all_working_results
  ))
}

# Function to prepare Sheet 1: Working age population by skill level
prepare_sheet1 <- function(working_age_results) {
  # Get unique periods from all countries
  all_periods <- c()
  for (country in names(working_age_results)) {
    if (!is.null(working_age_results[[country]])) {
      all_periods <- c(all_periods, unique(working_age_results[[country]]$period))
    }
  }
  all_periods <- unique(all_periods)
  all_periods <- all_periods[order(all_periods)]
  
  # Create empty dataframe with periods as rows
  sheet1 <- data.frame(
    period = all_periods,
    stringsAsFactors = FALSE
  )
  
  # Add columns for each country-skill combination
  for (country in names(working_age_results)) {
    if (!is.null(working_age_results[[country]])) {
      # Process each skill level
      for (skill_level in c("Low", "Middle", "High")) {
        # Extract data for this country and skill level
        country_skill_data <- working_age_results[[country]] %>%
          filter(skill == skill_level) %>%
          select(period, total)
        
        # Create column name
        col_name <- paste0(toupper(country), "_", skill_level)
        
        # Add to sheet1
        sheet1 <- left_join(
          sheet1,
          country_skill_data %>% rename(!!col_name := total),
          by = "period"
        )
      }
    }
  }
  
  return(sheet1)
}

# Function to prepare Sheet 2: Working population by skill level
prepare_sheet2 <- function(working_results) {
  # Get unique periods from all countries
  all_periods <- c()
  for (country in names(working_results)) {
    if (!is.null(working_results[[country]])) {
      all_periods <- c(all_periods, unique(working_results[[country]]$period))
    }
  }
  all_periods <- unique(all_periods)
  all_periods <- all_periods[order(all_periods)]
  
  # Create empty dataframe with periods as rows
  sheet2 <- data.frame(
    period = all_periods,
    stringsAsFactors = FALSE
  )
  
  # Add columns for each country-skill combination
  for (country in names(working_results)) {
    if (!is.null(working_results[[country]])) {
      # Process each skill level
      for (skill_level in c("Low", "Middle", "High")) {
        # Extract data for this country and skill level
        country_skill_data <- working_results[[country]] %>%
          filter(skill == skill_level) %>%
          select(period, total)
        
        # Create column name
        col_name <- paste0(toupper(country), "_", skill_level)
        
        # Add to sheet2
        sheet2 <- left_join(
          sheet2,
          country_skill_data %>% rename(!!col_name := total),
          by = "period"
        )
      }
    }
  }
  
  return(sheet2)
}

# Function to calculate country aggregates (LAC-8 and LAC-7)
calculate_aggregates <- function(sheet, country_groups) {
  # Copy the sheet
  sheet_with_aggregates <- sheet
  
  # Process each country group (LAC-8, LAC-7, etc.)
  for (group_name in names(country_groups)) {
    countries <- country_groups[[group_name]]
    
    # For each period and skill level, calculate aggregate
    for (skill_level in c("Low", "Middle", "High")) {
      # Create column name for aggregate
      agg_col <- paste0(group_name, "_", skill_level)
      
      # Initialize aggregate column
      sheet_with_aggregates[[agg_col]] <- 0
      
      # Sum values from countries in this group
      for (country in countries) {
        # Create column name for country
        country_col <- paste0(toupper(country), "_", skill_level)
        
        # Add to aggregate if country column exists
        if (country_col %in% colnames(sheet_with_aggregates)) {
          sheet_with_aggregates[[agg_col]] <- sheet_with_aggregates[[agg_col]] + 
            ifelse(is.na(sheet_with_aggregates[[country_col]]), 0, sheet_with_aggregates[[country_col]])
        }
      }
    }
  }
  
  return(sheet_with_aggregates)
}

# Function to calculate annualized growth
calculate_annualized_growth <- function(start_value, end_value, start_year, start_quarter, end_year, end_quarter) {
  # Calculate years between periods
  start_date <- as.Date(paste0(start_year, "-", (start_quarter * 3), "-01"))
  end_date <- as.Date(paste0(end_year, "-", (end_quarter * 3), "-01"))
  years_diff <- as.numeric(difftime(end_date, start_date, units = "days")) / 365.25
  
  # Calculate annualized growth rate
  if (start_value > 0 && !is.na(start_value) && !is.na(end_value)) {
    return(((end_value / start_value) ^ (1 / years_diff) - 1) * 100)
  } else {
    return(NA)
  }
}

# Function to prepare Sheet 4: Annualized growth in working age population
prepare_sheet4 <- function(sheet1, country_configs) {
  # Get all country-skill columns
  country_skill_cols <- colnames(sheet1)[!colnames(sheet1) %in% c("period")]
  
  # Initialize dataframe with skill levels as rows
  sheet4 <- data.frame(
    skill = c("Low", "Middle", "High"),
    stringsAsFactors = FALSE
  )
  
  # Extract country and skill from each column
  for (col in country_skill_cols) {
    # Split column name into country and skill
    parts <- strsplit(col, "_")[[1]]
    country <- parts[1]
    skill_level <- parts[2]
    
    # Find corresponding country config
    country_code <- tolower(country)
    
    # Skip if not a valid country
    if (!(country_code %in% names(country_configs)) && country != "LAC8" && country != "LAC7") {
      next
    }
    
    # If this is the first column for this country, initialize country column in sheet4
    if (!(country %in% colnames(sheet4))) {
      sheet4[[country]] <- NA
    }
    
    # Find start and end periods
    if (country == "LAC8" || country == "LAC7") {
      # For LAC-8 and LAC-7, use 2016Q2 to 2024Q2
      start_period <- "2016Q2"
      end_period <- "2024Q2"
      start_year <- 2016
      start_quarter <- 2
      end_year <- 2024
      end_quarter <- 2
    } else {
      # For countries, use their config
      config <- country_configs[[country_code]]
      start_period <- paste0(config$start_year, "Q", config$start_quarter)
      end_period <- paste0(config$end_year, "Q", config$end_quarter)
      start_year <- config$start_year
      start_quarter <- config$start_quarter
      end_year <- config$end_year
      end_quarter <- config$end_quarter
    }
    
    # Get start and end values
    start_row <- which(sheet1$period == start_period)
    end_row <- which(sheet1$period == end_period)
    
    if (length(start_row) > 0 && length(end_row) > 0) {
      start_value <- sheet1[start_row, col]
      end_value <- sheet1[end_row, col]
      
      # Calculate growth
      growth <- calculate_annualized_growth(
        start_value, end_value,
        start_year, start_quarter,
        end_year, end_quarter
      )
      
      # Add to sheet4
      sheet4[sheet4$skill == skill_level, country] <- growth
    }
  }
  
  return(sheet4)
}

# Function to prepare Sheet 5: Annualized growth in working population
prepare_sheet5 <- function(sheet2, country_configs) {
  # Get all country-skill columns
  country_skill_cols <- colnames(sheet2)[!colnames(sheet2) %in% c("period")]
  
  # Initialize dataframe with skill levels as rows
  sheet5 <- data.frame(
    skill = c("Low", "Middle", "High"),
    stringsAsFactors = FALSE
  )
  
  # Extract country and skill from each column
  for (col in country_skill_cols) {
    # Split column name into country and skill
    parts <- strsplit(col, "_")[[1]]
    country <- parts[1]
    skill_level <- parts[2]
    
    # Find corresponding country config
    country_code <- tolower(country)
    
    # Skip if not a valid country
    if (!(country_code %in% names(country_configs)) && country != "LAC8") {
      next
    }
    
    # If this is the first column for this country, initialize country column in sheet5
    if (!(country %in% colnames(sheet5))) {
      sheet5[[country]] <- NA
    }
    
    # Find start and end periods
    if (country == "LAC8") {
      # For LAC-8, use 2016Q2 to 2024Q2
      start_period <- "2016Q2"
      end_period <- "2024Q2"
      start_year <- 2016
      start_quarter <- 2
      end_year <- 2024
      end_quarter <- 2
    } else {
      # For countries, use their config
      config <- country_configs[[country_code]]
      start_period <- paste0(config$start_year, "Q", config$start_quarter)
      end_period <- paste0(config$end_year, "Q", config$end_quarter)
      start_year <- config$start_year
      start_quarter <- config$start_quarter
      end_year <- config$end_year
      end_quarter <- config$end_quarter
    }
    
    # Get start and end values
    start_row <- which(sheet2$period == start_period)
    end_row <- which(sheet2$period == end_period)
    
    if (length(start_row) > 0 && length(end_row) > 0) {
      start_value <- sheet2[start_row, col]
      end_value <- sheet2[end_row, col]
      
      # Calculate growth
      growth <- calculate_annualized_growth(
        start_value, end_value,
        start_year, start_quarter,
        end_year, end_quarter
      )
      
      # Add to sheet5
      sheet5[sheet5$skill == skill_level, country] <- growth
    }
  }
  
  return(sheet5)
}

# Add memory management at the beginning
options(scipen = 999)           # Avoid scientific notation
options(digits = 4)             # Reduce precision to save memory
memory.limit(size = 8000)       # Set memory limit if running on Windows
gc(reset = TRUE)                # Clean garbage collection

# Main execution block
cat("\n=== STARTING PROCESSING ===\n")
start_time <- Sys.time()
cat("Start time:", format(start_time), "\n")

# Process the data efficiently
results <- process_data_by_skill()

# Free memory after data processing
rm(list = setdiff(ls(), c("results", "country_configs", "lac8_countries", "lac7_countries",
                          "start_time", "output_file", "skill_level")))
#gc()

# Prepare sheets with progress tracking
cat("\n=== PREPARING EXCEL SHEETS ===\n")

# Sheet 1: Working age population by skill level
cat("Preparing Sheet 1: Working Age by Skill...\n")
sheet1 <- prepare_sheet1(results$working_age)

# Sheet 2: Working population by skill level
cat("Preparing Sheet 2: Working by Skill...\n")
sheet2 <- prepare_sheet2(results$working)

# Add LAC-8 and LAC-7 aggregates
cat("Calculating LAC-8 and LAC-7 aggregates...\n")
country_groups <- list(
  "LAC8" = lac8_countries,
  "LAC7" = lac7_countries
)
sheet1 <- calculate_aggregates(sheet1, country_groups)
sheet2 <- calculate_aggregates(sheet2, country_groups)

# Free memory after creating main sheets
rm(list = setdiff(ls(), c("sheet1", "sheet2", "country_configs", "lac8_countries", "lac7_countries", 
                          "start_time", "output_file", "prepare_sheet4", "prepare_sheet5")))
gc()

# Sheet 4: Annualized growth in working age population
cat("Preparing Sheet 4: Working Age Growth...\n")
sheet4 <- prepare_sheet4(sheet1, country_configs)

# Sheet 5: Annualized growth in working population
cat("Preparing Sheet 5: Working Growth...\n")
sheet5 <- prepare_sheet5(sheet2, country_configs)

# Create description sheet
cat("Creating Description sheet...\n")
description <- data.frame(
  Sheet = c(
    "Description",
    "Working Age by Skill",
    "Working by Skill",
    "Working Age Growth",
    "Working Growth"
  ),
  Description = c(
    "This sheet provides a description of all sheets in this file",
    "Working age population (aged 15+) by skill level, country, and period. Includes LAC-8 aggregate (ARG, BOL, BRA, CHL, CRI, MEX, PER, URY) and LAC-7 aggregate (ARG, BOL, BRA, CHL, CRI, MEX, PER)",
    "Working population (aged 15+ and ocupado=1) by skill level, country, and period. Includes LAC-8 aggregate (ARG, BOL, BRA, CHL, CRI, MEX, PER, URY) and LAC-7 aggregate (ARG, BOL, BRA, CHL, CRI, MEX, PER)",
    "Annualized growth in working age population by skill level and country. Includes LAC-8 and LAC-7 aggregates",
    "Annualized growth in working population by skill level and country. Includes LAC-8 and LAC-7 aggregates"
  )
)

# Combine all sheets with memory management
cat("Combining sheets...\n")
sheets <- list(
  "Description" = description,
  "Working Age by Skill" = sheet1,
  "Working by Skill" = sheet2,
  "Working Age Growth" = sheet4,
  "Working Growth" = sheet5
)

# Remove any NULL sheets
sheets <- sheets[!sapply(sheets, is.null)]

# Write to Excel with error handling
cat("\n=== WRITING TO EXCEL ===\n")
cat("Output file:", output_file, "\n")

tryCatch({
  writexl::write_xlsx(sheets, path = output_file)
  end_time <- Sys.time()
  duration <- difftime(end_time, start_time, units = "mins")
  cat("\n✅ Results saved to:", output_file, "\n")
  cat("Processing completed in", round(duration, 2), "minutes\n")
}, error = function(e) {
  cat("\n❌ Error saving results:", e$message, "\n")
})

# Final memory cleanup
rm(list = ls())
gc()
