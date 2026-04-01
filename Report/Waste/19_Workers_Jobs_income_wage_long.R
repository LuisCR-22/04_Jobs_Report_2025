# Labor Market Analysis - Workers, Population, and Income
# -----------------------------------------------------------------------------
# Last modification: 2025-04-25

# Clean environment and load packages
rm(list = ls())
cat("\014")  # Clear console

# Start timer for performance tracking
start_time <- Sys.time()

# Install (if needed) and load required packages
required_pkgs <- c("haven", "dplyr", "tidyr", "writexl", "stringr", "lubridate", "readxl")
for(pkg in required_pkgs) {
  if (!require(pkg, character.only = TRUE)) {
    install.packages(pkg, dependencies = TRUE)
    library(pkg, character.only = TRUE)
  }
}

# Define data directory path
data_dir <- "C:/Users/wb593225/OneDrive - WBG/Desktop/Luis - Private/Lablac/Semiannual report/Lablac data/No cohh"

# Define output directory path
output_dir <- "C:/Users/wb593225/WBG/LAC Team for Statistical Development - WB Group - Documents/General/FY2025/Semiannual_Report/May/excel/Outputs/13 Dashboard data"

# Ensure output directory exists
dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

# Define current date for output file naming
current_date <- format(Sys.Date(), "%Y-%m-%d")
output_file_detailed <- file.path(output_dir, paste0("07_", current_date, "-workers-wap-income-wage-long.xlsx"))
output_file_total <- file.path(output_dir, paste0("07_", current_date, "-workers-wap-income-2012-2024.xlsx"))

# Create mapping for country codes with comparable periods (for detailed output)
country_periods_detailed <- list(
  arg = "Argentina (2016-2024)",
  bol = "Bolivia (2016-2024)",
  bra = "Brazil (2016-2024)",
  chl = "Chile (2016-2024)",
  col = c("Colombia (2016-2019)", "Colombia (2021-2024)"),
  cri = "Costa Rica (2016-2024)",
  dom = "Dominican Republic (2017-2024)",
  ecu = "Ecuador (2021-2024)",
  mex = "Mexico (2016-2024)",
  pry = c("Paraguay (2017-2019)", "Paraguay (2022-2024)"),
  per = "Peru (2016-2024)",
  slv = "El Salvador (2016-2023)",
  ury = c("Uruguay (2016-2019)", "Uruguay (2022-2024)")
)

# Define valid periods for each country (for detailed output)
valid_periods_detailed <- list(
  arg = 2016:2024,
  bol = 2016:2024,
  bra = 2016:2024,
  chl = 2016:2024,
  col = c(2016:2019, 2021:2024),
  cri = 2016:2024,
  dom = 2017:2024,
  ecu = 2021:2024,
  mex = 2016:2024,
  pry = c(2017:2019, 2022:2024),
  per = 2016:2024,
  slv = 2016:2023,
  ury = c(2016:2019, 2022:2024)
)

# Define valid periods for each country (for total output - from 2012 onwards)
valid_periods_total <- list(
  arg = 2012:2024,
  bol = 2012:2024,
  bra = 2012:2024,
  chl = 2012:2024,
  col = 2012:2024,  # No gaps for second output
  cri = 2012:2024,
  dom = 2012:2024,
  ecu = 2021:2024,  # Only 2021 onwards as specified
  mex = 2012:2024,
  pry = 2012:2024,  # No gaps for second output
  per = 2012:2024,
  slv = 2012:2023,  # Until 2023 as specified
  ury = 2012:2024   # No gaps for second output
)

# Function to get country name with correct period based on year (for detailed output)
get_country_name_detailed <- function(country_code, year) {
  if(country_code %in% names(country_periods_detailed)) {
    periods <- country_periods_detailed[[country_code]]
    
    # For countries with multiple periods
    if(length(periods) > 1) {
      if(country_code == "col") {
        if(year <= 2019) return(periods[1])
        else return(periods[2])
      }
      else if(country_code %in% c("pry", "ury")) {
        if(year <= 2019) return(periods[1])
        else return(periods[2])
      }
    }
    
    # For countries with a single period
    return(periods[1])
  } else {
    return(country_code)  # Return the code if no mapping found
  }
}

# Function to get simple country name without period (for total output)
get_country_name_simple <- function(country_code) {
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
  
  if(country_code %in% names(country_names)) {
    return(country_names[[country_code]])
  } else {
    return(country_code)  # Return the code if no mapping found
  }
}

# Function to check if a country-year combination is valid for detailed output
is_valid_period_detailed <- function(country_code, year) {
  if(country_code %in% names(valid_periods_detailed)) {
    return(year %in% valid_periods_detailed[[country_code]])
  }
  return(FALSE)
}

# Function to check if a country-year combination is valid for total output
is_valid_period_total <- function(country_code, year) {
  if(country_code %in% names(valid_periods_total)) {
    return(year %in% valid_periods_total[[country_code]])
  }
  return(FALSE)
}

# Function to list files for processing
list_files_to_process <- function(test_mode = FALSE) {
  # List all .dta files in the directory
  all_files <- list.files(path = data_dir, pattern = "\\.dta$", full.names = TRUE)
  
  if(test_mode) {
    # For test mode, only process Argentina 2020-2021 files
    files_to_process <- grep("LABLAC_arg_(2020|2021)_q\\d{2}", basename(all_files), value = TRUE)
    files_to_process <- file.path(data_dir, files_to_process)
    cat("Test mode activated - Only processing Argentina 2020-2021 files\n")
    cat("Number of Argentina 2020-2021 files found:", length(files_to_process), "\n")
  } else {
    # For full processing, return all files and filter during processing
    files_to_process <- all_files
  }
  
  return(files_to_process)
}

# Helper function to calculate indicators for a given data subset
calculate_indicators <- function(data) {
  # 1. Total weighted workers
  workers_filter <- !is.na(data$ocupado) & data$ocupado == 1 & data$edad > 14
  total_workers <- sum(data$pondera[workers_filter], na.rm = TRUE)
  
  # 2. Total people over 15
  wap_filter <- data$edad > 14
  total_wap <- sum(data$pondera[wap_filter], na.rm = TRUE)
  
  # 3. Mean monthly laboral income
  income_filter <- !is.na(data$ocupado) & data$ocupado == 1 & 
    !is.na(data$cohi) & data$cohi == 1 & 
    !is.na(data$ila_ppp17)
  
  if(sum(data$pondera[income_filter], na.rm = TRUE) > 0) {
    mean_income <- weighted.mean(data$ila_ppp17[income_filter], 
                                 data$pondera[income_filter], na.rm = TRUE)
  } else {
    mean_income <- NA
  }
  
  # 4. Per capita monthly total income (considering ALL observations)
  per_capita_filter <- rep(TRUE, nrow(data))  # All observations
  
  if(sum(data$pondera[per_capita_filter], na.rm = TRUE) > 0) {
    # Create a temporary income variable where missing values are replaced with 0
    temp_income <- data$ila_ppp17
    temp_income[is.na(temp_income)] <- 0
    
    per_capita_income <- weighted.mean(temp_income[per_capita_filter], 
                                       data$pondera[per_capita_filter], na.rm = TRUE)
  } else {
    per_capita_income <- NA
  }
  
  # 5. Mean hourly wage
  wage_filter <- !is.na(data$ocupado) & data$ocupado == 1 & 
    !is.na(data$cohi) & data$cohi == 1 & 
    !is.na(data$asal) & data$asal == 1 & 
    !is.na(data$wage_ppp17)
  
  if(sum(data$pondera[wage_filter], na.rm = TRUE) > 0) {
    mean_wage <- weighted.mean(data$wage_ppp17[wage_filter], 
                               data$pondera[wage_filter], na.rm = TRUE)
  } else {
    mean_wage <- NA
  }
  
  return(list(
    total_workers = total_workers,
    total_wap = total_wap,
    mean_income = mean_income,
    per_capita_income = per_capita_income,
    mean_wage = mean_wage
  ))
}

# Function to calculate overall indicators and add to results
calculate_overall_indicators <- function(data, results, country_name, period) {
  indicators <- calculate_indicators(data)
  
  # Add total workers
  results[[length(results) + 1]] <- data.frame(
    Period = period,
    Indicator = "Total Workers - Total",
    Country = country_name,
    Value = indicators$total_workers
  )
  
  # Add total working age population
  results[[length(results) + 1]] <- data.frame(
    Period = period,
    Indicator = "Total Population 15+ - Total",
    Country = country_name,
    Value = indicators$total_wap
  )
  
  # Add mean monthly labor income
  results[[length(results) + 1]] <- data.frame(
    Period = period,
    Indicator = "Mean Monthly Labor Income - Total",
    Country = country_name,
    Value = indicators$mean_income
  )
  
  # Add per capita monthly total income
  results[[length(results) + 1]] <- data.frame(
    Period = period,
    Indicator = "Per Capita Monthly Total Income - Total",
    Country = country_name,
    Value = indicators$per_capita_income
  )
  
  # Add mean hourly wage
  results[[length(results) + 1]] <- data.frame(
    Period = period,
    Indicator = "Mean Hourly Wage - Total",
    Country = country_name,
    Value = indicators$mean_wage
  )
  
  return(results)
}

# Function to calculate indicators by sex and add to results
calculate_indicators_by_sex <- function(data, results, country_name, period) {
  # Male indicators
  male_data <- data %>% filter(hombre == 1)
  male_indicators <- calculate_indicators(male_data)
  
  # Add male-specific indicators
  results[[length(results) + 1]] <- data.frame(
    Period = period,
    Indicator = "Total Workers by sex - Male",
    Country = country_name,
    Value = male_indicators$total_workers
  )
  
  results[[length(results) + 1]] <- data.frame(
    Period = period,
    Indicator = "Total Population 15+ by sex - Male",
    Country = country_name,
    Value = male_indicators$total_wap
  )
  
  results[[length(results) + 1]] <- data.frame(
    Period = period,
    Indicator = "Mean Monthly Labor Income by sex - Male",
    Country = country_name,
    Value = male_indicators$mean_income
  )
  
  results[[length(results) + 1]] <- data.frame(
    Period = period,
    Indicator = "Per Capita Monthly Total Income by sex - Male",
    Country = country_name,
    Value = male_indicators$per_capita_income
  )
  
  results[[length(results) + 1]] <- data.frame(
    Period = period,
    Indicator = "Mean Hourly Wage by sex - Male",
    Country = country_name,
    Value = male_indicators$mean_wage
  )
  
  # Female indicators
  female_data <- data %>% filter(hombre == 0)
  female_indicators <- calculate_indicators(female_data)
  
  # Add female-specific indicators
  results[[length(results) + 1]] <- data.frame(
    Period = period,
    Indicator = "Total Workers by sex - Female",
    Country = country_name,
    Value = female_indicators$total_workers
  )
  
  results[[length(results) + 1]] <- data.frame(
    Period = period,
    Indicator = "Total Population 15+ by sex - Female",
    Country = country_name,
    Value = female_indicators$total_wap
  )
  
  results[[length(results) + 1]] <- data.frame(
    Period = period,
    Indicator = "Mean Monthly Labor Income by sex - Female",
    Country = country_name,
    Value = female_indicators$mean_income
  )
  
  results[[length(results) + 1]] <- data.frame(
    Period = period,
    Indicator = "Per Capita Monthly Total Income by sex - Female",
    Country = country_name,
    Value = female_indicators$per_capita_income
  )
  
  results[[length(results) + 1]] <- data.frame(
    Period = period,
    Indicator = "Mean Hourly Wage by sex - Female",
    Country = country_name,
    Value = female_indicators$mean_wage
  )
  
  return(results)
}

# Function to calculate indicators by skill level and add to results
calculate_indicators_by_skill <- function(data, results, country_name, period) {
  # Define skill level for each observation
  data$skill <- NA
  data$skill[data$nivel %in% c(0,1,2,3)] <- "Low"
  data$skill[data$nivel %in% c(4,5)] <- "Middle"
  data$skill[data$nivel == 6] <- "High"
  
  skill_levels <- c("Low", "Middle", "High")
  
  for(skill in skill_levels) {
    skill_data <- data %>% filter(skill == !!skill)
    skill_indicators <- calculate_indicators(skill_data)
    
    # Add skill-specific indicators
    results[[length(results) + 1]] <- data.frame(
      Period = period,
      Indicator = paste("Total Workers by skill -", skill),
      Country = country_name,
      Value = skill_indicators$total_workers
    )
    
    results[[length(results) + 1]] <- data.frame(
      Period = period,
      Indicator = paste("Total Population 15+ by skill -", skill),
      Country = country_name,
      Value = skill_indicators$total_wap
    )
    
    results[[length(results) + 1]] <- data.frame(
      Period = period,
      Indicator = paste("Mean Monthly Labor Income by skill -", skill),
      Country = country_name,
      Value = skill_indicators$mean_income
    )
    
    results[[length(results) + 1]] <- data.frame(
      Period = period,
      Indicator = paste("Per Capita Monthly Total Income by skill -", skill),
      Country = country_name,
      Value = skill_indicators$per_capita_income
    )
    
    results[[length(results) + 1]] <- data.frame(
      Period = period,
      Indicator = paste("Mean Hourly Wage by skill -", skill),
      Country = country_name,
      Value = skill_indicators$mean_wage
    )
  }
  
  return(results)
}

# Function to calculate indicators by education level and add to results
calculate_indicators_by_education <- function(data, results, country_name, period) {
  # Define education level for each observation
  data$education <- NA
  data$education[data$nivel %in% c(0,1)] <- "Never attended or incomplete primary"
  data$education[data$nivel %in% c(2,3)] <- "Complete primary"
  data$education[data$nivel %in% c(4,5)] <- "Complete secondary"
  data$education[data$nivel == 6] <- "Complete higher education"
  
  education_levels <- c("Never attended or incomplete primary", "Complete primary", 
                        "Complete secondary", "Complete higher education")
  
  for(edu in education_levels) {
    edu_data <- data %>% filter(education == !!edu)
    edu_indicators <- calculate_indicators(edu_data)
    
    # Add education-specific indicators
    results[[length(results) + 1]] <- data.frame(
      Period = period,
      Indicator = paste("Total Workers by education -", edu),
      Country = country_name,
      Value = edu_indicators$total_workers
    )
    
    results[[length(results) + 1]] <- data.frame(
      Period = period,
      Indicator = paste("Total Population 15+ by education -", edu),
      Country = country_name,
      Value = edu_indicators$total_wap
    )
    
    results[[length(results) + 1]] <- data.frame(
      Period = period,
      Indicator = paste("Mean Monthly Labor Income by education -", edu),
      Country = country_name,
      Value = edu_indicators$mean_income
    )
    
    results[[length(results) + 1]] <- data.frame(
      Period = period,
      Indicator = paste("Per Capita Monthly Total Income by education -", edu),
      Country = country_name,
      Value = edu_indicators$per_capita_income
    )
    
    results[[length(results) + 1]] <- data.frame(
      Period = period,
      Indicator = paste("Mean Hourly Wage by education -", edu),
      Country = country_name,
      Value = edu_indicators$mean_wage
    )
  }
  
  return(results)
}

# Function to calculate indicators by area and add to results
calculate_indicators_by_area <- function(data, results, country_name, period) {
  # Urban indicators
  urban_data <- data %>% filter(urbano == 1)
  urban_indicators <- calculate_indicators(urban_data)
  
  # Add urban-specific indicators
  results[[length(results) + 1]] <- data.frame(
    Period = period,
    Indicator = "Total Workers by area - Urban",
    Country = country_name,
    Value = urban_indicators$total_workers
  )
  
  results[[length(results) + 1]] <- data.frame(
    Period = period,
    Indicator = "Total Population 15+ by area - Urban",
    Country = country_name,
    Value = urban_indicators$total_wap
  )
  
  results[[length(results) + 1]] <- data.frame(
    Period = period,
    Indicator = "Mean Monthly Labor Income by area - Urban",
    Country = country_name,
    Value = urban_indicators$mean_income
  )
  
  results[[length(results) + 1]] <- data.frame(
    Period = period,
    Indicator = "Per Capita Monthly Total Income by area - Urban",
    Country = country_name,
    Value = urban_indicators$per_capita_income
  )
  
  results[[length(results) + 1]] <- data.frame(
    Period = period,
    Indicator = "Mean Hourly Wage by area - Urban",
    Country = country_name,
    Value = urban_indicators$mean_wage
  )
  
  # Rural indicators
  rural_data <- data %>% filter(urbano == 0)
  rural_indicators <- calculate_indicators(rural_data)
  
  # Add rural-specific indicators
  results[[length(results) + 1]] <- data.frame(
    Period = period,
    Indicator = "Total Workers by area - Rural",
    Country = country_name,
    Value = rural_indicators$total_workers
  )
  
  results[[length(results) + 1]] <- data.frame(
    Period = period,
    Indicator = "Total Population 15+ by area - Rural",
    Country = country_name,
    Value = rural_indicators$total_wap
  )
  
  results[[length(results) + 1]] <- data.frame(
    Period = period,
    Indicator = "Mean Monthly Labor Income by area - Rural",
    Country = country_name,
    Value = rural_indicators$mean_income
  )
  
  results[[length(results) + 1]] <- data.frame(
    Period = period,
    Indicator = "Per Capita Monthly Total Income by area - Rural",
    Country = country_name,
    Value = rural_indicators$per_capita_income
  )
  
  results[[length(results) + 1]] <- data.frame(
    Period = period,
    Indicator = "Mean Hourly Wage by area - Rural",
    Country = country_name,
    Value = rural_indicators$mean_wage
  )
  
  return(results)
}

# Function to calculate indicators by age group and add to results
calculate_indicators_by_age <- function(data, results, country_name, period) {
  age_groups <- list(
    "15-24" = data %>% filter(edad >= 15 & edad <= 24),
    "25-44" = data %>% filter(edad >= 25 & edad <= 44),
    "45-54" = data %>% filter(edad >= 45 & edad <= 54),
    "55-64" = data %>% filter(edad >= 55 & edad <= 64),
    "65+" = data %>% filter(edad >= 65)
  )
  
  for(age_name in names(age_groups)) {
    age_data <- age_groups[[age_name]]
    age_indicators <- calculate_indicators(age_data)
    
    # Add age-specific indicators
    results[[length(results) + 1]] <- data.frame(
      Period = period,
      Indicator = paste("Total Workers by age -", age_name),
      Country = country_name,
      Value = age_indicators$total_workers
    )
    
    results[[length(results) + 1]] <- data.frame(
      Period = period,
      Indicator = paste("Total Population 15+ by age -", age_name),
      Country = country_name,
      Value = age_indicators$total_wap
    )
    
    results[[length(results) + 1]] <- data.frame(
      Period = period,
      Indicator = paste("Mean Monthly Labor Income by age -", age_name),
      Country = country_name,
      Value = age_indicators$mean_income
    )
    
    results[[length(results) + 1]] <- data.frame(
      Period = period,
      Indicator = paste("Per Capita Monthly Total Income by age -", age_name),
      Country = country_name,
      Value = age_indicators$per_capita_income
    )
    
    results[[length(results) + 1]] <- data.frame(
      Period = period,
      Indicator = paste("Mean Hourly Wage by age -", age_name),
      Country = country_name,
      Value = age_indicators$mean_wage
    )
  }
  
  return(results)
}

# Combined function to process a single file for both outputs
process_file <- function(file_path) {
  # Extract file information from the filename
  filename <- basename(file_path)
  parts <- str_match(filename, "LABLAC_([a-z]{3})_(\\d{4})_q(\\d{2})_.*\\.dta$")
  
  if (is.na(parts[1])) {
    cat("⚠️ Warning: Filename doesn't match the expected pattern:", filename, "\n")
    return(list(detailed = NULL, total = NULL))
  }
  
  country_code <- parts[2]
  year <- as.numeric(parts[3])
  quarter <- as.numeric(parts[4])
  
  # Check if this file is valid for either output
  valid_for_detailed <- is_valid_period_detailed(country_code, year)
  valid_for_total <- is_valid_period_total(country_code, year)
  
  if(!valid_for_detailed && !valid_for_total) {
    # Skip silently - this file is not needed for either output
    return(list(detailed = NULL, total = NULL))
  }
  
  # Format the period as YYYY-QX
  period <- paste0(year, "-Q", quarter)
  
  # Get country names for both outputs
  detailed_country_name <- NULL
  total_country_name <- NULL
  
  if(valid_for_detailed) {
    detailed_country_name <- get_country_name_detailed(country_code, year)
    cat("Processing:", detailed_country_name, "for period", period, "\n")
  }
  
  if(valid_for_total) {
    total_country_name <- get_country_name_simple(country_code)
  }
  
  # Read dataset (only once)
  dat <- tryCatch({
    read_dta(file_path)
  }, error = function(e) {
    cat("⚠️ Error reading file:", file_path, " - ", e$message, "\n")
    return(list(detailed = NULL, total = NULL))
  })
  
  if(is.null(dat)) {
    return(list(detailed = NULL, total = NULL))
  }
  
  # Check if required variables exist for detailed output
  detailed_required_vars <- c("pondera", "ocupado", "edad", "hombre", "cohi", "asal", "ila_ppp17", "wage_ppp17", "nivel", "urbano")
  detailed_missing_vars <- detailed_required_vars[!detailed_required_vars %in% names(dat)]
  has_detailed_vars <- length(detailed_missing_vars) == 0
  
  # Check if required variables exist for total output
  total_required_vars <- c("pondera", "ocupado", "edad", "cohi", "asal", "ila_ppp17")
  total_missing_vars <- total_required_vars[!total_required_vars %in% names(dat)]
  has_total_vars <- length(total_missing_vars) == 0
  
  # For Peru, use only urban observations
  if(country_code == "per") {
    if("urbano" %in% names(dat)) {
      dat <- dat %>% filter(urbano == 1)
    } else {
      cat("⚠️ Warning: 'urbano' variable missing for Peru in", file_path, "\n")
      return(list(detailed = NULL, total = NULL))
    }
  }
  
  # Filter for working age population (15+)
  dat <- dat %>% filter(edad > 14)
  
  # Initialize results
  detailed_results <- NULL
  total_results <- NULL
  
  # Process for detailed output if valid and has required variables
  if(valid_for_detailed && has_detailed_vars) {
    # Initialize results list
    detailed_results_list <- list()
    
    # Calculate overall indicators
    detailed_results_list <- calculate_overall_indicators(dat, detailed_results_list, detailed_country_name, period)
    
    # Calculate indicators by sex
    detailed_results_list <- calculate_indicators_by_sex(dat, detailed_results_list, detailed_country_name, period)
    
    # Calculate indicators by skill level (using nivel)
    detailed_results_list <- calculate_indicators_by_skill(dat, detailed_results_list, detailed_country_name, period)
    
    # Calculate indicators by education level (using nivel)
    detailed_results_list <- calculate_indicators_by_education(dat, detailed_results_list, detailed_country_name, period)
    
    # Calculate indicators by area (using urbano)
    if("urbano" %in% names(dat)) {
      detailed_results_list <- calculate_indicators_by_area(dat, detailed_results_list, detailed_country_name, period)
    }
    
    # Calculate indicators by age group
    detailed_results_list <- calculate_indicators_by_age(dat, detailed_results_list, detailed_country_name, period)
    
    # Convert results list to data frame
    if(length(detailed_results_list) > 0) {
      detailed_results <- do.call(rbind, detailed_results_list)
      detailed_results <- as.data.frame(detailed_results)
    }
  }
  
  # Process for total output if valid and has required variables
  if(valid_for_total && has_total_vars) {
    # Calculate indicators for total output
    indicators <- calculate_indicators(dat)
    
    # Create results dataframe for total output
    total_results <- data.frame(
      Period = period,
      Indicator = c("Total Workers", "Total Population 15+", 
                    "Mean Monthly Labor Income", "Per Capita Monthly Total Income"),
      Country = total_country_name,
      Value = c(indicators$total_workers, indicators$total_wap, 
                indicators$mean_income, indicators$per_capita_income)
    )
  } else if(valid_for_total && !has_total_vars) {
    # Still return a row with NAs for this period for the total output
    total_results <- data.frame(
      Period = period,
      Indicator = c("Total Workers", "Total Population 15+", 
                    "Mean Monthly Labor Income", "Per Capita Monthly Total Income"),
      Country = total_country_name,
      Value = NA
    )
  }
  
  return(list(detailed = detailed_results, total = total_results))
}

# Generate all possible periods from 2016-Q1 to 2024-Q4 for detailed output
generate_all_periods_detailed <- function() {
  periods <- character()
  for(year in 2016:2024) {
    for(quarter in 1:4) {
      periods <- c(periods, paste0(year, "-Q", quarter))
    }
  }
  return(periods)
}

# Generate all possible periods from 2012-Q1 to 2024-Q4 for total output
generate_all_periods_total <- function() {
  periods <- character()
  for(year in 2012:2024) {
    for(quarter in 1:4) {
      periods <- c(periods, paste0(year, "-Q", quarter))
    }
  }
  return(periods)
}

# Add missing periods to detailed output
add_missing_periods_detailed <- function(data) {
  all_periods <- generate_all_periods_detailed()
  
  # Get unique Country-Indicator combinations
  combinations <- data %>% 
    select(Country, Indicator) %>% 
    distinct()
  
  # Initialize empty dataframe for the complete dataset
  complete_data <- data.frame()
  
  # Process each Country-Indicator combination
  cat("Adding missing periods for detailed output...\n")
  total_combinations <- nrow(combinations)
  progress_step <- max(1, round(total_combinations / 20))  # Show progress every ~5% of combinations
  
  for(i in 1:nrow(combinations)) {
    if(i %% progress_step == 0 || i == total_combinations) {
      cat(sprintf("Progress: %d/%d combinations (%.1f%%)\n", 
                  i, total_combinations, i/total_combinations*100))
    }
    
    country <- combinations$Country[i]
    indicator <- combinations$Indicator[i]
    
    # Extract current data for this combination
    current_data <- data %>% 
      filter(Country == country, Indicator == indicator)
    
    # Check if we need to add periods
    current_periods <- current_data$Period
    
    # Filter all periods based on the country's valid period
    # Extract country code from the country name
    country_code <- tolower(substr(strsplit(country, " ")[[1]][1], 1, 3))
    
    # For countries with split periods, handle accordingly
    valid_years <- numeric()
    if(country_code == "col") {
      if(grepl("2016-2019", country)) {
        valid_years <- 2016:2019
      } else if(grepl("2021-2024", country)) {
        valid_years <- 2021:2024
      }
    } else if(country_code == "pry") {
      if(grepl("2017-2019", country)) {
        valid_years <- 2017:2019
      } else if(grepl("2022-2024", country)) {
        valid_years <- 2022:2024
      }
    } else if(country_code == "ury") {
      if(grepl("2016-2019", country)) {
        valid_years <- 2016:2019
      } else if(grepl("2022-2024", country)) {
        valid_years <- 2022:2024
      }
    } else {
      # For other countries, extract years from the parenthesis
      years_match <- regexpr("\\((\\d{4})-(\\d{4})\\)", country)
      if(years_match > 0) {
        years_str <- regmatches(country, years_match)
        years <- as.numeric(strsplit(gsub("[\\(\\)]", "", years_str), "-")[[1]])
        valid_years <- years[1]:years[2]
      }
    }
    
    # Filter periods that match the valid years for this country
    valid_periods <- character()
    for(period in all_periods) {
      year <- as.numeric(substr(period, 1, 4))
      if(year %in% valid_years) {
        valid_periods <- c(valid_periods, period)
      }
    }
    
    missing_periods <- setdiff(valid_periods, current_periods)
    
    if(length(missing_periods) > 0) {
      # Create dataframe with missing periods
      missing_data <- data.frame(
        Period = missing_periods,
        Indicator = indicator,
        Country = country,
        Value = NA,
        Order = NA  # We'll calculate this later
      )
      
      # Combine with existing data
      combined_data <- bind_rows(current_data, missing_data)
      
      # Sort by period and recalculate the Order column
      combined_data <- combined_data %>%
        arrange(Period) %>%
        mutate(Order = row_number())
    } else {
      combined_data <- current_data
    }
    
    # Add to the complete dataset
    complete_data <- bind_rows(complete_data, combined_data)
  }
  
  return(complete_data)
}

# Add missing periods to total output
add_missing_periods_total <- function(data) {
  all_periods <- generate_all_periods_total()
  
  # Get unique Country-Indicator combinations
  combinations <- data %>% 
    select(Country, Indicator) %>% 
    distinct()
  
  # Initialize empty dataframe for the complete dataset
  complete_data <- data.frame()
  
  # Process each Country-Indicator combination
  cat("Adding missing periods for total output...\n")
  total_combinations <- nrow(combinations)
  progress_step <- max(1, round(total_combinations / 20))  # Show progress every ~5% of combinations
  
  for(i in 1:nrow(combinations)) {
    if(i %% progress_step == 0 || i == total_combinations) {
      cat(sprintf("Progress: %d/%d combinations (%.1f%%)\n", 
                  i, total_combinations, i/total_combinations*100))
    }
    
    country <- combinations$Country[i]
    indicator <- combinations$Indicator[i]
    
    # Extract current data for this combination
    current_data <- data %>% 
      filter(Country == country, Indicator == indicator)
    
    # Check if we need to add periods
    current_periods <- current_data$Period
    
    # Determine valid periods for this country
    country_code <- tolower(substr(country, 1, 3))
    valid_years <- NULL
    
    # Special case for Ecuador - only 2021 onwards
    if(country_code == "ecu") {
      valid_years <- 2021:2024
    }
    # Special case for El Salvador - only up to 2023
    else if(country_code == "slv" || country == "El Salvador") {
      valid_years <- 2012:2023
    }
    # Default case - 2012 to 2024
    else {
      valid_years <- 2012:2024
    }
    
    # Filter periods based on valid years
    valid_periods <- character()
    for(period in all_periods) {
      year <- as.numeric(substr(period, 1, 4))
      if(year %in% valid_years) {
        valid_periods <- c(valid_periods, period)
      }
    }
    
    missing_periods <- setdiff(valid_periods, current_periods)
    
    if(length(missing_periods) > 0) {
      # Create dataframe with missing periods
      missing_data <- data.frame(
        Period = missing_periods,
        Indicator = indicator,
        Country = country,
        Value = NA
      )
      
      # Combine with existing data
      combined_data <- bind_rows(current_data, missing_data)
      
      # Sort by period
      combined_data <- combined_data %>%
        arrange(Period)
    } else {
      combined_data <- current_data
    }
    
    # Add to the complete dataset
    complete_data <- bind_rows(complete_data, combined_data)
  }
  
  return(complete_data)
}

# Main processing function for both outputs
process_all_files <- function(test_mode = FALSE) {
  # Get files to process
  files_to_process <- list_files_to_process(test_mode)
  
  if(length(files_to_process) == 0) {
    cat("No files found for processing.\n")
    return(list(detailed = NULL, total = NULL))
  }
  
  cat("Found", length(files_to_process), "files to process.\n")
  
  # Initialize dataframes to store results
  all_detailed_results <- data.frame()
  all_total_results <- data.frame()
  
  # Track progress
  total_files <- length(files_to_process)
  cat("\nStarting to process", total_files, "files...\n")
  progress_step <- max(1, round(total_files / 10))  # Show progress every ~10% of files
  
  # Process each file
  for(i in seq_along(files_to_process)) {
    file <- files_to_process[i]
    
    # Display progress
    if(i %% progress_step == 0 || i == total_files) {
      cat(sprintf("\nProgress: %d/%d files (%.1f%%)\n", i, total_files, i/total_files*100))
    }
    
    # Process the file once for both outputs
    file_results <- process_file(file)
    
    # Add detailed results if any
    if(!is.null(file_results$detailed)) {
      all_detailed_results <- bind_rows(all_detailed_results, file_results$detailed)
    }
    
    # Add total results if any
    if(!is.null(file_results$total)) {
      all_total_results <- bind_rows(all_total_results, file_results$total)
    }
    
    # Clean up memory
    rm(file_results)
    gc()
  }
  
  # Add Order column to detailed output
  if(nrow(all_detailed_results) > 0) {
    all_detailed_results <- all_detailed_results %>%
      group_by(Country, Indicator) %>%
      arrange(Period) %>%
      mutate(Order = row_number()) %>%
      ungroup()
  }
  
  return(list(detailed = all_detailed_results, total = all_total_results))
}

# Function to generate footnotes for detailed output
generate_footnotes_detailed <- function(data) {
  # Get unique indicators and sort them alphabetically
  unique_indicators <- unique(data$Indicator)
  unique_indicators <- sort(unique_indicators)
  
  # Function to generate footnote based on indicator name
  generate_footnote <- function(indicator) {
    # Extract the base indicator type and demographic dimension
    parts <- strsplit(indicator, " - ")[[1]]
    base_indicator <- parts[1]
    
    if (length(parts) == 1) {
      # This is a total indicator with no demographic dimension
      demographic <- "Total"
    } else {
      demographic <- parts[2]
    }
    
    # Base definitions for each indicator type
    if (grepl("Total Workers", base_indicator)) {
      base_definition <- "Weighted sum of workers (ocupado=1) aged 15 and above"
    } else if (grepl("Total Population 15\\+", base_indicator)) {
      base_definition <- "Weighted sum of population aged 15 and above"
    } else if (grepl("Mean Monthly Labor Income", base_indicator)) {
      base_definition <- "Weighted mean of monthly labor income (ila_ppp17) for workers (ocupado=1, cohi=1) with non-missing values"
    } else if (grepl("Per Capita Monthly Total Income", base_indicator)) {
      base_definition <- "Weighted mean of monthly labor income (ila_ppp17) for all individuals in the sample, counting missing values as 0"
    } else if (grepl("Mean Hourly Wage", base_indicator)) {
      base_definition <- "Weighted mean of hourly wage (wage_ppp17) for wage workers (ocupado=1, cohi=1, asal=1) with non-missing values"
    } else {
      base_definition <- "Indicator calculated from household survey data"
    }
    
    # Add demographic specificity if not "Total"
    if (demographic == "Total") {
      return(base_definition)
    } else if (grepl("^by sex", base_indicator)) {
      return(paste0(base_definition, " - ", demographic, " only"))
    } else if (grepl("^by skill", base_indicator)) {
      skill_descriptions <- list(
        "Low" = "educational level 0-3 (up to incomplete secondary)",
        "Middle" = "educational level 4-5 (complete secondary or incomplete tertiary)",
        "High" = "educational level 6 (complete tertiary or higher)"
      )
      
      if (demographic %in% names(skill_descriptions)) {
        return(paste0(base_definition, " - Population with ", skill_descriptions[[demographic]]))
      } else {
        return(paste0(base_definition, " - ", demographic, " skill level"))
      }
    } else if (grepl("^by education", base_indicator)) {
      return(paste0(base_definition, " - Population with ", demographic, " education"))
    } else if (grepl("^by area", base_indicator)) {
      return(paste0(base_definition, " - ", demographic, " areas only"))
    } else if (grepl("^by age", base_indicator)) {
      return(paste0(base_definition, " - Population aged ", demographic))
    } else {
      return(paste0(base_definition, " - ", demographic))
    }
  }
  
  # Create the footnotes dataframe
  footnotes <- data.frame(
    Indicator = unique_indicators,
    Footnote = sapply(unique_indicators, generate_footnote)
  )
  
  return(footnotes)
}

# Function to generate footnotes for total output
generate_footnotes_total <- function() {
  # Create the footnotes dataframe for the total output
  footnotes <- data.frame(
    Indicator = c("Total Workers", 
                  "Total Population 15+", 
                  "Mean Monthly Labor Income", 
                  "Per Capita Monthly Total Income"),
    Footnote = c("Weighted sum of workers (ocupado=1) aged 15 and above",
                 "Weighted sum of population aged 15 and above",
                 "Weighted mean of monthly labor income (ila_ppp17) for workers (ocupado=1, cohi=1) with non-missing values",
                 "Weighted mean of monthly labor income (ila_ppp17) for all individuals in the sample, counting missing values as 0")
  )
  
  return(footnotes)
}

# Run the processing and save results
cat("\n=============================\n")
cat("STARTING LABOR MARKET ANALYSIS\n")
cat("=============================\n")

# Process files - Test mode switch
# Set to TRUE to process only Argentina 2020-2021 files (for testing)
# Set to FALSE for full processing of all countries
test_mode <- TRUE  

if(test_mode) {
  cat("\n⚠️ RUNNING IN TEST MODE - ONLY ARGENTINA 2020-2021 FILES WILL BE PROCESSED ⚠️\n")
  cat("To process all countries, change test_mode to FALSE\n\n")
} else {
  cat("\nProcessing ALL countries\n\n")
}

# Process all files once for both outputs
results <- process_all_files(test_mode)

# Handle detailed output (2016 onwards with demographic breakdowns)
if(!is.null(results$detailed) && nrow(results$detailed) > 0) {
  # Add missing periods
  detailed_results <- add_missing_periods_detailed(results$detailed)
  
  # Display summary information
  cat("\nDetailed output processing complete.\n")
  cat("Total observations:", nrow(detailed_results), "\n")
  cat("Countries:", paste(unique(detailed_results$Country), collapse=", "), "\n")
  cat("Indicators:", length(unique(detailed_results$Indicator)), "different indicators\n")
  cat("Time span:", paste(min(detailed_results$Period), "to", max(detailed_results$Period)), "\n")
  
  # Generate footnotes
  footnotes_detailed <- generate_footnotes_detailed(detailed_results)
  
  # Save to Excel
  sheets_detailed <- list("Data" = detailed_results, "Footnotes" = footnotes_detailed)
  write_xlsx(sheets_detailed, output_file_detailed)
  cat("\nDetailed results saved to:", output_file_detailed, "\n")
} else {
  cat("\nNo detailed results were generated.\n")
}

# Handle total output (2012 onwards with only totals)
if(!is.null(results$total) && nrow(results$total) > 0) {
  # Add missing periods
  total_results <- add_missing_periods_total(results$total)
  
  # Display summary information
  cat("\nTotal output processing complete.\n")
  cat("Total observations:", nrow(total_results), "\n")
  cat("Countries:", paste(unique(total_results$Country), collapse=", "), "\n")
  cat("Indicators:", length(unique(total_results$Indicator)), "different indicators\n")
  cat("Time span:", paste(min(total_results$Period), "to", max(total_results$Period)), "\n")
  
  # Generate footnotes for total output
  footnotes_total <- generate_footnotes_total()
  
  # Save to Excel
  sheets_total <- list("Data" = total_results, "Footnotes" = footnotes_total)
  write_xlsx(sheets_total, output_file_total)
  cat("\nTotal results saved to:", output_file_total, "\n")
} else {
  cat("\nNo total results were generated.\n")
}

# Calculate and display running time
end_time <- Sys.time()
elapsed <- end_time - start_time
cat("\nTotal run time:", format(elapsed), "\n")

cat("\n=============================\n")
cat("ANALYSIS COMPLETE\n")
cat("=============================\n")
