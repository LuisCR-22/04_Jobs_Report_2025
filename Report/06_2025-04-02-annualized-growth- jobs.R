# Annualized Job Growth Analysis for LAC-8 Countries
# This script analyzes job growth trends in Latin American countries by calculating annualized growth rates between key periods (2016-2019, 2019-2024, 2016-2024), handling country-specific data limitations and methodological breaks, and producing a LAC-8 regional aggregate for cross-country comparison.
# Author: Luis Castellanos - Stats Team
# Last modified: 2025-06-05
#----------------------------------------------------------------------------------------------

rm(list = ls())  # Remove all objects from the environment
gc()   


# Load required libraries
library(readxl)
library(writexl)
library(dplyr)
library(tidyr)
library(lubridate)
library(stringr)

# Set the date for file naming
today_date <- format(Sys.Date(), "%Y-%m-%d")

# Define file paths
input_path <- "C:/Users/wb593225/WBG/LAC Team for Statistical Development - WB Group - Documentos/General/FY2025/Semiannual_Report/May/excel/Outputs/07 jobs nocohh/2025-04-01-jobs-series-countries.xlsx"
output_path <- paste0("C:/Users/wb593225/WBG/LAC Team for Statistical Development - WB Group - Documentos/General/FY2025/Semiannual_Report/May/excel/Outputs/07 jobs nocohh/", today_date, "-Annualized-job-growth.xlsx")

# Read the data with error handling
tryCatch({
  jobs_data <- read_excel(input_path)
}, error = function(e) {
  stop(paste("Error reading input file:", e$message))
})

# Function to calculate annualized growth rate
calculate_growth_rate <- function(start_value, end_value, num_years) {
  if (is.na(start_value) || is.na(end_value) || start_value <= 0 || num_years <= 0) {
    warning("Cannot calculate growth rate with missing or invalid values")
    return(NA)
  }
  
  return(((end_value / start_value) ^ (1 / num_years) - 1) * 100)
}

# Function to safely extract data for a specific period and country
safe_extract <- function(data, country_code, period) {
  filtered <- data %>% filter(Period == period)
  if (nrow(filtered) == 0) {
    warning(paste("Period", period, "not found in data"))
    return(NA)
  }
  
  if (!country_code %in% colnames(data)) {
    warning(paste("Country code", country_code, "not found in data"))
    return(NA)
  }
  
  value <- filtered %>% pull(!!sym(country_code))
  if (is.na(value)) {
    warning(paste("Missing value for", country_code, "in period", period))
  }
  
  return(value)
}

# Initialize results dataframes
growth_rates <- data.frame(
  country = character(),
  period_2016_2019 = numeric(),
  period_2019_2024 = numeric(),
  period_2016_2024 = numeric(),
  stringsAsFactors = FALSE
)

values_df <- data.frame(
  country = character(),
  initial_period = character(),
  initial_value = numeric(),
  mid_period = character(),
  mid_value = numeric(),
  final_period = character(),
  final_value = numeric(),
  stringsAsFactors = FALSE
)

notes_df <- data.frame(
  country = character(),
  notes = character(),
  stringsAsFactors = FALSE
)

# Define standard periods
std_first_period <- "2016q02"
std_mid_period <- "2019q02"
std_last_period <- "2024q02"

# Define LAC-8 countries for validation and later aggregation
lac8_countries <- c("arg", "bol", "bra", "chl", "cri", "mex", "per", "ury")

# Process countries with standard periods (Argentina, Bolivia, Brazil, Chile, Costa Rica, Mexico, Peru)
std_countries <- list(
  arg = "Argentina",
  bol = "Bolivia",
  bra = "Brazil", 
  chl = "Chile",
  cri = "Costa Rica",
  mex = "Mexico",
  per = "Peru"
)

for (code in names(std_countries)) {
  country_name <- std_countries[[code]]
  
  # Extract values for each period
  first_val <- safe_extract(jobs_data, code, std_first_period)
  mid_val <- safe_extract(jobs_data, code, std_mid_period)
  last_val <- safe_extract(jobs_data, code, std_last_period)
  
  # Calculate growth rates
  growth_2016_2019 <- calculate_growth_rate(first_val, mid_val, 3)
  growth_2019_2024 <- calculate_growth_rate(mid_val, last_val, 5)
  growth_2016_2024 <- calculate_growth_rate(first_val, last_val, 8)
  
  # Add to results
  growth_rates <- rbind(growth_rates, data.frame(
    country = country_name,
    period_2016_2019 = growth_2016_2019,
    period_2019_2024 = growth_2019_2024,
    period_2016_2024 = growth_2016_2024
  ))
  
  values_df <- rbind(values_df, data.frame(
    country = country_name,
    initial_period = std_first_period,
    initial_value = first_val,
    mid_period = std_mid_period,
    mid_value = mid_val,
    final_period = std_last_period,
    final_value = last_val
  ))
  
  notes_df <- rbind(notes_df, data.frame(
    country = country_name,
    notes = "Standard periods used: 2016Q2-2019Q2, 2019Q2-2024Q2, 2016Q2-2024Q2"
  ))
}

# Process Dominican Republic (uses 2017Q2 instead of 2016Q2)
dom_first_period <- "2017q02"
dom_first <- safe_extract(jobs_data, "dom", dom_first_period)
dom_mid <- safe_extract(jobs_data, "dom", std_mid_period)
dom_last <- safe_extract(jobs_data, "dom", std_last_period)

dom_growth_2016_2019 <- calculate_growth_rate(dom_first, dom_mid, 2) # 2 years between 2017Q2 and 2019Q2
dom_growth_2019_2024 <- calculate_growth_rate(dom_mid, dom_last, 5)
dom_growth_2016_2024 <- calculate_growth_rate(dom_first, dom_last, 7) # 7 years between 2017Q2 and 2024Q2

growth_rates <- rbind(growth_rates, data.frame(
  country = "Dominican Republic",
  period_2016_2019 = dom_growth_2016_2019,
  period_2019_2024 = dom_growth_2019_2024,
  period_2016_2024 = dom_growth_2016_2024
))

values_df <- rbind(values_df, data.frame(
  country = "Dominican Republic",
  initial_period = dom_first_period,
  initial_value = dom_first,
  mid_period = std_mid_period,
  mid_value = dom_mid,
  final_period = std_last_period,
  final_value = dom_last
))

notes_df <- rbind(notes_df, data.frame(
  country = "Dominican Republic",
  notes = "Uses 2017Q2 instead of 2016Q2 for initial period."
))

# Process Guatemala (uses 2016Q4 instead of 2016Q2 and 2022Q4 instead of 2024Q2)
gtm_first_period <- "2016q04"
gtm_last_period <- "2022q04"
gtm_first <- safe_extract(jobs_data, "gtm", gtm_first_period)
gtm_mid <- safe_extract(jobs_data, "gtm", std_mid_period)
gtm_last <- safe_extract(jobs_data, "gtm", gtm_last_period)

gtm_growth_2016_2019 <- calculate_growth_rate(gtm_first, gtm_mid, 2.5) # 2.5 years between 2016Q4 and 2019Q2
gtm_growth_2019_2024 <- calculate_growth_rate(gtm_mid, gtm_last, 3.5) # 3.5 years between 2019Q2 and 2022Q4
gtm_growth_2016_2024 <- calculate_growth_rate(gtm_first, gtm_last, 6) # 6 years between 2016Q4 and 2022Q4

growth_rates <- rbind(growth_rates, data.frame(
  country = "Guatemala",
  period_2016_2019 = gtm_growth_2016_2019,
  period_2019_2024 = gtm_growth_2019_2024,
  period_2016_2024 = gtm_growth_2016_2024
))

values_df <- rbind(values_df, data.frame(
  country = "Guatemala",
  initial_period = gtm_first_period,
  initial_value = gtm_first,
  mid_period = std_mid_period,
  mid_value = gtm_mid,
  final_period = gtm_last_period,
  final_value = gtm_last
))

notes_df <- rbind(notes_df, data.frame(
  country = "Guatemala",
  notes = "Uses 2016Q4 instead of 2016Q2 and 2022Q4 instead of 2024Q2."
))

# Process Colombia (methodological break in 2021)
col_first <- safe_extract(jobs_data, "col", std_first_period)
col_mid <- safe_extract(jobs_data, "col", std_mid_period)
col_break_period <- "2022q02"
col_break <- safe_extract(jobs_data, "col", col_break_period)
col_last <- safe_extract(jobs_data, "col", std_last_period)

col_growth_2016_2019 <- calculate_growth_rate(col_first, col_mid, 3)
col_growth_2022_2024 <- calculate_growth_rate(col_break, col_last, 2) # 2 years between 2022Q2 and 2024Q2
col_growth_2016_2024 <- (col_growth_2016_2019 + col_growth_2022_2024) / 2 # Simple average of the two periods

growth_rates <- rbind(growth_rates, data.frame(
  country = "Colombia",
  period_2016_2019 = col_growth_2016_2019,
  period_2019_2024 = col_growth_2022_2024, # Using 2022Q2-2024Q2 instead
  period_2016_2024 = col_growth_2016_2024
))

values_df <- rbind(values_df, data.frame(
  country = "Colombia",
  initial_period = std_first_period,
  initial_value = col_first,
  mid_period = col_break_period, # Using 2022Q2 as the middle period
  mid_value = col_break,
  final_period = std_last_period,
  final_value = col_last
))

notes_df <- rbind(notes_df, data.frame(
  country = "Colombia",
  notes = "Due to methodological break in 2021, uses 2022Q2-2024Q2 instead of 2019Q2-2024Q2. The 2016-2024 growth is the simple average of 2016Q2-2019Q2 and 2022Q2-2024Q2 growth rates."
))

# Process Paraguay (methodological break in 2021)
pry_first_period <- "2017q02"
pry_break_period <- "2022q02"
pry_first <- safe_extract(jobs_data, "pry", pry_first_period)
pry_mid <- safe_extract(jobs_data, "pry", std_mid_period)
pry_break <- safe_extract(jobs_data, "pry", pry_break_period)
pry_last <- safe_extract(jobs_data, "pry", std_last_period)

pry_growth_2016_2019 <- calculate_growth_rate(pry_first, pry_mid, 2) # 2 years between 2017Q2 and 2019Q2
pry_growth_2022_2024 <- calculate_growth_rate(pry_break, pry_last, 2) # 2 years between 2022Q2 and 2024Q2
pry_growth_2016_2024 <- (pry_growth_2016_2019 + pry_growth_2022_2024) / 2 # Simple average of the two periods

growth_rates <- rbind(growth_rates, data.frame(
  country = "Paraguay",
  period_2016_2019 = pry_growth_2016_2019,
  period_2019_2024 = pry_growth_2022_2024, # Using 2022Q2-2024Q2 instead
  period_2016_2024 = pry_growth_2016_2024
))

values_df <- rbind(values_df, data.frame(
  country = "Paraguay",
  initial_period = pry_first_period,
  initial_value = pry_first,
  mid_period = pry_break_period, # Using 2022Q2 as the middle period
  mid_value = pry_break,
  final_period = std_last_period,
  final_value = pry_last
))

notes_df <- rbind(notes_df, data.frame(
  country = "Paraguay",
  notes = "Uses 2017Q2-2019Q2 instead of 2016Q2-2019Q2 and 2022Q2-2024Q2 instead of 2019Q2-2024Q2 due to methodological break in 2021. The 2016-2024 growth is the simple average of growth rates from these two periods."
))

# Process Ecuador (only data from 2021Q2)
ecu_first_period <- "2021q02"
ecu_first <- safe_extract(jobs_data, "ecu", ecu_first_period)
ecu_last <- safe_extract(jobs_data, "ecu", std_last_period)

ecu_growth_2021_2024 <- calculate_growth_rate(ecu_first, ecu_last, 3) # 3 years between 2021Q2 and 2024Q2

growth_rates <- rbind(growth_rates, data.frame(
  country = "Ecuador",
  period_2016_2019 = NA, # No data available for this period
  period_2019_2024 = ecu_growth_2021_2024, # Using 2021Q2-2024Q2 instead
  period_2016_2024 = ecu_growth_2021_2024 # Using only available period
))

values_df <- rbind(values_df, data.frame(
  country = "Ecuador",
  initial_period = ecu_first_period,
  initial_value = ecu_first,
  mid_period = NA, # No middle period
  mid_value = NA,
  final_period = std_last_period,
  final_value = ecu_last
))

notes_df <- rbind(notes_df, data.frame(
  country = "Ecuador",
  notes = "Only presents annualized growth between 2021Q2 and 2024Q2 as no data is available before 2021."
))

# Process El Salvador (use 2023Q2 instead of 2024Q2)
slv_last_period <- "2023q02"
slv_first <- safe_extract(jobs_data, "slv", std_first_period)
slv_mid <- safe_extract(jobs_data, "slv", std_mid_period)
slv_last <- safe_extract(jobs_data, "slv", slv_last_period)

slv_growth_2016_2019 <- calculate_growth_rate(slv_first, slv_mid, 3)
slv_growth_2019_2023 <- calculate_growth_rate(slv_mid, slv_last, 4) # 4 years between 2019Q2 and 2023Q2
slv_growth_2016_2023 <- calculate_growth_rate(slv_first, slv_last, 7) # 7 years between 2016Q2 and 2023Q2

growth_rates <- rbind(growth_rates, data.frame(
  country = "El Salvador",
  period_2016_2019 = slv_growth_2016_2019,
  period_2019_2024 = slv_growth_2019_2023, # Using 2019Q2-2023Q2 instead
  period_2016_2024 = slv_growth_2016_2023 # Using 2016Q2-2023Q2 instead
))

values_df <- rbind(values_df, data.frame(
  country = "El Salvador",
  initial_period = std_first_period,
  initial_value = slv_first,
  mid_period = std_mid_period,
  mid_value = slv_mid,
  final_period = slv_last_period,
  final_value = slv_last
))

notes_df <- rbind(notes_df, data.frame(
  country = "El Salvador",
  notes = "Uses 2023Q2 instead of 2024Q2 for final period."
))

# Process Uruguay (methodological break)
ury_first <- safe_extract(jobs_data, "ury", std_first_period)
ury_mid <- safe_extract(jobs_data, "ury", std_mid_period)
ury_break_period <- "2022q02"
ury_break <- safe_extract(jobs_data, "ury", ury_break_period)
ury_last <- safe_extract(jobs_data, "ury", std_last_period)

ury_growth_2016_2019 <- calculate_growth_rate(ury_first, ury_mid, 3)
ury_growth_2022_2024 <- calculate_growth_rate(ury_break, ury_last, 2) # 2 years between 2022Q2 and 2024Q2
ury_growth_2016_2024 <- (ury_growth_2016_2019 + ury_growth_2022_2024) / 2 # Simple average of the two periods

growth_rates <- rbind(growth_rates, data.frame(
  country = "Uruguay",
  period_2016_2019 = ury_growth_2016_2019,
  period_2019_2024 = ury_growth_2022_2024, # Using 2022Q2-2024Q2 instead
  period_2016_2024 = ury_growth_2016_2024
))

values_df <- rbind(values_df, data.frame(
  country = "Uruguay",
  initial_period = std_first_period,
  initial_value = ury_first,
  mid_period = ury_break_period, # Using 2022Q2 as the middle period
  mid_value = ury_break,
  final_period = std_last_period,
  final_value = ury_last
))

notes_df <- rbind(notes_df, data.frame(
  country = "Uruguay",
  notes = "Uses 2022Q2-2024Q2 instead of 2019Q2-2024Q2 due to methodological break. The 2016-2024 growth is the simple average of 2016Q2-2019Q2 and 2022Q2-2024Q2 growth rates."
))

# Calculate LAC-8 aggregate (Argentina, Bolivia, Brazil, Chile, Costa Rica, Mexico, Peru, Uruguay)
# Function to calculate LAC-8 values for each period
calculate_lac8 <- function(data, period) {
  filtered_data <- data %>% filter(Period == period)
  if (nrow(filtered_data) == 0) {
    warning(paste("Period", period, "not found in data"))
    return(NA)
  }
  
  sum_workers <- 0
  all_missing <- TRUE
  
  for (country in lac8_countries) {
    if (!country %in% colnames(data)) {
      warning(paste("Country code", country, "not found in data"))
      next
    }
    
    workers <- filtered_data %>% pull(!!sym(country))
    if (!is.na(workers)) {
      sum_workers <- sum_workers + workers
      all_missing <- FALSE
    }
  }
  
  if (all_missing) {
    warning(paste("All LAC-8 countries have missing data for period", period))
    return(NA)
  }
  
  return(sum_workers)
}

lac8_first <- calculate_lac8(jobs_data, std_first_period)
lac8_mid <- calculate_lac8(jobs_data, std_mid_period)
lac8_last <- calculate_lac8(jobs_data, std_last_period)

lac8_growth_2016_2019 <- calculate_growth_rate(lac8_first, lac8_mid, 3)
lac8_growth_2019_2024 <- calculate_growth_rate(lac8_mid, lac8_last, 5)
lac8_growth_2016_2024 <- calculate_growth_rate(lac8_first, lac8_last, 8)

growth_rates <- rbind(growth_rates, data.frame(
  country = "LAC-8",
  period_2016_2019 = lac8_growth_2016_2019,
  period_2019_2024 = lac8_growth_2019_2024,
  period_2016_2024 = lac8_growth_2016_2024
))

values_df <- rbind(values_df, data.frame(
  country = "LAC-8",
  initial_period = std_first_period,
  initial_value = lac8_first,
  mid_period = std_mid_period,
  mid_value = lac8_mid,
  final_period = std_last_period,
  final_value = lac8_last
))

notes_df <- rbind(notes_df, data.frame(
  country = "LAC-8",
  notes = "Aggregate of Argentina, Bolivia, Brazil, Chile, Costa Rica, Mexico, Peru, and Uruguay. Standard periods used."
))

# Rename columns for better readability in Excel
growth_rates <- growth_rates %>% 
  rename(
    "Country" = country,
    "2016-2019 (%)" = period_2016_2019,
    "2019-2024 (%)" = period_2019_2024,
    "2016-2024 (%)" = period_2016_2024
  )

values_df <- values_df %>%
  rename(
    "Country" = country,
    "Initial Period" = initial_period,
    "Initial Value" = initial_value,
    "Middle Period" = mid_period,
    "Middle Value" = mid_value,
    "Final Period" = final_period,
    "Final Value" = final_value
  )

notes_df <- notes_df %>%
  rename(
    "Country" = country,
    "Notes" = notes
  )

# Create methodological notes sheet
method_notes <- data.frame(
  "Section" = c("Overview", "Standard Periods", "Exceptions: Dominican Republic", "Exceptions: Guatemala", 
                "Exceptions: Colombia", "Exceptions: Paraguay", "Exceptions: Ecuador", "Exceptions: El Salvador",
                "Exceptions: Uruguay", "LAC-8 Aggregate", "Calculation Method"),
  "Description" = c(
    "This file presents annualized job growth rates for Latin American and Caribbean countries across different time periods.",
    "Standard periods used for most countries are: 2016Q2-2019Q2, 2019Q2-2024Q2, and 2016Q2-2024Q2.",
    "Dominican Republic presents the annualized growth using 2017Q2 instead of 2016Q2.",
    "Guatemala presents the annualized growth using 2016Q4 instead of 2016Q2 and 2022Q4 instead of 2024Q2.",
    "Colombia presents the annualized growth for 2022Q2-2024Q2 instead of 2019Q2-2024Q2 due to a methodological break in 2021. Its 2016-2024 growth is calculated as a simple average of 2016Q2-2019Q2 and 2022Q2-2024Q2 growth rates.",
    "Paraguay presents the annualized growth for 2017Q2-2019Q2 instead of 2016Q2-2019Q2 and 2022Q2-2024Q2 instead of 2019Q2-2024Q2 due to a methodological break in 2021. Its 2016-2024 growth is calculated as a simple average of growth rates from these two periods.",
    "Ecuador presents only the annualized change between 2021Q2 and 2024Q2 as no data is available before 2021.",
    "El Salvador uses 2023Q2 instead of 2024Q2 for final period.",
    "Uruguay uses 2022Q2-2024Q2 instead of 2019Q2-2024Q2 due to methodological break. Its 2016-2024 growth is calculated as a simple average of 2016Q2-2019Q2 and 2022Q2-2024Q2 growth rates.",
    "The LAC-8 aggregate includes workers in Argentina, Bolivia, Brazil, Chile, Costa Rica, Mexico, Peru, and Uruguay. Standard periods are used for this calculation.",
    "Annualized growth rates are calculated using the formula: ((final_value / initial_value)^(1/years) - 1) * 100"
  )
)

# Create list of sheets for Excel workbook
sheets <- list(
  "Growth Rates" = growth_rates,
  "Values" = values_df,
  "Country Notes" = notes_df,
  "Methodological Notes" = method_notes
)

# Write Excel file with error handling
tryCatch({
  write_xlsx(sheets, output_path)
  cat("Processing complete. Excel file saved to:", output_path, "\n")
}, error = function(e) {
  stop(paste("Error writing output file:", e$message))
})

