# Percentalises of labor income distribution in LAC-7 countries
# This script creates a LAC-7 regional aggregate for labor income distribution by calculating weighted P20, median, and P80 income percentiles across seven Latin American countries (Argentina, Bolivia, Brazil, Costa Rica, Mexico, Peru, Uruguay), using worker counts as weights and standardizing outputs for cross-country comparison.
# Author: Luis Castellanos - Stats Team (lcasterodr@worldbank.org)
# Date: 2025-05-06

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


# ----- Setup: Clean Environment and Load Packages -----
rm(list = ls())
cat("\014")  # Clear console

# Required libraries
library(readxl)
library(writexl)
library(dplyr)
library(tidyr)
library(lubridate)

# Define paths
input_path <- "<<DATALIBWEB_PATH_OR_GENERIC_URL>>"
output_path <- "<<DATALIBWEB_PATH_OR_GENERIC_URL>>"

# Read all sheets from the input file
p20_data <- read_excel(input_path, sheet = "ILA P20")
median_data <- read_excel(input_path, sheet = "ILA Median")
p80_data <- read_excel(input_path, sheet = "ILA P80")
weights_data <- read_excel(input_path, sheet = "Workers weight")

# Define LAC-7 countries
lac7_countries <- c("Argentina", "Bolivia", "Brazil", "Costa Rica", "Mexico", "Peru", "Uruguay")

# Function to calculate weighted aggregates for a given percentile data
calculate_weighted_aggregate <- function(percentile_data, weights_data, lac7_countries) {
  # Convert to long format for easier manipulation
  percentile_long <- percentile_data %>%
    pivot_longer(cols = -Period, names_to = "Country", values_to = "Value")
  
  weights_long <- weights_data %>%
    pivot_longer(cols = -Period, names_to = "Country", values_to = "Weight")
  
  # Join percentile and weight data
  combined_data <- percentile_long %>%
    inner_join(weights_long, by = c("Period", "Country"))
  
  # Filter for LAC-7 countries and non-NA values
  lac7_data <- combined_data %>%
    filter(Country %in% lac7_countries, !is.na(Value), !is.na(Weight))
  
  # Calculate the sum of weights for LAC-7 countries in each period
  lac7_weight_sums <- lac7_data %>%
    group_by(Period) %>%
    summarize(TotalWeight = sum(Weight, na.rm = TRUE))
  
  # Join the weight sums and calculate rescaled weights and weighted values
  lac7_aggregate <- lac7_data %>%
    inner_join(lac7_weight_sums, by = "Period") %>%
    mutate(
      RescaledWeight = Weight / TotalWeight,
      WeightedValue = Value * RescaledWeight
    ) %>%
    group_by(Period) %>%
    summarize(WeightedAggregate = sum(WeightedValue, na.rm = TRUE))
  
  return(lac7_aggregate)
}

# Calculate the weighted aggregates for each percentile
p20_aggregate <- calculate_weighted_aggregate(p20_data, weights_data, lac7_countries)
median_aggregate <- calculate_weighted_aggregate(median_data, weights_data, lac7_countries)
p80_aggregate <- calculate_weighted_aggregate(p80_data, weights_data, lac7_countries)

# Combine the aggregates into a single data frame
aggregates <- p20_aggregate %>%
  rename(P20 = WeightedAggregate) %>%
  inner_join(median_aggregate %>% rename(P50 = WeightedAggregate), by = "Period") %>%
  inner_join(p80_aggregate %>% rename(P80 = WeightedAggregate), by = "Period")

# Calculate rescaled weights for LAC-7 countries
calculate_rescaled_weights <- function(weights_data, lac7_countries) {
  # Convert to long format
  weights_long <- weights_data %>%
    pivot_longer(cols = -Period, names_to = "Country", values_to = "Weight")
  
  # Filter for LAC-7 countries and add a flag for LAC-7
  weights_with_flag <- weights_long %>%
    mutate(IsLAC7 = Country %in% lac7_countries)
  
  # Calculate the sum of weights for LAC-7 countries in each period
  lac7_weight_sums <- weights_with_flag %>%
    filter(IsLAC7) %>%
    group_by(Period) %>%
    summarize(TotalWeight = sum(Weight, na.rm = TRUE))
  
  # Join with original data and calculate rescaled weights
  weights_rescaled <- weights_with_flag %>%
    left_join(lac7_weight_sums, by = "Period") %>%
    mutate(
      RescaledWeight = ifelse(IsLAC7, Weight / TotalWeight, 0)
    ) %>%
    select(Period, Country, OriginalWeight = Weight, RescaledWeight)
  
  return(weights_rescaled)
}

weights_rescaled <- calculate_rescaled_weights(weights_data, lac7_countries)

# Prepare data for sheets
# Sheet 1: Aggregated series
sheet1_data <- aggregates

# Sheet 2: Rescaled weights
sheet2_data <- weights_rescaled %>%
  select(Period, Country, RescaledWeight) %>%
  pivot_wider(names_from = Country, values_from = RescaledWeight)

# Sheet 3: Original weights
sheet3_data <- weights_rescaled %>%
  select(Period, Country, OriginalWeight) %>%
  pivot_wider(names_from = Country, values_from = OriginalWeight)

# Get current date in YYYY-MM-DD format
current_date <- format(Sys.Date(), "%Y-%m-%d")

# Update output path with current date (YYYY-MM-DD format)
output_path <- gsub("YYYY-MM-DD", current_date, output_path)

# Create a list of data frames for the Excel file
excel_data <- list(
  "Percentile Aggregates" = sheet1_data,
  "Rescaled Weights" = sheet2_data,
  "Original Weights" = sheet3_data
)

# Write to Excel file
write_xlsx(excel_data, output_path)

cat("Excel file created successfully at:", output_path, "\n")
