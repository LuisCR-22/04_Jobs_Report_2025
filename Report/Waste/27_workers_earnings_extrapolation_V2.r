# 1. Clean the environment
rm(list = ls())

# 2. Load required libraries
library(readxl)       # For reading Excel files
library(dplyr)        # For data manipulation
library(zoo)          # For time series operations (na.approx and na.spline)
library(openxlsx)     # For Excel formatting and writing
library(lubridate)    # For date handling

# Get current date for filenames
current_date <- format(Sys.Date(), "%Y_%m_%d")

# 3. Import the Excel file
file_path <- "C:/Users/wb593225/WBG/LAC Team for Statistical Development - WB Group - Documentos/General/FY2025/Semiannual_Report/May/excel/Outputs/08 jobs and earnings proyections/07_2025-04-25-workers-wap-income-2012-2024.xlsx"

# Extract the directory path for saving outputs
output_dir <- dirname(file_path)

# Read the Data sheet
data_df <- read_excel(file_path, sheet = "Data")

# Read the Footnotes sheet
footnotes_df <- read_excel(file_path, sheet = "Footnotes")

# 4. Convert zeros to NA (interpret values equal to 0 as missing values)
data_df$Value[data_df$Value == 0] <- NA

# 5. Create a function to impute missing values using linear interpolation
impute_linear <- function(df) {
  # Parse the period into a proper format for sorting
  # Extract year and quarter from Period (format: "YYYY-QN")
  year <- as.numeric(sub("(\\d{4})-Q\\d", "\\1", df$Period))
  quarter <- as.numeric(sub("\\d{4}-Q(\\d)", "\\1", df$Period))
  
  # Create a numeric time index for sorting
  df$time_index <- year + (quarter - 1) * 0.25
  
  # Sort by time_index
  df <- df[order(df$time_index), ]
  
  # Initialize the imputed flag
  df$Imputed <- FALSE
  
  # Only proceed with imputation if there are at least 2 non-NA values
  if (sum(!is.na(df$Value)) >= 2) {
    # Store original values to identify which ones were imputed
    original_values <- df$Value
    
    # Perform linear interpolation (only fills NAs between non-NA values)
    df$Value <- na.approx(df$Value, na.rm = FALSE)
    
    # Mark which values were imputed
    df$Imputed <- is.na(original_values) & !is.na(df$Value)
  }
  
  # Remove the time_index column and return the sorted data
  df$time_index <- NULL
  
  return(df)
}

# 6. Create a function to impute missing values using cubic spline interpolation
impute_spline <- function(df) {
  # Parse the period into a proper format for sorting
  # Extract year and quarter from Period (format: "YYYY-QN")
  year <- as.numeric(sub("(\\d{4})-Q\\d", "\\1", df$Period))
  quarter <- as.numeric(sub("\\d{4}-Q(\\d)", "\\1", df$Period))
  
  # Create a numeric time index for sorting
  df$time_index <- year + (quarter - 1) * 0.25
  
  # Sort by time_index
  df <- df[order(df$time_index), ]
  
  # Initialize the imputed flag
  df$Imputed <- FALSE
  
  # Only proceed with imputation if there are at least 2 non-NA values
  if (sum(!is.na(df$Value)) >= 2) {
    # Store original values to identify which ones were imputed
    original_values <- df$Value
    
    # Perform cubic spline interpolation (only fills NAs between non-NA values)
    df$Value <- na.spline(df$Value, na.rm = FALSE)
    
    # Mark which values were imputed
    df$Imputed <- is.na(original_values) & !is.na(df$Value)
  }
  
  # Remove the time_index column and return the sorted data
  df$time_index <- NULL
  
  return(df)
}

# 7. Apply the imputation functions to each country-indicator group
imputed_linear_data <- data_df %>%
  group_by(Country, Indicator) %>%
  group_modify(~ impute_linear(.x)) %>%
  ungroup()

imputed_spline_data <- data_df %>%
  group_by(Country, Indicator) %>%
  group_modify(~ impute_spline(.x)) %>%
  ungroup()

# Ensure columns are in the same order as the original file
imputed_linear_data <- imputed_linear_data %>% 
  select(Period, Indicator, Country, Value, Imputed)

imputed_spline_data <- imputed_spline_data %>% 
  select(Period, Indicator, Country, Value, Imputed)

# 8. Function to create and save workbook with given data and imputation method
create_workbook <- function(data, output_path, method_name, method_description) {
  wb <- createWorkbook()
  
  # Add the Data sheet with imputed values
  addWorksheet(wb, "Data")
  
  # Writing data without the Imputed column
  writeData(wb, "Data", data %>% select(-Imputed))
  
  # Create a style for imputed cells
  blue_style <- createStyle(fgFill = "#DDEBF7")  # Light blue
  
  # Apply the style to imputed cells
  for (i in 1:nrow(data)) {
    if (data$Imputed[i]) {
      # Add the style to the Value cell for imputed values
      addStyle(wb, "Data", 
               style = blue_style, 
               rows = i + 1,  # +1 for header row
               cols = 4,      # Column 4 is the Value column
               gridExpand = TRUE)
    }
  }
  
  # Add the Footnotes sheet
  addWorksheet(wb, "Footnotes")
  
  # Add a new row to the footnotes explaining the imputation method
  imputation_note <- data.frame(
    Indicator = "Imputation Method",
    Footnote = paste(
      method_description,
      "This method was chosen to account for the characteristics of economic time series data.",
      "The imputation was only applied when there were values available both before and after the missing value for the same country-indicator combination.",
      "Values equal to 0 in the original data were treated as missing values.",
      "Imputed cells are highlighted in light blue in the Data sheet.",
      paste0("To replicate: use the zoo::", method_name, "() function on each country-indicator time series after sorting by date."),
      "This approach is appropriate for economic indicators as it preserves the overall pattern while filling gaps in a way that respects the structure of the data.",
      sep = " "
    )
  )
  
  # Combine original footnotes with the imputation note
  footnotes_with_note <- rbind(footnotes_df, imputation_note)
  writeData(wb, "Footnotes", footnotes_with_note)
  
  # Save the workbook
  saveWorkbook(wb, output_path, overwrite = TRUE)
  
  print(paste("Workbook saved to:", output_path))
}

# 9. Define output paths and create workbooks
linear_output_path <- file.path(output_dir, paste0("08_", current_date, "_workers_income_linear_imputation.xlsx"))
spline_output_path <- file.path(output_dir, paste0("09_", current_date, "_workers_income_spline_imputation.xlsx"))

# 10. Create the linear interpolation workbook
create_workbook(
  imputed_linear_data,
  linear_output_path,
  "na.approx",
  "Missing values were imputed using linear interpolation via the na.approx() function from the R package 'zoo'. This method fills gaps by drawing straight lines between existing values."
)

# 11. Create the cubic spline interpolation workbook
create_workbook(
  imputed_spline_data,
  spline_output_path,
  "na.spline",
  "Missing values were imputed using cubic spline interpolation via the na.spline() function from the R package 'zoo'. Unlike linear interpolation, cubic spline interpolation creates smooth curves between data points that can better capture seasonal patterns and non-linear trends in the time series."
)

print("Imputation complete. Both files have been saved to the input directory.")
