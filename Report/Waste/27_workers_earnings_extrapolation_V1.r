# 1. Clean the environment
rm(list = ls())

# 2. Load required libraries
library(readxl)       # For reading Excel files
library(dplyr)        # For data manipulation
library(zoo)          # For time series operations and na.approx()
library(openxlsx)     # For Excel formatting and writing

# 3. Import the Excel file
file_path <- "C:/Users/wb593225/WBG/LAC Team for Statistical Development - WB Group - Documentos/General/FY2025/Semiannual_Report/May/excel/Outputs/08 jobs and earnings proyections/07_2025-04-25-workers-wap-income-2012-2024.xlsx"

# Read the Data sheet
data_df <- read_excel(file_path, sheet = "Data")

# Read the Footnotes sheet
footnotes_df <- read_excel(file_path, sheet = "Footnotes")

# 4. Convert zeros to NA (interpret values equal to 0 as missing values)
data_df$Value[data_df$Value == 0] <- NA

# 5. Create a function to impute missing values for each country-indicator combination
impute_time_series <- function(df) {
  # Parse the period into a proper format for sorting
  # Extract year and quarter from Period (format: "YYYY-QN")
  year <- as.numeric(sub("(\\d{4})-Q\\d", "\\1", df$Period))
  quarter <- as.numeric(sub("\\d{4}-Q(\\d)", "\\1", df$Period))
  
  # Create a numeric time index for sorting (e.g., 2012.00, 2012.25, 2012.50, 2012.75, 2013.00, ...)
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

# 6. Apply the imputation function to each country-indicator group
imputed_data <- data_df %>%
  group_by(Country, Indicator) %>%
  group_modify(~ impute_time_series(.x)) %>%
  ungroup()

# Ensure columns are in the same order as the original file
imputed_data <- imputed_data %>% 
  select(Period, Indicator, Country, Value, Imputed)

# 7. Create a new Excel workbook
wb <- createWorkbook()

# 8. Add the Data sheet with imputed values
addWorksheet(wb, "Data")

# Writing data without the Imputed column
writeData(wb, "Data", imputed_data %>% select(-Imputed))

# 9. Highlight imputed cells in light blue
# Create a style for imputed cells
blue_style <- createStyle(fgFill = "#DDEBF7")  # Light blue

# Apply the style to imputed cells
for (i in 1:nrow(imputed_data)) {
  if (imputed_data$Imputed[i]) {
    # Highlight the Value cell for imputed values
    # Row and column are 1-indexed in openxlsx, and we need to account for header row
    styleCell(wb, "Data", style = blue_style, rows = i + 1, cols = 4)  # Column 4 is the Value column
  }
}

# 10. Add the Footnotes sheet
addWorksheet(wb, "Footnotes")

# 11. Add a new row to the footnotes explaining the imputation method
imputation_note <- data.frame(
  Indicator = "Imputation Method",
  Footnote = paste(
    "Missing values were imputed using linear interpolation via the na.approx() function from the R package 'zoo'.",
    "This method was chosen because it provides a simple and intuitive way to fill gaps in time series data by drawing a straight line between existing values.",
    "The imputation was only applied when there were values available both before and after the missing value for the same country-indicator combination.",
    "Values equal to 0 in the original data were treated as missing values.",
    "Imputed cells are highlighted in light blue in the Data sheet.",
    "To replicate: use the zoo::na.approx() function on each country-indicator time series after sorting by date.",
    "Linear interpolation is appropriate for this data as it preserves trends and provides reasonable estimates for economic indicators that typically change gradually over time.",
    sep = " "
  )
)

# Combine original footnotes with the imputation note
footnotes_with_note <- rbind(footnotes_df, imputation_note)
writeData(wb, "Footnotes", footnotes_with_note)

# 12. Save the workbook
output_path <- "Imputed_workers_wap_income_2012_2024.xlsx"
saveWorkbook(wb, output_path, overwrite = TRUE)

print(paste("Imputation complete. Output file saved to:", output_path))
