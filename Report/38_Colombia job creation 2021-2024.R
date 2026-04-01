# Colombia Workers Change Analysis - Simplified Version
# This script analyzes changes in employment in Colombia between 2021-Q2 and 2024-Q2
# Author: Luis Castellanos (Simplified version)
# Last modified: 2025-06-16
# -----------------------------------------------------------------------------

# Clean environment and load packages
rm(list = ls())
cat("\014")  # Clear console

required_pkgs <- c("haven", "dplyr", "writexl", "data.table", "lubridate")
for(pkg in required_pkgs) {
  if (!require(pkg, character.only = TRUE)) {
    install.packages(pkg, dependencies = TRUE)
    library(pkg, character.only = TRUE)
  }
}

# Define input and output paths
input_path <- "C:/Users/wb593225/OneDrive - WBG/Desktop/Luis - Private/Lablac/Semiannual report/Lablac data/No cohh"
output_path <- "C:/Users/wb593225/WBG/LAC Team for Statistical Development - WB Group - Documents/General/FY2025/Semiannual_Report/May/excel/Outputs/17 BBL Col"
current_date <- format(Sys.Date(), "%Y-%m-%d")
output_file <- file.path(output_path, "02_change_total_workers.xlsx")

# Colombia configuration
country <- "col"
start_year <- 2021
start_quarter <- 2
end_year <- 2024
end_quarter <- 2

# Define grouping variables to analyze
groupings <- c("sector", "sector1d", "empresa", "hombre", "gedad1", "relab", "urbano", "nivel")

# Define age groups classification function
age_groups <- function(age) {
  if(is.na(age)) return(NA_character_)
  if(age >= 15 && age <= 24) return("15-24")
  if(age >= 25 && age <= 44) return("25-44")
  if(age >= 45 && age <= 54) return("45-54")
  if(age >= 55 && age <= 64) return("55-64")
  if(age >= 64) return("64+")
  return(NA_character_)
}

# Define skill level classification function
skill_level <- function(nivel) {
  if(is.na(nivel)) return(NA_character_)
  if(nivel %in% c(0,1,2,3)) return("Low") 
  if(nivel %in% c(4,5)) return("Middle")
  if(nivel == 6) return("High")
  return(NA_character_)
}

# Function to find data file based on year and quarter
find_data_file <- function(year, quarter) {
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

# Load Colombia datasets
cat(paste0("=== LOADING COLOMBIA ", start_year, "-Q", start_quarter, " DATA ===\n"))

# Start data
start_file <- find_data_file(start_year, start_quarter)
if (!is.null(start_file)) {
  cat("Reading file:", start_file, "\n")
  
  tryCatch({
    data_start <- read_dta(start_file)
    cat("Successfully loaded file with", nrow(data_start), "rows and", ncol(data_start), "columns\n")
    
    # Filter for workers and edad > 14 and ocupado == 1
    data_start <- data_start %>% 
      filter(edad > 14, ocupado == 1)
    cat("After filtering:", nrow(data_start), "workers\n")
    
  }, error = function(e) {
    cat("Error loading file:", e$message, "\n")
    stop("Could not load start period data")
  })
} else {
  stop(paste0("No data file found for Colombia ", start_year, "-Q", start_quarter))
}

# End data
cat(paste0("\n=== LOADING COLOMBIA ", end_year, "-Q", end_quarter, " DATA ===\n"))
end_file <- find_data_file(end_year, end_quarter)
if (!is.null(end_file)) {
  cat("Reading file:", end_file, "\n")
  
  tryCatch({
    data_end <- read_dta(end_file)
    cat("Successfully loaded file with", nrow(data_end), "rows and", ncol(data_end), "columns\n")
    
    # Filter for workers with edad > 14 and ocupado == 1
    data_end <- data_end %>% 
      filter(edad > 14, ocupado == 1)
    cat("After filtering:", nrow(data_end), "workers\n")
    
  }, error = function(e) {
    cat("Error loading file:", e$message, "\n")
    stop("Could not load end period data")
  })
} else {
  stop(paste0("No data file found for Colombia ", end_year, "-Q", end_quarter))
}

# Function to process one variable
process_variable <- function(var_name, data_start, data_end) {
  cat("\n--- Processing", toupper(var_name), "---\n")
  
  # Check if variable exists in both datasets
  if (!(var_name %in% names(data_start)) || !(var_name %in% names(data_end))) {
    cat("Variable", var_name, "doesn't exist in one of the datasets\n")
    return(NULL)
  }
  
  # Process start data
  start_summary <- data_start %>%
    group_by(.data[[var_name]]) %>%
    summarise(total = sum(pondera, na.rm = TRUE)) %>%
    rename(Group = !!var_name) %>%
    mutate(Group = as.character(Group))
  
  cat("Start data summary has", nrow(start_summary), "groups\n")
  
  # Process end data
  end_summary <- data_end %>%
    group_by(.data[[var_name]]) %>%
    summarise(total = sum(pondera, na.rm = TRUE)) %>%
    rename(Group = !!var_name) %>%
    mutate(Group = as.character(Group))
  
  cat("End data summary has", nrow(end_summary), "groups\n")
  
  # Calculate absolute changes
  abs_change <- full_join(
    start_summary,
    end_summary,
    by = "Group"
  ) %>%
    mutate(
      total.x = ifelse(is.na(total.x), 0, total.x),
      total.y = ifelse(is.na(total.y), 0, total.y),
      abs_change = total.y - total.x
    ) %>%
    select(Group, abs_change)
  
  # Add total row
  total_change <- sum(end_summary$total, na.rm = TRUE) - sum(start_summary$total, na.rm = TRUE)
  abs_change <- abs_change %>%
    bind_rows(
      data.frame(
        Group = "Total",
        abs_change = total_change
      )
    )
  
  # Calculate relative contributions
  rel_change <- abs_change %>%
    filter(Group != "Total") %>%
    mutate(rel_contribution = abs_change / total_change * 100)
  
  # Add total row to relative contributions
  rel_change <- rel_change %>%
    bind_rows(
      data.frame(
        Group = "Total",
        abs_change = total_change,
        rel_contribution = 100
      )
    )
  
  return(list(
    abs_change = abs_change,
    rel_change = rel_change
  ))
}

# Process edad groups
process_edad_groups <- function(data_start, data_end) {
  cat("\n--- Processing EDAD GROUPS ---\n")
  
  # Check if edad variable exists in both datasets
  if (!("edad" %in% names(data_start)) || !("edad" %in% names(data_end))) {
    cat("Variable edad doesn't exist in one of the datasets\n")
    return(NULL)
  }
  
  # Process start data
  start_summary <- data_start %>%
    mutate(age_group = sapply(edad, age_groups)) %>%
    group_by(age_group) %>%
    summarise(total = sum(pondera, na.rm = TRUE)) %>%
    rename(Group = age_group) %>%
    mutate(Group = as.character(Group))
  
  cat("Start data summary has", nrow(start_summary), "groups\n")
  
  # Process end data
  end_summary <- data_end %>%
    mutate(age_group = sapply(edad, age_groups)) %>%
    group_by(age_group) %>%
    summarise(total = sum(pondera, na.rm = TRUE)) %>%
    rename(Group = age_group) %>%
    mutate(Group = as.character(Group))
  
  cat("End data summary has", nrow(end_summary), "groups\n")
  
  # Calculate absolute changes
  abs_change <- full_join(
    start_summary,
    end_summary,
    by = "Group"
  ) %>%
    mutate(
      total.x = ifelse(is.na(total.x), 0, total.x),
      total.y = ifelse(is.na(total.y), 0, total.y),
      abs_change = total.y - total.x
    ) %>%
    select(Group, abs_change)
  
  # Add total row
  total_change <- sum(end_summary$total, na.rm = TRUE) - sum(start_summary$total, na.rm = TRUE)
  abs_change <- abs_change %>%
    bind_rows(
      data.frame(
        Group = "Total",
        abs_change = total_change
      )
    )
  
  # Calculate relative contributions
  rel_change <- abs_change %>%
    filter(Group != "Total") %>%
    mutate(rel_contribution = abs_change / total_change * 100)
  
  # Add total row to relative contributions
  rel_change <- rel_change %>%
    bind_rows(
      data.frame(
        Group = "Total",
        abs_change = total_change,
        rel_contribution = 100
      )
    )
  
  return(list(
    abs_change = abs_change,
    rel_change = rel_change
  ))
}

# Process skill level
process_skill_level <- function(data_start, data_end) {
  cat("\n--- Processing SKILL LEVEL ---\n")
  
  # Check if nivel variable exists in both datasets
  if (!("nivel" %in% names(data_start)) || !("nivel" %in% names(data_end))) {
    cat("Variable nivel doesn't exist in one of the datasets\n")
    return(NULL)
  }
  
  # Process start data
  start_summary <- data_start %>%
    mutate(skill = sapply(nivel, skill_level)) %>%
    group_by(skill) %>%
    summarise(total = sum(pondera, na.rm = TRUE)) %>%
    rename(Group = skill) %>%
    mutate(Group = as.character(Group))
  
  cat("Start data summary has", nrow(start_summary), "groups\n")
  
  # Process end data
  end_summary <- data_end %>%
    mutate(skill = sapply(nivel, skill_level)) %>%
    group_by(skill) %>%
    summarise(total = sum(pondera, na.rm = TRUE)) %>%
    rename(Group = skill) %>%
    mutate(Group = as.character(Group))
  
  cat("End data summary has", nrow(end_summary), "groups\n")
  
  # Calculate absolute changes
  abs_change <- full_join(
    start_summary,
    end_summary,
    by = "Group"
  ) %>%
    mutate(
      total.x = ifelse(is.na(total.x), 0, total.x),
      total.y = ifelse(is.na(total.y), 0, total.y),
      abs_change = total.y - total.x
    ) %>%
    select(Group, abs_change)
  
  # Add total row
  total_change <- sum(end_summary$total, na.rm = TRUE) - sum(start_summary$total, na.rm = TRUE)
  abs_change <- abs_change %>%
    bind_rows(
      data.frame(
        Group = "Total",
        abs_change = total_change
      )
    )
  
  # Calculate relative contributions
  rel_change <- abs_change %>%
    filter(Group != "Total") %>%
    mutate(rel_contribution = abs_change / total_change * 100)
  
  # Add total row to relative contributions
  rel_change <- rel_change %>%
    bind_rows(
      data.frame(
        Group = "Total",
        abs_change = total_change,
        rel_contribution = 100
      )
    )
  
  return(list(
    abs_change = abs_change,
    rel_change = rel_change
  ))
}

# Create description and readme sheets
create_description_sheet <- function() {
  descriptions <- data.frame(
    Sheet = c(
      "Description",
      "ReadMe", 
      "Summary",
      "Absolute by Sector",
      "Absolute by Sector1d", 
      "Absolute by Empresa",
      "Absolute by Hombre",
      "Absolute by Gedad1",
      "Absolute by Edad Groups",
      "Absolute by Relab",
      "Absolute by Urbano",
      "Absolute by Skill Level",
      "Relative by Sector",
      "Relative by Sector1d",
      "Relative by Empresa", 
      "Relative by Hombre",
      "Relative by Gedad1",
      "Relative by Edad Groups",
      "Relative by Relab",
      "Relative by Urbano",
      "Relative by Skill Level"
    ),
    Description = c(
      "This sheet provides a description of all sheets in this file",
      "Methodological notes and data sources for Colombia analysis",
      "Summary of worker changes by main categories (Age, Gender, Sector, etc.)",
      "Absolute change in workers by sector between 2021-Q2 and 2024-Q2",
      "Absolute change in workers by sector1d between 2021-Q2 and 2024-Q2",
      "Absolute change in workers by empresa between 2021-Q2 and 2024-Q2",
      "Absolute change in workers by hombre between 2021-Q2 and 2024-Q2", 
      "Absolute change in workers by gedad1 between 2021-Q2 and 2024-Q2",
      "Absolute change in workers by age groups between 2021-Q2 and 2024-Q2",
      "Absolute change in workers by relab between 2021-Q2 and 2024-Q2",
      "Absolute change in workers by urbano between 2021-Q2 and 2024-Q2",
      "Absolute change in workers by skill level between 2021-Q2 and 2024-Q2",
      "Relative contribution to total change in workers by sector",
      "Relative contribution to total change in workers by sector1d",
      "Relative contribution to total change in workers by empresa",
      "Relative contribution to total change in workers by hombre",
      "Relative contribution to total change in workers by gedad1", 
      "Relative contribution to total change in workers by age groups",
      "Relative contribution to total change in workers by relab",
      "Relative contribution to total change in workers by urbano",
      "Relative contribution to total change in workers by skill level"
    )
  )
  
  return(descriptions)
}

create_readme_sheet <- function() {
  # Period info
  period_info <- data.frame(
    Country = "COLOMBIA",
    StartPeriod = paste0(start_year, "-Q", start_quarter),
    EndPeriod = paste0(end_year, "-Q", end_quarter),
    stringsAsFactors = FALSE
  )
  
  # Add methodological notes
  method_notes <- data.frame(
    Note = c(
      "Methodology",
      "Data source", 
      "Filtering criteria",
      "Age groups",
      "Skill levels",
      "Calculation method",
      "Summary sheet",
      "Variable mappings note"
    ),
    Description = c(
      "This analysis compares workers in Colombia between 2021-Q2 and 2024-Q2",
      "Data from LABLAC household surveys for Colombia",
      "Only observations with edad>14 and ocupado=1 are included",
      "Age groups are defined as: 15-24, 25-44, 45-54, 55-64, 64+",
      "Skill levels are defined based on nivel: Low (0,1,2,3), Middle (4,5), High (6)",
      "Absolute changes are differences in weighted totals; relative contributions are percentages of total change",
      "Summary sheet consolidates key categories with standardized subcategory names",
      "Some mappings (empresa, relab values) may need adjustment based on actual data coding - check raw data if values appear as NA"
    )
  )
  
  return(list(
    periods = period_info,
    notes = method_notes
  ))
}

# Process all variables
cat("\n=== PROCESSING ALL VARIABLES FOR COLOMBIA ===\n")
results <- list()

# Process each regular variable
for (var in groupings) {
  results[[var]] <- process_variable(var, data_start, data_end)
}

# Process special variables
results[["edad_groups"]] <- process_edad_groups(data_start, data_end)
results[["skill"]] <- process_skill_level(data_start, data_end)

# Function to create summary sheet with specific categories
create_summary_sheet <- function(results) {
  # Define the structure
  summary_data <- data.frame(
    Category = c(
      rep("Age", 5),
      rep("Firm Size", 3), 
      rep("Gender", 2),
      rep("Labor Status", 4),
      rep("Sector", 9),
      rep("Skill level", 3),
      rep("Urban", 2)
    ),
    Subcategory = c(
      # Age groups
      "15-24", "64+", "55-64", "45-54", "25-44",
      # Firm Size (empresa values need to be mapped based on actual data)
      "Public", "Small", "Big",
      # Gender 
      "Men", "Women",
      # Labor Status (relab values need to be mapped based on actual data)
      "Unpaid workers", "Employer", "Self-employed", "Employee",
      # Sector (mapping from sector variable)
      "Primary Activities", "Domestic Service", "Public Administration and Defense",
      "Construction", "Industry", "Electricity, Gas, Water, Transport, Communications",
      "Banks, Finance, Insurance, Professional Services", 
      "Education, Health, Personal Services",
      "Retail and Wholesale Trade, Restaurants, Hotels, Repairs",
      # Skill level
      "Low", "High", "Middle",
      # Urban
      "Rural", "Urban"
    ),
    Value = NA_real_,
    stringsAsFactors = FALSE
  )
  
  # Helper function to get value from results
  get_value <- function(var_name, group_name) {
    if (!is.null(results[[var_name]]) && !is.null(results[[var_name]]$abs_change)) {
      result <- results[[var_name]]$abs_change %>%
        filter(Group == group_name) %>%
        pull(abs_change)
      if (length(result) > 0) return(result[1])
    }
    return(NA_real_)
  }
  
  # Fill in values for each category
  for (i in 1:nrow(summary_data)) {
    category <- summary_data$Category[i]
    subcategory <- summary_data$Subcategory[i]
    
    value <- switch(category,
                    "Age" = get_value("edad_groups", subcategory),
                    "Gender" = case_when(
                      subcategory == "Men" ~ get_value("hombre", "1"),
                      subcategory == "Women" ~ get_value("hombre", "0"),
                      TRUE ~ NA_real_
                    ),
                    "Skill level" = get_value("skill", subcategory),
                    "Urban" = case_when(
                      subcategory == "Urban" ~ get_value("urbano", "1"),
                      subcategory == "Rural" ~ get_value("urbano", "0"),
                      TRUE ~ NA_real_
                    ),
                    "Firm Size" = {
                      # Map empresa categories - these might need adjustment based on actual data values
                      empresa_map <- case_when(
                        subcategory == "Public" ~ "1",  # Adjust based on actual empresa coding
                        subcategory == "Small" ~ "2",   # Adjust based on actual empresa coding  
                        subcategory == "Big" ~ "3",     # Adjust based on actual empresa coding
                        TRUE ~ subcategory
                      )
                      get_value("empresa", empresa_map)
                    },
                    "Labor Status" = {
                      # Map relab categories - these might need adjustment based on actual data values
                      relab_map <- case_when(
                        subcategory == "Employee" ~ "1",        # Adjust based on actual relab coding
                        subcategory == "Self-employed" ~ "2",   # Adjust based on actual relab coding
                        subcategory == "Employer" ~ "3",        # Adjust based on actual relab coding
                        subcategory == "Unpaid workers" ~ "4",  # Adjust based on actual relab coding
                        TRUE ~ subcategory
                      )
                      get_value("relab", relab_map)
                    },
                    "Sector" = {
                      # Map sector names to match the specific subcategories
                      sector_map <- case_when(
                        subcategory == "Primary Activities" ~ "Actividades Primarias",
                        subcategory == "Domestic Service" ~ "Servicio Doméstico", 
                        subcategory == "Public Administration and Defense" ~ "Administración Pública y Defensa",
                        subcategory == "Construction" ~ "Construcción",
                        subcategory == "Industry" ~ "Industrias de Baja Tecnología (Industria Alimenticia, Bebidas y Tabaco, Textiles y Confecciones)",
                        subcategory == "Electricity, Gas, Water, Transport, Communications" ~ "Electricidad, Gas, Agua, Transporte, Comunicaciones",
                        subcategory == "Banks, Finance, Insurance, Professional Services" ~ "Bancos, Finanzas, Seguros, Servicios Profesionales",
                        subcategory == "Education, Health, Personal Services" ~ "Educación, Salud, Servicios Personales",
                        subcategory == "Retail and Wholesale Trade, Restaurants, Hotels, Repairs" ~ "Comercio Minorista y Mayorista, Restaurants, Hoteles, Reparaciones",
                        TRUE ~ subcategory
                      )
                      get_value("sector", sector_map)
                    },
                    NA_real_
    )
    
    summary_data$Value[i] <- value
  }
  
  return(summary_data)
}

# Prepare Excel sheets
cat("\n=== PREPARING EXCEL SHEETS ===\n")

readme_info <- create_readme_sheet()

sheets <- list(
  "Description" = create_description_sheet(),
  "ReadMe" = readme_info$notes,
  "ReadMe Periods" = readme_info$periods,
  "Summary" = create_summary_sheet(results)
)

# Add absolute change sheets
for (var in names(results)) {
  if (!is.null(results[[var]])) {
    sheet_name <- case_when(
      var == "edad_groups" ~ "Absolute by Edad Groups",
      var == "skill" ~ "Absolute by Skill Level", 
      TRUE ~ paste("Absolute by", str_to_title(var))
    )
    sheets[[sheet_name]] <- results[[var]]$abs_change
  }
}

# Add relative contribution sheets
for (var in names(results)) {
  if (!is.null(results[[var]])) {
    sheet_name <- case_when(
      var == "edad_groups" ~ "Relative by Edad Groups",
      var == "skill" ~ "Relative by Skill Level",
      TRUE ~ paste("Relative by", str_to_title(var))
    )
    sheets[[sheet_name]] <- results[[var]]$rel_change
  }
}

# Write to Excel
cat("\n=== WRITING TO EXCEL ===\n")
cat("Output file:", output_file, "\n")

# Remove any NULL sheets
sheets <- sheets[!sapply(sheets, is.null)]

tryCatch({
  writexl::write_xlsx(sheets, path = output_file)
  cat("\n✅ Results saved to:", output_file, "\n")
  cat("Analysis completed for Colombia (", start_year, "-Q", start_quarter, " to ", end_year, "-Q", end_quarter, ")\n")
}, error = function(e) {
  cat("\n❌ Error writing to Excel:", e$message, "\n")
  # Create directory if it doesn't exist
  if (!dir.exists(output_path)) {
    dir.create(output_path, recursive = TRUE)
    cat("Created output directory:", output_path, "\n")
    # Try again
    writexl::write_xlsx(sheets, path = output_file)
    cat("\n✅ Results saved to:", output_file, "\n")
  }
})
