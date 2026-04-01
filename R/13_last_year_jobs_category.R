# Multi-Country LABLAC Workers Last Year Analysis
# This script processes the latest survey data for Latin American countries to analyze the number of workers across various categories (sectors, gender, age groups, employment status, urban/rural, education level), standardizing and organizing the data for cross-country comparison.
# Author: Luis Castellanos - Stats Team (lcasterodr@worldbank.org)
# Last modified: 2025-05-06
# -----------------------------------------------------------------------------

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

required_pkgs <- c("haven", "dplyr", "writexl", "data.table", "lubridate")
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
output_file <- file.path(output_path, paste0(current_date, "-Workers-last-year.xlsx"))

# Country configurations - only the latest survey for each country
country_configs <- list(
  arg = list(year = 2024, quarter = 2),
  bol = list(year = 2024, quarter = 2),
  bra = list(year = 2024, quarter = 2),
  chl = list(year = 2024, quarter = 2),
  col = list(year = 2024, quarter = 2),
  cri = list(year = 2024, quarter = 2),
  dom = list(year = 2024, quarter = 2),
  ecu = list(year = 2024, quarter = 2),
  mex = list(year = 2024, quarter = 2),
  pry = list(year = 2024, quarter = 2),
  per = list(year = 2024, quarter = 2),
  slv = list(year = 2023, quarter = 2),
  ury = list(year = 2024, quarter = 2)
)

# Mapping from sector1d values to the new "sector" values for Chile
sector_map <- c(
  "Agricultura, Ganadería, Caza y Silvicultura" = "Actividades Primarias",
  "Pesca" = "Actividades Primarias",
  "Explotación de Minas y Canteras" = "Actividades Primarias",
  "Industrias Manufactureras" = "Industrias de Baja Tecnología (Industria Alimenticia, Bebidas y Tabaco, Textiles y Confecciones)",
  "Construcción" = "Construcción",
  "Comercio" = "Comercio Minorista y Mayorista, Restaurants, Hoteles, Reparaciones",
  "Hoteles y Restaurantes" = "Comercio Minorista y Mayorista, Restaurants, Hoteles, Reparaciones",
  "Suministro de Electricidad, Gas y Agua" = "Electricidad, Gas, Agua, Transporte, Comunicaciones",
  "Transporte, Almacenamiento y Comunicaciones" = "Electricidad, Gas, Agua, Transporte, Comunicaciones",
  "Intermediación Financiera" = "Bancos, Finanzas, Seguros, Servicios Profesionales",
  "Actividades Inmobiliarias, Empresariales y de Alquiler" = "Bancos, Finanzas, Seguros, Servicios Profesionales",
  "Administración Pública y Defensa" = "Administración Pública y Defensa",
  "Organizaciones y Órganos Extraterritoriales" = "Administración Pública y Defensa",
  "Enseñanza" = "Educación, Salud, Servicios Personales",
  "Servicios Sociales y de Salud" = "Educación, Salud, Servicios Personales",
  "Otras Actividades de Servicios Comunitarios, Sociales y Personales" = "Educación, Salud, Servicios Personales",
  "Hogares Privados con Servicio Doméstico" = "Servicio Doméstico",
  "Total" = "Total"
)

# Define grouping variables
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

# Initialize results list for each variable
results <- list()
for (var_name in c(groupings, "edad_groups", "skill")) {
  results[[var_name]] <- list()
}

# Process each country one by one
for (country in names(country_configs)) {
  config <- country_configs[[country]]
  
  # Load the latest survey data
  data_file <- find_data_file(country, config$year, config$quarter)
  
  if (!is.null(data_file)) {
    cat(paste0("\n=== PROCESSING ", toupper(country), " ", config$year, "-Q", config$quarter, " DATA ===\n"))
    cat("Reading file:", data_file, "\n")
    
    tryCatch({
      country_data <- read_dta(data_file)
      cat("Successfully loaded file with", nrow(country_data), "rows and", ncol(country_data), "columns\n")
      
      # Filter for workers and edad > 14 and ocupado == 1
      country_data <- country_data %>% 
        filter(edad > 14, ocupado == 1)
      cat("After filtering:", nrow(country_data), "workers\n")
      
      # Process all variables for this country at once
      for (var_name in groupings) {
        # Skip if variable doesn't exist
        if (!(var_name %in% names(country_data))) {
          cat("Variable", var_name, "doesn't exist in the dataset for", toupper(country), "\n")
          next
        }
        
        # Special case for Chile sector variable
        if (country == "chl" && var_name == "sector") {
          # Check if sector1d exists
          if (!("sector1d" %in% names(country_data))) {
            cat("Variable sector1d doesn't exist in the dataset for", toupper(country), "\n")
            next
          }
          
          # Create sector variable from sector1d for Chile
          country_data <- country_data %>%
            mutate(sector = sector_map[as.character(sector1d)])
          
          cat("Created sector variable for Chile from sector1d mapping\n")
        }
        
        # Calculate totals by group
        var_summary <- country_data %>%
          group_by(.data[[var_name]]) %>%
          summarise(total = sum(pondera, na.rm = TRUE)) %>%
          rename(Group = !!var_name) %>%
          mutate(Group = as.character(Group))
        
        # Add total row
        total_sum <- sum(var_summary$total, na.rm = TRUE)
        var_summary <- var_summary %>%
          bind_rows(
            data.frame(
              Group = "Total",
              total = total_sum
            )
          )
        
        # Store results
        results[[var_name]][[country]] <- var_summary
      }
      
      # Process age groups
      if ("edad" %in% names(country_data)) {
        age_summary <- country_data %>%
          mutate(age_group = sapply(edad, age_groups)) %>%
          group_by(age_group) %>%
          summarise(total = sum(pondera, na.rm = TRUE)) %>%
          rename(Group = age_group) %>%
          mutate(Group = as.character(Group))
        
        # Add total row
        total_sum <- sum(age_summary$total, na.rm = TRUE)
        age_summary <- age_summary %>%
          bind_rows(
            data.frame(
              Group = "Total",
              total = total_sum
            )
          )
        
        # Store results
        results[["edad_groups"]][[country]] <- age_summary
      } else {
        cat("Variable edad doesn't exist in the dataset for", toupper(country), "\n")
      }
      
      # Process skill levels
      if ("nivel" %in% names(country_data)) {
        skill_summary <- country_data %>%
          mutate(skill = sapply(nivel, skill_level)) %>%
          group_by(skill) %>%
          summarise(total = sum(pondera, na.rm = TRUE)) %>%
          rename(Group = skill) %>%
          mutate(Group = as.character(Group))
        
        # Add total row
        total_sum <- sum(skill_summary$total, na.rm = TRUE)
        skill_summary <- skill_summary %>%
          bind_rows(
            data.frame(
              Group = "Total",
              total = total_sum
            )
          )
        
        # Store results
        results[["skill"]][[country]] <- skill_summary
      } else {
        cat("Variable nivel doesn't exist in the dataset for", toupper(country), "\n")
      }
      
      # Remove dataset from memory to save space
      rm(country_data)
      gc()
      
    }, error = function(e) {
      cat("Error processing file:", e$message, "\n")
    })
  } else {
    cat(paste0("WARNING: No data file found for ", toupper(country), " ", config$year, "-Q", config$quarter, "\n"))
  }
}

# Function to combine results from all countries into a single dataframe
combine_results <- function(var_results) {
  # Get list of all unique groups from all countries
  all_groups <- c()
  for (country in names(var_results)) {
    if (!is.null(var_results[[country]])) {
      all_groups <- c(all_groups, var_results[[country]]$Group)
    }
  }
  all_groups <- unique(all_groups)
  
  # Create empty dataframe with all groups
  combined_df <- data.frame(Group = all_groups)
  
  # Add column for each country
  for (country in names(var_results)) {
    if (!is.null(var_results[[country]])) {
      country_data <- var_results[[country]]
      
      # Join with combined dataframe
      combined_df <- left_join(
        combined_df,
        country_data %>% select(Group, total),
        by = "Group"
      ) %>%
        rename(!!country := total)
    } else {
      # Add NA column if country data is missing
      combined_df[[country]] <- NA
    }
  }
  
  # Move Total row to the end
  combined_df <- combined_df %>%
    arrange(factor(Group, levels = c(setdiff(all_groups, "Total"), "Total")))
  
  return(combined_df)
}

# Create description and readme sheets
create_description_sheet <- function() {
  descriptions <- data.frame(
    Sheet = c(
      "Description",
      "ReadMe",
      "Workers by Sector",
      "Workers by Sector1d",
      "Workers by Empresa",
      "Workers by Hombre",
      "Workers by Gedad1",
      "Workers by Edad Groups",
      "Workers by Relab",
      "Workers by Urbano",
      "Workers by Skill Level"
    ),
    Description = c(
      "This sheet provides a description of all sheets in this file",
      "Methodological notes and data sources",
      "Number of workers by sector in the latest survey period",
      "Number of workers by sector1d in the latest survey period",
      "Number of workers by empresa in the latest survey period",
      "Number of workers by hombre in the latest survey period",
      "Number of workers by gedad1 in the latest survey period",
      "Number of workers by age groups in the latest survey period",
      "Number of workers by relab in the latest survey period",
      "Number of workers by urbano in the latest survey period",
      "Number of workers by skill level in the latest survey period"
    )
  )
  
  return(descriptions)
}

create_readme_sheet <- function() {
  # Create period info for each country
  period_info <- data.frame(
    Country = character(),
    Period = character(),
    stringsAsFactors = FALSE
  )
  
  for (country in names(country_configs)) {
    config <- country_configs[[country]]
    
    # Format period
    period <- paste0(config$year, "-Q", config$quarter)
    
    # Add to dataframe
    period_info <- rbind(
      period_info,
      data.frame(
        Country = toupper(country),
        Period = period,
        stringsAsFactors = FALSE
      )
    )
  }
  
  # Add methodological notes
  method_notes <- data.frame(
    Note = c(
      "Methodology",
      "Data source",
      "Filtering criteria",
      "Age groups",
      "Skill levels",
      "Chile sector mapping"
    ),
    Description = c(
      "This analysis reports the number of workers by different categories in the latest survey period for each country",
      "Data from LABLAC household surveys",
      "Only observations with edad>14, and ocupado=1 are included",
      "Age groups are defined as: 15-24, 25-44, 45-54, 55-64, 64+",
      "Skill levels are defined based on nivel: Low (0,1,2,3), Middle (4,5), High (6)",
      "For Chile, sector variable was created from sector1d using a predefined mapping"
    )
  )
  
  # Combine into a list
  readme <- list(
    periods = period_info,
    notes = method_notes
  )
  
  return(readme)
}

# Combine results into sheets for Excel
cat("\n=== PREPARING EXCEL SHEETS ===\n")

# Description and readme sheets
sheets <- list(
  "Description" = create_description_sheet(),
  "ReadMe" = create_readme_sheet()$notes
)

# Add country periods to ReadMe sheet
sheets[["ReadMe Periods"]] <- create_readme_sheet()$periods

# Create sheets for each variable
sheets[["Workers by Sector"]] <- combine_results(results$sector)
sheets[["Workers by Sector1d"]] <- combine_results(results$sector1d)
sheets[["Workers by Empresa"]] <- combine_results(results$empresa)
sheets[["Workers by Hombre"]] <- combine_results(results$hombre)
sheets[["Workers by Gedad1"]] <- combine_results(results$gedad1)
sheets[["Workers by Edad Groups"]] <- combine_results(results$edad_groups)
sheets[["Workers by Relab"]] <- combine_results(results$relab)
sheets[["Workers by Urbano"]] <- combine_results(results$urbano)
sheets[["Workers by Skill Level"]] <- combine_results(results$skill)

# Write to Excel
cat("\n=== WRITING TO EXCEL ===\n")
cat("Output file:", output_file, "\n")

# Remove any NULL sheets
sheets <- sheets[!sapply(sheets, is.null)]

tryCatch({
  writexl::write_xlsx(sheets, path = output_file)
  cat("\n✅ Results saved to:", output_file, "\n")
}, error = function(e) {
  cat("\n❌ Error writing to Excel:", e$message, "\n")
})
