# Multi-Country LABLAC Workers Analysis
# This script analyzes changes in employment across Latin American countries by comparing worker demographics between two time periods, calculating both absolute changes and relative contributions by various characteristics (sector, gender, age, skill level), and generating standardized cross-country comparisons.
# Author: Luis Castellanos
# Last modified: 2025-05-06
# -----------------------------------------------------------------------------

# ==============================================================================
# DATALIBWEB & NOCOHH NOTICE
# ==============================================================================
# NOTE: Uers should retrieve data directly from datalibweb:
#
# IMPORTANT: Results, might change due to:
#   1. Updates in the LABLAC datasets
#   2. The use of the nocohh option
# For MEX, using nocohh is critical so that labor statistics are consistent
# with those reported by the ILO.
# ==============================================================================


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
output_file <- file.path(output_path, paste0(current_date, "-change-workers-V5-optimized.xlsx"))

# Country configurations
country_configs <- list(
  arg = list(start_year = 2016, start_quarter = 2, end_year = 2024, end_quarter = 2),
  bol = list(start_year = 2016, start_quarter = 2, end_year = 2024, end_quarter = 2),
  bra = list(start_year = 2016, start_quarter = 2, end_year = 2024, end_quarter = 2,
             special_start_year = 2016, special_start_quarter = 4,
             special_variable = "empresa"),  # Special case for empresa variable
  chl = list(start_year = 2016, start_quarter = 4, end_year = 2023, end_quarter = 4),
  col = list(start_year = 2021, start_quarter = 4, end_year = 2023, end_quarter = 4),
  cri = list(start_year = 2017, start_quarter = 4, end_year = 2024, end_quarter = 4),
  dom = list(start_year = 2017, start_quarter = 2, end_year = 2024, end_quarter = 2),
  ecu = list(start_year = 2021, start_quarter = 2, end_year = 2024, end_quarter = 2),
  mex = list(start_year = 2016, start_quarter = 2, end_year = 2024, end_quarter = 2),
  pry = list(start_year = 2022, start_quarter = 2, end_year = 2024, end_quarter = 2),
  per = list(start_year = 2016, start_quarter = 2, end_year = 2024, end_quarter = 2),
  slv = list(start_year = 2016, start_quarter = 2, end_year = 2023, end_quarter = 2),
  ury = list(start_year = 2022, start_quarter = 2, end_year = 2024, end_quarter = 2)
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

# Create storage for country datasets
country_data <- list()

# Load all country datasets
for (country in names(country_configs)) {
  config <- country_configs[[country]]
  
  # Start data
  start_file <- find_data_file(country, config$start_year, config$start_quarter)
  if (!is.null(start_file)) {
    cat(paste0("=== LOADING ", toupper(country), " ", config$start_year, "-Q", config$start_quarter, " DATA ===\n"))
    cat("Reading file:", start_file, "\n")
    
    tryCatch({
      data_start <- read_dta(start_file)
      cat("Successfully loaded file with", nrow(data_start), "rows and", ncol(data_start), "columns\n")
      
      # Filter for workers and edad > 14 and ocupado == 1
      data_start <- data_start %>% 
        filter(edad > 14, ocupado == 1)
      cat("After filtering:", nrow(data_start), "workers\n")
      
      # Store in list
      country_data[[paste0(country, "_start")]] <- data_start
    }, error = function(e) {
      cat("Error loading file:", e$message, "\n")
      country_data[[paste0(country, "_start")]] <- NULL
    })
  } else {
    cat(paste0("WARNING: No data file found for ", toupper(country), " ", config$start_year, "-Q", config$start_quarter, "\n"))
    country_data[[paste0(country, "_start")]] <- NULL
  }
  
  # End data
  end_file <- find_data_file(country, config$end_year, config$end_quarter)
  if (!is.null(end_file)) {
    cat(paste0("\n=== LOADING ", toupper(country), " ", config$end_year, "-Q", config$end_quarter, " DATA ===\n"))
    cat("Reading file:", end_file, "\n")
    
    tryCatch({
      data_end <- read_dta(end_file)
      cat("Successfully loaded file with", nrow(data_end), "rows and", ncol(data_end), "columns\n")
      
      # Filter for workers with edad > 14 and ocupado == 1
      data_end <- data_end %>% 
        filter(edad > 14, ocupado == 1)
      cat("After filtering:", nrow(data_end), "workers\n")
      
      # Store in list
      country_data[[paste0(country, "_end")]] <- data_end
    }, error = function(e) {
      cat("Error loading file:", e$message, "\n")
      country_data[[paste0(country, "_end")]] <- NULL
    })
  } else {
    cat(paste0("WARNING: No data file found for ", toupper(country), " ", config$end_year, "-Q", config$end_quarter, "\n"))
    country_data[[paste0(country, "_end")]] <- NULL
  }
  
  # Special case for Brazil "empresa" variable
  if (country == "bra" && !is.null(config$special_variable) && config$special_variable == "empresa") {
    special_file <- find_data_file(country, config$special_start_year, config$special_start_quarter)
    if (!is.null(special_file)) {
      cat(paste0("\n=== LOADING SPECIAL ", toupper(country), " ", config$special_start_year, "-Q", config$special_start_quarter, " DATA FOR VARIABLE ", config$special_variable, " ===\n"))
      cat("Reading file:", special_file, "\n")
      
      tryCatch({
        special_data <- read_dta(special_file)
        cat("Successfully loaded file with", nrow(special_data), "rows and", ncol(special_data), "columns\n")
        
        # Filter for workers with and edad > 14 and ocupado == 1
        special_data <- special_data %>% 
          filter(edad > 14, ocupado == 1)
        cat("After filtering:", nrow(special_data), "workers\n")
        
        # Store in list for special case
        country_data[[paste0(country, "_", config$special_variable, "_start")]] <- special_data
      }, error = function(e) {
        cat("Error loading special file:", e$message, "\n")
        country_data[[paste0(country, "_", config$special_variable, "_start")]] <- NULL
      })
    }
  }
}

# Function to process one variable for all countries
process_variable <- function(var_name) {
  cat("\n=== PROCESSING", toupper(var_name), "FOR ALL COUNTRIES ===\n")
  
  # Initialize results list
  all_countries_results <- list()
  
  # Process each country
  for (country in names(country_configs)) {
    cat("\n--- Processing", toupper(country), "---\n")
    
    # Determine which datasets to use
    if (country == "bra" && var_name == "empresa" && !is.null(country_data[[paste0(country, "_empresa_start")]])) {
      data_start <- country_data[[paste0(country, "_empresa_start")]]
    } else {
      data_start <- country_data[[paste0(country, "_start")]]
    }
    
    data_end <- country_data[[paste0(country, "_end")]]
    
    # Skip if either dataset is missing
    if (is.null(data_start) || is.null(data_end)) {
      cat("Skipping", toupper(country), "due to missing data\n")
      all_countries_results[[country]] <- NULL
      next
    }
    
    # Check if variable exists in both datasets
    if (!(var_name %in% names(data_start)) || !(var_name %in% names(data_end))) {
      cat("Variable", var_name, "doesn't exist in one of the datasets for", toupper(country), "\n")
      all_countries_results[[country]] <- NULL
      next
    }
    
    # Special case for Chile sector variable
    if (country == "chl" && var_name == "sector") {
      # Check if sector1d exists
      if (!("sector1d" %in% names(data_start)) || !("sector1d" %in% names(data_end))) {
        cat("Variable sector1d doesn't exist in one of the datasets for", toupper(country), "\n")
        all_countries_results[[country]] <- NULL
        next
      }
      
      # Create sector variable from sector1d for Chile
      data_start <- data_start %>%
        mutate(sector = sector_map[as.character(sector1d)])
      
      data_end <- data_end %>%
        mutate(sector = sector_map[as.character(sector1d)])
      
      cat("Created sector variable for Chile from sector1d mapping\n")
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
    
    # Store results
    all_countries_results[[country]] <- list(
      abs_change = abs_change,
      rel_change = rel_change
    )
  }
  
  return(all_countries_results)
}

# Process special cases (edad groups and skill level)
process_edad_groups <- function() {
  cat("\n=== PROCESSING EDAD GROUPS FOR ALL COUNTRIES ===\n")
  
  # Initialize results list
  all_countries_results <- list()
  
  # Process each country
  for (country in names(country_configs)) {
    cat("\n--- Processing", toupper(country), "---\n")
    
    data_start <- country_data[[paste0(country, "_start")]]
    data_end <- country_data[[paste0(country, "_end")]]
    
    # Skip if either dataset is missing
    if (is.null(data_start) || is.null(data_end)) {
      cat("Skipping", toupper(country), "due to missing data\n")
      all_countries_results[[country]] <- NULL
      next
    }
    
    # Check if edad variable exists in both datasets
    if (!("edad" %in% names(data_start)) || !("edad" %in% names(data_end))) {
      cat("Variable edad doesn't exist in one of the datasets for", toupper(country), "\n")
      all_countries_results[[country]] <- NULL
      next
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
    
    # Store results
    all_countries_results[[country]] <- list(
      abs_change = abs_change,
      rel_change = rel_change
    )
  }
  
  return(all_countries_results)
}

process_skill_level <- function() {
  cat("\n=== PROCESSING SKILL LEVEL FOR ALL COUNTRIES ===\n")
  
  # Initialize results list
  all_countries_results <- list()
  
  # Process each country
  for (country in names(country_configs)) {
    cat("\n--- Processing", toupper(country), "---\n")
    
    data_start <- country_data[[paste0(country, "_start")]]
    data_end <- country_data[[paste0(country, "_end")]]
    
    # Skip if either dataset is missing
    if (is.null(data_start) || is.null(data_end)) {
      cat("Skipping", toupper(country), "due to missing data\n")
      all_countries_results[[country]] <- NULL
      next
    }
    
    # Check if nivel variable exists in both datasets
    if (!("nivel" %in% names(data_start)) || !("nivel" %in% names(data_end))) {
      cat("Variable nivel doesn't exist in one of the datasets for", toupper(country), "\n")
      all_countries_results[[country]] <- NULL
      next
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
    
    # Store results
    all_countries_results[[country]] <- list(
      abs_change = abs_change,
      rel_change = rel_change
    )
  }
  
  return(all_countries_results)
}

# Function to combine results from all countries into a single dataframe
combine_results <- function(results, type = "abs") {
  # Get list of all unique groups from all countries
  all_groups <- c()
  for (country in names(results)) {
    if (!is.null(results[[country]])) {
      if (type == "abs") {
        all_groups <- c(all_groups, results[[country]]$abs_change$Group)
      } else {
        all_groups <- c(all_groups, results[[country]]$rel_change$Group)
      }
    }
  }
  all_groups <- unique(all_groups)
  
  # Create empty dataframe with all groups
  combined_df <- data.frame(Group = all_groups)
  
  # Add column for each country
  for (country in names(results)) {
    if (!is.null(results[[country]])) {
      country_data <- if (type == "abs") {
        results[[country]]$abs_change
      } else {
        results[[country]]$rel_change
      }
      
      # Change column name based on type
      value_col <- if (type == "abs") "abs_change" else "rel_contribution"
      
      # Join with combined dataframe
      combined_df <- left_join(
        combined_df,
        country_data %>% select(Group, !!value_col),
        by = "Group"
      ) %>%
        rename(!!country := !!value_col)
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
      "Methodological notes and data sources",
      "Absolute change in workers by sector between start and end periods",
      "Absolute change in workers by sector1d between start and end periods",
      "Absolute change in workers by empresa between start and end periods",
      "Absolute change in workers by hombre between start and end periods",
      "Absolute change in workers by gedad1 between start and end periods",
      "Absolute change in workers by age groups between start and end periods",
      "Absolute change in workers by relab between start and end periods",
      "Absolute change in workers by urbano between start and end periods",
      "Absolute change in workers by skill level between start and end periods",
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
  # Create period info for each country
  period_info <- data.frame(
    Country = character(),
    StartPeriod = character(),
    EndPeriod = character(),
    stringsAsFactors = FALSE
  )
  
  for (country in names(country_configs)) {
    config <- country_configs[[country]]
    
    # Format periods
    start_period <- paste0(config$start_year, "-Q", config$start_quarter)
    end_period <- paste0(config$end_year, "-Q", config$end_quarter)
    
    # Special case for Brazil empresa
    empresa_note <- ""
    if (country == "bra" && !is.null(config$special_variable) && config$special_variable == "empresa") {
      empresa_note <- paste0(" (", config$special_start_year, "-Q", config$special_start_quarter, " for empresa variable)")
    }
    
    # Add to dataframe
    period_info <- rbind(
      period_info,
      data.frame(
        Country = toupper(country),
        StartPeriod = paste0(start_period, empresa_note),
        EndPeriod = end_period,
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
      "Chile sector mapping",
      "Brazil special case"
    ),
    Description = c(
      "This analysis compares workers between two time periods for each country",
      "Data from LABLAC household surveys",
      "Only observations with edad>14, and ocupado=1 are included",
      "Age groups are defined as: 15-24, 25-44, 45-54, 55-64, 64+",
      "Skill levels are defined based on nivel: Low (0,1,2,3), Middle (4,5), High (6)",
      "For Chile, sector variable was created from sector1d using a predefined mapping",
      "For Brazil, the empresa variable analysis uses Q4 2016 data instead of Q2 2016"
    )
  )
  
  # Combine into a list
  readme <- list(
    periods = period_info,
    notes = method_notes
  )
  
  return(readme)
}

# Process all regular variables
cat("\n=== PROCESSING ALL VARIABLES ===\n")
results <- list()

# Process sector variable for all countries
results$sector <- process_variable("sector")

# Process sector1d variable for all countries
results$sector1d <- process_variable("sector1d")

# Process empresa variable for all countries
results$empresa <- process_variable("empresa")

# Process hombre variable for all countries
results$hombre <- process_variable("hombre")

# Process gedad1 variable for all countries
results$gedad1 <- process_variable("gedad1")

# Process edad groups for all countries
results$edad_groups <- process_edad_groups()

# Process relab variable for all countries
results$relab <- process_variable("relab")

# Process urbano variable for all countries
results$urbano <- process_variable("urbano")

# Process skill level for all countries
results$skill <- process_skill_level()

# Combine results into sheets for Excel
cat("\n=== PREPARING EXCEL SHEETS ===\n")

# Description and readme sheets
sheets <- list(
  "Description" = create_description_sheet(),
  "ReadMe" = create_readme_sheet()$notes
)

# Add country periods to ReadMe sheet
sheets[["ReadMe Periods"]] <- create_readme_sheet()$periods

# Absolute change sheets
sheets[["Absolute by Sector"]] <- combine_results(results$sector, "abs")
sheets[["Absolute by Sector1d"]] <- combine_results(results$sector1d, "abs")
sheets[["Absolute by Empresa"]] <- combine_results(results$empresa, "abs")
sheets[["Absolute by Hombre"]] <- combine_results(results$hombre, "abs")
sheets[["Absolute by Gedad1"]] <- combine_results(results$gedad1, "abs")
sheets[["Absolute by Edad Groups"]] <- combine_results(results$edad_groups, "abs")
sheets[["Absolute by Relab"]] <- combine_results(results$relab, "abs")
sheets[["Absolute by Urbano"]] <- combine_results(results$urbano, "abs")
sheets[["Absolute by Skill Level"]] <- combine_results(results$skill, "abs")

# Relative contribution sheets
sheets[["Relative by Sector"]] <- combine_results(results$sector, "rel")
sheets[["Relative by Sector1d"]] <- combine_results(results$sector1d, "rel")
sheets[["Relative by Empresa"]] <- combine_results(results$empresa, "rel")
sheets[["Relative by Hombre"]] <- combine_results(results$hombre, "rel")
sheets[["Relative by Gedad1"]] <- combine_results(results$gedad1, "rel")
sheets[["Relative by Edad Groups"]] <- combine_results(results$edad_groups, "rel")
sheets[["Relative by Relab"]] <- combine_results(results$relab, "rel")
sheets[["Relative by Urbano"]] <- combine_results(results$urbano, "rel")
sheets[["Relative by Skill Level"]] <- combine_results(results$skill, "rel")

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
