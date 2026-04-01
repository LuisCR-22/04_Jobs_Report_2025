# Youth Labor Market Analysis: 2016-2024 
# -----------------------------------------------------------------------------
# Author: Luis Castellanos
# Last modification: 2025-04-28

# Clean environment and load packages
rm(list = ls())
cat("\014")  # Clear console

# Define sector and relab label mappings
sector_labels <- c(
  "1" = "Primary Activities",
  "2" = "Manufacturing",
  "3" = "Manufacturing",
  "4" = "Construction",
  "5" = "Retail and Wholesale, Restaurants, Hotels, Repairs",
  "6" = "Electricity, Gas, Water, Transportation, Communications",
  "7" = "Banks, Finance, Insurance, Professional Services",
  "8" = "Public Administration and Defence",
  "9" = "Education, Health, Personal Services",
  "10" = "Domestic Services"
)

relab_labels <- c(
  "1" = "Employeer",
  "2" = "Salaried employee",
  "3" = "Self-employed",
  "4" = "Unpaid worker",
  "5" = "Unemployed"
)

# Start timer for performance tracking
start_time <- Sys.time()

# Install (if needed) and load required packages
required_pkgs <- c("haven", "dplyr", "tidyr", "writexl", "stringr", "lubridate")
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
output_file <- file.path(output_dir, paste0("08_", current_date, "-Youth-LM-2016-2024.xlsx"))

# Define countries and years to analyze
countries_to_include <- c("arg", "bol", "bra", "chl", "cri", "mex", "per")
years_to_include <- c(2016, 2024)
quarters_to_include <- c(2)  # Q2 for both years

# Create mapping for country codes to full names
country_names <- list(
  arg = "Argentina",
  bol = "Bolivia",
  bra = "Brazil",
  chl = "Chile",
  cri = "Costa Rica",
  mex = "Mexico",
  per = "Peru"
)

# Define the sector mapping for Chile from sector1d to sector
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

# Function to list files for processing
list_files_to_process <- function() {
  # List all .dta files in the directory
  all_files <- list.files(path = data_dir, pattern = "\\.dta$", full.names = TRUE)
  
  files_to_process <- c()
  
  for(country_code in countries_to_include) {
    for(year in years_to_include) {
      for(quarter in quarters_to_include) {
        # Format quarter with leading zero
        quarter_str <- sprintf("%02d", quarter)
        
        # Create regex pattern to match files
        pattern <- sprintf("LABLAC_%s_%d_q%s_.*\\.dta$", country_code, year, quarter_str)
        
        # Find matching files
        matches <- grep(pattern, basename(all_files), value = TRUE)
        
        if(length(matches) > 0) {
          # Add the first matching file to the list
          files_to_process <- c(files_to_process, file.path(data_dir, matches[1]))
        } else {
          cat("⚠️ Warning: No file found for", country_code, year, "Q", quarter, "\n")
        }
      }
    }
  }
  
  return(files_to_process)
}

# Helper function to calculate basic labor market rates for youth
calculate_youth_rates <- function(data) {
  # Youth filter (15-24 years old)
  youth_filter <- data$edad >= 15 & data$edad <= 24
  youth_data <- data[youth_filter, ]
  
  # Total weighted youth population
  youth_total <- sum(youth_data$pondera, na.rm = TRUE)
  
  # Youth in labor force
  youth_labor_force_filter <- !is.na(youth_data$pea) & youth_data$pea == 1
  youth_labor_force_total <- sum(youth_data$pondera[youth_labor_force_filter], na.rm = TRUE)
  
  # Employed youth
  youth_employed_filter <- !is.na(youth_data$ocupado) & youth_data$ocupado == 1
  youth_employed_total <- sum(youth_data$pondera[youth_employed_filter], na.rm = TRUE)
  
  # Unemployed youth
  youth_unemployed_filter <- !is.na(youth_data$desocupa) & youth_data$desocupa == 1 & youth_labor_force_filter
  youth_unemployed_total <- sum(youth_data$pondera[youth_unemployed_filter], na.rm = TRUE)
  
  # Youth outside labor force who are studying
  if("asiste" %in% names(youth_data)) {
    youth_inactives_filter <- !youth_labor_force_filter
    youth_inactives_total <- sum(youth_data$pondera[youth_inactives_filter], na.rm = TRUE)
    
    youth_studying_filter <- youth_inactives_filter & !is.na(youth_data$asiste) & youth_data$asiste == 1
    youth_studying_total <- sum(youth_data$pondera[youth_studying_filter], na.rm = TRUE)
    
    youth_studying_rate <- if(youth_inactives_total > 0) (youth_studying_total / youth_inactives_total) * 100 else NA
  } else {
    youth_studying_rate <- NA
  }
  
  # Calculate rates
  lfp_rate <- if(youth_total > 0) (youth_labor_force_total / youth_total) * 100 else NA
  employment_rate <- if(youth_total > 0) (youth_employed_total / youth_total) * 100 else NA
  unemployment_rate <- if(youth_labor_force_total > 0) (youth_unemployed_total / youth_labor_force_total) * 100 else NA
  
  return(list(
    lfp_rate = lfp_rate,
    employment_rate = employment_rate,
    unemployment_rate = unemployment_rate,
    studying_rate = youth_studying_rate,
    youth_employed_data = youth_data[youth_employed_filter, ],
    youth_total = youth_total,
    youth_employed_total = youth_employed_total
  ))
}

# Helper function to calculate employment distribution by relab
calculate_relab_distribution <- function(youth_employed_data) {
  if(!"relab" %in% names(youth_employed_data) || nrow(youth_employed_data) == 0) {
    return(NULL)
  }
  
  # Create labeled relab variable
  youth_employed_data$relab_labeled <- relab_labels[as.character(youth_employed_data$relab)]
  youth_employed_data$relab_labeled[is.na(youth_employed_data$relab_labeled)] <- paste("Category", youth_employed_data$relab[is.na(youth_employed_data$relab_labeled)])
  
  # Calculate distribution by relab
  relab_dist <- youth_employed_data %>%
    group_by(relab_labeled) %>%
    summarize(
      weighted_count = sum(pondera, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      percentage = (weighted_count / sum(weighted_count, na.rm = TRUE)) * 100
    ) %>%
    rename(relab = relab_labeled) %>%
    select(relab, percentage)
  
  return(relab_dist)
}

# Helper function to calculate employment distribution by sector
calculate_sector_distribution <- function(youth_employed_data, country_code) {
  # For Chile, use sector1d and map to sector categories
  if(country_code == "chl") {
    if(!"sector1d" %in% names(youth_employed_data) || nrow(youth_employed_data) == 0) {
      return(NULL)
    }
    
    # Map sector1d to sector categories
    youth_employed_data$mapped_sector <- sector_map[youth_employed_data$sector1d]
    
    # Calculate distribution by mapped sector
    sector_dist <- youth_employed_data %>%
      group_by(mapped_sector) %>%
      summarize(
        weighted_count = sum(pondera, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      mutate(
        percentage = (weighted_count / sum(weighted_count, na.rm = TRUE)) * 100
      ) %>%
      rename(sector = mapped_sector) %>%
      select(sector, percentage)
    
  } else {
    if(!"sector" %in% names(youth_employed_data) || nrow(youth_employed_data) == 0) {
      return(NULL)
    }
    
    # Create labeled sector variable
    youth_employed_data$sector_labeled <- sector_labels[as.character(youth_employed_data$sector)]
    youth_employed_data$sector_labeled[is.na(youth_employed_data$sector_labeled)] <- paste("Category", youth_employed_data$sector[is.na(youth_employed_data$sector_labeled)])
    
    # Calculate distribution by sector
    sector_dist <- youth_employed_data %>%
      group_by(sector_labeled) %>%
      summarize(
        weighted_count = sum(pondera, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      mutate(
        percentage = (weighted_count / sum(weighted_count, na.rm = TRUE)) * 100
      ) %>%
      rename(sector = sector_labeled) %>%
      select(sector, percentage)
  }
  
  return(sector_dist)
}

# Function to process a single file
process_file <- function(file_path) {
  # Extract file information from the filename
  filename <- basename(file_path)
  parts <- str_match(filename, "LABLAC_([a-z]{3})_(\\d{4})_q(\\d{2})_.*\\.dta$")
  
  if (is.na(parts[1])) {
    cat("⚠️ Warning: Filename doesn't match the expected pattern:", filename, "\n")
    return(NULL)
  }
  
  country_code <- parts[2]
  year <- as.numeric(parts[3])
  quarter <- as.numeric(parts[4])
  
  # Format the period as YYYY-QX
  period <- paste0(year, "-Q", quarter)
  
  # Get country name
  country_name <- country_names[[country_code]]
  if(is.null(country_name)) country_name <- country_code
  
  cat("Processing:", country_name, "for period", period, "\n")
  
  # Read dataset
  dat <- tryCatch({
    read_dta(file_path)
  }, error = function(e) {
    cat("⚠️ Error reading file:", file_path, " - ", e$message, "\n")
    return(NULL)
  })
  
  if(is.null(dat)) return(NULL)
  
  # Check if required variables exist
  required_vars <- c("pondera", "ocupado", "edad", "pea", "desocupa")
  missing_vars <- required_vars[!required_vars %in% names(dat)]
  
  if(length(missing_vars) > 0) {
    cat("⚠️ Warning: Missing required variables in", file_path, ":", paste(missing_vars, collapse=", "), "\n")
    return(NULL)
  }
  
  # For Peru, use only urban observations
  if(country_code == "per") {
    if("urbano" %in% names(dat)) {
      dat <- dat %>% filter(urbano == 1)
    } else {
      cat("⚠️ Warning: 'urbano' variable missing for Peru in", file_path, "\n")
      return(NULL)
    }
  }
  
  # Filter for working age population (15+)
  dat <- dat %>% filter(edad > 14)
  
  # Initialize results list
  results <- list()
  
  # Calculate youth labor market rates
  youth_rates <- calculate_youth_rates(dat)
  
  # Add labor force participation rate
  results[[length(results) + 1]] <- data.frame(
    Period = period,
    Indicator = "Labor Force Participation Rate",
    Country = country_name,
    Value = youth_rates$lfp_rate
  )
  
  # Add employment rate
  results[[length(results) + 1]] <- data.frame(
    Period = period,
    Indicator = "Employment Rate",
    Country = country_name,
    Value = youth_rates$employment_rate
  )
  
  # Add unemployment rate
  results[[length(results) + 1]] <- data.frame(
    Period = period,
    Indicator = "Unemployment Rate",
    Country = country_name,
    Value = youth_rates$unemployment_rate
  )
  
  # Add percentage of youth outside labor force who are studying
  results[[length(results) + 1]] <- data.frame(
    Period = period,
    Indicator = "Youth Outside Labor Force Studying",
    Country = country_name,
    Value = youth_rates$studying_rate
  )
  
  # Calculate and add employment distribution by relab
  relab_dist <- calculate_relab_distribution(youth_rates$youth_employed_data)
  if(!is.null(relab_dist)) {
    for(i in 1:nrow(relab_dist)) {
      results[[length(results) + 1]] <- data.frame(
        Period = period,
        Indicator = paste("Employed by relab -", relab_dist$relab[i]),
        Country = country_name,
        Value = relab_dist$percentage[i]
      )
    }
  }
  
  # Calculate and add employment distribution by sector
  sector_dist <- calculate_sector_distribution(youth_rates$youth_employed_data, country_code)
  if(!is.null(sector_dist)) {
    for(i in 1:nrow(sector_dist)) {
      results[[length(results) + 1]] <- data.frame(
        Period = period,
        Indicator = paste("Employed by sector -", sector_dist$sector[i]),
        Country = country_name,
        Value = sector_dist$percentage[i]
      )
    }
  }
  
  # Convert results list to data frame
  if(length(results) > 0) {
    results_df <- do.call(rbind, results)
    return(as.data.frame(results_df))
  } else {
    return(NULL)
  }
}

# Function to create second sheet with readable comparison table
create_comparison_table <- function(all_results) {
  if(nrow(all_results) == 0) return(NULL)
  
  # Extract unique countries
  countries <- unique(all_results$Country)
  
  # Initialize data frame for comparison table
  comparison <- data.frame(
    Indicator = character(),
    stringsAsFactors = FALSE
  )
  
  for(country in countries) {
    # Add columns for this country
    column_2016 <- paste0(country, " 2016-Q2")
    column_2024 <- paste0(country, " 2024-Q2")
    column_change <- paste0(country, " Change")
    
    comparison[[column_2016]] <- numeric()
    comparison[[column_2024]] <- numeric()
    comparison[[column_change]] <- numeric()
  }
  
  # List of main indicators (in desired order)
  main_indicators <- c(
    "Labor Force Participation Rate",
    "Employment Rate",
    "Unemployment Rate",
    "Youth Outside Labor Force Studying"
  )
  
  # Add rows for main indicators
  for(indicator in main_indicators) {
    row_data <- c(indicator)
    
    for(country in countries) {
      # Get 2016 value
      value_2016 <- all_results %>% 
        filter(Country == country, Indicator == indicator, Period == "2016-Q2") %>%
        pull(Value)
      
      # Get 2024 value
      value_2024 <- all_results %>% 
        filter(Country == country, Indicator == indicator, Period == "2024-Q2") %>%
        pull(Value)
      
      # Calculate change
      change <- if(length(value_2016) > 0 && length(value_2024) > 0) {
        value_2024 - value_2016
      } else {
        NA
      }
      
      # Add values to row
      row_data <- c(
        row_data, 
        if(length(value_2016) > 0) value_2016 else NA,
        if(length(value_2024) > 0) value_2024 else NA,
        change
      )
    }
    
    # Add row to comparison table
    comparison[nrow(comparison) + 1, ] <- row_data
  }
  
  # Find all relab categories
  relab_indicators <- unique(all_results$Indicator[grepl("Employed by relab -", all_results$Indicator)])
  
  # Add separator row
  comparison[nrow(comparison) + 1, ] <- c("Employment by relab", rep(NA, ncol(comparison) - 1))
  
  # Add rows for relab indicators
  for(indicator in sort(relab_indicators)) {
    # Extract category name
    category <- sub("Employed by relab - ", "", indicator)
    row_data <- c(paste("   ", category))  # Indent category name
    
    for(country in countries) {
      # Get 2016 value
      value_2016 <- all_results %>% 
        filter(Country == country, Indicator == indicator, Period == "2016-Q2") %>%
        pull(Value)
      
      # Get 2024 value
      value_2024 <- all_results %>% 
        filter(Country == country, Indicator == indicator, Period == "2024-Q2") %>%
        pull(Value)
      
      # Calculate change
      change <- if(length(value_2016) > 0 && length(value_2024) > 0) {
        value_2024 - value_2016
      } else {
        NA
      }
      
      # Add values to row
      row_data <- c(
        row_data, 
        if(length(value_2016) > 0) value_2016 else NA,
        if(length(value_2024) > 0) value_2024 else NA,
        change
      )
    }
    
    # Add row to comparison table
    comparison[nrow(comparison) + 1, ] <- row_data
  }
  
  # Find all sector categories
  sector_indicators <- unique(all_results$Indicator[grepl("Employed by sector -", all_results$Indicator)])
  
  # Add separator row
  comparison[nrow(comparison) + 1, ] <- c("Employment by sector", rep(NA, ncol(comparison) - 1))
  
  # Add rows for sector indicators
  for(indicator in sort(sector_indicators)) {
    # Extract category name
    category <- sub("Employed by sector - ", "", indicator)
    row_data <- c(paste("   ", category))  # Indent category name
    
    for(country in countries) {
      # Get 2016 value
      value_2016 <- all_results %>% 
        filter(Country == country, Indicator == indicator, Period == "2016-Q2") %>%
        pull(Value)
      
      # Get 2024 value
      value_2024 <- all_results %>% 
        filter(Country == country, Indicator == indicator, Period == "2024-Q2") %>%
        pull(Value)
      
      # Calculate change
      change <- if(length(value_2016) > 0 && length(value_2024) > 0) {
        value_2024 - value_2016
      } else {
        NA
      }
      
      # Add values to row
      row_data <- c(
        row_data, 
        if(length(value_2016) > 0) value_2016 else NA,
        if(length(value_2024) > 0) value_2024 else NA,
        change
      )
    }
    
    # Add row to comparison table
    comparison[nrow(comparison) + 1, ] <- row_data
  }
  
  return(comparison)
}

# Main processing function
process_all_files <- function() {
  # Get files to process
  files_to_process <- list_files_to_process()
  
  if(length(files_to_process) == 0) {
    cat("No files found for processing.\n")
    return(NULL)
  }
  
  cat("Found", length(files_to_process), "files to process.\n")
  
  # Initialize dataframe to store all results
  all_results <- data.frame()
  
  # Process each file
  for(file in files_to_process) {
    file_results <- process_file(file)
    
    if(!is.null(file_results)) {
      all_results <- bind_rows(all_results, file_results)
    }
  }
  
  # Create comparison table for second sheet
  comparison_table <- create_comparison_table(all_results)
  
  return(list(
    all_results = all_results,
    comparison_table = comparison_table
  ))
}

# Create methodological notes
create_methodological_notes <- function() {
  notes <- data.frame(
    Indicator = character(),
    Description = character(),
    Formula = character(),
    Notes = character(),
    stringsAsFactors = FALSE
  )
  
  # Labor Force Participation Rate
  notes[nrow(notes) + 1, ] <- c(
    "Labor Force Participation Rate",
    "Percentage of youth population (15-24) that is economically active (either employed or unemployed).",
    "LFPR = (Labor Force / Total Youth Population) * 100",
    "Uses 'pea' variable to identify labor force participants."
  )
  
  # Employment Rate
  notes[nrow(notes) + 1, ] <- c(
    "Employment Rate",
    "Percentage of youth population (15-24) that is employed.",
    "ER = (Employed Youth / Total Youth Population) * 100",
    "Uses 'ocupado' variable to identify employed youth."
  )
  
  # Unemployment Rate
  notes[nrow(notes) + 1, ] <- c(
    "Unemployment Rate",
    "Percentage of youth labor force (15-24) that is unemployed.",
    "UR = (Unemployed Youth / Youth Labor Force) * 100",
    "Uses 'desocupa' variable to identify unemployed youth."
  )
  
  # Youth Outside Labor Force Studying
  notes[nrow(notes) + 1, ] <- c(
    "Youth Outside Labor Force Studying",
    "Percentage of youth outside the labor force who are studying.",
    "Study Rate = (Youth outside LF studying / Total Youth outside LF) * 100",
    "Uses 'asiste' variable to identify youth who are studying."
  )
  
  # Employment by relab
  notes[nrow(notes) + 1, ] <- c(
    "Employment by relab",
    "Distribution of employed youth (15-24) by employment relationship (employer, employee, self-employed, etc.).",
    "Category % = (Employed Youth in Category / Total Employed Youth) * 100",
    "Uses 'relab' variable with the following categories: 1=Employeer, 2=Salaried employee, 3=Self-employed, 4=Unpaid worker, 5=Unemployed."
  )
  
  # Employment by sector
  notes[nrow(notes) + 1, ] <- c(
    "Employment by sector",
    "Distribution of employed youth (15-24) by economic sector.",
    "Sector % = (Employed Youth in Sector / Total Employed Youth) * 100",
    "Uses 'sector' variable for most countries and 'sector1d' mapped to standardized categories for Chile."
  )
  
  return(notes)
}

# Run the processing and save results
cat("\n=============================\n")
cat("STARTING YOUTH LABOR MARKET ANALYSIS 2016-2024\n")
cat("=============================\n")

results <- process_all_files()

if(!is.null(results) && nrow(results$all_results) > 0) {
  # Display summary information
  cat("\nProcessing complete.\n")
  cat("Total observations:", nrow(results$all_results), "\n")
  cat("Countries:", paste(unique(results$all_results$Country), collapse=", "), "\n")
  cat("Indicators:", length(unique(results$all_results$Indicator)), "different indicators\n")
  cat("Time span:", paste(min(results$all_results$Period), "to", max(results$all_results$Period)), "\n")
  
  # Create methodological notes
  methodological_notes <- create_methodological_notes()
  
  # Save to Excel
  sheets <- list(
    "Data" = results$all_results, 
    "Comparison" = results$comparison_table,
    "Methodological Notes" = methodological_notes
  )
  write_xlsx(sheets, output_file)
  cat("\nResults saved to:", output_file, "\n")
} else {
  cat("\nNo results were generated.\n")
}

# Calculate and display running time
end_time <- Sys.time()
elapsed <- end_time - start_time
cat("\nTotal run time:", format(elapsed), "\n")

cat("\n=============================\n")
cat("ANALYSIS COMPLETE\n")
cat("=============================\n")
