# Multi-Country Labor Market Analysis (2016-2024) 
# Analyzes labor force data for multiple countries with specified time periods
# This script analyzes job growth trends in Latin American countries by calculating annualized growth rates between key periods (2016-2024), breaking down employment and earnings changes by skill level, gender, and economic sector, handling country-specific data limitations, and producing standardized outputs for cross-country comparison.
# Author: Luis Castellanos - Stats Team (lcasterodr@worldbank.org)
# Last modification: 2022-03-29
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
# -----------------------------------------------------------------------------


# Clean environment and load packages
rm(list = ls())
cat("\014")  # Clear console

# Install (if needed) and load required packages
required_pkgs <- c("haven", "dplyr", "writexl", "stats", "stringr", "purrr", "tidyr")
for(pkg in required_pkgs) {
  if (!require(pkg, character.only = TRUE)) {
    install.packages(pkg, dependencies = TRUE)
    library(pkg, character.only = TRUE)
  }
}

# Define data directory path (same as original)
data_dir <- "<<DATALIBWEB_PATH_OR_GENERIC_URL>>"

# Define output directory path (same as original)
output_dir <- "<<DATALIBWEB_PATH_OR_GENERIC_URL>>"

# Function to calculate weighted mean with improved error handling
calculate_weighted_mean <- function(x, w) {
  valid <- !is.na(x) & !is.na(w)
  if(sum(valid) == 0) return(NA)
  return(weighted.mean(x[valid], w[valid], na.rm = TRUE))
}

# Function to get the skill level label
get_skill_level <- function(nivel) {
  case_when(
    nivel %in% c(0, 1, 2, 3) ~ "Low",
    nivel %in% c(4, 5) ~ "Middle",
    nivel == 6 ~ "High",
    TRUE ~ NA_character_
  )
}

# Function to get gender label
get_gender <- function(hombre) {
  case_when(
    hombre == 1 ~ "Male",
    hombre == 0 ~ "Female",
    TRUE ~ NA_character_
  )
}

# Function to calculate annualized change rate
calculate_annualized_change <- function(start_value, end_value, years) {
  if(is.na(start_value) || is.na(end_value) || start_value == 0) return(NA)
  return(((end_value / start_value) ^ (1 / years) - 1) * 100)
}

# Function to convert quarter specification to year fraction
quarter_to_year <- function(period) {
  year <- as.numeric(substr(period, 1, 4))
  quarter <- as.numeric(substr(period, 8, 8))
  return(year + (quarter - 1) / 4)
}

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

# Define country configurations for analysis
country_configs <- list(
  arg = list(start_year = 2016, start_quarter = 2, end_year = 2024, end_quarter = 2, sector_var = "sector"),
  bol = list(start_year = 2016, start_quarter = 2, end_year = 2024, end_quarter = 2, sector_var = "sector"),
  bra = list(start_year = 2016, start_quarter = 2, end_year = 2024, end_quarter = 2, sector_var = "sector"),
  chl = list(start_year = 2016, start_quarter = 4, end_year = 2023, end_quarter = 4, sector_var = "sector1d"),
  col = list(start_year = 2021, start_quarter = 4, end_year = 2023, end_quarter = 4, sector_var = "sector"),
  cri = list(start_year = 2017, start_quarter = 4, end_year = 2024, end_quarter = 4, sector_var = "sector"),
  dom = list(start_year = 2017, start_quarter = 2, end_year = 2024, end_quarter = 2, sector_var = "sector"),
  ecu = list(start_year = 2021, start_quarter = 2, end_year = 2024, end_quarter = 2, sector_var = "sector"),
  mex = list(start_year = 2016, start_quarter = 2, end_year = 2024, end_quarter = 2, sector_var = "sector"),
  pry = list(start_year = 2022, start_quarter = 2, end_year = 2024, end_quarter = 2, sector_var = "sector"),
  per = list(start_year = 2016, start_quarter = 2, end_year = 2024, end_quarter = 2, sector_var = "sector"),
  slv = list(start_year = 2016, start_quarter = 2, end_year = 2023, end_quarter = 2, sector_var = "sector"),
  ury = list(start_year = 2022, start_quarter = 2, end_year = 2024, end_quarter = 2, sector_var = "sector")
)

# Function to process a dataset and return analysis data
process_dataset <- function(file, period_label, sector_var = "sector", country_code = "") {
  cat("Processing file:", basename(file), "\n")
  
  # Read the data file
  dat <- tryCatch({
    read_dta(file)
  }, error = function(e) {
    cat("⚠️ Error reading file:", file, "-", e$message, "\n")
    return(NULL)
  })
  
  if(is.null(dat)) return(NULL)
  
  # Check for required variables - using the appropriate sector variable
  required_vars <- c("pondera", "ocupado", "edad", "cohi", "nivel", "hombre", sector_var, "ila_ppp17", "wage_ppp17")
  if(!all(required_vars %in% names(dat))) {
    missing_vars <- required_vars[!required_vars %in% names(dat)]
    cat("⚠️ Missing required variables in", basename(file), ":", paste(missing_vars, collapse=", "), "\n")
    return(NULL)
  }
  
  # Apply only age filter, not cohi filter for worker calculations
  dat <- dat %>% filter(edad > 14)
  
  if(nrow(dat) == 0) {
    cat("⚠️ No observations after applying base filters in", basename(file), "\n")
    return(NULL)
  }
  
  # Further filter to workers only (ocupado == 1)
  workers <- dat %>% filter(ocupado == 1)
  
  # Create a separate dataset with cohi filter for income calculations
  income_workers <- workers %>% filter(cohi == 1)
  
  # Free memory by removing the original dataset
  rm(dat)
  gc()
  
  if(nrow(workers) == 0) {
    cat("⚠️ No workers (ocupado == 1) found in", basename(file), "\n")
    return(NULL)
  }
  
  # Add skill level and gender classification
  workers <- workers %>% 
    mutate(
      skill_level = get_skill_level(nivel),
      gender = get_gender(hombre)
    )
  
  income_workers <- income_workers %>% 
    mutate(
      skill_level = get_skill_level(nivel),
      gender = get_gender(hombre)
    )
  
  # Special handling for Chile: Map sector1d to sector
  if(country_code == "chl" && sector_var == "sector1d") {
    cat("Applying special sector mapping for Chile...\n")
    
    # Get sector1d labels
    sector1d_labels <- attr(workers[[sector_var]], "labels")
    
    # Create a function to map sector1d labels to sector
    map_sector <- function(sector1d_value) {
      if(is.na(sector1d_value)) return(NA)
      
      # Get the sector1d label
      sector1d_label <- names(sector1d_labels)[sector1d_labels == sector1d_value]
      if(length(sector1d_label) == 0) return(NA)
      
      # Map to the corresponding sector
      return(sector_map[sector1d_label])
    }
    
    # Apply the mapping to create a new sector variable
    workers$sector_value <- sapply(workers[[sector_var]], map_sector)
    income_workers$sector_value <- sapply(income_workers[[sector_var]], map_sector)
  } else {
    # For other countries, use the sector variable directly
    workers$sector_value <- workers[[sector_var]]
    income_workers$sector_value <- income_workers[[sector_var]]
  }
  
  # Get sector labels
  if(country_code == "chl") {
    # For Chile, use the mapped sector names directly
    sector_values <- unique(workers$sector_value[!is.na(workers$sector_value)])
    sector_labels <- setNames(sector_values, sector_values)
  } else {
    # For other countries, get sector labels from the sector variable
    sector_labels <- attr(workers$sector_value, "labels")
    if(is.null(sector_labels)) {
      # If labels aren't available, create numeric placeholders
      sector_labels <- workers %>% 
        filter(!is.na(sector_value)) %>% 
        distinct(sector_value) %>% 
        pull(sector_value)
      names(sector_labels) <- paste("Sector", sector_labels)
    }
  }
  
  # Create new 'sector_label' variable in both datasets
  workers <- workers %>%
    mutate(sector_label = case_when(
      !is.na(sector_value) ~ names(sector_labels)[match(sector_value, sector_labels)],
      TRUE ~ NA_character_
    ))
  
  income_workers <- income_workers %>%
    mutate(sector_label = case_when(
      !is.na(sector_value) ~ names(sector_labels)[match(sector_value, sector_labels)],
      TRUE ~ NA_character_
    ))
  
  # Merge "Industrias de Baja Tecnología" (sector code 2) with "Resto de Industria Manufacturera" (sector code 3)
  # into a new category called "industry"
  if(country_code != "chl") {  # Skip for Chile as it has a different mapping
    # Identify sectors with codes 2 and 3
    sector_2_label <- names(sector_labels)[sector_labels == 2]
    sector_3_label <- names(sector_labels)[sector_labels == 3]
    
    if(length(sector_2_label) > 0 && length(sector_3_label) > 0) {
      cat("Merging sector codes 2 and 3 into 'Industry'...\n")
      
      # Update sector_label for both workers and income_workers
      workers <- workers %>%
        mutate(sector_label = case_when(
          sector_label %in% c(sector_2_label, sector_3_label) ~ "Industry",
          TRUE ~ sector_label
        ))
      
      income_workers <- income_workers %>%
        mutate(sector_label = case_when(
          sector_label %in% c(sector_2_label, sector_3_label) ~ "Industry",
          TRUE ~ sector_label
        ))
    }
  } else {
    # For Chile, we need to merge the two industrial categories based on the mapped names
    industry_categories <- c(
      "Industrias de Baja Tecnología (Industria Alimenticia, Bebidas y Tabaco, Textiles y Confecciones)",
      "Resto de Industria Manufacturera"
    )
    
    workers <- workers %>%
      mutate(sector_label = case_when(
        sector_label %in% industry_categories ~ "Industry",
        TRUE ~ sector_label
      ))
    
    income_workers <- income_workers %>%
      mutate(sector_label = case_when(
        sector_label %in% industry_categories ~ "Industry",
        TRUE ~ sector_label
      ))
  }
  
  # Update sector_value to match sector_label
  workers <- workers %>%
    group_by(sector_label) %>%
    mutate(new_sector_value = cur_group_id()) %>%
    ungroup() %>%
    mutate(sector_value = new_sector_value) %>%
    select(-new_sector_value)
  
  income_workers <- income_workers %>%
    group_by(sector_label) %>%
    mutate(new_sector_value = cur_group_id()) %>%
    ungroup() %>%
    mutate(sector_value = new_sector_value) %>%
    select(-new_sector_value)
  
  # Create results for this period
  results <- list()
  results$period <- period_label
  
  # 1. Calculate total weighted workers by skill level
  skill_results <- workers %>%
    filter(!is.na(skill_level)) %>%
    group_by(skill_level) %>%
    summarize(
      total_workers = sum(pondera, na.rm = TRUE),
      .groups = "drop"
    )
  
  # Calculate income metrics separately with cohi filter
  skill_income_results <- income_workers %>%
    filter(!is.na(skill_level)) %>%
    group_by(skill_level) %>%
    summarize(
      mean_ila = calculate_weighted_mean(ila_ppp17, pondera),
      mean_wage = calculate_weighted_mean(wage_ppp17, pondera),
      .groups = "drop"
    )
  
  # Combine results
  skill_results <- skill_results %>%
    left_join(skill_income_results, by = "skill_level")
  
  # 1a. Add total row for skill level
  total_workers_skill <- sum(skill_results$total_workers, na.rm = TRUE)
  
  total_skill <- data.frame(
    skill_level = "Total",
    total_workers = total_workers_skill,
    mean_ila = calculate_weighted_mean(income_workers$ila_ppp17[!is.na(income_workers$skill_level)], 
                                       income_workers$pondera[!is.na(income_workers$skill_level)]),
    mean_wage = calculate_weighted_mean(income_workers$wage_ppp17[!is.na(income_workers$skill_level)], 
                                        income_workers$pondera[!is.na(income_workers$skill_level)])
  )
  skill_results <- bind_rows(skill_results, total_skill)
  
  # 2. Calculate total weighted workers by gender
  gender_results <- workers %>%
    filter(!is.na(gender)) %>%
    group_by(gender) %>%
    summarize(
      total_workers = sum(pondera, na.rm = TRUE),
      .groups = "drop"
    )
  
  # Calculate income metrics separately with cohi filter
  gender_income_results <- income_workers %>%
    filter(!is.na(gender)) %>%
    group_by(gender) %>%
    summarize(
      mean_ila = calculate_weighted_mean(ila_ppp17, pondera),
      mean_wage = calculate_weighted_mean(wage_ppp17, pondera),
      .groups = "drop"
    )
  
  # Combine results
  gender_results <- gender_results %>%
    left_join(gender_income_results, by = "gender")
  
  # 2a. Add total row for gender
  total_workers_gender <- sum(gender_results$total_workers, na.rm = TRUE)
  
  total_gender <- data.frame(
    gender = "Total",
    total_workers = total_workers_gender,
    mean_ila = calculate_weighted_mean(income_workers$ila_ppp17[!is.na(income_workers$gender)], 
                                       income_workers$pondera[!is.na(income_workers$gender)]),
    mean_wage = calculate_weighted_mean(income_workers$wage_ppp17[!is.na(income_workers$gender)], 
                                        income_workers$pondera[!is.na(income_workers$gender)])
  )
  gender_results <- bind_rows(gender_results, total_gender)
  
  # 3. Calculate total weighted workers by sector using sector_label
  sector_results <- workers %>%
    filter(!is.na(sector_label)) %>%
    group_by(sector_label) %>%
    summarize(
      total_workers = sum(pondera, na.rm = TRUE),
      .groups = "drop"
    )
  
  # Calculate income metrics separately with cohi filter
  sector_income_results <- income_workers %>%
    filter(!is.na(sector_label)) %>%
    group_by(sector_label) %>%
    summarize(
      mean_ila = calculate_weighted_mean(ila_ppp17, pondera),
      mean_wage = calculate_weighted_mean(wage_ppp17, pondera),
      .groups = "drop"
    )
  
  # Combine results
  sector_results <- sector_results %>%
    left_join(sector_income_results, by = "sector_label")
  
  # Copy sector_label to sector_value for consistency with the rest of the code
  sector_results$sector_value <- match(sector_results$sector_label, unique(workers$sector_label))
  
  # 3b. Add total row for sector
  total_workers_sector <- sum(sector_results$total_workers, na.rm = TRUE)
  
  total_sector <- data.frame(
    sector_label = "Total",
    total_workers = total_workers_sector,
    mean_ila = calculate_weighted_mean(income_workers$ila_ppp17[!is.na(income_workers$sector_label)], 
                                       income_workers$pondera[!is.na(income_workers$sector_label)]),
    mean_wage = calculate_weighted_mean(income_workers$wage_ppp17[!is.na(income_workers$sector_label)], 
                                        income_workers$pondera[!is.na(income_workers$sector_label)]),
    sector_value = NA
  )
  sector_results <- bind_rows(sector_results, total_sector)
  
  # 4. Calculate results by gender and sector
  gender_sector_results <- workers %>%
    filter(!is.na(gender), !is.na(sector_label)) %>%
    group_by(gender, sector_label) %>%
    summarize(
      total_workers = sum(pondera, na.rm = TRUE),
      .groups = "drop"
    )
  
  # Add sector_value for joining
  gender_sector_results$sector_value <- match(gender_sector_results$sector_label, unique(workers$sector_label))
  
  # Calculate income metrics separately with cohi filter
  gender_sector_income_results <- income_workers %>%
    filter(!is.na(gender), !is.na(sector_label)) %>%
    group_by(gender, sector_label) %>%
    summarize(
      mean_ila = calculate_weighted_mean(ila_ppp17, pondera),
      mean_wage = calculate_weighted_mean(wage_ppp17, pondera),
      .groups = "drop"
    )
  
  # Add sector_value for joining
  gender_sector_income_results$sector_value <- match(gender_sector_income_results$sector_label, unique(income_workers$sector_label))
  
  # Combine results
  gender_sector_results <- gender_sector_results %>%
    left_join(gender_sector_income_results, by = c("gender", "sector_label", "sector_value"))
  
  # 5. Calculate results by gender and skill level
  gender_skill_results <- workers %>%
    filter(!is.na(gender), !is.na(skill_level)) %>%
    group_by(gender, skill_level) %>%
    summarize(
      total_workers = sum(pondera, na.rm = TRUE),
      .groups = "drop"
    )
  
  # Calculate income metrics separately with cohi filter
  gender_skill_income_results <- income_workers %>%
    filter(!is.na(gender), !is.na(skill_level)) %>%
    group_by(gender, skill_level) %>%
    summarize(
      mean_ila = calculate_weighted_mean(ila_ppp17, pondera),
      mean_wage = calculate_weighted_mean(wage_ppp17, pondera),
      .groups = "drop"
    )
  
  # Combine results
  gender_skill_results <- gender_skill_results %>%
    left_join(gender_skill_income_results, by = c("gender", "skill_level"))
  
  # Return all results
  return(list(
    skill = skill_results,
    gender = gender_results,
    sector = sector_results,
    gender_sector = gender_sector_results,
    gender_skill = gender_skill_results
  ))
}

# Function to create a formatted result dataframe with the period row
create_formatted_result <- function(df, id_col, country_code, period_text) {
  # Select the requested column and add the country code
  result_df <- df %>% 
    select(!!sym(id_col), annualized_change)
  
  # Rename columns
  names(result_df) <- c(id_col, country_code)
  
  # Convert numeric values to character to ensure consistent types when binding
  result_df[[country_code]] <- as.character(result_df[[country_code]])
  
  # Create a new row for the period
  period_row <- data.frame(matrix(ncol = ncol(result_df), nrow = 1))
  names(period_row) <- names(result_df)
  period_row[1, 1] <- "Period"
  period_row[1, 2] <- period_text
  
  # Combine the period row with the result
  bind_rows(period_row, result_df)
}

# Function to process a country
process_country <- function(country_code, config) {
  cat("\n=============================\n")
  cat(paste0("PROCESSING ", toupper(country_code), " DATA\n"))
  cat("=============================\n")
  
  # Create file patterns for start and end periods
  start_pattern <- sprintf("LABLAC_%s.*%d_q%02d.*\\.dta$", country_code, config$start_year, config$start_quarter)
  end_pattern <- sprintf("LABLAC_%s.*%d_q%02d.*\\.dta$", country_code, config$end_year, config$end_quarter)
  
  # List files for start and end periods
  start_files <- list.files(path = data_dir, pattern = start_pattern, full.names = TRUE)
  end_files <- list.files(path = data_dir, pattern = end_pattern, full.names = TRUE)
  
  # Check if files exist
  if(length(start_files) == 0) {
    cat(sprintf("⚠️ No %s %d Q%d files found. Skipping analysis.\n", 
                toupper(country_code), config$start_year, config$start_quarter))
    return(NULL)
  }
  
  if(length(end_files) == 0) {
    cat(sprintf("⚠️ No %s %d Q%d files found. Skipping analysis.\n", 
                toupper(country_code), config$end_year, config$end_quarter))
    return(NULL)
  }
  
  cat("Found", length(start_files), "files for", config$start_year, "Q", config$start_quarter, "\n")
  cat("Found", length(end_files), "files for", config$end_year, "Q", config$end_quarter, "\n")
  
  # Process the start and end datasets
  start_label <- sprintf("%d-Q%d", config$start_year, config$start_quarter)
  end_label <- sprintf("%d-Q%d", config$end_year, config$end_quarter)
  
  # Pass country_code to the process_dataset function
  results_start <- process_dataset(start_files[1], start_label, config$sector_var, country_code)
  results_end <- process_dataset(end_files[1], end_label, config$sector_var, country_code)
  
  if(is.null(results_start) || is.null(results_end)) {
    cat("⚠️ Unable to process one or both periods. Skipping analysis.\n")
    return(NULL)
  }
  
  # Calculate years between the two periods (could be fractional for different quarters)
  start_time <- config$start_year + (config$start_quarter - 1) / 4
  end_time <- config$end_year + (config$end_quarter - 1) / 4
  years_diff <- end_time - start_time
  
  # Create period text for the output
  period_text <- sprintf("%d-Q%d to %d-Q%d", 
                         config$start_year, config$start_quarter,
                         config$end_year, config$end_quarter)
  
  # 1. Annualized change in total workers by skill level
  workers_by_skill <- results_start$skill %>%
    select(skill_level, total_workers_start = total_workers) %>%
    left_join(
      results_end$skill %>% select(skill_level, total_workers_end = total_workers),
      by = "skill_level"
    ) %>%
    mutate(
      annualized_change = mapply(
        calculate_annualized_change, 
        total_workers_start, 
        total_workers_end, 
        MoreArgs = list(years = years_diff)
      )
    )
  workers_by_skill_formatted <- create_formatted_result(workers_by_skill, "skill_level", country_code, period_text)
  
  # 2. Annualized change in total workers by gender
  workers_by_gender <- results_start$gender %>%
    select(gender, total_workers_start = total_workers) %>%
    left_join(
      results_end$gender %>% select(gender, total_workers_end = total_workers),
      by = "gender"
    ) %>%
    mutate(
      annualized_change = mapply(
        calculate_annualized_change, 
        total_workers_start, 
        total_workers_end, 
        MoreArgs = list(years = years_diff)
      )
    )
  workers_by_gender_formatted <- create_formatted_result(workers_by_gender, "gender", country_code, period_text)
  
  # 3. Annualized change in total workers by sector
  workers_by_sector <- results_start$sector %>%
    select(sector_label, total_workers_start = total_workers) %>%
    left_join(
      results_end$sector %>% select(sector_label, total_workers_end = total_workers),
      by = "sector_label"
    ) %>%
    mutate(
      annualized_change = mapply(
        calculate_annualized_change, 
        total_workers_start, 
        total_workers_end, 
        MoreArgs = list(years = years_diff)
      )
    )
  workers_by_sector_formatted <- create_formatted_result(workers_by_sector, "sector_label", country_code, period_text)
  
  # 4. Annualized change in mean ila_ppp17 by skill level
  ila_by_skill <- results_start$skill %>%
    select(skill_level, ila_start = mean_ila) %>%
    left_join(
      results_end$skill %>% select(skill_level, ila_end = mean_ila),
      by = "skill_level"
    ) %>%
    mutate(
      annualized_change = mapply(
        calculate_annualized_change, 
        ila_start, 
        ila_end, 
        MoreArgs = list(years = years_diff)
      )
    )
  ila_by_skill_formatted <- create_formatted_result(ila_by_skill, "skill_level", country_code, period_text)
  
  # 5. Annualized change in mean ila_ppp17 by gender
  ila_by_gender <- results_start$gender %>%
    select(gender, ila_start = mean_ila) %>%
    left_join(
      results_end$gender %>% select(gender, ila_end = mean_ila),
      by = "gender"
    ) %>%
    mutate(
      annualized_change = mapply(
        calculate_annualized_change, 
        ila_start, 
        ila_end, 
        MoreArgs = list(years = years_diff)
      )
    )
  ila_by_gender_formatted <- create_formatted_result(ila_by_gender, "gender", country_code, period_text)
  
  # 6. Annualized change in mean ila_ppp17 by sector
  ila_by_sector <- results_start$sector %>%
    select(sector_label, ila_start = mean_ila) %>%
    left_join(
      results_end$sector %>% select(sector_label, ila_end = mean_ila),
      by = "sector_label"
    ) %>%
    mutate(
      annualized_change = mapply(
        calculate_annualized_change, 
        ila_start, 
        ila_end, 
        MoreArgs = list(years = years_diff)
      )
    )
  ila_by_sector_formatted <- create_formatted_result(ila_by_sector, "sector_label", country_code, period_text)
  
  # 7. Annualized change in mean wage_ppp17 by skill level
  wage_by_skill <- results_start$skill %>%
    select(skill_level, wage_start = mean_wage) %>%
    left_join(
      results_end$skill %>% select(skill_level, wage_end = mean_wage),
      by = "skill_level"
    ) %>%
    mutate(
      annualized_change = mapply(
        calculate_annualized_change, 
        wage_start, 
        wage_end, 
        MoreArgs = list(years = years_diff)
      )
    )
  wage_by_skill_formatted <- create_formatted_result(wage_by_skill, "skill_level", country_code, period_text)
  
  # 8. Annualized change in mean wage_ppp17 by gender
  wage_by_gender <- results_start$gender %>%
    select(gender, wage_start = mean_wage) %>%
    left_join(
      results_end$gender %>% select(gender, wage_end = mean_wage),
      by = "gender"
    ) %>%
    mutate(
      annualized_change = mapply(
        calculate_annualized_change, 
        wage_start, 
        wage_end, 
        MoreArgs = list(years = years_diff)
      )
    )
  wage_by_gender_formatted <- create_formatted_result(wage_by_gender, "gender", country_code, period_text)
  
  # 9. Annualized change in mean wage_ppp17 by sector
  wage_by_sector <- results_start$sector %>%
    select(sector_label, wage_start = mean_wage) %>%
    left_join(
      results_end$sector %>% select(sector_label, wage_end = mean_wage),
      by = "sector_label"
    ) %>%
    mutate(
      annualized_change = mapply(
        calculate_annualized_change, 
        wage_start, 
        wage_end, 
        MoreArgs = list(years = years_diff)
      )
    )
  wage_by_sector_formatted <- create_formatted_result(wage_by_sector, "sector_label", country_code, period_text)
  
  # 10. Workers by sector (Males only)
  male_workers_by_sector <- results_start$gender_sector %>%
    filter(gender == "Male") %>%
    select(sector_label, total_workers_start = total_workers) %>%
    left_join(
      results_end$gender_sector %>% 
        filter(gender == "Male") %>%
        select(sector_label, total_workers_end = total_workers),
      by = "sector_label"
    ) %>%
    mutate(
      annualized_change = mapply(
        calculate_annualized_change, 
        total_workers_start, 
        total_workers_end, 
        MoreArgs = list(years = years_diff)
      )
    )
  male_workers_by_sector_formatted <- create_formatted_result(
    male_workers_by_sector, "sector_label", country_code, period_text)
  
  # 11. Workers by sector (Females only)
  female_workers_by_sector <- results_start$gender_sector %>%
    filter(gender == "Female") %>%
    select(sector_label, total_workers_start = total_workers) %>%
    left_join(
      results_end$gender_sector %>% 
        filter(gender == "Female") %>%
        select(sector_label, total_workers_end = total_workers),
      by = "sector_label"
    ) %>%
    mutate(
      annualized_change = mapply(
        calculate_annualized_change, 
        total_workers_start, 
        total_workers_end, 
        MoreArgs = list(years = years_diff)
      )
    )
  female_workers_by_sector_formatted <- create_formatted_result(
    female_workers_by_sector, "sector_label", country_code, period_text)
  
  # 12. Earnings by sector (Males only)
  male_ila_by_sector <- results_start$gender_sector %>%
    filter(gender == "Male") %>%
    select(sector_label, ila_start = mean_ila) %>%
    left_join(
      results_end$gender_sector %>% 
        filter(gender == "Male") %>%
        select(sector_label, ila_end = mean_ila),
      by = "sector_label"
    ) %>%
    mutate(
      annualized_change = mapply(
        calculate_annualized_change, 
        ila_start, 
        ila_end, 
        MoreArgs = list(years = years_diff)
      )
    )
  male_ila_by_sector_formatted <- create_formatted_result(
    male_ila_by_sector, "sector_label", country_code, period_text)
  
  # 13. Earnings by sector (Females only)
  female_ila_by_sector <- results_start$gender_sector %>%
    filter(gender == "Female") %>%
    select(sector_label, ila_start = mean_ila) %>%
    left_join(
      results_end$gender_sector %>% 
        filter(gender == "Female") %>%
        select(sector_label, ila_end = mean_ila),
      by = "sector_label"
    ) %>%
    mutate(
      annualized_change = mapply(
        calculate_annualized_change, 
        ila_start, 
        ila_end, 
        MoreArgs = list(years = years_diff)
      )
    )
  female_ila_by_sector_formatted <- create_formatted_result(
    female_ila_by_sector, "sector_label", country_code, period_text)
  
  # 14. Wages by sector (Males only)
  male_wage_by_sector <- results_start$gender_sector %>%
    filter(gender == "Male") %>%
    select(sector_label, wage_start = mean_wage) %>%
    left_join(
      results_end$gender_sector %>% 
        filter(gender == "Male") %>%
        select(sector_label, wage_end = mean_wage),
      by = "sector_label"
    ) %>%
    mutate(
      annualized_change = mapply(
        calculate_annualized_change, 
        wage_start, 
        wage_end, 
        MoreArgs = list(years = years_diff)
      )
    )
  male_wage_by_sector_formatted <- create_formatted_result(
    male_wage_by_sector, "sector_label", country_code, period_text)
  
  # 15. Wages by sector (Females only)
  female_wage_by_sector <- results_start$gender_sector %>%
    filter(gender == "Female") %>%
    select(sector_label, wage_start = mean_wage) %>%
    left_join(
      results_end$gender_sector %>% 
        filter(gender == "Female") %>%
        select(sector_label, wage_end = mean_wage),
      by = "sector_label"
    ) %>%
    mutate(
      annualized_change = mapply(
        calculate_annualized_change, 
        wage_start, 
        wage_end, 
        MoreArgs = list(years = years_diff)
      )
    )
  female_wage_by_sector_formatted <- create_formatted_result(
    female_wage_by_sector, "sector_label", country_code, period_text)
  
  # 16. Workers by skill (Males only)
  male_workers_by_skill <- results_start$gender_skill %>%
    filter(gender == "Male") %>%
    select(skill_level, total_workers_start = total_workers) %>%
    left_join(
      results_end$gender_skill %>% 
        filter(gender == "Male") %>%
        select(skill_level, total_workers_end = total_workers),
      by = "skill_level"
    ) %>%
    mutate(
      annualized_change = mapply(
        calculate_annualized_change, 
        total_workers_start, 
        total_workers_end, 
        MoreArgs = list(years = years_diff)
      )
    )
  male_workers_by_skill_formatted <- create_formatted_result(
    male_workers_by_skill, "skill_level", country_code, period_text)
  
  # 17. Workers by skill (Females only)
  female_workers_by_skill <- results_start$gender_skill %>%
    filter(gender == "Female") %>%
    select(skill_level, total_workers_start = total_workers) %>%
    left_join(
      results_end$gender_skill %>% 
        filter(gender == "Female") %>%
        select(skill_level, total_workers_end = total_workers),
      by = "skill_level"
    ) %>%
    mutate(
      annualized_change = mapply(
        calculate_annualized_change, 
        total_workers_start, 
        total_workers_end, 
        MoreArgs = list(years = years_diff)
      )
    )
  female_workers_by_skill_formatted <- create_formatted_result(
    female_workers_by_skill, "skill_level", country_code, period_text)
  
  # 18. Earnings by skill (Males only)
  male_ila_by_skill <- results_start$gender_skill %>%
    filter(gender == "Male") %>%
    select(skill_level, ila_start = mean_ila) %>%
    left_join(
      results_end$gender_skill %>% 
        filter(gender == "Male") %>%
        select(skill_level, ila_end = mean_ila),
      by = "skill_level"
    ) %>%
    mutate(
      annualized_change = mapply(
        calculate_annualized_change, 
        ila_start, 
        ila_end, 
        MoreArgs = list(years = years_diff)
      )
    )
  male_ila_by_skill_formatted <- create_formatted_result(
    male_ila_by_skill, "skill_level", country_code, period_text)
  
  # 19. Earnings by skill (Females only)
  female_ila_by_skill <- results_start$gender_skill %>%
    filter(gender == "Female") %>%
    select(skill_level, ila_start = mean_ila) %>%
    left_join(
      results_end$gender_skill %>% 
        filter(gender == "Female") %>%
        select(skill_level, ila_end = mean_ila),
      by = "skill_level"
    ) %>%
    mutate(
      annualized_change = mapply(
        calculate_annualized_change, 
        ila_start, 
        ila_end, 
        MoreArgs = list(years = years_diff)
      )
    )
  female_ila_by_skill_formatted <- create_formatted_result(
    female_ila_by_skill, "skill_level", country_code, period_text)
  
  # 20. Wages by skill (Males only)
  male_wage_by_skill <- results_start$gender_skill %>%
    filter(gender == "Male") %>%
    select(skill_level, wage_start = mean_wage) %>%
    left_join(
      results_end$gender_skill %>% 
        filter(gender == "Male") %>%
        select(skill_level, wage_end = mean_wage),
      by = "skill_level"
    ) %>%
    mutate(
      annualized_change = mapply(
        calculate_annualized_change, 
        wage_start, 
        wage_end, 
        MoreArgs = list(years = years_diff)
      )
    )
  male_wage_by_skill_formatted <- create_formatted_result(
    male_wage_by_skill, "skill_level", country_code, period_text)
  
  # 21. Wages by skill (Females only)
  female_wage_by_skill <- results_start$gender_skill %>%
    filter(gender == "Female") %>%
    select(skill_level, wage_start = mean_wage) %>%
    left_join(
      results_end$gender_skill %>% 
        filter(gender == "Female") %>%
        select(skill_level, wage_end = mean_wage),
      by = "skill_level"
    ) %>%
    mutate(
      annualized_change = mapply(
        calculate_annualized_change, 
        wage_start, 
        wage_end, 
        MoreArgs = list(years = years_diff)
      )
    )
  female_wage_by_skill_formatted <- create_formatted_result(
    female_wage_by_skill, "skill_level", country_code, period_text)
  
  # 22. Detailed values table
  # Prepare detailed table with all values for skill level
  detailed_skill <- results_start$skill %>%
    select(Skill_Level = skill_level, 
           Workers_Start = total_workers, 
           Earnings_Start = mean_ila, 
           Wage_Start = mean_wage) %>%
    left_join(
      results_end$skill %>% 
        select(Skill_Level = skill_level, 
               Workers_End = total_workers, 
               Earnings_End = mean_ila, 
               Wage_End = mean_wage),
      by = "Skill_Level"
    ) %>%
    mutate(Country = toupper(country_code), Period = period_text)
  
  # Prepare detailed table with all values for gender
  detailed_gender <- results_start$gender %>%
    select(Gender = gender, 
           Workers_Start = total_workers, 
           Earnings_Start = mean_ila, 
           Wage_Start = mean_wage) %>%
    left_join(
      results_end$gender %>% 
        select(Gender = gender, 
               Workers_End = total_workers, 
               Earnings_End = mean_ila, 
               Wage_End = mean_wage),
      by = "Gender"
    ) %>%
    mutate(Country = toupper(country_code), Period = period_text)
  
  # Prepare detailed table with all values for sector
  detailed_sector <- results_start$sector %>%
    select(Sector = sector_label, 
           Workers_Start = total_workers, 
           Earnings_Start = mean_ila, 
           Wage_Start = mean_wage) %>%
    left_join(
      results_end$sector %>% 
        select(Sector = sector_label, 
               Workers_End = total_workers, 
               Earnings_End = mean_ila, 
               Wage_End = mean_wage),
      by = "Sector"
    ) %>%
    mutate(Country = toupper(country_code), Period = period_text)
  
  # Combine all detailed tables
  detailed_skill_with_group <- detailed_skill %>% 
    mutate(Group_Type = "Skill Level", Group = Skill_Level)
  
  detailed_gender_with_group <- detailed_gender %>% 
    mutate(Group_Type = "Gender", Group = Gender)
  
  detailed_sector_with_group <- detailed_sector %>% 
    mutate(Group_Type = "Sector", Group = Sector)
  
  detailed_values <- bind_rows(
    detailed_skill_with_group,
    detailed_gender_with_group,
    detailed_sector_with_group
  ) %>%
    select(Group_Type, Group, Country, Period,
           Workers_Start, Workers_End,
           Earnings_Start, Earnings_End,
           Wage_Start, Wage_End)
  
  # Return results for this country
  return(list(
    workers_by_skill = workers_by_skill_formatted,
    workers_by_gender = workers_by_gender_formatted,
    workers_by_sector = workers_by_sector_formatted,
    ila_by_skill = ila_by_skill_formatted,
    ila_by_gender = ila_by_gender_formatted,
    ila_by_sector = ila_by_sector_formatted,
    wage_by_skill = wage_by_skill_formatted,
    wage_by_gender = wage_by_gender_formatted,
    wage_by_sector = wage_by_sector_formatted,
    male_workers_by_sector = male_workers_by_sector_formatted,
    female_workers_by_sector = female_workers_by_sector_formatted,
    male_ila_by_sector = male_ila_by_sector_formatted,
    female_ila_by_sector = female_ila_by_sector_formatted,
    male_wage_by_sector = male_wage_by_sector_formatted,
    female_wage_by_sector = female_wage_by_sector_formatted,
    male_workers_by_skill = male_workers_by_skill_formatted,
    female_workers_by_skill = female_workers_by_skill_formatted,
    male_ila_by_skill = male_ila_by_skill_formatted,
    female_ila_by_skill = female_ila_by_skill_formatted,
    male_wage_by_skill = male_wage_by_skill_formatted,
    female_wage_by_skill = female_wage_by_skill_formatted,
    detailed_values = detailed_values
  ))
}

# Process all countries
all_results <- list()
for (country_code in names(country_configs)) {
  cat("\nProcessing country:", toupper(country_code), "\n")
  country_results <- process_country(country_code, country_configs[[country_code]])
  if (!is.null(country_results)) {
    all_results[[country_code]] <- country_results
    cat("✅ Successfully processed", toupper(country_code), "\n")
  } else {
    cat("⚠️ Failed to process", toupper(country_code), "\n")
  }
  
  # Clean up to free memory
  gc()
}

# Combine results across countries
combined_results <- list()

# Function to combine country results for a specific metric
combine_country_results <- function(metric_name) {
  # Get all non-null results for this metric
  valid_countries <- names(all_results)[sapply(all_results, function(x) !is.null(x[[metric_name]]))]
  
  if (length(valid_countries) == 0) {
    return(NULL)
  }
  
  # Get the name of the first column (ID column)
  first_country <- valid_countries[1]
  first_df <- all_results[[first_country]][[metric_name]]
  id_col_name <- names(first_df)[1]
  
  # Start with an empty dataframe with just the ID column
  result <- data.frame(id_column = character())
  names(result)[1] <- id_col_name
  
  # Add each country's data using full join to handle different row counts
  for (country in valid_countries) {
    country_data <- all_results[[country]][[metric_name]]
    
    # If result is empty, use the first country's data to initialize
    if (nrow(result) == 0) {
      result <- country_data
    } else {
      # Use full join to combine, keeping all rows from both datasets
      result <- full_join(
        result, 
        country_data, 
        by = id_col_name
      )
    }
  }
  
  return(result)
}

# Combine all metrics
metrics <- c(
  "workers_by_skill", "workers_by_gender", "workers_by_sector",
  "ila_by_skill", "ila_by_gender", "ila_by_sector",
  "wage_by_skill", "wage_by_gender", "wage_by_sector",
  "male_workers_by_sector", "female_workers_by_sector",
  "male_ila_by_sector", "female_ila_by_sector",
  "male_wage_by_sector", "female_wage_by_sector",
  "male_workers_by_skill", "female_workers_by_skill",
  "male_ila_by_skill", "female_ila_by_skill",
  "male_wage_by_skill", "female_wage_by_skill"
)

for (metric in metrics) {
  combined_results[[metric]] <- combine_country_results(metric)
}

# Combine detailed values
detailed_values_list <- lapply(all_results, function(x) x$detailed_values)
detailed_values_list <- detailed_values_list[!sapply(detailed_values_list, is.null)]
combined_results$detailed_values <- bind_rows(detailed_values_list)

# Create the output Excel file with V3 added to the name
today_date <- format(Sys.Date(), "%Y-%m-%d")
output_file <- file.path(output_dir, paste0(today_date, "-labor-market-analysis-V3.xlsx"))

# Ensure output directory exists
dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

# Create Excel workbook with numbered sheets
sheets <- list(
  "1. Workers by Skill" = combined_results$workers_by_skill,
  "2. Workers by Gender" = combined_results$workers_by_gender,
  "3. Workers by Sector" = combined_results$workers_by_sector,
  "4. Earnings by Skill" = combined_results$ila_by_skill,
  "5. Earnings by Gender" = combined_results$ila_by_gender,
  "6. Earnings by Sector" = combined_results$ila_by_sector,
  "7. Wages by Skill" = combined_results$wage_by_skill,
  "8. Wages by Gender" = combined_results$wage_by_gender,
  "9. Wages by Sector" = combined_results$wage_by_sector,
  "10. Workers by Sector (Men)" = combined_results$male_workers_by_sector,
  "11. Workers by Sector (Women)" = combined_results$female_workers_by_sector,
  "12. Earnings by Sector (Men)" = combined_results$male_ila_by_sector,
  "13. Earnings by Sector (Women)" = combined_results$female_ila_by_sector,
  "14. Wages by Sector (Men)" = combined_results$male_wage_by_sector,
  "15. Wages by Sector (Women)" = combined_results$female_wage_by_sector,
  "16. Workers by Skill (Men)" = combined_results$male_workers_by_skill,
  "17. Workers by Skill (Women)" = combined_results$female_workers_by_skill,
  "18. Earnings by Skill (Men)" = combined_results$male_ila_by_skill,
  "19. Earnings by Skill (Women)" = combined_results$female_ila_by_skill,
  "20. Wages by Skill (Men)" = combined_results$male_wage_by_skill,
  "21. Wages by Skill (Women)" = combined_results$female_wage_by_skill,
  "22. Detailed Values" = combined_results$detailed_values
)

# Remove any NULL sheets
sheets <- sheets[!sapply(sheets, is.null)]

write_xlsx(sheets, path = output_file)
cat("✅ Analysis saved to:", output_file, "\n")

cat("\n=============================\n")
cat("ANALYSIS COMPLETE\n")
cat("=============================\n")

# Clean up environment to free memory
rm(list = ls())
gc()