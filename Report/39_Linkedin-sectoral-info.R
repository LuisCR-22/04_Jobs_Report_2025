# Script para analizar datos de LinkedIn LHR por industria y región
# Basado en la fusión con un archivo de regiones

# 0. Limpiar espacio de trabajo y cargar bibliotecas necesarias
rm(list = ls())
gc()

# 1. Cargar bibliotecas
library(readxl)
library(dplyr)
library(tidyverse)
library(ggplot2)
library(openxlsx)  # Para guardar Excel con múltiples hojas
library(haven)     # Para leer archivos .dta de Stata

# 2. Definir rutas de archivos
linkedin_file <- "C:/Users/wb593225/OneDrive - WBG/Desktop/Luis - Private/Lablac/Semiannual report/Excel/Inputs/Linkedin/LinkedIn_LHR by Industry_Feb2025.xlsx"
regions_file <- "C:/Users/wb593225/OneDrive - WBG/Desktop/Luis - Private/Lablac/Semiannual report/dta/2025-03-24-regions.dta"

# Verificar que los archivos existan
if (!file.exists(linkedin_file)) {
  stop("El archivo de LinkedIn no existe en la ruta especificada: ", linkedin_file)
}

if (!file.exists(regions_file)) {
  stop("El archivo de regiones no existe en la ruta especificada: ", regions_file)
}

# 3. Cargar y preparar datos de LinkedIn - ahora usando la hoja por industria
LHR_industry <- read_excel(linkedin_file, sheet = "2B - LHR by Ctry, Ind")

# Aplicar el mismo procesamiento que en el script anterior
LHR_industry <- LHR_industry[-c(1:3),]
colnames(LHR_industry) <- LHR_industry[1,]
LHR_industry <- LHR_industry[-1,]
LHR_industry <- LHR_industry[,-1]  # Eliminando la primera columna como antes

# 4. Convertir fechas seriales de Excel correctamente
LHR_industry$Date_obj <- as.Date(as.numeric(LHR_industry$Month), origin = "1899-12-30")
LHR_industry$Date <- format(LHR_industry$Date_obj, "%Y-%m")  # Formato YYYY-MM para mejor ordenamiento
LHR_industry$Year <- substr(LHR_industry$Date, 1, 4)  # Extraer año

# 5. Asegurar que LHR YOY se interprete como numérico
LHR_industry$`LHR YOY` <- as.numeric(LHR_industry$`LHR (YOY)`)
View(LHR_industry)
# 6. Cargar el archivo de regiones (.dta)
regions <- read_dta(regions_file)

# 7. Preparar los dataframes para el merge
# Renombrar para facilitar el merge
colnames(LHR_industry)[colnames(LHR_industry) == "Country"] <- "countryname"

# 8. Hacer el merge de los dataframes (múltiple a 1)
merged_data <- LHR_industry %>%
  left_join(regions, by = "countryname")

# Verificar si hay países sin asignación de región
countries_without_region <- merged_data %>%
  filter(is.na(regionname)) %>%
  distinct(countryname) %>%
  pull(countryname)

if (length(countries_without_region) > 0) {
  cat("ADVERTENCIA: Los siguientes países no tienen asignación de región:\n")
  print(countries_without_region)
}

# 9. Verificar industrias disponibles
industries <- sort(unique(merged_data$Industry))
cat("Industrias disponibles (", length(industries), "):\n", sep="")
print(industries)

# 10. Encontrar el período más reciente disponible para la hoja final
latest_period <- max(merged_data$Date)
cat("Período más reciente:", latest_period, "\n")

# 11. Crear un workbook para el resultado
output_filename <- paste0(format(Sys.Date(), "%Y-%m-%d"), "-linkedin-industry-regional-analysis.xlsx")
output_path <- file.path("C:/Users/wb593225/OneDrive - WBG/Desktop/Luis - Private/Lablac/Semiannual report/Excel/Inputs/Linkedin", output_filename)
wb <- createWorkbook()

# Función para sanitizar nombres de hojas
sanitize_sheet_name <- function(name) {
  # Eliminar caracteres especiales y reemplazar espacios por guiones bajos
  clean_name <- gsub("[^A-Za-z0-9 ]", "", name)
  # Reemplazar espacios múltiples por un solo espacio
  clean_name <- gsub("\\s+", " ", clean_name)
  # Truncar si es demasiado largo (Excel tiene límite de 31 caracteres)
  if (nchar(clean_name) > 31) {
    clean_name <- substr(clean_name, 1, 31)
  }
  return(clean_name)
}

# 12. Para cada industria, crear una hoja y llenarla con datos
for (industry in industries) {
  # Filtrar datos para esta industria
  industry_data <- merged_data %>%
    filter(Industry == industry)
  
  # Crear una tabla pivote: filas = fechas, columnas = regiones, valores = promedio de LHR YOY
  industry_pivot <- industry_data %>%
    filter(!is.na(regionname)) %>%  # Eliminar países sin región asignada
    group_by(Date, regionname) %>%
    summarise(LHR_YOY_avg = mean(`LHR YOY`, na.rm = TRUE), .groups = "drop") %>%
    pivot_wider(
      id_cols = Date,
      names_from = regionname,
      values_from = LHR_YOY_avg
    ) %>%
    arrange(Date)
  
  # Crear una hoja para esta industria
  sheet_name <- sanitize_sheet_name(industry)
  
  addWorksheet(wb, sheet_name)
  writeData(wb, sheet_name, industry_pivot)
  
  # Agregar formato básico
  setColWidths(wb, sheet_name, cols = 1, widths = 12)  # Ajustar ancho de la columna de fecha
}

# 13. Crear la hoja final con todos los sectores en el período más reciente
latest_data <- merged_data %>%
  filter(Date == latest_period, !is.na(regionname)) %>%
  group_by(regionname, Industry) %>%
  summarise(LHR_YOY_avg = mean(`LHR YOY`, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(
    id_cols = regionname,
    names_from = Industry,
    values_from = LHR_YOY_avg
  ) %>%
  arrange(regionname)

# Agregar la hoja final al workbook
final_sheet_name <- sanitize_sheet_name(paste0("Latest ", latest_period))
addWorksheet(wb, final_sheet_name)
writeData(wb, final_sheet_name, latest_data)

# 14. Guardar el workbook
tryCatch({
  saveWorkbook(wb, output_path, overwrite = TRUE)
  cat("Análisis completado. Archivo guardado como:", output_path, "\n")
  cat("Se crearon", length(industries) + 1, "hojas en el archivo Excel:\n")
  cat("- Una hoja para cada industria (", length(industries), " hojas)\n", sep="")
  cat("- Una hoja resumen para el período más reciente (", latest_period, ")\n", sep="")
}, error = function(e) {
  cat("ERROR al guardar el archivo:", e$message, "\n")
})
