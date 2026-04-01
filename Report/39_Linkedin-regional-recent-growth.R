# Script para analizar datos de LinkedIn LHR por región
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

# 2. Cargar datos de LinkedIn
LHR_country <- read_excel("C:/Users/wb593225/OneDrive - WBG/Desktop/Luis - Private/Lablac/Semiannual report/Excel/Inputs/Linkedin/LinkedIn_LHR by Industry_Feb2025.xlsx", sheet = "2A - LHR by Ctry")
LHR_country <- LHR_country[-c(1:3),]
colnames(LHR_country) <- LHR_country[1,]
LHR_country <- LHR_country[-1,]
LHR_country <- LHR_country[,-1]

# 3. Convertir fechas seriales de Excel correctamente
LHR_country$Date_obj <- as.Date(as.numeric(LHR_country$Month), origin = "1899-12-30")
LHR_country$Date <- format(LHR_country$Date_obj, "%Y-%m")  # Formato YYYY-MM para mejor ordenamiento
LHR_country$Year <- substr(LHR_country$Date, 1, 4)  # Extraer año

# 4. Asegurar que LHR YOY se interprete como numérico
LHR_country$`LHR YOY` <- as.numeric(LHR_country$`LHR YOY`)
View(LHR_country)
# 5. Cargar el archivo de regiones (.dta)
regions <- read_dta("C:/Users/wb593225/OneDrive - WBG/Desktop/Luis - Private/Lablac/Semiannual report/dta/2025-03-24-regions.dta")
View(regions)
# 6. Preparar los dataframes para el merge
# Renombrar para facilitar el merge
colnames(LHR_country)[colnames(LHR_country) == "Country"] <- "countryname"

# 7. Hacer el merge de los dataframes (múltiple a 1)
merged_data <- LHR_country %>%
  left_join(regions, by = "countryname")

# 8. Verificar años disponibles y seleccionar 2025 (o el más reciente)
available_years <- unique(merged_data$Year)
cat("Años disponibles en los datos:", paste(available_years, collapse=", "), "\n")

if ("2025" %in% available_years) {
  year_to_use <- "2025"
} else {
  # Si no hay datos de 2025, usar el año más reciente disponible
  year_to_use <- max(available_years)
  warning(paste("No se encontraron datos para 2025. Usando el año más reciente:", year_to_use))
}

cat("Usando datos del año:", year_to_use, "\n")

# 9. Filtrar para el año seleccionado
data_selected_year <- merged_data %>%
  filter(Year == year_to_use)

# 10. Calcular promedios por región para el año seleccionado
# - Promedio por región para todo el año
region_avg <- data_selected_year %>%
  group_by(regionname) %>%
  summarise(LHR_YOY_avg = mean(`LHR YOY`, na.rm = TRUE)) %>%
  arrange(regionname)

# - Promedio por región-mes para el año
region_month_avg <- data_selected_year %>%
  group_by(regionname, Date) %>%
  summarise(LHR_YOY_avg = mean(`LHR YOY`, na.rm = TRUE)) %>%
  ungroup() %>%
  arrange(regionname, Date)

# 11. Convertir a formato ancho para Excel (para la vista de región-mes)
region_month_wide <- region_month_avg %>%
  pivot_wider(id_cols = Date, names_from = regionname, values_from = LHR_YOY_avg) %>%
  arrange(Date)

# 12. Crear y guardar archivo Excel con las dos hojas
output_filename <- paste0(format(Sys.Date(), "%Y-%m-%d"), "-linkedin-regional-analysis.xlsx")
output_path <- file.path("C:/Users/wb593225/OneDrive - WBG/Desktop/Luis - Private/Lablac/Semiannual report/Excel/Inputs/Linkedin", output_filename)

# Crear workbook
wb <- createWorkbook()
addWorksheet(wb, paste0("Regional_Averages_", year_to_use))
addWorksheet(wb, paste0("Regional_Monthly_", year_to_use))

# Escribir datos en cada hoja
writeData(wb, paste0("Regional_Averages_", year_to_use), region_avg)
writeData(wb, paste0("Regional_Monthly_", year_to_use), region_month_wide)

# Guardar archivo
saveWorkbook(wb, output_path, overwrite = TRUE)

# Mostrar mensaje de confirmación
cat("Análisis completado. Archivo guardado como:", output_path, "\n")
cat("Sheets generadas:", paste0("Regional_Averages_", year_to_use), "y", paste0("Regional_Monthly_", year_to_use), "\n")

#see list of countries that have NA in the regionname column
data_selected_year %>%
  filter(is.na(regionname)) %>%
  select(countryname) %>%
  distinct() %>%
  arrange(countryname)
