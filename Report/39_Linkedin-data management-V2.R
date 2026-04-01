# 0. Limpiar espacio de trabajo y cargar bibliotecas necesarias
rm(list = ls())
gc()

# 1. Cargar bibliotecas (asumiendo que ya están instaladas según el código original)
library(readxl)
library(dplyr)
library(tidyverse)
library(ggplot2)
library(openxlsx)  # Para guardar Excel con múltiples hojas

# 2. Cargar datos
LHR_country <- read_excel("C:/Users/wb593225/OneDrive - WBG/Desktop/Luis - Private/Lablac/Semiannual report/Excel/Inputs/Linkedin/LinkedIn_LHR by Industry_Feb2025.xlsx", sheet = "2A - LHR by Ctry")
LHR_country <- LHR_country[-c(1:3),]
colnames(LHR_country) <- LHR_country[1,]
LHR_country <- LHR_country[-1,]
LHR_country <- LHR_country[,-1]

# 3. Convertir fechas seriales de Excel correctamente
LHR_country$Date <- format(as.Date(as.numeric(LHR_country$Month), origin = "1899-12-30"), "%Y-%B")
LHR_country$Year <- substr(LHR_country$Date, 1, 4)  # Extraer año para agrupación anual

# Verificar países disponibles en el dataset
available_countries <- unique(LHR_country$Country)
cat("Países disponibles en el dataset:\n")
print(available_countries)

# 5. Definir países de Latinoamérica y sus agrupaciones regionales
latam_countries <- c(
  # Cono Sur
  "Argentina", "Brazil", "Chile", "Uruguay",
  # Región Andina
  "Bolivia", "Colombia", "Ecuador", "Peru", "Venezuela", 
  # Centroamérica y México y República Dominicana
  "Costa Rica", "Dominican Republic", "Guatemala", "Mexico"
)

# Definir agrupaciones regionales
central_america <- c("Costa Rica", "Dominican Republic", "Guatemala")  # Idealmente incluiría Guatemala, Honduras, El Salvador, Nicaragua, Panamá
andean_region <- c("Bolivia", "Colombia", "Ecuador", "Peru")
southern_cone <- c("Argentina", "Chile", "Uruguay")

# 6. Filtrar solo países de Latinoamérica
latam_data <- LHR_country %>% 
  filter(Country %in% latam_countries)

# Verificar qué países de Latinoamérica están disponibles en los datos
latam_available <- latam_data %>% 
  distinct(Country) %>%
  pull(Country)

cat("Países de Latinoamérica disponibles en los datos:\n")
print(latam_available)

# Verificar si algún país definido no está disponible
missing_countries <- setdiff(latam_countries, latam_available)
if(length(missing_countries) > 0) {
  cat("ADVERTENCIA: Los siguientes países definidos no están disponibles en los datos:\n")
  print(missing_countries)
}

# 7. Calcular valores mensuales para cada país y región

# Datos mensuales por país
monthly_by_country <- latam_data %>%
  select(Country, Date, `LHR YOY`) %>%
  rename(LHR_YOY = `LHR YOY`)

# Calcular promedios mensuales para cada región
# Central America
monthly_central_america <- latam_data %>%
  filter(Country %in% central_america) %>%
  group_by(Date) %>%
  summarise(LHR_YOY = mean(`LHR YOY`, na.rm = TRUE)) %>%
  mutate(Country = "Central America")

# Andean Region
monthly_andean_region <- latam_data %>%
  filter(Country %in% andean_region) %>%
  group_by(Date) %>%
  summarise(LHR_YOY = mean(`LHR YOY`, na.rm = TRUE)) %>%
  mutate(Country = "Andean Region")

# Southern Cone
monthly_southern_cone <- latam_data %>%
  filter(Country %in% southern_cone) %>%
  group_by(Date) %>%
  summarise(LHR_YOY = mean(`LHR YOY`, na.rm = TRUE)) %>%
  mutate(Country = "Southern Cone")

# Combinar todos los datos mensuales
monthly_all <- bind_rows(
  monthly_by_country,
  monthly_central_america,
  monthly_andean_region,
  monthly_southern_cone
)

# 8. Calcular promedios anuales para cada país y región
annual_data <- monthly_all %>%
  mutate(Year = substr(Date, 1, 4)) %>%
  group_by(Country, Year) %>%
  summarise(LHR_YOY = mean(LHR_YOY, na.rm = TRUE)) %>%
  ungroup()

# 9. Convertir a formato ancho para Excel (periodos en filas, países en columnas)
# 9. Convertir a formato ancho para Excel (periodos en filas, países en columnas)

# Definir el orden de las columnas para la exportación
column_order <- c(
  # Primero los agregados regionales
  "Central America", "Andean Region", "Southern Cone",
  # Luego los países individuales ordenados por región
  central_america, andean_region, southern_cone
)

# Filtrar solo las columnas que existen en los datos
column_order <- column_order[column_order %in% c("Date", "Year", unique(monthly_all$Country))]

# Crear versiones en formato ancho
monthly_wide <- monthly_all %>%
  pivot_wider(id_cols = Date, names_from = Country, values_from = LHR_YOY) %>%
  arrange(Date)  # Ordenar cronológicamente (ahora funcionará correctamente al ser YYYY-MM)

annual_wide <- annual_data %>%
  pivot_wider(id_cols = Year, names_from = Country, values_from = LHR_YOY) %>%
  arrange(Year)  # Ordenar cronológicamente

# Reordenar las columnas según la secuencia definida (primero columnas de fecha, luego el orden personalizado)
monthly_cols <- names(monthly_wide)
date_cols <- monthly_cols[grepl("Date", monthly_cols)]
country_cols <- setdiff(monthly_cols, date_cols)
# Filtrar y ordenar las columnas de países según column_order
country_cols_ordered <- c(column_order[column_order %in% country_cols], 
                         setdiff(country_cols, column_order))
monthly_wide <- monthly_wide %>% select(all_of(c(date_cols, country_cols_ordered)))

annual_cols <- names(annual_wide)
year_cols <- annual_cols[grepl("Year", annual_cols)]
country_cols <- setdiff(annual_cols, year_cols)
# Filtrar y ordenar las columnas de países según column_order
country_cols_ordered <- c(column_order[column_order %in% country_cols], 
                         setdiff(country_cols, column_order))
annual_wide <- annual_wide %>% select(all_of(c(year_cols, country_cols_ordered)))

# 10. Crear y guardar archivo Excel con las dos hojas
output_filename <- paste0(format(Sys.Date(), "%Y-%m-%d"), "-linkedin-analysis.xlsx")
output_path <- file.path("C:/Users/wb593225/OneDrive - WBG/Desktop/Luis - Private/Lablac/Semiannual report/Excel/Inputs/Linkedin", output_filename)

# Crear workbook
wb <- createWorkbook()
addWorksheet(wb, "Annual_Averages")
addWorksheet(wb, "Monthly_Values")

# Escribir datos en cada hoja
writeData(wb, "Annual_Averages", annual_wide)
writeData(wb, "Monthly_Values", monthly_wide)

# Guardar archivo
saveWorkbook(wb, output_path, overwrite = TRUE)

# Mostrar mensaje de confirmación
cat("Análisis completado. Archivo guardado como:", output_path, "\n")

# Mostrar los países incluidos en cada región
cat("\nPaíses incluidos en cada región:\n")
cat("Central America:", paste(central_america, collapse=", "), "\n")
cat("Andean Region:", paste(andean_region, collapse=", "), "\n")
cat("Southern Cone:", paste(southern_cone, collapse=", "), "\n")