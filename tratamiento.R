library(haven)
library(dplyr)
library(writexl)
library(tidyr)
library(readxl)
options(scipen = 999)
rm(list = ls())

setwd("C:/Users/cjara/OneDrive - Universidad Católica de Chile/Mercado drogas/Tratamiento")
datos <- read_dta("C:/Users/cjara/OneDrive - Universidad Católica de Chile/Mercado drogas/Tratamiento/trat.dta")

datos <- datos %>%
  rename(
    año = agno_ing_tto,
    Región = region_del_centro,
    edad = grupo_edad,
    droga = sustancia_principal
  )

# Crear un vector con los nuevos nombres y su orden
nuevas_regiones <- c(
  "Tarapacá", "Antofagasta", "Atacama", "Coquimbo", "Valparaíso",
  "Lib. Bernardo O´Higgins", "Maule", "Bio Bio", "Araucanía", 
  "De los Lagos", "Aysén", "Magallanes", "RM", "De los Ríos", 
  "Arica y Parinacota", "Ñuble"
)

# Crear el vector de años
años <- seq(2010, 2022)

# Recodificar la variable Región
datos <- datos %>%
  mutate(Región = case_when(
    Región == "De Tarapaca" ~ "Tarapacá",
    Región == "De Antofagasta" ~ "Antofagasta",
    Región == "De Atacama" ~ "Atacama",
    Región == "De Coquimbo" ~ "Coquimbo",
    Región == "De Valparaiso" ~ "Valparaíso",
    Región == "Del Libertador General  Bernardo Ohiggins" ~ "Lib. Bernardo O´Higgins",
    Región == "Del Maule" ~ "Maule",
    Región == "Del Bio-bio" ~ "Bio Bio",
    Región == "De La Araucania" ~ "Araucanía",
    Región == "De Los Lagos" ~ "De los Lagos",
    Región == "De Aysen Del General Carlos Ibañes Del Campo" ~ "Aysén",
    Región == "De Magallanes Y La Antartica Chilena" ~ "Magallanes",
    Región == "Metropolitana" ~ "RM",
    Región == "De Los Rios" ~ "De los Ríos",
    Región == "De Arica Y Parinacota" ~ "Arica y Parinacota",
    Región == "De Ñuble" ~ "Ñuble",
    TRUE ~ Región # Mantener valores sin cambios si no se especifican
  )) %>%
  mutate(Región = factor(Región, levels = nuevas_regiones)) # Ordenar según el vector

# Sumar todos los valores de n_tratamiento
suma_total <- datos %>%
  summarize(total = sum(n_tratamientos)) %>%
  pull(total) # Extraer el valor de la suma total

# Paso 1: Suma total por año
suma_total_por_region <- datos %>%
  group_by(Región) %>%
  summarize(total_region = sum(n_tratamientos)) # Suma total por año

suma_total_por_año <- datos %>%
  group_by(año) %>%
  summarize(total_anual = sum(n_tratamientos)) # Suma total por año

# Paso 2: Suma por año y por región
suma_por_año_y_region <- datos %>%
  group_by(Región, año) %>%
  summarize(suma_region = sum(n_tratamientos)) # Suma por región y año

# Paso 3: Calcular el porcentaje por año y región
tasa_por_año_y_region <- suma_por_año_y_region %>%
  left_join(suma_total_por_region, by = "Región") %>% # Unir con los totales anuales
  mutate(tasa = (suma_region / total_region)) %>% # Calcular el porcentaje
  select(año, Región, tasa) # Mantener solo las columnas necesarias

tasa_por_año_y_region <- tasa_por_año_y_region %>%
  mutate(
    año = factor(año, levels = años),
    Región = factor(Región, levels = nuevas_regiones) # Ordenar por nuevas_regiones                 # Ordenar por años
  ) %>%
  arrange(año, Región) # Ordena primero por Región, luego por año


# Paso 4: Calcular la tasa por año respecto al total general
tasa_por_año <- suma_total_por_año %>%
  mutate(tasa = (total_anual / suma_total)) %>% # Calcular el porcentaje respecto al total
  select(año, tasa)

write_xlsx(list(
  "Tasa total" = tasa_por_año,
  "Tasa total por region" = tasa_por_año_y_region),
  "tratamiento1.xlsx"
  )

suma1 <- datos %>%
  group_by(año, Región, droga) %>%
  summarize(suma = sum(n_tratamientos), .groups = "drop")

suma_total_por_region <- datos %>%
  group_by(Región) %>%
  summarize(total_region = sum(n_tratamientos), .groups = "drop") # Total por región

# Paso 2: Calcular la suma de tratamientos por droga, año y región
suma_por_droga <- datos %>%
  group_by(Región, año, droga) %>%
  summarize(suma_droga = sum(n_tratamientos), .groups = "drop") # Total por droga

# Paso 3: Calcular la tasa de cada droga en relación al total de la región
tasa_por_droga <- suma_por_droga %>%
  left_join(suma_total_por_region, by = "Región") %>% # Unir con el total de la región
  mutate(tasa_droga = (suma_droga / total_region)) %>% # Calcular la tasa
  select(año, Región, droga, tasa_droga) 
tasa_por_droga <- tasa_por_droga %>%
  mutate(
    año = factor(año, levels = años),
    Región = factor(Región, levels = nuevas_regiones) # Ordenar por nuevas_regiones                 # Ordenar por años
  ) %>%
  arrange(año, Región)

orden_drogas <- c("Alcohol", "Cocaí­na", "Marihuana", "Pasta Base", "Hipnóticos", "Sedantes", 
                  "Inhalables", "Opioides Analgésicos", "Otros", "Anfetaminas", "Otros Alucinógenos", 
                  "Crack", "Heroí­na", "Otros Estimulantes", "Extasis", "Esteroides Anabólicos", 
                  "Metanfetaminas y otros derivados", "Metadona", "Fenilciclidina", "LSD", 
                  "Tranquilizantes", "Sin especificar")


tasa_por_droga <- tasa_por_droga %>%
  pivot_wider(
    names_from = droga,    
    values_from = tasa_droga,    
    names_sort = TRUE
  ) %>%
  select(año, Región, all_of(orden_drogas))  # Esto asegura el orden especificado


write_xlsx(tasa_por_droga, "tratamiento2.xlsx")

suma1 <- datos %>%
  group_by(año, droga) %>%
  summarize(suma = sum(n_tratamientos), .groups = "drop")

suma_total_por_region <- datos %>%
  group_by(año) %>%
  summarize(total_region = sum(n_tratamientos), .groups = "drop") # Total por región

# Paso 2: Calcular la suma de tratamientos por droga, año y región
suma_por_droga <- datos %>%
  group_by(año, droga) %>%
  summarize(suma_droga = sum(n_tratamientos), .groups = "drop") # Total por droga

# Paso 3: Calcular la tasa de cada droga en relación al total de la región
tasa_por_droga <- suma_por_droga %>%
  left_join(suma_total_por_region, by = "año") %>% # Unir con el total de la región
  mutate(tasa_droga = (suma_droga / total_region)) %>% # Calcular la tasa
  select(año, droga, tasa_droga) 
tasa_por_droga <- tasa_por_droga %>%
  mutate(
    año = factor(año, levels = años),
    Región = factor(Región, levels = nuevas_regiones) # Ordenar por nuevas_regiones                 # Ordenar por años
  ) %>%
  arrange(año, Región)

orden_drogas <- c("Alcohol", "Cocaí­na", "Marihuana", "Pasta Base", "Hipnóticos", "Sedantes", 
                  "Inhalables", "Opioides Analgésicos", "Otros", "Anfetaminas", "Otros Alucinógenos", 
                  "Crack", "Heroí­na", "Otros Estimulantes", "Extasis", "Esteroides Anabólicos", 
                  "Metanfetaminas y otros derivados", "Metadona", "Fenilciclidina", "LSD", 
                  "Tranquilizantes", "Sin especificar")


tasa_por_droga <- tasa_por_droga %>%
  pivot_wider(
    names_from = droga,    
    values_from = tasa_droga,    
    names_sort = TRUE
  ) %>%
  select(año, Región, all_of(orden_drogas))  # Esto asegura el orden especificado


write_xlsx(tasa_por_droga, "tratamiento4.xlsx")


# Sumar los tratamientos por droga y por año
suma_por_droga_año <- datos %>%
  group_by(año, droga) %>%
  summarize(suma_droga = sum(n_tratamientos), .groups = "drop")

# Sumar los tratamientos totales por año
suma_total_por_año <- datos %>%
  group_by(año) %>%
  summarize(total_anual = sum(n_tratamientos)) # Total por año

# Calcular la tasa de cada droga respecto al total del año
tasa_por_droga_año <- suma_por_droga_año %>%
  left_join(suma_total_por_año, by = "año") %>% # Unir con el total de tratamientos por año
  mutate(tasa_droga = suma_droga / total_anual) %>% # Calcular la tasa de cada droga
  select(año, droga, tasa_droga) # Mantener solo las columnas necesarias

tasa_por_droga_año <- tasa_por_droga_año %>%
  pivot_wider(
    names_from = droga,    
    values_from = tasa_droga,    
    names_sort = TRUE
  ) %>%
  select(año, all_of(orden_drogas)) 
write_xlsx(tasa_por_droga_año, "tratamiento3.xlsx")
