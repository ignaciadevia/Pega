rm(list = ls())
library(haven)
library(dplyr)
library(tidyverse)
library(readxl)
library(writexl)
options(scipen = 999)
setwd("C:/Users/cjara/OneDrive - Universidad Católica de Chile/Mercado drogas/Carabineros")
datos <- read_excel("C:/Users/cjara/OneDrive - Universidad Católica de Chile/Mercado drogas/Carabineros/Incautaciones_Carabineros.xlsx", sheet="Hoja1")

datos <- datos %>%
  filter(Comuna=="Total")

drogas <- c("Cocaina", "PastaBase", "Marihuana", "PlantaMarihuana")
# Obtener los nombres actuales
nombres_actuales <- names(datos)

# Crear los nuevos nombres para las columnas de drogas
años <- 2005:2023
nuevos_nombres <- c()

for (año in años) {
  for (droga in drogas) {
    nuevos_nombres <- c(nuevos_nombres, paste(año, droga, sep = "_"))
  }
}

# Ahora combinamos con las primeras columnas fijas (ej. Región, Comuna)
nombres_finales <- c(nombres_actuales[1:2], nuevos_nombres)

# Asignamos los nuevos nombres
names(datos) <- nombres_finales

datos2 <- datos %>%
  pivot_longer(cols = -c(Región, Comuna),
               names_to = "Año_Droga",
               values_to = "valor") %>%
  separate(Año_Droga, into = c("Año", "Droga"), sep = "_") %>%
  pivot_wider(names_from = Droga, values_from = valor)

datos2 <- datos2 %>%
  arrange(as.integer(Año), Región)

datos2 <- datos2 %>%
  mutate(
    Cocaina = as.numeric(Cocaina),
    PastaBase = as.numeric(PastaBase),
    Marihuana = as.numeric(Marihuana)
  ) %>%
  mutate(Droga = Cocaina + PastaBase + Marihuana)

datos2 <- datos2 %>%
  group_by(Año) %>%
  mutate(Porcentaje_Droga = (Droga / Droga[Región == "Total general"])) %>%
  ungroup()

datos2 <- datos2 %>%
  # Convertimos la variable PlantaMarihuana a numérica, limpiando si es necesario
  mutate(PlantaMarihuana = parse_number(as.character(PlantaMarihuana))) %>%
  group_by(Año) %>%
  mutate(
    Total_Planta = PlantaMarihuana[Región == "Total general"],
    Porcentaje_Planta = PlantaMarihuana / Total_Planta
  ) %>%
  ungroup()

datos2 <- datos2 %>%
  # Convertimos la variable PlantaMarihuana a numérica, limpiando si es necesario
  mutate(Cocaina = parse_number(as.character(Cocaina))) %>%
  group_by(Año) %>%
  mutate(
    Total_Coca = Cocaina[Región == "Total general"],
    Coca = Cocaina / Total_Coca
  ) %>%
  ungroup()

datos2 <- datos2 %>%
  # Convertimos la variable PlantaMarihuana a numérica, limpiando si es necesario
  mutate(PastaBase = parse_number(as.character(PastaBase))) %>%
  group_by(Año) %>%
  mutate(
    Total_Pasta = PastaBase[Región == "Total general"],
    Pasta = PastaBase / Total_Pasta
  ) %>%
  ungroup()

datos2 <- datos2 %>%
  # Convertimos la variable PlantaMarihuana a numérica, limpiando si es necesario
  mutate(Marihuana = parse_number(as.character(Marihuana))) %>%
  group_by(Año) %>%
  mutate(
    Total_Mari = Marihuana[Región == "Total general"],
    Mari = Marihuana / Total_Mari
  ) %>%
  ungroup()

setwd("C:/Users/cjara/OneDrive - Universidad Católica de Chile/Mercado drogas/Carabineros")
write_xlsx(datos2, "drogas_agrupadas.xlsx")


