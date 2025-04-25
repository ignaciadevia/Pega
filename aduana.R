rm(list = ls())
library(haven)
library(dplyr)
library(tidyverse)
library(readxl)
library(writexl)
options(scipen = 999)
setwd("C:/Users/cjara/OneDrive - Universidad Católica de Chile/Mercado drogas/Aduana")
datos <- read_excel("C:/Users/cjara/OneDrive - Universidad Católica de Chile/Mercado drogas/Aduana/Aduana.xlsx", sheet="Hoja1")


# Filtramos solo las columnas de gramos
datos_largos <- datos %>%
  pivot_longer(
    cols = matches("20\\d{2}\\.\\.\\d+$"),  # columnas con nombres tipo 2013...5
    names_to = "Año_Columna",
    values_to = "Cantidad"
  ) %>%
  mutate(
    Año = str_extract(Año_Columna, "^\\d{4}"),  # Extrae el año
    Año = as.numeric(Año),
    Cantidad = parse_number(Cantidad)          # Limpia cantidades (saca comas, convierte a número)
  ) %>%
  select(Aduana = ...1, Droga = ...2, Año, Cantidad) %>%
  arrange(Año)

datos_largos <- datos_largos %>%
  mutate(
    CategoriaDroga = case_when(
      Droga %in% c(
        "KETAMINA", "MDMA EN PASTILLAS (EXTASIS)", "MDMA EN POLVO (EXTASIS)", 
        "AB-001 (CANNABINOID)", "METANFETAMINA (UNIDADES)", "LSD",
        "2-bencilamino-1-(3,4-metilendioxifenil)-1-butanona (BMDB)LIQUIDA",
        "25C-NBOME EN ESTAMPILLAS", "25C-NBOME EN PASTILLAS", "25C-NBOME EN POLVO",
        "25D-NBOME", "25D-NBOME EN ESTAMPILLAS", "25I-NBOME", "25I-NBOME (POLVO)",
        "2C-B (PHENETHYLAMINE)", "2C-C (PHENETHYLAMINE)", "2C-E (PHENETHYLAMINE)",
        "2C-E (PHENETHYLAMINE) EN PASTILLAS", "2C-E (PHENETHYLAMINE) EN POLVO",
        "2C-H (PHENETHYLAMINE)", "2C-I FENITALAMINA", "2C-N (PHENETHYLAMINE)",
        "2C-N (PHENETHYLAMINE) EN ESTAMPILLAS", "2C-P (PHENETHYLAMINE)",
        "3,4METILENODIOXIFENIL-2-PROPANONA", "3-FLUOROAMPHETAMINE", "3-MEO-PCP",
        "3-MMC (CATHINONE)", "4-CYANO CUMYL-BUTINACA (GRAMOS)", "4-FLUOROAMPHETAMINE",
        "4-FLUOROMETHAMPHETAMINE", "4-MEO-DMT", "4-hidroxi-N-metil-N-etiltriptamina (4-HO-MET) GRAMOS",
        "4F-MDMB (GRAMOS)", "5-APB", "ANFETAMINA EN PASTILLAS", "ANFETAMINA EN POLVO",
        "ANFETAMINA LIQUIDA", "BZP", "CATHINONE", "DIISOPROPILTRIPTAMINA (DIPT)",
        "DIMETOXIANFETAMINA(UNIDADES)", "METANFETAMINA", "METANFETAMINA (UNIDADES)",
        "METILFENIDATO (EN PASTILLAS)", "METILFENIDATO (EN POLVO)", "MEXEDRONE",
        "N,N-DIISOPROPILTRIPTAMINA", "NITRITO DE AMILO (GRAMOS)", "NITRITO DE BUTILO (GRAMOS)",
        "NITRITO DE ISOAMILO(GRAMOS)", "NITRITO DE ISOBUTILO (GRAMOS)", "NITRITO DE ISOBUTILO (UNIDADES)",
        "NITRITO DE ISOPROPILO (GRAMOS)", "NITRITO DE ISOPROPILO (UNIDADES)", "O-PCE (GRAMOS)",
        "1-FENIL-2-PROPANONA"
      ) ~ "Sinteticas",
      
      Droga %in% c("MARIHUANA (CANNABIS SATIVA)", "HASHIS (RESINA DE LA CANNABIS)", 
                   "MARIHUANA (SEMILLA)", "MARIHUANA (PLANTA)") ~ "Marihuana",
      
      Droga %in% c("COCAINA (CLORHIDRATO)", "COCAINE HCL") ~ "Cocaina",
      
      Droga == "COCAINA (BASE)" ~ "PastaBase",
      
      Droga %in% c("5-MEO-DMT GRAMOS (NSP)", "5-MEO-DMT UNIDADES (NSP)", "HONGO PSILOCINA",
                   "HONGO PSILOSIBINA", "MITRAGYNA SPECIOSA(KRATOM-GRAMOS)", 
                   "N,N-DIMETILTRIPTAMINA (DMT)", "SALVIA DIVINORUM(GRAMOS)") ~ "Alucinogenos",
      
      Droga %in% c("HEROINA", "OXYCODONE (UNIDADES)") ~ "Opioides",
      
      TRUE ~ "Otra"
    )
  )

resumen_droga <- datos_largos %>%
  group_by(Año, Aduana, CategoriaDroga) %>%
  summarise(CantidadTotal = sum(Cantidad, na.rm = TRUE)) %>%
  ungroup()
resumen_droga <- resumen_droga %>%
  filter(CantidadTotal > 0)

resumen_droga2 <- resumen_droga %>%
  group_by(Año, CategoriaDroga) %>%
  summarise(CantidadTotal = sum(CantidadTotal, na.rm = TRUE)) %>%
  ungroup()

resumen_droga2 <- resumen_droga2 %>%
  group_by(Año) %>%
  mutate(
    TotalAño = sum(CantidadTotal, na.rm = TRUE),  # Sumar las cantidades totales por año
    Porcentaje = (CantidadTotal / TotalAño) * 100  # Calcular el porcentaje
  ) %>%
  ungroup() %>%
  select(Año, CategoriaDroga, CantidadTotal, Porcentaje)  # Seleccionar las columnas relevantes

setwd("C:/Users/cjara/OneDrive - Universidad Católica de Chile/Mercado drogas/Aduana")
write_xlsx(
  list(
    "Aduana" = resumen_droga,
    "Agregador por año" = resumen_droga2
  ),
  "incautaciones.xlsx"
)
