library(haven)
library(dplyr)
library(sjlabelled)
library(writexl)
library(tidyr)
options(scipen = 999)

#2022
rm(list = ls())
setwd("C:/Users/cjara/OneDrive - Universidad Católica de Chile/Mercado drogas/SENDA/2022")
datos <- read_dta("C:/Users/cjara/OneDrive - Universidad Católica de Chile/Mercado drogas/SENDA/2022/BBDD.dta")

#Drogas sintéticas
datos <- datos %>% 
  mutate(OD_10_1_Rp = ifelse(OD_10_1_Rp %in% c(1, 2), 1, 0),
         OD_10_4_Rp = ifelse(OD_10_4_Rp %in% c(1, 2), 1, 0),
         OD_10_2_Rp = ifelse(OD_10_2_Rp %in% c(1, 2), 1, 0),
         OD_2_3_Rp = ifelse(OD_2_3_Rp %in% c(1, 2), 1, 0),
         OD_8_8_Rp = ifelse(OD_8_8_Rp %in% c(1, 2), 1, 0),
         OD_14_7_Rp = ifelse(OD_14_7_Rp %in% c(1, 2), 1, 0),
         OD_14_8_Rp = ifelse(OD_14_8_Rp %in% c(1, 2), 1, 0),
         OD_14_9_Rp = ifelse(OD_14_9_Rp %in% c(1, 2), 1, 0),
         OD_14_10_Rp = ifelse(OD_14_10_Rp %in% c(1, 2), 1, 0),
         OD_14_11_Rp = ifelse(OD_14_11_Rp %in% c(1, 2), 1, 0),
         OD_6_1_Rp = ifelse(OD_6_1_Rp %in% c(1, 2), 1, 0),
         OD_6_2_Rp = ifelse(OD_6_2_Rp %in% c(1, 2), 1, 0),
         OD_2_2_Rp = ifelse(OD_2_2_Rp %in% c(1, 2), 1, 0),
         OD_2_4_Rp = ifelse(OD_2_4_Rp %in% c(1, 2), 1, 0),
         OD_2_6_Rp = ifelse(OD_2_6_Rp %in% c(1, 2), 1, 0),
         OD_10_3_Rp = ifelse(OD_10_3_Rp %in% c(1, 2), 1, 0),
         T_ANALG_2_6 = ifelse(T_ANALG_2_6 %in% c(1, 2), 1, 0)) %>%
  mutate(sinteticas = case_when(
    OD_10_1_Rp == 1 | OD_10_4_Rp == 1 | OD_10_2_Rp == 1 | OD_2_3_Rp == 1 | 
      OD_8_8_Rp == 1 | OD_14_7_Rp == 1 | OD_14_8_Rp == 1 | OD_14_9_Rp == 1 | 
      OD_14_10_Rp == 1 | OD_14_11_Rp == 1 | OD_6_1_Rp == 1 | OD_6_2_Rp == 1 | 
      OD_2_2_Rp == 1 | OD_2_4_Rp == 1 | OD_2_6_Rp == 1 | OD_10_3_Rp == 1 |
      T_ANALG_2_6 == 1 ~ 1,
    TRUE ~ 0
  ))


tasa_consumo_sinteticas <- datos %>%
  group_by(REGION) %>%
  summarise(
    total_consumidores = sum(sinteticas * FACTOR_EXPANSION, na.rm = TRUE),  # Ignorar NA en los cálculos
    total_individuos = sum(FACTOR_EXPANSION, na.rm = TRUE),             # Ignorar NA en los cálculos
    tasa_consumo = ifelse(total_individuos == 0, NA, total_consumidores / total_individuos)  # Evitar división por cero
  )

tasa_consumo_sinteticas_total <- datos %>%
  summarise(
    total_consumidores = sum(sinteticas * FACTOR_EXPANSION, na.rm = TRUE),   # Total ponderado de consumidores
    total_individuos = sum(FACTOR_EXPANSION, na.rm = TRUE),            # Total ponderado de individuos
    tasa_consumo = ifelse(total_individuos == 0, NA, total_consumidores / total_individuos )  # Calcular la tasa como porcentaje
  )

#Alucinogenos
datos <- datos %>% 
  mutate(OD_10_5_Rp = ifelse(OD_10_5_Rp %in% c(1, 2), 1, 0),
         OD_10_6_Rp = ifelse(OD_10_6_Rp %in% c(1, 2), 1, 0),
         OD_10_7_Rp = ifelse(OD_10_7_Rp %in% c(1, 2), 1, 0),
         OD_10_8_Rp = ifelse(OD_10_8_Rp %in% c(1, 2), 1, 0),
         OD_10_9_Rp = ifelse(OD_10_9_Rp %in% c(1, 2), 1, 0),
         OD_10_10_Rp = ifelse(OD_10_10_Rp %in% c(1, 2), 1, 0)) %>%
  mutate(alucinogenos = case_when(
    OD_10_5_Rp == 1 | OD_10_6_Rp == 1 | OD_10_7_Rp == 1 | OD_10_8_Rp == 1 | 
      OD_10_9_Rp == 1 | OD_10_10_Rp == 1 ~ 1,
    TRUE ~ 0
  ))

tasa_consumo_alucinogenos <- datos %>%
  group_by(REGION) %>%
  summarise(
    total_consumidores = sum(alucinogenos * FACTOR_EXPANSION, na.rm = TRUE),  # Ignorar NA en los cálculos
    total_individuos = sum(FACTOR_EXPANSION, na.rm = TRUE),             # Ignorar NA en los cálculos
    tasa_consumo = ifelse(total_individuos == 0, NA, total_consumidores / total_individuos)  # Evitar división por cero
  )

tasa_consumo_alucinogenos_total <- datos %>%
  summarise(
    total_consumidores = sum(alucinogenos * FACTOR_EXPANSION, na.rm = TRUE),   # Total ponderado de consumidores
    total_individuos = sum(FACTOR_EXPANSION, na.rm = TRUE),            # Total ponderado de individuos
    tasa_consumo = ifelse(total_individuos == 0, NA, total_consumidores / total_individuos )  # Calcular la tasa como porcentaje
  )

write_xlsx(
  list(
    "Tasa sinteticas" = tasa_consumo_sinteticas,
    "Tasa sinteticas total" = tasa_consumo_sinteticas_total,
    "Tasa alucinogenos" = tasa_consumo_alucinogenos,
    "Tasa alucinogenos total" = tasa_consumo_alucinogenos_total
  ),
  "tasa_consumo3.xlsx"
)

#2020
rm(list = ls())
setwd("C:/Users/cjara/OneDrive - Universidad Católica de Chile/Mercado drogas/SENDA/2020")
datos <- read_dta("C:/Users/cjara/OneDrive - Universidad Católica de Chile/Mercado drogas/SENDA/2020/BBDD.dta")

datos <- datos %>% 
  mutate(T_OD_10_1 = ifelse(T_OD_10_1 %in% c(1, 2), 1, 0),
         T_OD_10_4 = ifelse(T_OD_10_4 %in% c(1, 2), 1, 0),
         T_OD_10_2 = ifelse(T_OD_10_2 %in% c(1, 2), 1, 0),
         T_OD_2_3 = ifelse(T_OD_2_3 %in% c(1, 2), 1, 0),
         T_OD_8_8 = ifelse(T_OD_8_8 %in% c(1, 2), 1, 0),
         T_OD_14_7 = ifelse(T_OD_14_7 %in% c(1, 2), 1, 0),
         T_OD_14_8 = ifelse(T_OD_14_8 %in% c(1, 2), 1, 0),
         T_OD_14_9 = ifelse(T_OD_14_9 %in% c(1, 2), 1, 0),
         T_OD_14_10 = ifelse(T_OD_14_10 %in% c(1, 2), 1, 0),
         T_OD_14_11 = ifelse(T_OD_14_11 %in% c(1, 2), 1, 0),
         T_OD_6_1 = ifelse(T_OD_6_1 %in% c(1, 2), 1, 0),
         T_OD_6_2 = ifelse(T_OD_6_2 %in% c(1, 2), 1, 0),
         T_OD_2_2 = ifelse(T_OD_2_2 %in% c(1, 2), 1, 0),
         T_OD_2_4 = ifelse(T_OD_2_4 %in% c(1, 2), 1, 0),
         T_OD_2_6 = ifelse(T_OD_2_6 %in% c(1, 2), 1, 0),
         T_OD_10_3 = ifelse(T_OD_10_3 %in% c(1, 2), 1, 0),
         T_ANALG_2_6 = ifelse(T_ANALG_2_6 %in% c(1, 2), 1, 0)) %>%
  mutate(sinteticas = case_when(
    T_OD_10_1 == 1 | T_OD_10_4 == 1 | T_OD_10_2 == 1 | T_OD_2_3 == 1 | 
      T_OD_8_8 == 1 | T_OD_14_7 == 1 | T_OD_14_8 == 1 | T_OD_14_9 == 1 | 
      T_OD_14_10 == 1 | T_OD_14_11 == 1 | T_OD_6_1 == 1 | T_OD_6_2 == 1 | 
      T_OD_2_2 == 1 | T_OD_2_4 == 1 | T_OD_2_6 == 1 | T_OD_10_3 == 1 |
      T_ANALG_2_6 == 1 ~ 1,
    TRUE ~ 0
  ))

tasa_consumo_sinteticas <- datos %>%
  group_by(REGION) %>%
  summarise(
    total_consumidores = sum(sinteticas * FACT_PERS_COMUNA, na.rm = TRUE),  # Ignorar NA en los cálculos
    total_individuos = sum(FACT_PERS_COMUNA, na.rm = TRUE),             # Ignorar NA en los cálculos
    tasa_consumo = ifelse(total_individuos == 0, NA, total_consumidores / total_individuos)  # Evitar división por cero
  )

tasa_consumo_sinteticas_total <- datos %>%
  summarise(
    total_consumidores = sum(sinteticas * FACT_PERS_COMUNA, na.rm = TRUE),   # Total ponderado de consumidores
    total_individuos = sum(FACT_PERS_COMUNA, na.rm = TRUE),            # Total ponderado de individuos
    tasa_consumo = ifelse(total_individuos == 0, NA, total_consumidores / total_individuos )  # Calcular la tasa como porcentaje
  )

#Alucinogenos

datos <- datos %>% 
  mutate(T_OD_10_5 = ifelse(T_OD_10_5 %in% c(1, 2), 1, 0),
         T_OD_10_6 = ifelse(T_OD_10_6 %in% c(1, 2), 1, 0),
         T_OD_10_7 = ifelse(T_OD_10_7 %in% c(1, 2), 1, 0),
         T_OD_10_8 = ifelse(T_OD_10_8 %in% c(1, 2), 1, 0),
         T_OD_10_9 = ifelse(T_OD_10_9 %in% c(1, 2), 1, 0),
         T_OD_10_10 = ifelse(T_OD_10_10 %in% c(1, 2), 1, 0)) %>%
  mutate(alucinogenos = case_when(
    T_OD_10_5 == 1 | T_OD_10_6 == 1 | T_OD_10_7 == 1 | T_OD_10_8 == 1 | 
      T_OD_10_9 == 1 | T_OD_10_10 == 1 ~ 1,
    TRUE ~ 0
  ))

tasa_alucinogenos <- datos %>%
  group_by(REGION) %>%
  summarise(
    total_consumidores = sum(alucinogenos * FACT_PERS_COMUNA, na.rm = TRUE),  # Ignorar NA en los cálculos
    total_individuos = sum(FACT_PERS_COMUNA, na.rm = TRUE),             # Ignorar NA en los cálculos
    tasa_consumo = ifelse(total_individuos == 0, NA, total_consumidores / total_individuos)  # Evitar división por cero
  )

tasa_alucinogenos_total <- datos %>%
  summarise(
    total_consumidores = sum(alucinogenos * FACT_PERS_COMUNA, na.rm = TRUE),   # Total ponderado de consumidores
    total_individuos = sum(FACT_PERS_COMUNA, na.rm = TRUE),            # Total ponderado de individuos
    tasa_consumo = ifelse(total_individuos == 0, NA, total_consumidores / total_individuos )  # Calcular la tasa como porcentaje
  )

write_xlsx(
  list(
    "Tasa sinteticas" = tasa_consumo_sinteticas,
    "Tasa sinteticas total" = tasa_consumo_sinteticas_total,
    "Tasa alucinogenos" = tasa_consumo_alucinogenos,
    "Tasa alucinogenos total" = tasa_consumo_alucinogenos_total
  ),
  "tasa_consumo3.xlsx"
)
