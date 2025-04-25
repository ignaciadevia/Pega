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

#TIPO MARIHUANA
table(datos$MAR_7)

datos <- datos %>%
  mutate(prensada = case_when(
    MAR_7 %in% 2:99 ~ 0,  # Recodificar valores 1 o 2 como 1 (Año)
    MAR_7 == 1 ~ 1,
  ))
table(datos$prensada)

datos <- datos %>%
  mutate(no_prensada = case_when(
    MAR_7 %in% 88:99 ~ 0,  # Recodificar valores 1 o 2 como 1 (Año)
    MAR_7 == 1 ~ 0,
    MAR_7 %in% 2:5 ~ 1, 
  ))
table(datos$no_prensada)

tasa_consumo_pensada <- datos %>%
  filter(!is.na(prensada)) %>%  
  group_by(REGION) %>%
  summarise(
    total_consumidores = sum((prensada == 1) * FACTOR_EXPANSION, na.rm = TRUE),  # Pondera solo consumidores
    total_respuestas = sum(FACTOR_EXPANSION, na.rm = TRUE),                  # Pondera todas las respuestas válidas
    tasa_consumo = ifelse(total_respuestas == 0, NA, total_consumidores / total_respuestas)  # Tasa ajustada
  )

tasa_consumo_pensada_total <- datos %>%
  filter(!is.na(prensada)) %>%  # Asegura que solo se usen respuestas válidas
  summarise(
    total_consumidores = sum((prensada == 1) * FACTOR_EXPANSION, na.rm = TRUE),  # Pondera solo consumidores
    total_respuestas = sum(FACTOR_EXPANSION, na.rm = TRUE),                  # Pondera todas las respuestas válidas
    tasa_consumo = ifelse(total_respuestas == 0, NA, total_consumidores / total_respuestas)  # Tasa ajustada
  )

tasa_consumo_no_pensada <- datos %>%
  filter(!is.na(no_prensada)) %>%  
  group_by(REGION) %>%
  summarise(
    total_consumidores = sum((no_prensada == 1) * FACTOR_EXPANSION, na.rm = TRUE),  # Pondera solo consumidores
    total_respuestas = sum(FACTOR_EXPANSION, na.rm = TRUE),                  # Pondera todas las respuestas válidas
    tasa_consumo = ifelse(total_respuestas == 0, NA, total_consumidores / total_respuestas)  # Tasa ajustada
  )

tasa_consumo_no_pensada_total <- datos %>%
  filter(!is.na(no_prensada)) %>%  # Asegura que solo se usen respuestas válidas
  summarise(
    total_consumidores = sum((no_prensada == 1) * FACTOR_EXPANSION, na.rm = TRUE),  # Pondera solo consumidores
    total_respuestas = sum(FACTOR_EXPANSION, na.rm = TRUE),                  # Pondera todas las respuestas válidas
    tasa_consumo = ifelse(total_respuestas == 0, NA, total_consumidores / total_respuestas)  # Tasa ajustada
  )

#EXPOSICION OFERTA MARIHUANA
table(datos$EB_5_1_Rp)

datos <- datos %>%
  mutate(mari = case_when(
    EB_5_1_Rp %in% 1:2 ~ 1,
    EB_5_1_Rp == 3 ~ 0,
    EB_5_1_Rp == 0 ~ 0,
  ))

table(datos$mari)

tasa_consumo_mari <- datos %>%
  group_by(REGION) %>%
  summarise(
    total_consumidores = sum(mari * FACTOR_EXPANSION, na.rm = TRUE),  # Ignorar NA en los cálculos
    total_individuos = sum(FACTOR_EXPANSION, na.rm = TRUE),             # Ignorar NA en los cálculos
    tasa_consumo = ifelse(total_individuos == 0, NA, total_consumidores / total_individuos)  # Evitar división por cero
  )

tasa_consumo_mari_total <- datos %>%
  summarise(
    total_consumidores = sum(mari * FACTOR_EXPANSION, na.rm = TRUE),   # Total ponderado de consumidores
    total_individuos = sum(FACTOR_EXPANSION, na.rm = TRUE),            # Total ponderado de individuos
    tasa_consumo = ifelse(total_individuos == 0, NA, total_consumidores / total_individuos )  # Calcular la tasa como porcentaje
  )

#COCA

table(datos$EB_5_2_Rp)

datos <- datos %>%
  mutate(coca = case_when(
    EB_5_2_Rp %in% 1:2 ~ 1,
    EB_5_2_Rp == 3 ~ 0,
    EB_5_2_Rp == 0 ~ 0,
  ))

tasa_consumo_coca <- datos %>%
  group_by(REGION) %>%
  summarise(
    total_consumidores = sum(coca * FACTOR_EXPANSION, na.rm = TRUE),  # Ignorar NA en los cálculos
    total_individuos = sum(FACTOR_EXPANSION, na.rm = TRUE),             # Ignorar NA en los cálculos
    tasa_consumo = ifelse(total_individuos == 0, NA, total_consumidores / total_individuos)  # Evitar división por cero
  )

tasa_consumo_coca_total <- datos %>%
  summarise(
    total_consumidores = sum(coca * FACTOR_EXPANSION, na.rm = TRUE),   # Total ponderado de consumidores
    total_individuos = sum(FACTOR_EXPANSION, na.rm = TRUE),            # Total ponderado de individuos
    tasa_consumo = ifelse(total_individuos == 0, NA, total_consumidores / total_individuos )  # Calcular la tasa como porcentaje
  )

#PASTA BASE

table(datos$EB_5_3_Rp)

datos <- datos %>%
  mutate(pasta = case_when(
    EB_5_3_Rp %in% 1:2 ~ 1,
    EB_5_3_Rp == 3 ~ 0,
    EB_5_3_Rp == 0 ~ 0,
  ))

tasa_consumo_pasta <- datos %>%
  group_by(REGION) %>%
  summarise(
    total_consumidores = sum(pasta * FACTOR_EXPANSION, na.rm = TRUE),  # Ignorar NA en los cálculos
    total_individuos = sum(FACTOR_EXPANSION, na.rm = TRUE),             # Ignorar NA en los cálculos
    tasa_consumo = ifelse(total_individuos == 0, NA, total_consumidores / total_individuos)  # Evitar división por cero
  )

tasa_consumo_pasta_total <- datos %>%
  summarise(
    total_consumidores = sum(pasta * FACTOR_EXPANSION, na.rm = TRUE),   # Total ponderado de consumidores
    total_individuos = sum(FACTOR_EXPANSION, na.rm = TRUE),            # Total ponderado de individuos
    tasa_consumo = ifelse(total_individuos == 0, NA, total_consumidores / total_individuos )  # Calcular la tasa como porcentaje
  )

#EXTASIS 

table(datos$EB_5_4_Rp)

datos <- datos %>%
  mutate(extasis = case_when(
    EB_5_4_Rp %in% 1:2 ~ 1,
    EB_5_4_Rp == 3 ~ 0,
    EB_5_4_Rp == 0 ~ 0,
  ))

tasa_consumo_extasis <- datos %>%
  group_by(REGION) %>%
  summarise(
    total_consumidores = sum(extasis * FACTOR_EXPANSION, na.rm = TRUE),  # Ignorar NA en los cálculos
    total_individuos = sum(FACTOR_EXPANSION, na.rm = TRUE),             # Ignorar NA en los cálculos
    tasa_consumo = ifelse(total_individuos == 0, NA, total_consumidores / total_individuos)  # Evitar división por cero
  )

tasa_consumo_extasis_total <- datos %>%
  summarise(
    total_consumidores = sum(extasis * FACTOR_EXPANSION, na.rm = TRUE),   # Total ponderado de consumidores
    total_individuos = sum(FACTOR_EXPANSION, na.rm = TRUE),            # Total ponderado de individuos
    tasa_consumo = ifelse(total_individuos == 0, NA, total_consumidores / total_individuos )  # Calcular la tasa como porcentaje
  )

write_xlsx(
  list(
    "Tasa prensada" = tasa_consumo_pensada,
    "Tasa prensada total" = tasa_consumo_pensada_total,
    "Tasa no prensada" = tasa_consumo_no_pensada,
    "Tasa no prensada total" = tasa_consumo_no_pensada_total,
    "Tasa mari" = tasa_consumo_mari,
    "Tasa mari total" = tasa_consumo_mari_total,
    "Tasa coca" = tasa_consumo_coca,
    "Tasa coca total" = tasa_consumo_coca_total,
    "Tasa pasta" = tasa_consumo_pasta,
    "Tasa pasta total" = tasa_consumo_pasta_total,
    "Tasa extasis" = tasa_consumo_extasis,
    "Tasa extasis total" = tasa_consumo_extasis_total
  ),
  "oferta.xlsx"
)

#2020

rm(list = ls())
setwd("C:/Users/cjara/OneDrive - Universidad Católica de Chile/Mercado drogas/SENDA/2020")
datos <- read_dta("C:/Users/cjara/OneDrive - Universidad Católica de Chile/Mercado drogas/SENDA/2020/BBDD.dta")

#TIPO MARIHUANA
table(datos$MAR_7)

datos <- datos %>%
  mutate(prensada = case_when(
    MAR_7 %in% 2:99 ~ 0,  # Recodificar valores 1 o 2 como 1 (Año)
    MAR_7 == 1 ~ 1,
  ))
table(datos$prensada)

datos <- datos %>%
  mutate(no_prensada = case_when(
    MAR_7 %in% 88:99 ~ 0,  # Recodificar valores 1 o 2 como 1 (Año)
    MAR_7 == 1 ~ 0,
    MAR_7 %in% 2:3 ~ 1, 
  ))
table(datos$no_prensada)

tasa_consumo_pensada <- datos %>%
  filter(!is.na(prensada)) %>%  
  group_by(REGION) %>%
  summarise(
    total_consumidores = sum((prensada == 1) * FACT_PERS_COMUNA, na.rm = TRUE),  # Pondera solo consumidores
    total_respuestas = sum(FACT_PERS_COMUNA, na.rm = TRUE),                  # Pondera todas las respuestas válidas
    tasa_consumo = ifelse(total_respuestas == 0, NA, total_consumidores / total_respuestas)  # Tasa ajustada
  )

tasa_consumo_pensada_total <- datos %>%
  filter(!is.na(prensada)) %>%  # Asegura que solo se usen respuestas válidas
  summarise(
    total_consumidores = sum((prensada == 1) * FACT_PERS_COMUNA, na.rm = TRUE),  # Pondera solo consumidores
    total_respuestas = sum(FACT_PERS_COMUNA, na.rm = TRUE),                  # Pondera todas las respuestas válidas
    tasa_consumo = ifelse(total_respuestas == 0, NA, total_consumidores / total_respuestas)  # Tasa ajustada
  )

tasa_consumo_no_pensada <- datos %>%
  filter(!is.na(no_prensada)) %>%  
  group_by(REGION) %>%
  summarise(
    total_consumidores = sum((no_prensada == 1) * FACT_PERS_COMUNA, na.rm = TRUE),  # Pondera solo consumidores
    total_respuestas = sum(FACT_PERS_COMUNA, na.rm = TRUE),                  # Pondera todas las respuestas válidas
    tasa_consumo = ifelse(total_respuestas == 0, NA, total_consumidores / total_respuestas)  # Tasa ajustada
  )

tasa_consumo_no_pensada_total <- datos %>%
  filter(!is.na(no_prensada)) %>%  # Asegura que solo se usen respuestas válidas
  summarise(
    total_consumidores = sum((no_prensada == 1) * FACT_PERS_COMUNA, na.rm = TRUE),  # Pondera solo consumidores
    total_respuestas = sum(FACT_PERS_COMUNA, na.rm = TRUE),                  # Pondera todas las respuestas válidas
    tasa_consumo = ifelse(total_respuestas == 0, NA, total_consumidores / total_respuestas)  # Tasa ajustada
  )

#EXPOSICION OFERTA MARIHUANA
table(datos$T_EB_5_1)

datos <- datos %>%
  mutate(mari = case_when(
    T_EB_5_1 %in% 1:2 ~ 1,
    T_EB_5_1 == 3 ~ 0,
    T_EB_5_1 == 0 ~ 0,
  ))

tasa_consumo_mari <- datos %>%
  group_by(REGION) %>%
  summarise(
    total_consumidores = sum(mari * FACT_PERS_COMUNA, na.rm = TRUE),  # Ignorar NA en los cálculos
    total_individuos = sum(FACT_PERS_COMUNA, na.rm = TRUE),             # Ignorar NA en los cálculos
    tasa_consumo = ifelse(total_individuos == 0, NA, total_consumidores / total_individuos)  # Evitar división por cero
  )

tasa_consumo_mari_total <- datos %>%
  summarise(
    total_consumidores = sum(mari * FACT_PERS_COMUNA, na.rm = TRUE),   # Total ponderado de consumidores
    total_individuos = sum(FACT_PERS_COMUNA, na.rm = TRUE),            # Total ponderado de individuos
    tasa_consumo = ifelse(total_individuos == 0, NA, total_consumidores / total_individuos )  # Calcular la tasa como porcentaje
  )

#COCA

datos <- datos %>%
  mutate(coca = case_when(
    T_EB_5_2 %in% 1:2 ~ 1,
    T_EB_5_2 == 3 ~ 0,
    T_EB_5_2 == 0 ~ 0,
  ))

tasa_consumo_coca <- datos %>%
  group_by(REGION) %>%
  summarise(
    total_consumidores = sum(coca * FACT_PERS_COMUNA, na.rm = TRUE),  # Ignorar NA en los cálculos
    total_individuos = sum(FACT_PERS_COMUNA, na.rm = TRUE),             # Ignorar NA en los cálculos
    tasa_consumo = ifelse(total_individuos == 0, NA, total_consumidores / total_individuos)  # Evitar división por cero
  )

tasa_consumo_coca_total <- datos %>%
  summarise(
    total_consumidores = sum(coca * FACT_PERS_COMUNA, na.rm = TRUE),   # Total ponderado de consumidores
    total_individuos = sum(FACT_PERS_COMUNA, na.rm = TRUE),            # Total ponderado de individuos
    tasa_consumo = ifelse(total_individuos == 0, NA, total_consumidores / total_individuos )  # Calcular la tasa como porcentaje
  )

#PASTA BASE

datos <- datos %>%
  mutate(pasta = case_when(
    T_EB_5_3 %in% 1:2 ~ 1,
    T_EB_5_3 == 3 ~ 0,
    T_EB_5_3 == 0 ~ 0,
  ))

tasa_consumo_pasta <- datos %>%
  group_by(REGION) %>%
  summarise(
    total_consumidores = sum(pasta * FACT_PERS_COMUNA, na.rm = TRUE),  # Ignorar NA en los cálculos
    total_individuos = sum(FACT_PERS_COMUNA, na.rm = TRUE),             # Ignorar NA en los cálculos
    tasa_consumo = ifelse(total_individuos == 0, NA, total_consumidores / total_individuos)  # Evitar división por cero
  )

tasa_consumo_pasta_total <- datos %>%
  summarise(
    total_consumidores = sum(pasta * FACT_PERS_COMUNA, na.rm = TRUE),   # Total ponderado de consumidores
    total_individuos = sum(FACT_PERS_COMUNA, na.rm = TRUE),            # Total ponderado de individuos
    tasa_consumo = ifelse(total_individuos == 0, NA, total_consumidores / total_individuos )  # Calcular la tasa como porcentaje
  )

#EXTASIS 

datos <- datos %>%
  mutate(extasis = case_when(
    T_EB_5_4 %in% 1:2 ~ 1,
    T_EB_5_4 == 3 ~ 0,
    T_EB_5_4 == 0 ~ 0,
  ))

tasa_consumo_extasis <- datos %>%
  group_by(REGION) %>%
  summarise(
    total_consumidores = sum(extasis * FACT_PERS_COMUNA, na.rm = TRUE),  # Ignorar NA en los cálculos
    total_individuos = sum(FACT_PERS_COMUNA, na.rm = TRUE),             # Ignorar NA en los cálculos
    tasa_consumo = ifelse(total_individuos == 0, NA, total_consumidores / total_individuos)  # Evitar división por cero
  )

tasa_consumo_extasis_total <- datos %>%
  summarise(
    total_consumidores = sum(extasis * FACT_PERS_COMUNA, na.rm = TRUE),   # Total ponderado de consumidores
    total_individuos = sum(FACT_PERS_COMUNA, na.rm = TRUE),            # Total ponderado de individuos
    tasa_consumo = ifelse(total_individuos == 0, NA, total_consumidores / total_individuos )  # Calcular la tasa como porcentaje
  )

write_xlsx(
  list(
    "Tasa prensada" = tasa_consumo_pensada,
    "Tasa prensada total" = tasa_consumo_pensada_total,
    "Tasa no prensada" = tasa_consumo_no_pensada,
    "Tasa no prensada total" = tasa_consumo_no_pensada_total,
    "Tasa mari" = tasa_consumo_mari,
    "Tasa mari total" = tasa_consumo_mari_total,
    "Tasa coca" = tasa_consumo_coca,
    "Tasa coca total" = tasa_consumo_coca_total,
    "Tasa pasta" = tasa_consumo_pasta,
    "Tasa pasta total" = tasa_consumo_pasta_total,
    "Tasa extasis" = tasa_consumo_extasis,
    "Tasa extasis total" = tasa_consumo_extasis_total
  ),
  "oferta.xlsx"
)

#2018

rm(list = ls())
setwd("C:/Users/cjara/OneDrive - Universidad Católica de Chile/Mercado drogas/SENDA/2018")
datos <- read_dta("C:/Users/cjara/OneDrive - Universidad Católica de Chile/Mercado drogas/SENDA/2018/BBDD.dta")

#TIPO MARIHUANA
table(datos$MAR_10)

datos <- datos %>%
  mutate(prensada = case_when(
    MAR_10 %in% 2:99 ~ 0,  # Recodificar valores 1 o 2 como 1 (Año)
    MAR_10 == 1 ~ 1,
  ))
table(datos$prensada)

datos <- datos %>%
  mutate(no_prensada = case_when(
    MAR_10 %in% 88:99 ~ 0,  # Recodificar valores 1 o 2 como 1 (Año)
    MAR_10 == 1 ~ 0,
    MAR_10 %in% 2:5 ~ 1, 
  ))
table(datos$no_prensada)

tasa_consumo_pensada <- datos %>%
  filter(!is.na(prensada)) %>%  
  group_by(Region) %>%
  summarise(
    total_consumidores = sum((prensada == 1) * Fexp, na.rm = TRUE),  # Pondera solo consumidores
    total_respuestas = sum(Fexp, na.rm = TRUE),                  # Pondera todas las respuestas válidas
    tasa_consumo = ifelse(total_respuestas == 0, NA, total_consumidores / total_respuestas)  # Tasa ajustada
  )

tasa_consumo_pensada_total <- datos %>%
  filter(!is.na(prensada)) %>%  # Asegura que solo se usen respuestas válidas
  summarise(
    total_consumidores = sum((prensada == 1) * Fexp, na.rm = TRUE),  # Pondera solo consumidores
    total_respuestas = sum(Fexp, na.rm = TRUE),                  # Pondera todas las respuestas válidas
    tasa_consumo = ifelse(total_respuestas == 0, NA, total_consumidores / total_respuestas)  # Tasa ajustada
  )

tasa_consumo_no_pensada <- datos %>%
  filter(!is.na(no_prensada)) %>%  
  group_by(Region) %>%
  summarise(
    total_consumidores = sum((no_prensada == 1) * Fexp, na.rm = TRUE),  # Pondera solo consumidores
    total_respuestas = sum(Fexp, na.rm = TRUE),                  # Pondera todas las respuestas válidas
    tasa_consumo = ifelse(total_respuestas == 0, NA, total_consumidores / total_respuestas)  # Tasa ajustada
  )

tasa_consumo_no_pensada_total <- datos %>%
  filter(!is.na(no_prensada)) %>%  # Asegura que solo se usen respuestas válidas
  summarise(
    total_consumidores = sum((no_prensada == 1) * Fexp, na.rm = TRUE),  # Pondera solo consumidores
    total_respuestas = sum(Fexp, na.rm = TRUE),                  # Pondera todas las respuestas válidas
    tasa_consumo = ifelse(total_respuestas == 0, NA, total_consumidores / total_respuestas)  # Tasa ajustada
  )

#MARIHUANA

datos <- datos %>%
  mutate(mari = case_when(
    T_EB_6_1 %in% 1:2 ~ 1,
    T_EB_6_1 == 3 ~ 0,
    T_EB_6_1 == 0 ~ 0,
  ))

tasa_consumo_mari <- datos %>%
  group_by(Region) %>%
  summarise(
    total_consumidores = sum(mari * Fexp, na.rm = TRUE),  # Ignorar NA en los cálculos
    total_individuos = sum(Fexp, na.rm = TRUE),             # Ignorar NA en los cálculos
    tasa_consumo = ifelse(total_individuos == 0, NA, total_consumidores / total_individuos)  # Evitar división por cero
  )

tasa_consumo_mari_total <- datos %>%
  summarise(
    total_consumidores = sum(mari * Fexp, na.rm = TRUE),   # Total ponderado de consumidores
    total_individuos = sum(Fexp, na.rm = TRUE),            # Total ponderado de individuos
    tasa_consumo = ifelse(total_individuos == 0, NA, total_consumidores / total_individuos )  # Calcular la tasa como porcentaje
  )

#COCA

datos <- datos %>%
  mutate(coca = case_when(
    T_EB_6_2 %in% 1:2 ~ 1,
    T_EB_6_2 == 3 ~ 0,
    T_EB_6_2 == 0 ~ 0,
  ))

tasa_consumo_coca <- datos %>%
  group_by(Region) %>%
  summarise(
    total_consumidores = sum(coca * Fexp, na.rm = TRUE),  # Ignorar NA en los cálculos
    total_individuos = sum(Fexp, na.rm = TRUE),             # Ignorar NA en los cálculos
    tasa_consumo = ifelse(total_individuos == 0, NA, total_consumidores / total_individuos)  # Evitar división por cero
  )

tasa_consumo_coca_total <- datos %>%
  summarise(
    total_consumidores = sum(coca * Fexp, na.rm = TRUE),   # Total ponderado de consumidores
    total_individuos = sum(Fexp, na.rm = TRUE),            # Total ponderado de individuos
    tasa_consumo = ifelse(total_individuos == 0, NA, total_consumidores / total_individuos )  # Calcular la tasa como porcentaje
  )

#PASTA BASE

datos <- datos %>%
  mutate(pasta = case_when(
    T_EB_6_3 %in% 1:2 ~ 1,
    T_EB_6_3 == 3 ~ 0,
    T_EB_6_3 == 0 ~ 0,
  ))

tasa_consumo_pasta <- datos %>%
  group_by(Region) %>%
  summarise(
    total_consumidores = sum(pasta * Fexp, na.rm = TRUE),  # Ignorar NA en los cálculos
    total_individuos = sum(Fexp, na.rm = TRUE),             # Ignorar NA en los cálculos
    tasa_consumo = ifelse(total_individuos == 0, NA, total_consumidores / total_individuos)  # Evitar división por cero
  )

tasa_consumo_pasta_total <- datos %>%
  summarise(
    total_consumidores = sum(pasta * Fexp, na.rm = TRUE),   # Total ponderado de consumidores
    total_individuos = sum(Fexp, na.rm = TRUE),            # Total ponderado de individuos
    tasa_consumo = ifelse(total_individuos == 0, NA, total_consumidores / total_individuos )  # Calcular la tasa como porcentaje
  )

#EXTASIS 

datos <- datos %>%
  mutate(extasis = case_when(
    T_EB_6_4 %in% 1:2 ~ 1,
    T_EB_6_4 == 3 ~ 0,
    T_EB_6_4 == 0 ~ 0,
  ))

tasa_consumo_extasis <- datos %>%
  group_by(Region) %>%
  summarise(
    total_consumidores = sum(extasis * Fexp, na.rm = TRUE),  # Ignorar NA en los cálculos
    total_individuos = sum(Fexp, na.rm = TRUE),             # Ignorar NA en los cálculos
    tasa_consumo = ifelse(total_individuos == 0, NA, total_consumidores / total_individuos)  # Evitar división por cero
  )

tasa_consumo_extasis_total <- datos %>%
  summarise(
    total_consumidores = sum(extasis * Fexp, na.rm = TRUE),   # Total ponderado de consumidores
    total_individuos = sum(Fexp, na.rm = TRUE),            # Total ponderado de individuos
    tasa_consumo = ifelse(total_individuos == 0, NA, total_consumidores / total_individuos )  # Calcular la tasa como porcentaje
  )

write_xlsx(
  list(
    "Tasa prensada" = tasa_consumo_pensada,
    "Tasa prensada total" = tasa_consumo_pensada_total,
    "Tasa no prensada" = tasa_consumo_no_pensada,
    "Tasa no prensada total" = tasa_consumo_no_pensada_total,
    "Tasa mari" = tasa_consumo_mari,
    "Tasa mari total" = tasa_consumo_mari_total,
    "Tasa coca" = tasa_consumo_coca,
    "Tasa coca total" = tasa_consumo_coca_total,
    "Tasa pasta" = tasa_consumo_pasta,
    "Tasa pasta total" = tasa_consumo_pasta_total,
    "Tasa extasis" = tasa_consumo_extasis,
    "Tasa extasis total" = tasa_consumo_extasis_total
  ),
  "oferta.xlsx"
)

#2016

rm(list = ls())
setwd("C:/Users/cjara/OneDrive - Universidad Católica de Chile/Mercado drogas/SENDA/2016")
datos <- read_dta("C:/Users/cjara/OneDrive - Universidad Católica de Chile/Mercado drogas/SENDA/2016/BBDD.dta")

datos <- datos %>%
  mutate(Region = case_when(
    comuna %in% c(15101) ~ 15,
    comuna %in% c(1101, 1107) ~ 1,
    comuna %in% c(2101, 2201) ~ 2,
    comuna %in% c(3101, 3301) ~ 3,
    comuna %in% c(4101, 4102, 4301) ~ 4,
    comuna %in% c(5101, 5103, 5109, 5301, 5501, 5502, 5601, 5701, 5801, 5802, 5804) ~ 5,
    comuna %in% c(6101, 6106, 6108, 6110, 6115, 6117, 6301, 6303) ~ 6,
    comuna %in% c(7101, 7102, 7201, 7301, 7401, 7404) ~ 7,
    comuna %in% c(8101, 8102, 8103, 8106, 8107, 8108, 8110, 8111, 8112, 8205, 8301) ~ 8,
    comuna %in% c(8401) ~ 16,
    comuna %in% c(9101, 9108, 9111, 9112, 9114, 9120, 9201, 9211) ~ 9,
    comuna %in% c(10101, 10201, 10202, 10301) ~ 10,
    comuna %in% c(11101, 11201) ~ 11,
    comuna %in% c(12101) ~ 12,
    comuna %in% c(13101, 13102, 13103, 13104, 13105, 13106, 13107, 13108, 13109, 13110, 13111, 13112, 13113, 13114, 13115, 13116, 13117, 13118, 13119, 13120, 13121, 13122, 13123, 13124, 13125, 13125, 13126, 13127, 13128, 13129, 13130, 13131, 13132, 13201, 13301, 13302, 13401, 13402, 13404, 13501, 13601, 13604, 13605) ~ 13,
    comuna %in% c(14101, 14108, 14201, 14204) ~ 14,
    # Agrega aquí las demás cod_areas con sus respectivas regiones
    TRUE ~ NA_real_  # Para asignar NA si una cod_area no tiene región definida
  ))

#TIPO MARIHUANA
table(datos$mar_10)

datos <- datos %>%
  mutate(prensada = case_when(
    mar_10 %in% 2:99 ~ 0,  # Recodificar valores 1 o 2 como 1 (Año)
    mar_10 == 1 ~ 1,
  ))
table(datos$prensada)

datos <- datos %>%
  mutate(no_prensada = case_when(
    mar_10 %in% 88:99 ~ 0,  # Recodificar valores 1 o 2 como 1 (Año)
    mar_10 == 1 ~ 0,
    mar_10 %in% 2:5 ~ 1, 
  ))
table(datos$no_prensada)

tasa_consumo_pensada <- datos %>%
  filter(!is.na(prensada)) %>%  
  group_by(Region) %>%
  summarise(
    total_consumidores = sum((prensada == 1) * Fexp, na.rm = TRUE),  # Pondera solo consumidores
    total_respuestas = sum(Fexp, na.rm = TRUE),                  # Pondera todas las respuestas válidas
    tasa_consumo = ifelse(total_respuestas == 0, NA, total_consumidores / total_respuestas)  # Tasa ajustada
  )

tasa_consumo_pensada_total <- datos %>%
  filter(!is.na(prensada)) %>%  # Asegura que solo se usen respuestas válidas
  summarise(
    total_consumidores = sum((prensada == 1) * Fexp, na.rm = TRUE),  # Pondera solo consumidores
    total_respuestas = sum(Fexp, na.rm = TRUE),                  # Pondera todas las respuestas válidas
    tasa_consumo = ifelse(total_respuestas == 0, NA, total_consumidores / total_respuestas)  # Tasa ajustada
  )

tasa_consumo_no_pensada <- datos %>%
  filter(!is.na(no_prensada)) %>%  
  group_by(Region) %>%
  summarise(
    total_consumidores = sum((no_prensada == 1) * Fexp, na.rm = TRUE),  # Pondera solo consumidores
    total_respuestas = sum(Fexp, na.rm = TRUE),                  # Pondera todas las respuestas válidas
    tasa_consumo = ifelse(total_respuestas == 0, NA, total_consumidores / total_respuestas)  # Tasa ajustada
  )

tasa_consumo_no_pensada_total <- datos %>%
  filter(!is.na(no_prensada)) %>%  # Asegura que solo se usen respuestas válidas
  summarise(
    total_consumidores = sum((no_prensada == 1) * Fexp, na.rm = TRUE),  # Pondera solo consumidores
    total_respuestas = sum(Fexp, na.rm = TRUE),                  # Pondera todas las respuestas válidas
    tasa_consumo = ifelse(total_respuestas == 0, NA, total_consumidores / total_respuestas)  # Tasa ajustada
  )

#MARIHUANA
table(datos$eb_6_a)

datos <- datos %>%
  mutate(mari = case_when(
    eb_6_a %in% 1:2 ~ 1,
    eb_6_a == 3 ~ 0,
    eb_6_a == 0 ~ 0,
  ))

tasa_consumo_mari <- datos %>%
  group_by(Region) %>%
  summarise(
    total_consumidores = sum(mari * Fexp, na.rm = TRUE),  # Ignorar NA en los cálculos
    total_individuos = sum(Fexp, na.rm = TRUE),             # Ignorar NA en los cálculos
    tasa_consumo = ifelse(total_individuos == 0, NA, total_consumidores / total_individuos)  # Evitar división por cero
  )

tasa_consumo_mari_total <- datos %>%
  summarise(
    total_consumidores = sum(mari * Fexp, na.rm = TRUE),   # Total ponderado de consumidores
    total_individuos = sum(Fexp, na.rm = TRUE),            # Total ponderado de individuos
    tasa_consumo = ifelse(total_individuos == 0, NA, total_consumidores / total_individuos )  # Calcular la tasa como porcentaje
  )

#COCA

datos <- datos %>%
  mutate(coca = case_when(
    eb_6_b %in% 1:2 ~ 1,
    eb_6_b == 3 ~ 0,
    eb_6_b == 0 ~ 0,
  ))

tasa_consumo_coca <- datos %>%
  group_by(Region) %>%
  summarise(
    total_consumidores = sum(coca * Fexp, na.rm = TRUE),  # Ignorar NA en los cálculos
    total_individuos = sum(Fexp, na.rm = TRUE),             # Ignorar NA en los cálculos
    tasa_consumo = ifelse(total_individuos == 0, NA, total_consumidores / total_individuos)  # Evitar división por cero
  )

tasa_consumo_coca_total <- datos %>%
  summarise(
    total_consumidores = sum(coca * Fexp, na.rm = TRUE),   # Total ponderado de consumidores
    total_individuos = sum(Fexp, na.rm = TRUE),            # Total ponderado de individuos
    tasa_consumo = ifelse(total_individuos == 0, NA, total_consumidores / total_individuos )  # Calcular la tasa como porcentaje
  )

#PASTA BASE

datos <- datos %>%
  mutate(pasta = case_when(
    eb_6_c %in% 1:2 ~ 1,
    eb_6_c == 3 ~ 0,
    eb_6_c == 0 ~ 0,
  ))

tasa_consumo_pasta <- datos %>%
  group_by(Region) %>%
  summarise(
    total_consumidores = sum(pasta * Fexp, na.rm = TRUE),  # Ignorar NA en los cálculos
    total_individuos = sum(Fexp, na.rm = TRUE),             # Ignorar NA en los cálculos
    tasa_consumo = ifelse(total_individuos == 0, NA, total_consumidores / total_individuos)  # Evitar división por cero
  )

tasa_consumo_pasta_total <- datos %>%
  summarise(
    total_consumidores = sum(pasta * Fexp, na.rm = TRUE),   # Total ponderado de consumidores
    total_individuos = sum(Fexp, na.rm = TRUE),            # Total ponderado de individuos
    tasa_consumo = ifelse(total_individuos == 0, NA, total_consumidores / total_individuos )  # Calcular la tasa como porcentaje
  )

#EXTASIS 

datos <- datos %>%
  mutate(extasis = case_when(
    eb_6_d %in% 1:2 ~ 1,
    eb_6_d == 3 ~ 0,
    eb_6_d == 0 ~ 0,
  ))

tasa_consumo_extasis <- datos %>%
  group_by(Region) %>%
  summarise(
    total_consumidores = sum(extasis * Fexp, na.rm = TRUE),  # Ignorar NA en los cálculos
    total_individuos = sum(Fexp, na.rm = TRUE),             # Ignorar NA en los cálculos
    tasa_consumo = ifelse(total_individuos == 0, NA, total_consumidores / total_individuos)  # Evitar división por cero
  )

tasa_consumo_extasis_total <- datos %>%
  summarise(
    total_consumidores = sum(extasis * Fexp, na.rm = TRUE),   # Total ponderado de consumidores
    total_individuos = sum(Fexp, na.rm = TRUE),            # Total ponderado de individuos
    tasa_consumo = ifelse(total_individuos == 0, NA, total_consumidores / total_individuos )  # Calcular la tasa como porcentaje
  )

write_xlsx(
  list(
    "Tasa prensada" = tasa_consumo_pensada,
    "Tasa prensada total" = tasa_consumo_pensada_total,
    "Tasa no prensada" = tasa_consumo_no_pensada,
    "Tasa no prensada total" = tasa_consumo_no_pensada_total,
    "Tasa mari" = tasa_consumo_mari,
    "Tasa mari total" = tasa_consumo_mari_total,
    "Tasa coca" = tasa_consumo_coca,
    "Tasa coca total" = tasa_consumo_coca_total,
    "Tasa pasta" = tasa_consumo_pasta,
    "Tasa pasta total" = tasa_consumo_pasta_total,
    "Tasa extasis" = tasa_consumo_extasis,
    "Tasa extasis total" = tasa_consumo_extasis_total
  ),
  "oferta.xlsx"
)

#2014

rm(list = ls())
setwd("C:/Users/cjara/OneDrive - Universidad Católica de Chile/Mercado drogas/SENDA/2014")
datos <- read_dta("C:/Users/cjara/OneDrive - Universidad Católica de Chile/Mercado drogas/SENDA/2014/BBDD.dta")

datos <- datos %>%
  mutate(Region = case_when(
    Comuna %in% c(15101) ~ 15,
    Comuna %in% c(1101, 1107) ~ 1,
    Comuna %in% c(2101, 2201) ~ 2,
    Comuna %in% c(3101, 3301) ~ 3,
    Comuna %in% c(4101, 4102, 4301) ~ 4,
    Comuna %in% c(5101, 5103, 5109, 5301, 5501, 5502, 5601, 5701, 5801, 5802, 5804) ~ 5,
    Comuna %in% c(6101, 6106, 6108, 6110, 6115, 6117, 6301, 6303) ~ 6,
    Comuna %in% c(7101, 7102, 7201, 7301, 7401, 7404) ~ 7,
    Comuna %in% c(8101, 8102, 8103, 8106, 8107, 8108, 8110, 8111, 8112, 8205, 8301) ~ 8,
    Comuna %in% c(8401) ~ 16,
    Comuna %in% c(9101, 9108, 9111, 9112, 9114, 9120, 9201, 9211) ~ 9,
    Comuna %in% c(10101, 10201, 10202, 10301) ~ 10,
    Comuna %in% c(11101, 11201) ~ 11,
    Comuna %in% c(12101) ~ 12,
    Comuna %in% c(13101, 13102, 13103, 13104, 13105, 13106, 13107, 13108, 13109, 13110, 13111, 13112, 13113, 13114, 13115, 13116, 13117, 13118, 13119, 13120, 13121, 13122, 13123, 13124, 13125, 13125, 13126, 13127, 13128, 13129, 13130, 13131, 13132, 13201, 13301, 13302, 13401, 13402, 13404, 13501, 13601, 13604, 13605) ~ 13,
    Comuna %in% c(14101, 14108, 14201, 14204) ~ 14,
    # Agrega aquí las demás cod_areas con sus respectivas regiones
    TRUE ~ NA_real_  # Para asignar NA si una cod_area no tiene región definida
  ))

datos <- datos %>%
  rename(Fexp = F2_MAY_AJUS_com)

#TIPO MARIHUANA
table(datos$mar9)

datos <- datos %>%
  mutate(prensada = case_when(
    mar9 %in% 2:99 ~ 0,  # Recodificar valores 1 o 2 como 1 (Año)
    mar9 == 1 ~ 1,
  ))
table(datos$prensada)

datos <- datos %>%
  mutate(no_prensada = case_when(
    mar9 %in% 88:99 ~ 0,  # Recodificar valores 1 o 2 como 1 (Año)
    mar9 == 1 ~ 0,
    mar9 %in% 2:3 ~ 1, 
  ))
table(datos$no_prensada)

tasa_consumo_pensada <- datos %>%
  filter(!is.na(prensada)) %>%  
  group_by(Region) %>%
  summarise(
    total_consumidores = sum((prensada == 1) * Fexp, na.rm = TRUE),  # Pondera solo consumidores
    total_respuestas = sum(Fexp, na.rm = TRUE),                  # Pondera todas las respuestas válidas
    tasa_consumo = ifelse(total_respuestas == 0, NA, total_consumidores / total_respuestas)  # Tasa ajustada
  )

tasa_consumo_pensada_total <- datos %>%
  filter(!is.na(prensada)) %>%  # Asegura que solo se usen respuestas válidas
  summarise(
    total_consumidores = sum((prensada == 1) * Fexp, na.rm = TRUE),  # Pondera solo consumidores
    total_respuestas = sum(Fexp, na.rm = TRUE),                  # Pondera todas las respuestas válidas
    tasa_consumo = ifelse(total_respuestas == 0, NA, total_consumidores / total_respuestas)  # Tasa ajustada
  )

tasa_consumo_no_pensada <- datos %>%
  filter(!is.na(no_prensada)) %>%  
  group_by(Region) %>%
  summarise(
    total_consumidores = sum((no_prensada == 1) * Fexp, na.rm = TRUE),  # Pondera solo consumidores
    total_respuestas = sum(Fexp, na.rm = TRUE),                  # Pondera todas las respuestas válidas
    tasa_consumo = ifelse(total_respuestas == 0, NA, total_consumidores / total_respuestas)  # Tasa ajustada
  )

tasa_consumo_no_pensada_total <- datos %>%
  filter(!is.na(no_prensada)) %>%  # Asegura que solo se usen respuestas válidas
  summarise(
    total_consumidores = sum((no_prensada == 1) * Fexp, na.rm = TRUE),  # Pondera solo consumidores
    total_respuestas = sum(Fexp, na.rm = TRUE),                  # Pondera todas las respuestas válidas
    tasa_consumo = ifelse(total_respuestas == 0, NA, total_consumidores / total_respuestas)  # Tasa ajustada
  )

#MARIHUANA
table(datos$eb6_1)

datos <- datos %>%
  mutate(mari = case_when(
    eb6_1 %in% 1:2 ~ 1,
    eb6_1 == 3 ~ 0,
    eb6_1 == 0 ~ 0,
  ))

tasa_consumo_mari <- datos %>%
  group_by(Region) %>%
  summarise(
    total_consumidores = sum(mari * Fexp, na.rm = TRUE),  # Ignorar NA en los cálculos
    total_individuos = sum(Fexp, na.rm = TRUE),             # Ignorar NA en los cálculos
    tasa_consumo = ifelse(total_individuos == 0, NA, total_consumidores / total_individuos)  # Evitar división por cero
  )

tasa_consumo_mari_total <- datos %>%
  summarise(
    total_consumidores = sum(mari * Fexp, na.rm = TRUE),   # Total ponderado de consumidores
    total_individuos = sum(Fexp, na.rm = TRUE),            # Total ponderado de individuos
    tasa_consumo = ifelse(total_individuos == 0, NA, total_consumidores / total_individuos )  # Calcular la tasa como porcentaje
  )

#COCA

datos <- datos %>%
  mutate(coca = case_when(
    eb6_2 %in% 1:2 ~ 1,
    eb6_2 == 3 ~ 0,
    eb6_2 == 0 ~ 0,
  ))

tasa_consumo_coca <- datos %>%
  group_by(Region) %>%
  summarise(
    total_consumidores = sum(coca * Fexp, na.rm = TRUE),  # Ignorar NA en los cálculos
    total_individuos = sum(Fexp, na.rm = TRUE),             # Ignorar NA en los cálculos
    tasa_consumo = ifelse(total_individuos == 0, NA, total_consumidores / total_individuos)  # Evitar división por cero
  )

tasa_consumo_coca_total <- datos %>%
  summarise(
    total_consumidores = sum(coca * Fexp, na.rm = TRUE),   # Total ponderado de consumidores
    total_individuos = sum(Fexp, na.rm = TRUE),            # Total ponderado de individuos
    tasa_consumo = ifelse(total_individuos == 0, NA, total_consumidores / total_individuos )  # Calcular la tasa como porcentaje
  )

#PASTA BASE

datos <- datos %>%
  mutate(pasta = case_when(
    eb6_3 %in% 1:2 ~ 1,
    eb6_3 == 3 ~ 0,
    eb6_3 == 0 ~ 0,
  ))

tasa_consumo_pasta <- datos %>%
  group_by(Region) %>%
  summarise(
    total_consumidores = sum(pasta * Fexp, na.rm = TRUE),  # Ignorar NA en los cálculos
    total_individuos = sum(Fexp, na.rm = TRUE),             # Ignorar NA en los cálculos
    tasa_consumo = ifelse(total_individuos == 0, NA, total_consumidores / total_individuos)  # Evitar división por cero
  )

tasa_consumo_pasta_total <- datos %>%
  summarise(
    total_consumidores = sum(pasta * Fexp, na.rm = TRUE),   # Total ponderado de consumidores
    total_individuos = sum(Fexp, na.rm = TRUE),            # Total ponderado de individuos
    tasa_consumo = ifelse(total_individuos == 0, NA, total_consumidores / total_individuos )  # Calcular la tasa como porcentaje
  )

#EXTASIS 

datos <- datos %>%
  mutate(extasis = case_when(
    eb6_4 %in% 1:2 ~ 1,
    eb6_4 == 3 ~ 0,
    eb6_4 == 0 ~ 0,
  ))

tasa_consumo_extasis <- datos %>%
  group_by(Region) %>%
  summarise(
    total_consumidores = sum(extasis * Fexp, na.rm = TRUE),  # Ignorar NA en los cálculos
    total_individuos = sum(Fexp, na.rm = TRUE),             # Ignorar NA en los cálculos
    tasa_consumo = ifelse(total_individuos == 0, NA, total_consumidores / total_individuos)  # Evitar división por cero
  )

tasa_consumo_extasis_total <- datos %>%
  summarise(
    total_consumidores = sum(extasis * Fexp, na.rm = TRUE),   # Total ponderado de consumidores
    total_individuos = sum(Fexp, na.rm = TRUE),            # Total ponderado de individuos
    tasa_consumo = ifelse(total_individuos == 0, NA, total_consumidores / total_individuos )  # Calcular la tasa como porcentaje
  )

write_xlsx(
  list(
    "Tasa prensada" = tasa_consumo_pensada,
    "Tasa prensada total" = tasa_consumo_pensada_total,
    "Tasa no prensada" = tasa_consumo_no_pensada,
    "Tasa no prensada total" = tasa_consumo_no_pensada_total,
    "Tasa mari" = tasa_consumo_mari,
    "Tasa mari total" = tasa_consumo_mari_total,
    "Tasa coca" = tasa_consumo_coca,
    "Tasa coca total" = tasa_consumo_coca_total,
    "Tasa pasta" = tasa_consumo_pasta,
    "Tasa pasta total" = tasa_consumo_pasta_total,
    "Tasa extasis" = tasa_consumo_extasis,
    "Tasa extasis total" = tasa_consumo_extasis_total
  ),
  "oferta.xlsx"
)

#2012

rm(list = ls())
setwd("C:/Users/cjara/OneDrive - Universidad Católica de Chile/Mercado drogas/SENDA/2012")
datos <- read_dta("C:/Users/cjara/OneDrive - Universidad Católica de Chile/Mercado drogas/SENDA/2012/BBDD.dta")

table(datos$región)
table(datos$Comuna)

datos <- datos %>%
  rename(Comuna = código_comuna)

datos <- datos %>%
  mutate(Region = case_when(
    Comuna %in% c(15101) ~ 15,
    Comuna %in% c(1101, 1107) ~ 1,
    Comuna %in% c(2101, 2201) ~ 2,
    Comuna %in% c(3101, 3301) ~ 3,
    Comuna %in% c(4101, 4102, 4301) ~ 4,
    Comuna %in% c(5101, 5103, 5109, 5301, 5501, 5502, 5601, 5701, 5801, 5802, 5804) ~ 5,
    Comuna %in% c(6101, 6106, 6108, 6110, 6115, 6117, 6301, 6303) ~ 6,
    Comuna %in% c(7101, 7102, 7201, 7301, 7401, 7404) ~ 7,
    Comuna %in% c(8101, 8102, 8103, 8106, 8107, 8108, 8110, 8111, 8112, 8205, 8301) ~ 8,
    Comuna %in% c(8401) ~ 16,
    Comuna %in% c(9101, 9108, 9111, 9112, 9114, 9120, 9201, 9211) ~ 9,
    Comuna %in% c(10101, 10201, 10202, 10301) ~ 10,
    Comuna %in% c(11101, 11201) ~ 11,
    Comuna %in% c(12101) ~ 12,
    Comuna %in% c(13101, 13102, 13103, 13104, 13105, 13106, 13107, 13108, 13109, 13110, 13111, 13112, 13113, 13114, 13115, 13116, 13117, 13118, 13119, 13120, 13121, 13122, 13123, 13124, 13125, 13125, 13126, 13127, 13128, 13129, 13130, 13131, 13132, 13201, 13301, 13302, 13401, 13402, 13404, 13501, 13601, 13604, 13605) ~ 13,
    Comuna %in% c(14101, 14108, 14201, 14204) ~ 14,
    # Agrega aquí las demás cod_areas con sus respectivas regiones
    TRUE ~ NA_real_  # Para asignar NA si una cod_area no tiene región definida
  ))

datos <- datos %>%
  rename(Fexp = PONDERADOR)

#TIPO MARIHUANA
table(datos$p43)

datos <- datos %>%
  mutate(prensada = case_when(
    p43 %in% 2:99 ~ 0,  # Recodificar valores 1 o 2 como 1 (Año)
    p43 == 1 ~ 1,
  ))
table(datos$prensada)

datos <- datos %>%
  mutate(no_prensada = case_when(
    p43 %in% 88:99 ~ 0,  # Recodificar valores 1 o 2 como 1 (Año)
    p43 == 1 ~ 0,
    p43 %in% 2:3 ~ 1, 
  ))
table(datos$no_prensada)

tasa_consumo_pensada <- datos %>%
  filter(!is.na(prensada)) %>%  
  group_by(Region) %>%
  summarise(
    total_consumidores = sum((prensada == 1) * Fexp, na.rm = TRUE),  # Pondera solo consumidores
    total_respuestas = sum(Fexp, na.rm = TRUE),                  # Pondera todas las respuestas válidas
    tasa_consumo = ifelse(total_respuestas == 0, NA, total_consumidores / total_respuestas)  # Tasa ajustada
  )

tasa_consumo_pensada_total <- datos %>%
  filter(!is.na(prensada)) %>%  # Asegura que solo se usen respuestas válidas
  summarise(
    total_consumidores = sum((prensada == 1) * Fexp, na.rm = TRUE),  # Pondera solo consumidores
    total_respuestas = sum(Fexp, na.rm = TRUE),                  # Pondera todas las respuestas válidas
    tasa_consumo = ifelse(total_respuestas == 0, NA, total_consumidores / total_respuestas)  # Tasa ajustada
  )

tasa_consumo_no_pensada <- datos %>%
  filter(!is.na(no_prensada)) %>%  
  group_by(Region) %>%
  summarise(
    total_consumidores = sum((no_prensada == 1) * Fexp, na.rm = TRUE),  # Pondera solo consumidores
    total_respuestas = sum(Fexp, na.rm = TRUE),                  # Pondera todas las respuestas válidas
    tasa_consumo = ifelse(total_respuestas == 0, NA, total_consumidores / total_respuestas)  # Tasa ajustada
  )

tasa_consumo_no_pensada_total <- datos %>%
  filter(!is.na(no_prensada)) %>%  # Asegura que solo se usen respuestas válidas
  summarise(
    total_consumidores = sum((no_prensada == 1) * Fexp, na.rm = TRUE),  # Pondera solo consumidores
    total_respuestas = sum(Fexp, na.rm = TRUE),                  # Pondera todas las respuestas válidas
    tasa_consumo = ifelse(total_respuestas == 0, NA, total_consumidores / total_respuestas)  # Tasa ajustada
  )

#MARIHUANA
table(datos$p153_1)

datos <- datos %>%
  mutate(mari = case_when(
    p153_1 %in% 1:2 ~ 1,
    p153_1 == 3 ~ 0,
    p153_1 == 0 ~ 0,
  ))

tasa_consumo_mari <- datos %>%
  group_by(Region) %>%
  summarise(
    total_consumidores = sum(mari * Fexp, na.rm = TRUE),  # Ignorar NA en los cálculos
    total_individuos = sum(Fexp, na.rm = TRUE),             # Ignorar NA en los cálculos
    tasa_consumo = ifelse(total_individuos == 0, NA, total_consumidores / total_individuos)  # Evitar división por cero
  )

tasa_consumo_mari_total <- datos %>%
  summarise(
    total_consumidores = sum(mari * Fexp, na.rm = TRUE),   # Total ponderado de consumidores
    total_individuos = sum(Fexp, na.rm = TRUE),            # Total ponderado de individuos
    tasa_consumo = ifelse(total_individuos == 0, NA, total_consumidores / total_individuos )  # Calcular la tasa como porcentaje
  )

#COCA

datos <- datos %>%
  mutate(coca = case_when(
    p153_2 %in% 1:2 ~ 1,
    p153_2 == 3 ~ 0,
    p153_2 == 0 ~ 0,
  ))

tasa_consumo_coca <- datos %>%
  group_by(Region) %>%
  summarise(
    total_consumidores = sum(coca * Fexp, na.rm = TRUE),  # Ignorar NA en los cálculos
    total_individuos = sum(Fexp, na.rm = TRUE),             # Ignorar NA en los cálculos
    tasa_consumo = ifelse(total_individuos == 0, NA, total_consumidores / total_individuos)  # Evitar división por cero
  )

tasa_consumo_coca_total <- datos %>%
  summarise(
    total_consumidores = sum(coca * Fexp, na.rm = TRUE),   # Total ponderado de consumidores
    total_individuos = sum(Fexp, na.rm = TRUE),            # Total ponderado de individuos
    tasa_consumo = ifelse(total_individuos == 0, NA, total_consumidores / total_individuos )  # Calcular la tasa como porcentaje
  )

#PASTA BASE

datos <- datos %>%
  mutate(pasta = case_when(
    p153_3 %in% 1:2 ~ 1,
    p153_3 == 3 ~ 0,
    p153_3 == 0 ~ 0,
  ))

tasa_consumo_pasta <- datos %>%
  group_by(Region) %>%
  summarise(
    total_consumidores = sum(pasta * Fexp, na.rm = TRUE),  # Ignorar NA en los cálculos
    total_individuos = sum(Fexp, na.rm = TRUE),             # Ignorar NA en los cálculos
    tasa_consumo = ifelse(total_individuos == 0, NA, total_consumidores / total_individuos)  # Evitar división por cero
  )

tasa_consumo_pasta_total <- datos %>%
  summarise(
    total_consumidores = sum(pasta * Fexp, na.rm = TRUE),   # Total ponderado de consumidores
    total_individuos = sum(Fexp, na.rm = TRUE),            # Total ponderado de individuos
    tasa_consumo = ifelse(total_individuos == 0, NA, total_consumidores / total_individuos )  # Calcular la tasa como porcentaje
  )

#EXTASIS 

datos <- datos %>%
  mutate(extasis = case_when(
    p153_4 %in% 1:2 ~ 1,
    p153_4 == 3 ~ 0,
    p153_4 == 0 ~ 0,
  ))

tasa_consumo_extasis <- datos %>%
  group_by(Region) %>%
  summarise(
    total_consumidores = sum(extasis * Fexp, na.rm = TRUE),  # Ignorar NA en los cálculos
    total_individuos = sum(Fexp, na.rm = TRUE),             # Ignorar NA en los cálculos
    tasa_consumo = ifelse(total_individuos == 0, NA, total_consumidores / total_individuos)  # Evitar división por cero
  )

tasa_consumo_extasis_total <- datos %>%
  summarise(
    total_consumidores = sum(extasis * Fexp, na.rm = TRUE),   # Total ponderado de consumidores
    total_individuos = sum(Fexp, na.rm = TRUE),            # Total ponderado de individuos
    tasa_consumo = ifelse(total_individuos == 0, NA, total_consumidores / total_individuos )  # Calcular la tasa como porcentaje
  )

write_xlsx(
  list(
    "Tasa prensada" = tasa_consumo_pensada,
    "Tasa prensada total" = tasa_consumo_pensada_total,
    "Tasa no prensada" = tasa_consumo_no_pensada,
    "Tasa no prensada total" = tasa_consumo_no_pensada_total,
    "Tasa mari" = tasa_consumo_mari,
    "Tasa mari total" = tasa_consumo_mari_total,
    "Tasa coca" = tasa_consumo_coca,
    "Tasa coca total" = tasa_consumo_coca_total,
    "Tasa pasta" = tasa_consumo_pasta,
    "Tasa pasta total" = tasa_consumo_pasta_total,
    "Tasa extasis" = tasa_consumo_extasis,
    "Tasa extasis total" = tasa_consumo_extasis_total
  ),
  "oferta.xlsx"
)

#2010

rm(list = ls())
setwd("C:/Users/cjara/OneDrive - Universidad Católica de Chile/Mercado drogas/SENDA/2010")
datos <- read_dta("C:/Users/cjara/OneDrive - Universidad Católica de Chile/Mercado drogas/SENDA/2010/BBDD.dta")

table(datos$pregion)

datos <- datos %>%
  rename(Comuna = pcodcom)

table(datos$Comuna)

datos <- datos %>%
  mutate(Region = case_when(
    Comuna %in% c(15101) ~ 15,
    Comuna %in% c(1101, 1107) ~ 1,
    Comuna %in% c(2101, 2201) ~ 2,
    Comuna %in% c(3101, 3301) ~ 3,
    Comuna %in% c(4101, 4102, 4301) ~ 4,
    Comuna %in% c(5101, 5103, 5109, 5301, 5501, 5502, 5601, 5701, 5801, 5802, 5804) ~ 5,
    Comuna %in% c(6101, 6106, 6108, 6110, 6115, 6117, 6301, 6303) ~ 6,
    Comuna %in% c(7101, 7102, 7201, 7301, 7401, 7404) ~ 7,
    Comuna %in% c(8101, 8102, 8103, 8106, 8107, 8108, 8110, 8111, 8112, 8205, 8301) ~ 8,
    Comuna %in% c(8401) ~ 16,
    Comuna %in% c(9101, 9108, 9111, 9112, 9114, 9120, 9201, 9211) ~ 9,
    Comuna %in% c(10101, 10201, 10202, 10301) ~ 10,
    Comuna %in% c(11101, 11201) ~ 11,
    Comuna %in% c(12101) ~ 12,
    Comuna %in% c(13101, 13102, 13103, 13104, 13105, 13106, 13107, 13108, 13109, 13110, 13111, 13112, 13113, 13114, 13115, 13116, 13117, 13118, 13119, 13120, 13121, 13122, 13123, 13124, 13125, 13125, 13126, 13127, 13128, 13129, 13130, 13131, 13132, 13201, 13301, 13302, 13401, 13402, 13404, 13501, 13601, 13604, 13605) ~ 13,
    Comuna %in% c(14101, 14108, 14201, 14204) ~ 14,
    # Agrega aquí las demás cod_areas con sus respectivas regiones
    TRUE ~ NA_real_  # Para asignar NA si una cod_area no tiene región definida
  ))

datos <- datos %>%
  rename(Fexp = factor_ajustado_com)

#MARIHUANA
table(datos$p245a)

datos <- datos %>%
  mutate(mari = case_when(
    p245a %in% 1:2 ~ 1,
    p245a == 3 ~ 0,
    p245a == 0 ~ 0,
  ))

tasa_consumo_mari <- datos %>%
  group_by(Region) %>%
  summarise(
    total_consumidores = sum(mari * Fexp, na.rm = TRUE),  # Ignorar NA en los cálculos
    total_individuos = sum(Fexp, na.rm = TRUE),             # Ignorar NA en los cálculos
    tasa_consumo = ifelse(total_individuos == 0, NA, total_consumidores / total_individuos)  # Evitar división por cero
  )

tasa_consumo_mari_total <- datos %>%
  summarise(
    total_consumidores = sum(mari * Fexp, na.rm = TRUE),   # Total ponderado de consumidores
    total_individuos = sum(Fexp, na.rm = TRUE),            # Total ponderado de individuos
    tasa_consumo = ifelse(total_individuos == 0, NA, total_consumidores / total_individuos )  # Calcular la tasa como porcentaje
  )

#COCA

datos <- datos %>%
  mutate(coca = case_when(
    p245b %in% 1:2 ~ 1,
    p245b == 3 ~ 0,
    p245b == 0 ~ 0,
  ))

tasa_consumo_coca <- datos %>%
  group_by(Region) %>%
  summarise(
    total_consumidores = sum(coca * Fexp, na.rm = TRUE),  # Ignorar NA en los cálculos
    total_individuos = sum(Fexp, na.rm = TRUE),             # Ignorar NA en los cálculos
    tasa_consumo = ifelse(total_individuos == 0, NA, total_consumidores / total_individuos)  # Evitar división por cero
  )

tasa_consumo_coca_total <- datos %>%
  summarise(
    total_consumidores = sum(coca * Fexp, na.rm = TRUE),   # Total ponderado de consumidores
    total_individuos = sum(Fexp, na.rm = TRUE),            # Total ponderado de individuos
    tasa_consumo = ifelse(total_individuos == 0, NA, total_consumidores / total_individuos )  # Calcular la tasa como porcentaje
  )

#PASTA BASE

datos <- datos %>%
  mutate(pasta = case_when(
    p245c %in% 1:2 ~ 1,
    p245c == 3 ~ 0,
    p245c == 0 ~ 0,
  ))

tasa_consumo_pasta <- datos %>%
  group_by(Region) %>%
  summarise(
    total_consumidores = sum(pasta * Fexp, na.rm = TRUE),  # Ignorar NA en los cálculos
    total_individuos = sum(Fexp, na.rm = TRUE),             # Ignorar NA en los cálculos
    tasa_consumo = ifelse(total_individuos == 0, NA, total_consumidores / total_individuos)  # Evitar división por cero
  )

tasa_consumo_pasta_total <- datos %>%
  summarise(
    total_consumidores = sum(pasta * Fexp, na.rm = TRUE),   # Total ponderado de consumidores
    total_individuos = sum(Fexp, na.rm = TRUE),            # Total ponderado de individuos
    tasa_consumo = ifelse(total_individuos == 0, NA, total_consumidores / total_individuos )  # Calcular la tasa como porcentaje
  )

#EXTASIS 

datos <- datos %>%
  mutate(extasis = case_when(
    p245d %in% 1:2 ~ 1,
    p245d == 3 ~ 0,
    p245d == 0 ~ 0,
  ))

tasa_consumo_extasis <- datos %>%
  group_by(Region) %>%
  summarise(
    total_consumidores = sum(extasis * Fexp, na.rm = TRUE),  # Ignorar NA en los cálculos
    total_individuos = sum(Fexp, na.rm = TRUE),             # Ignorar NA en los cálculos
    tasa_consumo = ifelse(total_individuos == 0, NA, total_consumidores / total_individuos)  # Evitar división por cero
  )

tasa_consumo_extasis_total <- datos %>%
  summarise(
    total_consumidores = sum(extasis * Fexp, na.rm = TRUE),   # Total ponderado de consumidores
    total_individuos = sum(Fexp, na.rm = TRUE),            # Total ponderado de individuos
    tasa_consumo = ifelse(total_individuos == 0, NA, total_consumidores / total_individuos )  # Calcular la tasa como porcentaje
  )

write_xlsx(
  list(
    "Tasa mari" = tasa_consumo_mari,
    "Tasa mari total" = tasa_consumo_mari_total,
    "Tasa coca" = tasa_consumo_coca,
    "Tasa coca total" = tasa_consumo_coca_total,
    "Tasa pasta" = tasa_consumo_pasta,
    "Tasa pasta total" = tasa_consumo_pasta_total,
    "Tasa extasis" = tasa_consumo_extasis,
    "Tasa extasis total" = tasa_consumo_extasis_total
  ),
  "oferta.xlsx"
)

#2008

rm(list = ls())
setwd("C:/Users/cjara/OneDrive - Universidad Católica de Chile/Mercado drogas/SENDA/2008")
datos <- read_dta("C:/Users/cjara/OneDrive - Universidad Católica de Chile/Mercado drogas/SENDA/2008/BBDD.dta")

table(datos$region)

datos <- datos %>%
  rename(cod_area = comuna)

datos <- datos %>%
  mutate(Region = case_when(
    cod_area %in% c(1) ~ 15,
    cod_area %in% c(2, 3) ~ 1,
    cod_area %in% c(4, 5) ~ 2,
    cod_area %in% c(6, 7) ~ 3,
    cod_area %in% c(8, 9, 10) ~ 4,
    cod_area %in% c(11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21) ~ 5,
    cod_area %in% c(22, 23, 24) ~ 6,
    cod_area %in% c(25, 26, 27, 28, 29) ~ 7,
    cod_area %in% c(31, 32, 33, 34, 35, 36, 37, 38, 39, 40, 41) ~ 8,
    cod_area %in% c(30) ~ 16,
    cod_area %in% c(42, 43, 44) ~ 9,
    cod_area %in% c(46, 47, 48, 49, 50) ~ 10,
    cod_area %in% c(51, 52) ~ 11,
    cod_area %in% c(53) ~ 12,
    cod_area %in% c(54, 55, 56, 57, 58, 59, 60, 61, 62, 63, 64, 65, 66, 67, 68, 69, 70, 71, 72, 73, 74, 75, 76, 77, 78, 79, 80, 81, 82, 83, 84, 85, 86, 87, 88, 89, 90, 91, 92, 93, 94, 95) ~ 13,
    cod_area %in% c(45) ~ 14,
    # Agrega aquí las demás cod_areas con sus respectivas regiones
    TRUE ~ NA_real_  # Para asignar NA si una cod_area no tiene región definida
  ))

table(datos$Region)

datos <- datos %>%
  rename(Fexp = exp)

#MARIHUANA
table(datos$q218)

datos <- datos %>%
  mutate(mari = case_when(
    q218 %in% 1:2 ~ 1,
    q218 %in% 3:4 ~ 0,
    q218 == 0 ~ 0,
  ))

table(datos$mari)

tasa_consumo_mari <- datos %>%
  group_by(Region) %>%
  summarise(
    total_consumidores = sum(mari * Fexp, na.rm = TRUE),  # Ignorar NA en los cálculos
    total_individuos = sum(Fexp, na.rm = TRUE),             # Ignorar NA en los cálculos
    tasa_consumo = ifelse(total_individuos == 0, NA, total_consumidores / total_individuos)  # Evitar división por cero
  )

tasa_consumo_mari_total <- datos %>%
  summarise(
    total_consumidores = sum(mari * Fexp, na.rm = TRUE),   # Total ponderado de consumidores
    total_individuos = sum(Fexp, na.rm = TRUE),            # Total ponderado de individuos
    tasa_consumo = ifelse(total_individuos == 0, NA, total_consumidores / total_individuos )  # Calcular la tasa como porcentaje
  )

#COCA
table(datos$q219)
datos <- datos %>%
  mutate(coca = case_when(
    q219 %in% 1:2 ~ 1,
    q219 %in% 3:4 ~ 0,
    q219 == 0 ~ 0,
  ))

tasa_consumo_coca <- datos %>%
  group_by(Region) %>%
  summarise(
    total_consumidores = sum(coca * Fexp, na.rm = TRUE),  # Ignorar NA en los cálculos
    total_individuos = sum(Fexp, na.rm = TRUE),             # Ignorar NA en los cálculos
    tasa_consumo = ifelse(total_individuos == 0, NA, total_consumidores / total_individuos)  # Evitar división por cero
  )

tasa_consumo_coca_total <- datos %>%
  summarise(
    total_consumidores = sum(coca * Fexp, na.rm = TRUE),   # Total ponderado de consumidores
    total_individuos = sum(Fexp, na.rm = TRUE),            # Total ponderado de individuos
    tasa_consumo = ifelse(total_individuos == 0, NA, total_consumidores / total_individuos )  # Calcular la tasa como porcentaje
  )

#PASTA BASE

datos <- datos %>%
  mutate(pasta = case_when(
    q220 %in% 1:2 ~ 1,
    q220 %in% 3:4 ~ 0,
    q220 == 0 ~ 0,
  ))

tasa_consumo_pasta <- datos %>%
  group_by(Region) %>%
  summarise(
    total_consumidores = sum(pasta * Fexp, na.rm = TRUE),  # Ignorar NA en los cálculos
    total_individuos = sum(Fexp, na.rm = TRUE),             # Ignorar NA en los cálculos
    tasa_consumo = ifelse(total_individuos == 0, NA, total_consumidores / total_individuos)  # Evitar división por cero
  )

tasa_consumo_pasta_total <- datos %>%
  summarise(
    total_consumidores = sum(pasta * Fexp, na.rm = TRUE),   # Total ponderado de consumidores
    total_individuos = sum(Fexp, na.rm = TRUE),            # Total ponderado de individuos
    tasa_consumo = ifelse(total_individuos == 0, NA, total_consumidores / total_individuos )  # Calcular la tasa como porcentaje
  )

#EXTASIS 

datos <- datos %>%
  mutate(extasis = case_when(
    q221 %in% 1:2 ~ 1,
    q221 %in% 3:4 ~ 0,
    q221 == 0 ~ 0,
  ))

tasa_consumo_extasis <- datos %>%
  group_by(Region) %>%
  summarise(
    total_consumidores = sum(extasis * Fexp, na.rm = TRUE),  # Ignorar NA en los cálculos
    total_individuos = sum(Fexp, na.rm = TRUE),             # Ignorar NA en los cálculos
    tasa_consumo = ifelse(total_individuos == 0, NA, total_consumidores / total_individuos)  # Evitar división por cero
  )

tasa_consumo_extasis_total <- datos %>%
  summarise(
    total_consumidores = sum(extasis * Fexp, na.rm = TRUE),   # Total ponderado de consumidores
    total_individuos = sum(Fexp, na.rm = TRUE),            # Total ponderado de individuos
    tasa_consumo = ifelse(total_individuos == 0, NA, total_consumidores / total_individuos )  # Calcular la tasa como porcentaje
  )

write_xlsx(
  list(
    "Tasa mari" = tasa_consumo_mari,
    "Tasa mari total" = tasa_consumo_mari_total,
    "Tasa coca" = tasa_consumo_coca,
    "Tasa coca total" = tasa_consumo_coca_total,
    "Tasa pasta" = tasa_consumo_pasta,
    "Tasa pasta total" = tasa_consumo_pasta_total,
    "Tasa extasis" = tasa_consumo_extasis,
    "Tasa extasis total" = tasa_consumo_extasis_total
  ),
  "oferta.xlsx"
)

#2006
rm(list = ls())
setwd("C:/Users/cjara/OneDrive - Universidad Católica de Chile/Mercado drogas/SENDA/2006")
datos <- read_dta("C:/Users/cjara/OneDrive - Universidad Católica de Chile/Mercado drogas/SENDA/2006/BBDD.dta")

table(datos$region)

datos <- datos %>%
  rename(cod_area = comuna)

datos <- datos %>%
  mutate(Region = case_when(
    cod_area %in% c(58) ~ 15,
    cod_area %in% c(81, 82) ~ 1,
    cod_area %in% c(59, 63) ~ 2,
    cod_area %in% c(87, 90) ~ 3,
    cod_area %in% c(84, 91, 89) ~ 4,
    cod_area %in% c(39, 40, 60, 70, 72, 73, 74, 75, 76, 80) ~ 5,
    cod_area %in% c(41, 77, 78) ~ 6,
    cod_area %in% c(52, 54, 56, 64, 65) ~ 7,
    cod_area %in% c(36, 44, 45, 46, 47, 48, 50, 51, 53, 55, 79) ~ 8,
    cod_area %in% c(49) ~ 16,
    cod_area %in% c(37, 38, 57) ~ 9,
    cod_area %in% c(43, 61, 71) ~ 10,
    cod_area %in% c(35, 42) ~ 11,
    cod_area %in% c(62) ~ 12,
    cod_area %in% c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 66, 67, 68, 69, 83, 85, 86) ~ 13,
    cod_area %in% c(88) ~ 14,
    # Agrega aquí las demás cod_areas con sus respectivas regiones
    TRUE ~ NA_real_  # Para asignar NA si una cod_area no tiene región definida
  ))

table(datos$Region)

datos <- datos %>%
  rename(Fexp = exp)

#MARIHUANA
table(datos$p155_1)

datos <- datos %>%
  mutate(mari = case_when(
    p155_1 %in% 1:2 ~ 1,
    p155_1 %in% 3:4 ~ 0,
    p155_1 == 0 ~ 0,
  ))

table(datos$mari)

tasa_consumo_mari <- datos %>%
  group_by(Region) %>%
  summarise(
    total_consumidores = sum(mari * Fexp, na.rm = TRUE),  # Ignorar NA en los cálculos
    total_individuos = sum(Fexp, na.rm = TRUE),             # Ignorar NA en los cálculos
    tasa_consumo = ifelse(total_individuos == 0, NA, total_consumidores / total_individuos)  # Evitar división por cero
  )

tasa_consumo_mari_total <- datos %>%
  summarise(
    total_consumidores = sum(mari * Fexp, na.rm = TRUE),   # Total ponderado de consumidores
    total_individuos = sum(Fexp, na.rm = TRUE),            # Total ponderado de individuos
    tasa_consumo = ifelse(total_individuos == 0, NA, total_consumidores / total_individuos )  # Calcular la tasa como porcentaje
  )

#COCA
datos <- datos %>%
  mutate(coca = case_when(
    p155_3 %in% 1:2 ~ 1,
    p155_3 %in% 3:4 ~ 0,
    p155_3 == 0 ~ 0,
  ))

tasa_consumo_coca <- datos %>%
  group_by(Region) %>%
  summarise(
    total_consumidores = sum(coca * Fexp, na.rm = TRUE),  # Ignorar NA en los cálculos
    total_individuos = sum(Fexp, na.rm = TRUE),             # Ignorar NA en los cálculos
    tasa_consumo = ifelse(total_individuos == 0, NA, total_consumidores / total_individuos)  # Evitar división por cero
  )

tasa_consumo_coca_total <- datos %>%
  summarise(
    total_consumidores = sum(coca * Fexp, na.rm = TRUE),   # Total ponderado de consumidores
    total_individuos = sum(Fexp, na.rm = TRUE),            # Total ponderado de individuos
    tasa_consumo = ifelse(total_individuos == 0, NA, total_consumidores / total_individuos )  # Calcular la tasa como porcentaje
  )

#PASTA BASE

datos <- datos %>%
  mutate(pasta = case_when(
    p155_2 %in% 1:2 ~ 1,
    p155_2 %in% 3:4 ~ 0,
    p155_2 == 0 ~ 0,
  ))

tasa_consumo_pasta <- datos %>%
  group_by(Region) %>%
  summarise(
    total_consumidores = sum(pasta * Fexp, na.rm = TRUE),  # Ignorar NA en los cálculos
    total_individuos = sum(Fexp, na.rm = TRUE),             # Ignorar NA en los cálculos
    tasa_consumo = ifelse(total_individuos == 0, NA, total_consumidores / total_individuos)  # Evitar división por cero
  )

tasa_consumo_pasta_total <- datos %>%
  summarise(
    total_consumidores = sum(pasta * Fexp, na.rm = TRUE),   # Total ponderado de consumidores
    total_individuos = sum(Fexp, na.rm = TRUE),            # Total ponderado de individuos
    tasa_consumo = ifelse(total_individuos == 0, NA, total_consumidores / total_individuos )  # Calcular la tasa como porcentaje
  )

#EXTASIS 

datos <- datos %>%
  mutate(extasis = case_when(
    p155_4 %in% 1:2 ~ 1,
    p155_4 %in% 3:4 ~ 0,
    p155_4 == 0 ~ 0,
  ))

tasa_consumo_extasis <- datos %>%
  group_by(Region) %>%
  summarise(
    total_consumidores = sum(extasis * Fexp, na.rm = TRUE),  # Ignorar NA en los cálculos
    total_individuos = sum(Fexp, na.rm = TRUE),             # Ignorar NA en los cálculos
    tasa_consumo = ifelse(total_individuos == 0, NA, total_consumidores / total_individuos)  # Evitar división por cero
  )

tasa_consumo_extasis_total <- datos %>%
  summarise(
    total_consumidores = sum(extasis * Fexp, na.rm = TRUE),   # Total ponderado de consumidores
    total_individuos = sum(Fexp, na.rm = TRUE),            # Total ponderado de individuos
    tasa_consumo = ifelse(total_individuos == 0, NA, total_consumidores / total_individuos )  # Calcular la tasa como porcentaje
  )

write_xlsx(
  list(
    "Tasa mari" = tasa_consumo_mari,
    "Tasa mari total" = tasa_consumo_mari_total,
    "Tasa coca" = tasa_consumo_coca,
    "Tasa coca total" = tasa_consumo_coca_total,
    "Tasa pasta" = tasa_consumo_pasta,
    "Tasa pasta total" = tasa_consumo_pasta_total,
    "Tasa extasis" = tasa_consumo_extasis,
    "Tasa extasis total" = tasa_consumo_extasis_total
  ),
  "oferta.xlsx"
)

#2004

rm(list = ls())
setwd("C:/Users/cjara/OneDrive - Universidad Católica de Chile/Mercado drogas/SENDA/2004")
datos <- read_dta("C:/Users/cjara/OneDrive - Universidad Católica de Chile/Mercado drogas/SENDA/2004/BBDD.dta")

table(datos$region)

datos <- datos %>%
  rename(cod_area = comuna)

datos <- datos %>%
  mutate(Region = case_when(
    cod_area %in% c(1) ~ 15,
    cod_area %in% c(2, 3) ~ 1,
    cod_area %in% c(4) ~ 2,
    cod_area %in% c(5, 6) ~ 3,
    cod_area %in% c(7, 8, 9) ~ 4,
    cod_area %in% c(10, 11, 12, 13, 14, 15, 16, 17, 18, 19) ~ 5,
    cod_area %in% c(20, 21, 22) ~ 6,
    cod_area %in% c(23, 24, 25, 26, 27) ~ 7,
    cod_area %in% c(29, 30, 31, 32, 33, 34, 35, 36, 37, 38) ~ 8,
    cod_area %in% c(28) ~ 16,
    cod_area %in% c(39, 40) ~ 9,
    cod_area %in% c(42, 43, 44) ~ 10,
    cod_area %in% c(45, 87) ~ 11,
    cod_area %in% c(46) ~ 12,
    cod_area %in% c(47, 48, 49, 50, 51, 52, 53, 54, 55, 56, 57, 58, 59, 60, 61, 62, 63, 64, 65, 66, 67, 68, 69, 70, 71, 72, 73, 74, 75, 76, 77, 78, 79, 80, 81, 82, 83, 84, 85, 86) ~ 13,
    cod_area %in% c(41) ~ 14,
    # Agrega aquí las demás cod_areas con sus respectivas regiones
    TRUE ~ NA_real_  # Para asignar NA si una cod_area no tiene región definida
  ))

table(datos$Region)

datos <- datos %>%
  rename(Fexp = exp)

#MARIHUANA
table(datos$p162a)

datos <- datos %>%
  mutate(mari = case_when(
    p162a %in% 1:2 ~ 1,
    p162a %in% 3:4 ~ 0,
    p162a == 0 ~ 0,
  ))

table(datos$mari)

tasa_consumo_mari <- datos %>%
  group_by(Region) %>%
  summarise(
    total_consumidores = sum(mari * Fexp, na.rm = TRUE),  # Ignorar NA en los cálculos
    total_individuos = sum(Fexp, na.rm = TRUE),             # Ignorar NA en los cálculos
    tasa_consumo = ifelse(total_individuos == 0, NA, total_consumidores / total_individuos)  # Evitar división por cero
  )

tasa_consumo_mari_total <- datos %>%
  summarise(
    total_consumidores = sum(mari * Fexp, na.rm = TRUE),   # Total ponderado de consumidores
    total_individuos = sum(Fexp, na.rm = TRUE),            # Total ponderado de individuos
    tasa_consumo = ifelse(total_individuos == 0, NA, total_consumidores / total_individuos )  # Calcular la tasa como porcentaje
  )

#COCA
datos <- datos %>%
  mutate(coca = case_when(
    p162c %in% 1:2 ~ 1,
    p162c %in% 3:4 ~ 0,
    p162c == 0 ~ 0,
  ))

tasa_consumo_coca <- datos %>%
  group_by(Region) %>%
  summarise(
    total_consumidores = sum(coca * Fexp, na.rm = TRUE),  # Ignorar NA en los cálculos
    total_individuos = sum(Fexp, na.rm = TRUE),             # Ignorar NA en los cálculos
    tasa_consumo = ifelse(total_individuos == 0, NA, total_consumidores / total_individuos)  # Evitar división por cero
  )

tasa_consumo_coca_total <- datos %>%
  summarise(
    total_consumidores = sum(coca * Fexp, na.rm = TRUE),   # Total ponderado de consumidores
    total_individuos = sum(Fexp, na.rm = TRUE),            # Total ponderado de individuos
    tasa_consumo = ifelse(total_individuos == 0, NA, total_consumidores / total_individuos )  # Calcular la tasa como porcentaje
  )

#PASTA BASE

datos <- datos %>%
  mutate(pasta = case_when(
    p162b %in% 1:2 ~ 1,
    p162b %in% 3:4 ~ 0,
    p162b == 0 ~ 0,
  ))

tasa_consumo_pasta <- datos %>%
  group_by(Region) %>%
  summarise(
    total_consumidores = sum(pasta * Fexp, na.rm = TRUE),  # Ignorar NA en los cálculos
    total_individuos = sum(Fexp, na.rm = TRUE),             # Ignorar NA en los cálculos
    tasa_consumo = ifelse(total_individuos == 0, NA, total_consumidores / total_individuos)  # Evitar división por cero
  )

tasa_consumo_pasta_total <- datos %>%
  summarise(
    total_consumidores = sum(pasta * Fexp, na.rm = TRUE),   # Total ponderado de consumidores
    total_individuos = sum(Fexp, na.rm = TRUE),            # Total ponderado de individuos
    tasa_consumo = ifelse(total_individuos == 0, NA, total_consumidores / total_individuos )  # Calcular la tasa como porcentaje
  )

#EXTASIS 

datos <- datos %>%
  mutate(extasis = case_when(
    p162d %in% 1:2 ~ 1,
    p162d %in% 3:4 ~ 0,
    p162d == 0 ~ 0,
  ))

tasa_consumo_extasis <- datos %>%
  group_by(Region) %>%
  summarise(
    total_consumidores = sum(extasis * Fexp, na.rm = TRUE),  # Ignorar NA en los cálculos
    total_individuos = sum(Fexp, na.rm = TRUE),             # Ignorar NA en los cálculos
    tasa_consumo = ifelse(total_individuos == 0, NA, total_consumidores / total_individuos)  # Evitar división por cero
  )

tasa_consumo_extasis_total <- datos %>%
  summarise(
    total_consumidores = sum(extasis * Fexp, na.rm = TRUE),   # Total ponderado de consumidores
    total_individuos = sum(Fexp, na.rm = TRUE),            # Total ponderado de individuos
    tasa_consumo = ifelse(total_individuos == 0, NA, total_consumidores / total_individuos )  # Calcular la tasa como porcentaje
  )

write_xlsx(
  list(
    "Tasa mari" = tasa_consumo_mari,
    "Tasa mari total" = tasa_consumo_mari_total,
    "Tasa coca" = tasa_consumo_coca,
    "Tasa coca total" = tasa_consumo_coca_total,
    "Tasa pasta" = tasa_consumo_pasta,
    "Tasa pasta total" = tasa_consumo_pasta_total,
    "Tasa extasis" = tasa_consumo_extasis,
    "Tasa extasis total" = tasa_consumo_extasis_total
  ),
  "oferta.xlsx"
)

#2002
rm(list = ls())
setwd("C:/Users/cjara/OneDrive - Universidad Católica de Chile/Mercado drogas/SENDA/2002")
datos <- read_dta("C:/Users/cjara/OneDrive - Universidad Católica de Chile/Mercado drogas/SENDA/2002/BBDD.dta")

table(datos$region)

datos <- datos %>%
  mutate(Region = case_when(
    cod_area %in% c(1) ~ 15,
    cod_area %in% c(2, 3) ~ 1,
    cod_area %in% c(4) ~ 2,
    cod_area %in% c(5, 6) ~ 3,
    cod_area %in% c(7, 8, 9) ~ 4,
    cod_area %in% c(10, 11, 12, 13, 14, 15, 16, 17, 18, 19) ~ 5,
    cod_area %in% c(20, 21, 22) ~ 6,
    cod_area %in% c(23, 24, 25, 26, 27) ~ 7,
    cod_area %in% c(29, 30, 31, 32, 33, 34, 35, 36, 37, 38) ~ 8,
    cod_area %in% c(28) ~ 16,
    cod_area %in% c(39, 40) ~ 9,
    cod_area %in% c(42, 43, 44) ~ 10,
    cod_area %in% c(45, 87) ~ 11,
    cod_area %in% c(46) ~ 12,
    cod_area %in% c(47, 48, 49, 50, 51, 52, 53, 54, 55, 56, 57, 58, 59, 60, 61, 62, 63, 64, 65, 66, 67, 68, 69, 70, 71, 72, 73, 74, 75, 76, 77, 78, 79, 80, 81, 82, 83, 84, 85, 86) ~ 13,
    cod_area %in% c(41) ~ 14,
    # Agrega aquí las demás cod_areas con sus respectivas regiones
    TRUE ~ NA_real_  # Para asignar NA si una cod_area no tiene región definida
  ))

table(datos$Region)

datos <- datos %>%
  rename(Fexp = exp)

#MARIHUANA
table(datos$p143a)

datos <- datos %>%
  mutate(mari = case_when(
    p143a %in% 1:2 ~ 1,
    p143a %in% 3:4 ~ 0,
    p143a == 0 ~ 0,
  ))

table(datos$mari)

tasa_consumo_mari <- datos %>%
  group_by(Region) %>%
  summarise(
    total_consumidores = sum(mari * Fexp, na.rm = TRUE),  # Ignorar NA en los cálculos
    total_individuos = sum(Fexp, na.rm = TRUE),             # Ignorar NA en los cálculos
    tasa_consumo = ifelse(total_individuos == 0, NA, total_consumidores / total_individuos)  # Evitar división por cero
  )

tasa_consumo_mari_total <- datos %>%
  summarise(
    total_consumidores = sum(mari * Fexp, na.rm = TRUE),   # Total ponderado de consumidores
    total_individuos = sum(Fexp, na.rm = TRUE),            # Total ponderado de individuos
    tasa_consumo = ifelse(total_individuos == 0, NA, total_consumidores / total_individuos )  # Calcular la tasa como porcentaje
  )

#COCA
datos <- datos %>%
  mutate(coca = case_when(
    p143c %in% 1:2 ~ 1,
    p143c %in% 3:4 ~ 0,
    p143c == 0 ~ 0,
  ))

tasa_consumo_coca <- datos %>%
  group_by(Region) %>%
  summarise(
    total_consumidores = sum(coca * Fexp, na.rm = TRUE),  # Ignorar NA en los cálculos
    total_individuos = sum(Fexp, na.rm = TRUE),             # Ignorar NA en los cálculos
    tasa_consumo = ifelse(total_individuos == 0, NA, total_consumidores / total_individuos)  # Evitar división por cero
  )

tasa_consumo_coca_total <- datos %>%
  summarise(
    total_consumidores = sum(coca * Fexp, na.rm = TRUE),   # Total ponderado de consumidores
    total_individuos = sum(Fexp, na.rm = TRUE),            # Total ponderado de individuos
    tasa_consumo = ifelse(total_individuos == 0, NA, total_consumidores / total_individuos )  # Calcular la tasa como porcentaje
  )

#PASTA BASE

datos <- datos %>%
  mutate(pasta = case_when(
    p143b %in% 1:2 ~ 1,
    p143b %in% 3:4 ~ 0,
    p143b == 0 ~ 0,
  ))

tasa_consumo_pasta <- datos %>%
  group_by(Region) %>%
  summarise(
    total_consumidores = sum(pasta * Fexp, na.rm = TRUE),  # Ignorar NA en los cálculos
    total_individuos = sum(Fexp, na.rm = TRUE),             # Ignorar NA en los cálculos
    tasa_consumo = ifelse(total_individuos == 0, NA, total_consumidores / total_individuos)  # Evitar división por cero
  )

tasa_consumo_pasta_total <- datos %>%
  summarise(
    total_consumidores = sum(pasta * Fexp, na.rm = TRUE),   # Total ponderado de consumidores
    total_individuos = sum(Fexp, na.rm = TRUE),            # Total ponderado de individuos
    tasa_consumo = ifelse(total_individuos == 0, NA, total_consumidores / total_individuos )  # Calcular la tasa como porcentaje
  )

#EXTASIS 

datos <- datos %>%
  mutate(extasis = case_when(
    p143d %in% 1:2 ~ 1,
    p143d %in% 3:4 ~ 0,
    p143d == 0 ~ 0,
  ))

tasa_consumo_extasis <- datos %>%
  group_by(Region) %>%
  summarise(
    total_consumidores = sum(extasis * Fexp, na.rm = TRUE),  # Ignorar NA en los cálculos
    total_individuos = sum(Fexp, na.rm = TRUE),             # Ignorar NA en los cálculos
    tasa_consumo = ifelse(total_individuos == 0, NA, total_consumidores / total_individuos)  # Evitar división por cero
  )

tasa_consumo_extasis_total <- datos %>%
  summarise(
    total_consumidores = sum(extasis * Fexp, na.rm = TRUE),   # Total ponderado de consumidores
    total_individuos = sum(Fexp, na.rm = TRUE),            # Total ponderado de individuos
    tasa_consumo = ifelse(total_individuos == 0, NA, total_consumidores / total_individuos )  # Calcular la tasa como porcentaje
  )

write_xlsx(
  list(
    "Tasa mari" = tasa_consumo_mari,
    "Tasa mari total" = tasa_consumo_mari_total,
    "Tasa coca" = tasa_consumo_coca,
    "Tasa coca total" = tasa_consumo_coca_total,
    "Tasa pasta" = tasa_consumo_pasta,
    "Tasa pasta total" = tasa_consumo_pasta_total,
    "Tasa extasis" = tasa_consumo_extasis,
    "Tasa extasis total" = tasa_consumo_extasis_total
  ),
  "oferta.xlsx"
)