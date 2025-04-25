library(haven)
library(dplyr)
library(sjlabelled)
library(writexl)
options(scipen = 999)


#2023
datos <- read_sav("bbdd2023.sav")

## Pandillas violentas

datos <- datos %>%
  mutate(pandillas = case_when(
    P_INCIVILIDADES_3 %in% 5 ~ 1,  # Recodificar valores 5 como 1
    P_INCIVILIDADES_3 %in% 1:4 ~ 0 # Recodificar valores 1 a 4 como 0
  ))

table(datos$enc_region)

datos <- datos %>%
  mutate(enc_region = factor(enc_region, levels = 1:16))


datos <- datos %>%
  mutate(region = factor(enc_region, 
                         levels = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16), 
                         labels = c("Tarapacá", "Antofagasta", "Atacama", "Coquimbo", "Valparaíso", 
                                    "Bernardo O'Higgins", "Maule", "Biobío", "Araucanía", 
                                    "Los Lagos", "Aysén", "Magallanes", "RM", "Los Ríos", 
                                    "Arica y Parinacota", "Ñuble")
  ))

tasa_presencia <- datos %>%
  group_by(region) %>%
  summarise(
    total_siempre = sum(pandillas * Fact_Pers_Com, na.rm = TRUE),  # Ignorar NA en los cálculos
    total_individuos = sum(Fact_Pers_Com, na.rm = TRUE),             # Ignorar NA en los cálculos
    tasa_presencia = ifelse(total_individuos == 0, NA, total_siempre / total_individuos)  # Evitar división por cero
  )


tasa_presencia2 <- datos %>%
  group_by(region) %>%
  summarise(
    total_siempre = sum(pandillas * Fact_Pers_Reg, na.rm = TRUE),  # Ignorar NA en los cálculos
    total_individuos = sum(Fact_Pers_Reg, na.rm = TRUE),             # Ignorar NA en los cálculos
    tasa_presencia = ifelse(total_individuos == 0, NA, total_siempre / total_individuos)  # Evitar división por cero
  )

tasa_presencia_total <- datos %>%
  summarise(
    total_siempre = sum(pandillas * Fact_Pers_Com, na.rm = TRUE),   # Total ponderado de consumidores
    total_individuos = sum(Fact_Pers_Com, na.rm = TRUE),            # Total ponderado de individuos
    tasa_presencia = ifelse(total_individuos == 0, NA, total_siempre / total_individuos )  # Calcular la tasa como porcentaje
  )

write_xlsx(
  list(
    "Tasa presencia" = tasa_presencia,
    "Tasa presencia total" = tasa_presencia_total
  ),
  "tasa_presencia_pandillas_2023.xlsx"
)


## Peleas calljeras con arma blanco o de fuego 

datos <- datos %>%
  mutate(peleas = case_when(
    P_INCIVILIDADES_4 %in% 5 ~ 1,  # Recodificar valores 5 como 1
    P_INCIVILIDADES_4 %in% 1:4 ~ 0 # Recodificar valores 1 a 4 como 0
  ))

table(datos$enc_region)

datos <- datos %>%
  mutate(enc_region = factor(enc_region, levels = 1:16))


datos <- datos %>%
  mutate(region = factor(enc_region, 
                         levels = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16), 
                         labels = c("Tarapacá", "Antofagasta", "Atacama", "Coquimbo", "Valparaíso", 
                                    "Bernardo O'Higgins", "Maule", "Biobío", "Araucanía", 
                                    "Los Lagos", "Aysén", "Magallanes", "RM", "Los Ríos", 
                                    "Arica y Parinacota", "Ñuble")
  ))

tasa_presencia <- datos %>%
  group_by(region) %>%
  summarise(
    total_siempre = sum(peleas * Fact_Pers_Com, na.rm = TRUE),  # Ignorar NA en los cálculos
    total_individuos = sum(Fact_Pers_Com, na.rm = TRUE),             # Ignorar NA en los cálculos
    tasa_presencia = ifelse(total_individuos == 0, NA, total_siempre / total_individuos)  # Evitar división por cero
  )


tasa_presencia2 <- datos %>%
  group_by(region) %>%
  summarise(
    total_siempre = sum(peleas * Fact_Pers_Reg, na.rm = TRUE),  # Ignorar NA en los cálculos
    total_individuos = sum(Fact_Pers_Reg, na.rm = TRUE),             # Ignorar NA en los cálculos
    tasa_presencia = ifelse(total_individuos == 0, NA, total_siempre / total_individuos)  # Evitar división por cero
  )

tasa_presencia_total <- datos %>%
  summarise(
    total_siempre = sum(peleas * Fact_Pers_Com, na.rm = TRUE),   # Total ponderado de consumidores
    total_individuos = sum(Fact_Pers_Com, na.rm = TRUE),            # Total ponderado de individuos
    tasa_presencia = ifelse(total_individuos == 0, NA, total_siempre / total_individuos )  # Calcular la tasa como porcentaje
  )

write_xlsx(
  list(
    "Tasa presencia" = tasa_presencia,
    "Tasa presencia total" = tasa_presencia_total
  ),
  "tasa_presencia_peleas_2023.xlsx"
)

## Balacera o Disparos

datos <- datos %>%
  mutate(balas = case_when(
    P_INCIVILIDADES_7 %in% 5 ~ 1,  # Recodificar valores 5 como 1
    P_INCIVILIDADES_7 %in% 1:4 ~ 0 # Recodificar valores 1 a 4 como 0
  ))

table(datos$enc_region)

datos <- datos %>%
  mutate(enc_region = factor(enc_region, levels = 1:16))


datos <- datos %>%
  mutate(region = factor(enc_region, 
                         levels = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16), 
                         labels = c("Tarapacá", "Antofagasta", "Atacama", "Coquimbo", "Valparaíso", 
                                    "Bernardo O'Higgins", "Maule", "Biobío", "Araucanía", 
                                    "Los Lagos", "Aysén", "Magallanes", "RM", "Los Ríos", 
                                    "Arica y Parinacota", "Ñuble")
  ))

tasa_presencia <- datos %>%
  group_by(region) %>%
  summarise(
    total_siempre = sum(balas * Fact_Pers_Com, na.rm = TRUE),  # Ignorar NA en los cálculos
    total_individuos = sum(Fact_Pers_Com, na.rm = TRUE),             # Ignorar NA en los cálculos
    tasa_presencia = ifelse(total_individuos == 0, NA, total_siempre / total_individuos)  # Evitar división por cero
  )


tasa_presencia2 <- datos %>%
  group_by(region) %>%
  summarise(
    total_siempre = sum(balas * Fact_Pers_Reg, na.rm = TRUE),  # Ignorar NA en los cálculos
    total_individuos = sum(Fact_Pers_Reg, na.rm = TRUE),             # Ignorar NA en los cálculos
    tasa_presencia = ifelse(total_individuos == 0, NA, total_siempre / total_individuos)  # Evitar división por cero
  )

tasa_presencia_total <- datos %>%
  summarise(
    total_siempre = sum(balas * Fact_Pers_Com, na.rm = TRUE),   # Total ponderado de consumidores
    total_individuos = sum(Fact_Pers_Com, na.rm = TRUE),            # Total ponderado de individuos
    tasa_presencia = ifelse(total_individuos == 0, NA, total_siempre / total_individuos )  # Calcular la tasa como porcentaje
  )

write_xlsx(
  list(
    "Tasa presencia" = tasa_presencia,
    "Tasa presencia total" = tasa_presencia_total
  ),
  "tasa_presencia_balas_2023.xlsx"
)


## Robo con violencia

datos <- datos %>%
  mutate(robo = case_when(
    SCREEN_INT_RVI %in% 1 ~ 1,  # Recodificar valores 5 como 1
    SCREEN_INT_RVI %in% 2 ~ 0 # Recodificar valores 1 a 4 como 0
  ))

table(datos$enc_region)

datos <- datos %>%
  mutate(enc_region = factor(enc_region, levels = 1:16))


datos <- datos %>%
  mutate(region = factor(enc_region, 
                         levels = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16), 
                         labels = c("Tarapacá", "Antofagasta", "Atacama", "Coquimbo", "Valparaíso", 
                                    "Bernardo O'Higgins", "Maule", "Biobío", "Araucanía", 
                                    "Los Lagos", "Aysén", "Magallanes", "RM", "Los Ríos", 
                                    "Arica y Parinacota", "Ñuble")
  ))

tasa_presencia <- datos %>%
  group_by(region) %>%
  summarise(
    total_siempre = sum(robo * Fact_Pers_Com, na.rm = TRUE),  # Ignorar NA en los cálculos
    total_individuos = sum(Fact_Pers_Com, na.rm = TRUE),             # Ignorar NA en los cálculos
    tasa_presencia = ifelse(total_individuos == 0, NA, total_siempre / total_individuos)  # Evitar división por cero
  )


tasa_presencia2 <- datos %>%
  group_by(region) %>%
  summarise(
    total_siempre = sum(robo * Fact_Pers_Reg, na.rm = TRUE),  # Ignorar NA en los cálculos
    total_individuos = sum(Fact_Pers_Reg, na.rm = TRUE),             # Ignorar NA en los cálculos
    tasa_presencia = ifelse(total_individuos == 0, NA, total_siempre / total_individuos)  # Evitar división por cero
  )

tasa_presencia_total <- datos %>%
  summarise(
    total_siempre = sum(robo * Fact_Pers_Com, na.rm = TRUE),   # Total ponderado de consumidores
    total_individuos = sum(Fact_Pers_Com, na.rm = TRUE),            # Total ponderado de individuos
    tasa_presencia = ifelse(total_individuos == 0, NA, total_siempre / total_individuos )  # Calcular la tasa como porcentaje
  )

write_xlsx(
  list(
    "Tasa presencia" = tasa_presencia,
    "Tasa presencia total" = tasa_presencia_total
  ),
  "tasa_presencia_robo_violento_2023.xlsx"
)

#2022

rm(list = ls())
datos <- read_sav("bbdd.sav")

## Pandillas violentas

datos <- datos %>%
  mutate(pandilla = case_when(
    P6_10_1 %in% 4 ~ 1,  # Recodificar valores 5 como 1
    P6_10_1 %in% 1:3 ~ 0 # Recodificar valores 1 a 4 como 0
  ))

table(datos$enc_region)

datos <- datos %>%
  mutate(enc_region = factor(enc_region, levels = 1:16))


datos <- datos %>%
  mutate(region = factor(enc_region, 
                         levels = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16), 
                         labels = c("Tarapacá", "Antofagasta", "Atacama", "Coquimbo", "Valparaíso", 
                                    "Bernardo O'Higgins", "Maule", "Biobío", "Araucanía", 
                                    "Los Lagos", "Aysén", "Magallanes", "RM", "Los Ríos", 
                                    "Arica y Parinacota", "Ñuble")
  ))

tasa_presencia <- datos %>%
  group_by(region) %>%
  summarise(
    total_siempre = sum(pandilla * Fact_Pers, na.rm = TRUE),  # Ignorar NA en los cálculos
    total_individuos = sum(Fact_Pers, na.rm = TRUE),             # Ignorar NA en los cálculos
    tasa_presencia = ifelse(total_individuos == 0, NA, total_siempre / total_individuos)  # Evitar división por cero
  )

tasa_presencia_total <- datos %>%
  summarise(
    total_siempre = sum(pandilla * Fact_Pers, na.rm = TRUE),   # Total ponderado de consumidores
    total_individuos = sum(Fact_Pers, na.rm = TRUE),            # Total ponderado de individuos
    tasa_presencia = ifelse(total_individuos == 0, NA, total_siempre / total_individuos )  # Calcular la tasa como porcentaje
  )

write_xlsx(
  list(
    "Tasa presencia" = tasa_presencia,
    "Tasa presencia total" = tasa_presencia_total
  ),
  "tasa_presencia_pandilla_2022.xlsx"
)


## Peleas calljeras con arma blanco o de fuego 

datos <- datos %>%
  mutate(peleas = case_when(
    P6_10_1 %in% 4 ~ 1,  # Recodificar valores 4 como 1
    P6_10_1 %in% 1:1 ~ 0 # Recodificar valores 1 a 3 como 0
  ))

table(datos$enc_region)

datos <- datos %>%
  mutate(enc_region = factor(enc_region, levels = 1:16))


datos <- datos %>%
  mutate(region = factor(enc_region, 
                         levels = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16), 
                         labels = c("Tarapacá", "Antofagasta", "Atacama", "Coquimbo", "Valparaíso", 
                                    "Bernardo O'Higgins", "Maule", "Biobío", "Araucanía", 
                                    "Los Lagos", "Aysén", "Magallanes", "RM", "Los Ríos", 
                                    "Arica y Parinacota", "Ñuble")
  ))

tasa_presencia <- datos %>%
  group_by(region) %>%
  summarise(
    total_siempre = sum(peleas * Fact_Pers_Com, na.rm = TRUE),  # Ignorar NA en los cálculos
    total_individuos = sum(Fact_Pers_Com, na.rm = TRUE),             # Ignorar NA en los cálculos
    tasa_presencia = ifelse(total_individuos == 0, NA, total_siempre / total_individuos)  # Evitar división por cero
  )


tasa_presencia2 <- datos %>%
  group_by(region) %>%
  summarise(
    total_siempre = sum(peleas * Fact_Pers_Reg, na.rm = TRUE),  # Ignorar NA en los cálculos
    total_individuos = sum(Fact_Pers_Reg, na.rm = TRUE),             # Ignorar NA en los cálculos
    tasa_presencia = ifelse(total_individuos == 0, NA, total_siempre / total_individuos)  # Evitar división por cero
  )

tasa_presencia_total <- datos %>%
  summarise(
    total_siempre = sum(peleas * Fact_Pers_Com, na.rm = TRUE),   # Total ponderado de consumidores
    total_individuos = sum(Fact_Pers_Com, na.rm = TRUE),            # Total ponderado de individuos
    tasa_presencia = ifelse(total_individuos == 0, NA, total_siempre / total_individuos )  # Calcular la tasa como porcentaje
  )

write_xlsx(
  list(
    "Tasa presencia" = tasa_presencia,
    "Tasa presencia total" = tasa_presencia_total
  ),
  "tasa_presencia_peleas_2022.xlsx"
)

## Balacera o Disparos

datos <- datos %>%
  mutate(balas = case_when(
    P6_16_1 %in% 4 ~ 1,  # Recodificar valores 4 como 1
    P6_16_1 %in% 1:3 ~ 0 # Recodificar valores 1 a 3 como 0
  ))

table(datos$enc_region)

datos <- datos %>%
  mutate(enc_region = factor(enc_region, levels = 1:16))


datos <- datos %>%
  mutate(region = factor(enc_region, 
                         levels = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16), 
                         labels = c("Tarapacá", "Antofagasta", "Atacama", "Coquimbo", "Valparaíso", 
                                    "Bernardo O'Higgins", "Maule", "Biobío", "Araucanía", 
                                    "Los Lagos", "Aysén", "Magallanes", "RM", "Los Ríos", 
                                    "Arica y Parinacota", "Ñuble")
  ))

tasa_presencia <- datos %>%
  group_by(region) %>%
  summarise(
    total_siempre = sum(balas * Fact_Pers_Com, na.rm = TRUE),  # Ignorar NA en los cálculos
    total_individuos = sum(Fact_Pers_Com, na.rm = TRUE),             # Ignorar NA en los cálculos
    tasa_presencia = ifelse(total_individuos == 0, NA, total_siempre / total_individuos)  # Evitar división por cero
  )


tasa_presencia2 <- datos %>%
  group_by(region) %>%
  summarise(
    total_siempre = sum(balas * Fact_Pers_Reg, na.rm = TRUE),  # Ignorar NA en los cálculos
    total_individuos = sum(Fact_Pers_Reg, na.rm = TRUE),             # Ignorar NA en los cálculos
    tasa_presencia = ifelse(total_individuos == 0, NA, total_siempre / total_individuos)  # Evitar división por cero
  )

tasa_presencia_total <- datos %>%
  summarise(
    total_siempre = sum(balas * Fact_Pers_Com, na.rm = TRUE),   # Total ponderado de consumidores
    total_individuos = sum(Fact_Pers_Com, na.rm = TRUE),            # Total ponderado de individuos
    tasa_presencia = ifelse(total_individuos == 0, NA, total_siempre / total_individuos )  # Calcular la tasa como porcentaje
  )

write_xlsx(
  list(
    "Tasa presencia" = tasa_presencia,
    "Tasa presencia total" = tasa_presencia_total
  ),
  "tasa_presencia_balas_2022.xlsx"
)


## Robo con violencia

datos <- datos %>%
  mutate(robo = case_when(
    A1_1_1 %in% 1 ~ 1,  # Recodificar valores 5 como 1
    A1_1_1 %in% 2 ~ 0 # Recodificar valores 1 a 4 como 0
  ))

table(datos$enc_region)

datos <- datos %>%
  mutate(enc_region = factor(enc_region, levels = 1:16))


datos <- datos %>%
  mutate(region = factor(enc_region, 
                         levels = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16), 
                         labels = c("Tarapacá", "Antofagasta", "Atacama", "Coquimbo", "Valparaíso", 
                                    "Bernardo O'Higgins", "Maule", "Biobío", "Araucanía", 
                                    "Los Lagos", "Aysén", "Magallanes", "RM", "Los Ríos", 
                                    "Arica y Parinacota", "Ñuble")
  ))

tasa_presencia <- datos %>%
  group_by(region) %>%
  summarise(
    total_siempre = sum(robo * Fact_Pers_Com, na.rm = TRUE),  # Ignorar NA en los cálculos
    total_individuos = sum(Fact_Pers_Com, na.rm = TRUE),             # Ignorar NA en los cálculos
    tasa_presencia = ifelse(total_individuos == 0, NA, total_siempre / total_individuos)  # Evitar división por cero
  )


tasa_presencia2 <- datos %>%
  group_by(region) %>%
  summarise(
    total_siempre = sum(robo * Fact_Pers_Reg, na.rm = TRUE),  # Ignorar NA en los cálculos
    total_individuos = sum(Fact_Pers_Reg, na.rm = TRUE),             # Ignorar NA en los cálculos
    tasa_presencia = ifelse(total_individuos == 0, NA, total_siempre / total_individuos)  # Evitar división por cero
  )

tasa_presencia_total <- datos %>%
  summarise(
    total_siempre = sum(robo * Fact_Pers_Com, na.rm = TRUE),   # Total ponderado de consumidores
    total_individuos = sum(Fact_Pers_Com, na.rm = TRUE),            # Total ponderado de individuos
    tasa_presencia = ifelse(total_individuos == 0, NA, total_siempre / total_individuos )  # Calcular la tasa como porcentaje
  )

write_xlsx(
  list(
    "Tasa presencia" = tasa_presencia,
    "Tasa presencia total" = tasa_presencia_total
  ),
  "tasa_presencia_robo_violento_2022.xlsx"
)


#2020

rm(list = ls())
datos <- read_sav("bbdd_2022.sav")

# Pandillas violentas

datos <- datos %>%
  mutate(pandillas = case_when(
    P5_10_1 %in% 4 ~ 1,  # Recodificar valores 5 como 1
    P5_10_1 %in% 1:3 ~ 0 # Recodificar valores 1 a 4 como 0
  ))

table(datos$enc_region)

datos <- datos %>%
  mutate(enc_region = factor(enc_region, levels = 1:16))


datos <- datos %>%
  mutate(region = factor(enc_region, 
                         levels = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16), 
                         labels = c("Tarapacá", "Antofagasta", "Atacama", "Coquimbo", "Valparaíso", 
                                    "Bernardo O'Higgins", "Maule", "Biobío", "Araucanía", 
                                    "Los Lagos", "Aysén", "Magallanes", "RM", "Los Ríos", 
                                    "Arica y Parinacota", "Ñuble")
  ))

tasa_presencia <- datos %>%
  group_by(enc_region) %>%
  summarise(
    total_siempre = sum(pandillas * Fact_Pers, na.rm = TRUE),  # Ignorar NA en los cálculos
    total_individuos = sum(Fact_Pers, na.rm = TRUE),             # Ignorar NA en los cálculos
    tasa_presencia = ifelse(total_individuos == 0, NA, total_siempre / total_individuos)  # Evitar división por cero
  )

tasa_presencia_total <- datos %>%
  summarise(
    total_siempre = sum(pandillas * Fact_Pers, na.rm = TRUE),   # Total ponderado de consumidores
    total_individuos = sum(Fact_Pers, na.rm = TRUE),            # Total ponderado de individuos
    tasa_presencia = ifelse(total_individuos == 0, NA, total_siempre / total_individuos )  # Calcular la tasa como porcentaje
  )

write_xlsx(
  list(
    "Tasa presencia" = tasa_presencia,
    "Tasa presencia total" = tasa_presencia_total
  ),
  "tasa_presencia_pandillas_2020.xlsx"
)

## Peleas calljeras con arma blanco o de fuego 

datos <- datos %>%
  mutate(peleas = case_when(
    P5_11_1 %in% 4 ~ 1,  # Recodificar valores 4 como 1
    P5_11_1 %in% 1:1 ~ 0 # Recodificar valores 1 a 3 como 0
  ))

table(datos$enc_region)

datos <- datos %>%
  mutate(enc_region = factor(enc_region, levels = 1:16))


datos <- datos %>%
  mutate(region = factor(enc_region, 
                         levels = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16), 
                         labels = c("Tarapacá", "Antofagasta", "Atacama", "Coquimbo", "Valparaíso", 
                                    "Bernardo O'Higgins", "Maule", "Biobío", "Araucanía", 
                                    "Los Lagos", "Aysén", "Magallanes", "RM", "Los Ríos", 
                                    "Arica y Parinacota", "Ñuble")
  ))

tasa_presencia <- datos %>%
  group_by(region) %>%
  summarise(
    total_siempre = sum(peleas * Fact_Pers_Com, na.rm = TRUE),  # Ignorar NA en los cálculos
    total_individuos = sum(Fact_Pers_Com, na.rm = TRUE),             # Ignorar NA en los cálculos
    tasa_presencia = ifelse(total_individuos == 0, NA, total_siempre / total_individuos)  # Evitar división por cero
  )


tasa_presencia2 <- datos %>%
  group_by(region) %>%
  summarise(
    total_siempre = sum(peleas * Fact_Pers_Reg, na.rm = TRUE),  # Ignorar NA en los cálculos
    total_individuos = sum(Fact_Pers_Reg, na.rm = TRUE),             # Ignorar NA en los cálculos
    tasa_presencia = ifelse(total_individuos == 0, NA, total_siempre / total_individuos)  # Evitar división por cero
  )

tasa_presencia_total <- datos %>%
  summarise(
    total_siempre = sum(peleas * Fact_Pers_Com, na.rm = TRUE),   # Total ponderado de consumidores
    total_individuos = sum(Fact_Pers_Com, na.rm = TRUE),            # Total ponderado de individuos
    tasa_presencia = ifelse(total_individuos == 0, NA, total_siempre / total_individuos )  # Calcular la tasa como porcentaje
  )

write_xlsx(
  list(
    "Tasa presencia" = tasa_presencia,
    "Tasa presencia total" = tasa_presencia_total
  ),
  "tasa_presencia_peleas_2020.xlsx"
)

## Balacera o Disparos

datos <- datos %>%
  mutate(balas = case_when(
    P5_16_1 %in% 4 ~ 1,  # Recodificar valores 4 como 1
    P5_16_1 %in% 1:3 ~ 0 # Recodificar valores 1 a 3 como 0
  ))

table(datos$enc_region)

datos <- datos %>%
  mutate(enc_region = factor(enc_region, levels = 1:16))


datos <- datos %>%
  mutate(region = factor(enc_region, 
                         levels = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16), 
                         labels = c("Tarapacá", "Antofagasta", "Atacama", "Coquimbo", "Valparaíso", 
                                    "Bernardo O'Higgins", "Maule", "Biobío", "Araucanía", 
                                    "Los Lagos", "Aysén", "Magallanes", "RM", "Los Ríos", 
                                    "Arica y Parinacota", "Ñuble")
  ))

tasa_presencia <- datos %>%
  group_by(region) %>%
  summarise(
    total_siempre = sum(balas * Fact_Pers_Com, na.rm = TRUE),  # Ignorar NA en los cálculos
    total_individuos = sum(Fact_Pers_Com, na.rm = TRUE),             # Ignorar NA en los cálculos
    tasa_presencia = ifelse(total_individuos == 0, NA, total_siempre / total_individuos)  # Evitar división por cero
  )


tasa_presencia2 <- datos %>%
  group_by(region) %>%
  summarise(
    total_siempre = sum(balas * Fact_Pers_Reg, na.rm = TRUE),  # Ignorar NA en los cálculos
    total_individuos = sum(Fact_Pers_Reg, na.rm = TRUE),             # Ignorar NA en los cálculos
    tasa_presencia = ifelse(total_individuos == 0, NA, total_siempre / total_individuos)  # Evitar división por cero
  )

tasa_presencia_total <- datos %>%
  summarise(
    total_siempre = sum(balas * Fact_Pers_Com, na.rm = TRUE),   # Total ponderado de consumidores
    total_individuos = sum(Fact_Pers_Com, na.rm = TRUE),            # Total ponderado de individuos
    tasa_presencia = ifelse(total_individuos == 0, NA, total_siempre / total_individuos )  # Calcular la tasa como porcentaje
  )

write_xlsx(
  list(
    "Tasa presencia" = tasa_presencia,
    "Tasa presencia total" = tasa_presencia_total
  ),
  "tasa_presencia_balas_2020.xlsx"
)


## Robo con violencia

datos <- datos %>%
  mutate(robo = case_when(
    A1_1_1 %in% 1 ~ 1,  # Recodificar valores 5 como 1
    A1_1_1 %in% 2 ~ 0 # Recodificar valores 1 a 4 como 0
  ))

table(datos$enc_region)

datos <- datos %>%
  mutate(enc_region = factor(enc_region, levels = 1:16))


datos <- datos %>%
  mutate(region = factor(enc_region, 
                         levels = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16), 
                         labels = c("Tarapacá", "Antofagasta", "Atacama", "Coquimbo", "Valparaíso", 
                                    "Bernardo O'Higgins", "Maule", "Biobío", "Araucanía", 
                                    "Los Lagos", "Aysén", "Magallanes", "RM", "Los Ríos", 
                                    "Arica y Parinacota", "Ñuble")
  ))

tasa_presencia <- datos %>%
  group_by(region) %>%
  summarise(
    total_siempre = sum(robo * Fact_Pers_Com, na.rm = TRUE),  # Ignorar NA en los cálculos
    total_individuos = sum(Fact_Pers_Com, na.rm = TRUE),             # Ignorar NA en los cálculos
    tasa_presencia = ifelse(total_individuos == 0, NA, total_siempre / total_individuos)  # Evitar división por cero
  )


tasa_presencia2 <- datos %>%
  group_by(region) %>%
  summarise(
    total_siempre = sum(robo * Fact_Pers_Reg, na.rm = TRUE),  # Ignorar NA en los cálculos
    total_individuos = sum(Fact_Pers_Reg, na.rm = TRUE),             # Ignorar NA en los cálculos
    tasa_presencia = ifelse(total_individuos == 0, NA, total_siempre / total_individuos)  # Evitar división por cero
  )

tasa_presencia_total <- datos %>%
  summarise(
    total_siempre = sum(robo * Fact_Pers_Com, na.rm = TRUE),   # Total ponderado de consumidores
    total_individuos = sum(Fact_Pers_Com, na.rm = TRUE),            # Total ponderado de individuos
    tasa_presencia = ifelse(total_individuos == 0, NA, total_siempre / total_individuos )  # Calcular la tasa como porcentaje
  )

write_xlsx(
  list(
    "Tasa presencia" = tasa_presencia,
    "Tasa presencia total" = tasa_presencia_total
  ),
  "tasa_presencia_robo_violento_2020.xlsx"
)

#2019

rm(list = ls())
datos <- read_sav("bbdd_2019.sav")

# Pandillas violentas

datos <- datos %>%
  mutate(pandillas = case_when(
    P11_10_1 %in% 4 ~ 1,  # Recodificar valores 5 como 1
    P11_10_1 %in% 1:3 ~ 0 # Recodificar valores 1 a 4 como 0
  ))

table(datos$enc_region)

datos <- datos %>%
  mutate(enc_region = factor(enc_region, levels = 1:16))


datos <- datos %>%
  mutate(region = factor(enc_region, 
                         levels = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16), 
                         labels = c("Tarapacá", "Antofagasta", "Atacama", "Coquimbo", "Valparaíso", 
                                    "Bernardo O'Higgins", "Maule", "Biobío", "Araucanía", 
                                    "Los Lagos", "Aysén", "Magallanes", "RM", "Los Ríos", 
                                    "Arica y Parinacota", "Ñuble")
  ))

tasa_presencia <- datos %>%
  group_by(enc_region) %>%
  summarise(
    total_siempre = sum(pandillas * Fact_Pers, na.rm = TRUE),  # Ignorar NA en los cálculos
    total_individuos = sum(Fact_Pers, na.rm = TRUE),             # Ignorar NA en los cálculos
    tasa_presencia = ifelse(total_individuos == 0, NA, total_siempre / total_individuos)  # Evitar división por cero
  )

tasa_presencia_total <- datos %>%
  summarise(
    total_siempre = sum(pandillas * Fact_Pers, na.rm = TRUE),   # Total ponderado de consumidores
    total_individuos = sum(Fact_Pers, na.rm = TRUE),            # Total ponderado de individuos
    tasa_presencia = ifelse(total_individuos == 0, NA, total_siempre / total_individuos )  # Calcular la tasa como porcentaje
  )

write_xlsx(
  list(
    "Tasa presencia" = tasa_presencia,
    "Tasa presencia total" = tasa_presencia_total
  ),
  "tasa_presencia_pandillas_2019.xlsx"
)

## Peleas calljeras con arma blanco o de fuego 

datos <- datos %>%
  mutate(peleas = case_when(
    P11_11_1 %in% 4 ~ 1,  # Recodificar valores 4 como 1
    P11_11_1 %in% 1:1 ~ 0 # Recodificar valores 1 a 3 como 0
  ))

table(datos$enc_region)

datos <- datos %>%
  mutate(enc_region = factor(enc_region, levels = 1:16))


datos <- datos %>%
  mutate(region = factor(enc_region, 
                         levels = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16), 
                         labels = c("Tarapacá", "Antofagasta", "Atacama", "Coquimbo", "Valparaíso", 
                                    "Bernardo O'Higgins", "Maule", "Biobío", "Araucanía", 
                                    "Los Lagos", "Aysén", "Magallanes", "RM", "Los Ríos", 
                                    "Arica y Parinacota", "Ñuble")
  ))

tasa_presencia <- datos %>%
  group_by(region) %>%
  summarise(
    total_siempre = sum(peleas * Fact_Pers_Com, na.rm = TRUE),  # Ignorar NA en los cálculos
    total_individuos = sum(Fact_Pers_Com, na.rm = TRUE),             # Ignorar NA en los cálculos
    tasa_presencia = ifelse(total_individuos == 0, NA, total_siempre / total_individuos)  # Evitar división por cero
  )


tasa_presencia2 <- datos %>%
  group_by(region) %>%
  summarise(
    total_siempre = sum(peleas * Fact_Pers_Reg, na.rm = TRUE),  # Ignorar NA en los cálculos
    total_individuos = sum(Fact_Pers_Reg, na.rm = TRUE),             # Ignorar NA en los cálculos
    tasa_presencia = ifelse(total_individuos == 0, NA, total_siempre / total_individuos)  # Evitar división por cero
  )

tasa_presencia_total <- datos %>%
  summarise(
    total_siempre = sum(peleas * Fact_Pers_Com, na.rm = TRUE),   # Total ponderado de consumidores
    total_individuos = sum(Fact_Pers_Com, na.rm = TRUE),            # Total ponderado de individuos
    tasa_presencia = ifelse(total_individuos == 0, NA, total_siempre / total_individuos )  # Calcular la tasa como porcentaje
  )

write_xlsx(
  list(
    "Tasa presencia" = tasa_presencia,
    "Tasa presencia total" = tasa_presencia_total
  ),
  "tasa_presencia_peleas_2019.xlsx"
)

## Balacera o Disparos

datos <- datos %>%
  mutate(balas = case_when(
    P11_16_1 %in% 4 ~ 1,  # Recodificar valores 4 como 1
    P11_16_1 %in% 1:3 ~ 0 # Recodificar valores 1 a 3 como 0
  ))

table(datos$enc_region)

datos <- datos %>%
  mutate(enc_region = factor(enc_region, levels = 1:16))


datos <- datos %>%
  mutate(region = factor(enc_region, 
                         levels = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16), 
                         labels = c("Tarapacá", "Antofagasta", "Atacama", "Coquimbo", "Valparaíso", 
                                    "Bernardo O'Higgins", "Maule", "Biobío", "Araucanía", 
                                    "Los Lagos", "Aysén", "Magallanes", "RM", "Los Ríos", 
                                    "Arica y Parinacota", "Ñuble")
  ))

tasa_presencia <- datos %>%
  group_by(region) %>%
  summarise(
    total_siempre = sum(balas * Fact_Pers_Com, na.rm = TRUE),  # Ignorar NA en los cálculos
    total_individuos = sum(Fact_Pers_Com, na.rm = TRUE),             # Ignorar NA en los cálculos
    tasa_presencia = ifelse(total_individuos == 0, NA, total_siempre / total_individuos)  # Evitar división por cero
  )


tasa_presencia2 <- datos %>%
  group_by(region) %>%
  summarise(
    total_siempre = sum(balas * Fact_Pers_Reg, na.rm = TRUE),  # Ignorar NA en los cálculos
    total_individuos = sum(Fact_Pers_Reg, na.rm = TRUE),             # Ignorar NA en los cálculos
    tasa_presencia = ifelse(total_individuos == 0, NA, total_siempre / total_individuos)  # Evitar división por cero
  )

tasa_presencia_total <- datos %>%
  summarise(
    total_siempre = sum(balas * Fact_Pers_Com, na.rm = TRUE),   # Total ponderado de consumidores
    total_individuos = sum(Fact_Pers_Com, na.rm = TRUE),            # Total ponderado de individuos
    tasa_presencia = ifelse(total_individuos == 0, NA, total_siempre / total_individuos )  # Calcular la tasa como porcentaje
  )

write_xlsx(
  list(
    "Tasa presencia" = tasa_presencia,
    "Tasa presencia total" = tasa_presencia_total
  ),
  "tasa_presencia_balas_2019.xlsx"
)


## Robo con violencia

datos <- datos %>%
  mutate(robo = case_when(
    A1_1_1 %in% 1 ~ 1,  # Recodificar valores 5 como 1
    A1_1_1 %in% 2 ~ 0 # Recodificar valores 1 a 4 como 0
  ))

table(datos$enc_region)

datos <- datos %>%
  mutate(enc_region = factor(enc_region, levels = 1:16))


datos <- datos %>%
  mutate(region = factor(enc_region, 
                         levels = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16), 
                         labels = c("Tarapacá", "Antofagasta", "Atacama", "Coquimbo", "Valparaíso", 
                                    "Bernardo O'Higgins", "Maule", "Biobío", "Araucanía", 
                                    "Los Lagos", "Aysén", "Magallanes", "RM", "Los Ríos", 
                                    "Arica y Parinacota", "Ñuble")
  ))

tasa_presencia <- datos %>%
  group_by(region) %>%
  summarise(
    total_siempre = sum(robo * Fact_Pers_Com, na.rm = TRUE),  # Ignorar NA en los cálculos
    total_individuos = sum(Fact_Pers_Com, na.rm = TRUE),             # Ignorar NA en los cálculos
    tasa_presencia = ifelse(total_individuos == 0, NA, total_siempre / total_individuos)  # Evitar división por cero
  )


tasa_presencia2 <- datos %>%
  group_by(region) %>%
  summarise(
    total_siempre = sum(robo * Fact_Pers_Reg, na.rm = TRUE),  # Ignorar NA en los cálculos
    total_individuos = sum(Fact_Pers_Reg, na.rm = TRUE),             # Ignorar NA en los cálculos
    tasa_presencia = ifelse(total_individuos == 0, NA, total_siempre / total_individuos)  # Evitar división por cero
  )

tasa_presencia_total <- datos %>%
  summarise(
    total_siempre = sum(robo * Fact_Pers_Com, na.rm = TRUE),   # Total ponderado de consumidores
    total_individuos = sum(Fact_Pers_Com, na.rm = TRUE),            # Total ponderado de individuos
    tasa_presencia = ifelse(total_individuos == 0, NA, total_siempre / total_individuos )  # Calcular la tasa como porcentaje
  )

write_xlsx(
  list(
    "Tasa presencia" = tasa_presencia,
    "Tasa presencia total" = tasa_presencia_total
  ),
  "tasa_presencia_robo_violento_2019.xlsx"
)



#2018

rm(list = ls())
datos <- read_sav("bbdd_2018.sav")

# Pandillas violentas

datos <- datos %>%
  mutate(pandillas = case_when(
    P11_10_1 %in% 4 ~ 1,  # Recodificar valores 5 como 1
    P11_10_1 %in% 1:3 ~ 0 # Recodificar valores 1 a 4 como 0
  ))

table(datos$enc_región16)

datos <- datos %>%
  mutate(enc_region = factor(enc_region, levels = 1:16))


datos <- datos %>%
  mutate(region = factor(enc_region, 
                         levels = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16), 
                         labels = c("Tarapacá", "Antofagasta", "Atacama", "Coquimbo", "Valparaíso", 
                                    "Bernardo O'Higgins", "Maule", "Biobío", "Araucanía", 
                                    "Los Lagos", "Aysén", "Magallanes", "RM", "Los Ríos", 
                                    "Arica y Parinacota", "Ñuble")
  ))

tasa_presencia <- datos %>%
  group_by(enc_región16) %>%
  summarise(
    total_siempre = sum(pandillas * Fact_pers, na.rm = TRUE),  # Ignorar NA en los cálculos
    total_individuos = sum(Fact_pers, na.rm = TRUE),             # Ignorar NA en los cálculos
    tasa_presencia = ifelse(total_individuos == 0, NA, total_siempre / total_individuos)  # Evitar división por cero
  )

tasa_presencia_total <- datos %>%
  summarise(
    total_siempre = sum(pandillas * Fact_pers, na.rm = TRUE),   # Total ponderado de consumidores
    total_individuos = sum(Fact_pers, na.rm = TRUE),            # Total ponderado de individuos
    tasa_presencia = ifelse(total_individuos == 0, NA, total_siempre / total_individuos )  # Calcular la tasa como porcentaje
  )

write_xlsx(
  list(
    "Tasa presencia total" = tasa_presencia_total,
    "Tasa presencia" = tasa_presencia
  ),
  "tasa_presencia_pandillas_2018.xlsx"
)


## Peleas calljeras con arma blanco o de fuego 

datos <- datos %>%
  mutate(peleas = case_when(
    P11_11_1 %in% 4 ~ 1,  # Recodificar valores 4 como 1
    P11_11_1 %in% 1:1 ~ 0 # Recodificar valores 1 a 3 como 0
  ))

table(datos$enc_región16)

datos <- datos %>%
  mutate(enc_region = factor(enc_region, levels = 1:16))


datos <- datos %>%
  mutate(region = factor(enc_region, 
                         levels = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16), 
                         labels = c("Tarapacá", "Antofagasta", "Atacama", "Coquimbo", "Valparaíso", 
                                    "Bernardo O'Higgins", "Maule", "Biobío", "Araucanía", 
                                    "Los Lagos", "Aysén", "Magallanes", "RM", "Los Ríos", 
                                    "Arica y Parinacota", "Ñuble")
  ))

tasa_presencia <- datos %>%
  group_by(enc_región16) %>%
  summarise(
    total_siempre = sum(peleas * Fact_pers, na.rm = TRUE),  # Ignorar NA en los cálculos
    total_individuos = sum(Fact_pers, na.rm = TRUE),             # Ignorar NA en los cálculos
    tasa_presencia = ifelse(total_individuos == 0, NA, total_siempre / total_individuos)  # Evitar división por cero
  )

tasa_presencia_total <- datos %>%
  summarise(
    total_siempre = sum(peleas * Fact_pers, na.rm = TRUE),   # Total ponderado de consumidores
    total_individuos = sum(Fact_pers, na.rm = TRUE),            # Total ponderado de individuos
    tasa_presencia = ifelse(total_individuos == 0, NA, total_siempre / total_individuos )  # Calcular la tasa como porcentaje
  )

write_xlsx(
  list(
    "Tasa presencia total" = tasa_presencia_total,
    "Tasa presencia" = tasa_presencia
  ),
  "tasa_presencia_peleas_2018.xlsx"
)



## Balacera o Disparos

datos <- datos %>%
  mutate(balas = case_when(
    P11_16_1 %in% 4 ~ 1,  # Recodificar valores 4 como 1
    P11_16_1 %in% 1:3 ~ 0 # Recodificar valores 1 a 3 como 0
  ))

table(datos$enc_región16)

datos <- datos %>%
  mutate(enc_region = factor(enc_region, levels = 1:16))


datos <- datos %>%
  mutate(region = factor(enc_region, 
                         levels = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16), 
                         labels = c("Tarapacá", "Antofagasta", "Atacama", "Coquimbo", "Valparaíso", 
                                    "Bernardo O'Higgins", "Maule", "Biobío", "Araucanía", 
                                    "Los Lagos", "Aysén", "Magallanes", "RM", "Los Ríos", 
                                    "Arica y Parinacota", "Ñuble")
  ))

tasa_presencia <- datos %>%
  group_by(enc_región16) %>%
  summarise(
    total_siempre = sum(balas * Fact_pers, na.rm = TRUE),  # Ignorar NA en los cálculos
    total_individuos = sum(Fact_pers, na.rm = TRUE),             # Ignorar NA en los cálculos
    tasa_presencia = ifelse(total_individuos == 0, NA, total_siempre / total_individuos)  # Evitar división por cero
  )

tasa_presencia_total <- datos %>%
  summarise(
    total_siempre = sum(balas * Fact_pers, na.rm = TRUE),   # Total ponderado de consumidores
    total_individuos = sum(Fact_pers, na.rm = TRUE),            # Total ponderado de individuos
    tasa_presencia = ifelse(total_individuos == 0, NA, total_siempre / total_individuos )  # Calcular la tasa como porcentaje
  )

write_xlsx(
  list(
    "Tasa presencia total" = tasa_presencia_total,
    "Tasa presencia" = tasa_presencia
  ),
  "tasa_presencia_balas_2018.xlsx"
)


## Robo con violencia

datos <- datos %>%
  mutate(robo = case_when(
    A1_1_1 %in% 1 ~ 1,  # Recodificar valores 5 como 1
    A1_1_1 %in% 2 ~ 0 # Recodificar valores 1 a 4 como 0
  ))

table(datos$enc_región16)

datos <- datos %>%
  mutate(enc_region = factor(enc_region, levels = 1:16))


datos <- datos %>%
  mutate(region = factor(enc_region, 
                         levels = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16), 
                         labels = c("Tarapacá", "Antofagasta", "Atacama", "Coquimbo", "Valparaíso", 
                                    "Bernardo O'Higgins", "Maule", "Biobío", "Araucanía", 
                                    "Los Lagos", "Aysén", "Magallanes", "RM", "Los Ríos", 
                                    "Arica y Parinacota", "Ñuble")
  ))

tasa_presencia <- datos %>%
  group_by(enc_región16) %>%
  summarise(
    total_siempre = sum(robo * Fact_pers, na.rm = TRUE),  # Ignorar NA en los cálculos
    total_individuos = sum(Fact_pers, na.rm = TRUE),             # Ignorar NA en los cálculos
    tasa_presencia = ifelse(total_individuos == 0, NA, total_siempre / total_individuos)  # Evitar división por cero
  )

tasa_presencia_total <- datos %>%
  summarise(
    total_siempre = sum(robo * Fact_pers, na.rm = TRUE),   # Total ponderado de consumidores
    total_individuos = sum(Fact_pers, na.rm = TRUE),            # Total ponderado de individuos
    tasa_presencia = ifelse(total_individuos == 0, NA, total_siempre / total_individuos )  # Calcular la tasa como porcentaje
  )

write_xlsx(
  list(
    "Tasa presencia total" = tasa_presencia_total,
    "Tasa presencia" = tasa_presencia
  ),
  "tasa_presencia_robo_2018.xlsx"
)





#2017

rm(list = ls())
datos <- read_sav("bbdd_2017.sav")

## Pandillas violentas 
datos <- datos %>%
  mutate(pandillas = case_when(
    P11_10_1 %in% 4 ~ 1,  # Recodificar valores 5 como 1
    P11_10_1 %in% 1:3 ~ 0 # Recodificar valores 1 a 4 como 0
  ))

table(datos$enc_region)

etiquetas <- get_labels(datos$enc_rpc)
valores <- get_values(datos$enc_rpc)

tabla_codigos_etiquetas <- data.frame(
  Codigo = valores,
  Etiqueta = etiquetas
)
print(tabla_codigos_etiquetas)

datos <- datos %>%
  mutate(region = case_when(
    enc_rpc %in% c(15101) ~ 15,
    enc_rpc %in% c(1101, 1107) ~ 1,
    enc_rpc %in% c(2101, 2201, 2301) ~ 2,
    enc_rpc %in% c(3101, 3301) ~ 3,
    enc_rpc %in% c(4101, 4102, 4201, 4203, 4301) ~ 4,
    enc_rpc %in% c(5101, 5103, 5109, 5301, 5501, 5502, 5601, 5701, 5801, 5802, 5804) ~ 5,
    enc_rpc %in% c(6101, 6115, 6117, 6301) ~ 6,
    enc_rpc %in% c(7101, 7102, 7201, 7301, 7304, 7401) ~ 7,
    enc_rpc %in% c(8101, 8102, 8103, 8106, 8107, 8107, 8108, 8110, 8111, 8112, 8301) ~ 8,
    enc_rpc %in% c(8401, 8416) ~ 16,
    enc_rpc %in% c(9101, 9112, 9120, 9201) ~ 9,
    enc_rpc %in% c(10101, 10201, 10202, 10301) ~ 10,
    enc_rpc %in% c(11101, 11201) ~ 11,
    enc_rpc %in% c(12101) ~ 12,
    enc_rpc %in% c(13101, 13102, 13103, 13104, 13105, 13106, 13107, 13108, 13109, 13110, 13111, 13112, 13113, 13114, 13115, 13116, 13117, 13118, 13119, 13120, 13121, 13122, 13123, 13124, 13125, 13126, 13127, 13128, 13129, 13130, 13131, 13132, 13201, 13301, 13302, 13401, 13402, 13404, 13501, 13601, 13604, 13605) ~ 13,
    enc_rpc %in% c(14101, 14201) ~ 14,
    # Agrega aquí las demás enc_rpcs con sus respectivas regiones
    TRUE ~ NA_real_ 
    ))# Para asignar NA si una enc_rpc no tiene región definida


datos <- datos %>%
  mutate(region = factor(enc_region, 
                         levels = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16), 
                         labels = c("Tarapacá", "Antofagasta", "Atacama", "Coquimbo", "Valparaíso", 
                                    "Bernardo O'Higgins", "Maule", "Biobío", "Araucanía", 
                                    "Los Lagos", "Aysén", "Magallanes", "RM", "Los Ríos", 
                                    "Arica y Parinacota", "Ñuble")
  ))

tasa_presencia <- datos %>%
  group_by(region) %>%
  summarise(
    total_siempre = sum(pandillas * Fact_pers, na.rm = TRUE),  # Ignorar NA en los cálculos
    total_individuos = sum(Fact_pers, na.rm = TRUE),             # Ignorar NA en los cálculos
    tasa_presencia = ifelse(total_individuos == 0, NA, total_siempre / total_individuos)  # Evitar división por cero
  )

tasa_presencia_total <- datos %>%
  summarise(
    total_siempre = sum(pandillas * Fact_pers, na.rm = TRUE),   # Total ponderado de consumidores
    total_individuos = sum(Fact_pers, na.rm = TRUE),            # Total ponderado de individuos
    tasa_presencia = ifelse(total_individuos == 0, NA, total_siempre / total_individuos )  # Calcular la tasa como porcentaje
  )

write_xlsx(
  list(
    "Tasa presencia total" = tasa_presencia_total,
    "Tasa presencia" = tasa_presencia
  ),
  "tasa_presencia_pandillas_2017.xlsx"
)


## Peleas calljeras con arma blanco o de fuego 

datos <- datos %>%
  mutate(peleas = case_when(
    P11_11_1 %in% 4 ~ 1,  # Recodificar valores 4 como 1
    P11_11_1 %in% 1:1 ~ 0 # Recodificar valores 1 a 3 como 0
  ))

table(datos$enc_region)

etiquetas <- get_labels(datos$enc_rpc)
valores <- get_values(datos$enc_rpc)

tabla_codigos_etiquetas <- data.frame(
  Codigo = valores,
  Etiqueta = etiquetas
)
print(tabla_codigos_etiquetas)

datos <- datos %>%
  mutate(region = case_when(
    enc_rpc %in% c(15101) ~ 15,
    enc_rpc %in% c(1101, 1107) ~ 1,
    enc_rpc %in% c(2101, 2201, 2301) ~ 2,
    enc_rpc %in% c(3101, 3301) ~ 3,
    enc_rpc %in% c(4101, 4102, 4201, 4203, 4301) ~ 4,
    enc_rpc %in% c(5101, 5103, 5109, 5301, 5501, 5502, 5601, 5701, 5801, 5802, 5804) ~ 5,
    enc_rpc %in% c(6101, 6115, 6117, 6301) ~ 6,
    enc_rpc %in% c(7101, 7102, 7201, 7301, 7304, 7401) ~ 7,
    enc_rpc %in% c(8101, 8102, 8103, 8106, 8107, 8107, 8108, 8110, 8111, 8112, 8301) ~ 8,
    enc_rpc %in% c(8401, 8416) ~ 16,
    enc_rpc %in% c(9101, 9112, 9120, 9201) ~ 9,
    enc_rpc %in% c(10101, 10201, 10202, 10301) ~ 10,
    enc_rpc %in% c(11101, 11201) ~ 11,
    enc_rpc %in% c(12101) ~ 12,
    enc_rpc %in% c(13101, 13102, 13103, 13104, 13105, 13106, 13107, 13108, 13109, 13110, 13111, 13112, 13113, 13114, 13115, 13116, 13117, 13118, 13119, 13120, 13121, 13122, 13123, 13124, 13125, 13126, 13127, 13128, 13129, 13130, 13131, 13132, 13201, 13301, 13302, 13401, 13402, 13404, 13501, 13601, 13604, 13605) ~ 13,
    enc_rpc %in% c(14101, 14201) ~ 14,
    # Agrega aquí las demás enc_rpcs con sus respectivas regiones
    TRUE ~ NA_real_ 
  ))# Para asignar NA si una enc_rpc no tiene región definida


datos <- datos %>%
  mutate(region = factor(enc_region, 
                         levels = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16), 
                         labels = c("Tarapacá", "Antofagasta", "Atacama", "Coquimbo", "Valparaíso", 
                                    "Bernardo O'Higgins", "Maule", "Biobío", "Araucanía", 
                                    "Los Lagos", "Aysén", "Magallanes", "RM", "Los Ríos", 
                                    "Arica y Parinacota", "Ñuble")
  ))

tasa_presencia <- datos %>%
  group_by(region) %>%
  summarise(
    total_siempre = sum(peleas * Fact_pers, na.rm = TRUE),  # Ignorar NA en los cálculos
    total_individuos = sum(Fact_pers, na.rm = TRUE),             # Ignorar NA en los cálculos
    tasa_presencia = ifelse(total_individuos == 0, NA, total_siempre / total_individuos)  # Evitar división por cero
  )

tasa_presencia_total <- datos %>%
  summarise(
    total_siempre = sum(peleas * Fact_pers, na.rm = TRUE),   # Total ponderado de consumidores
    total_individuos = sum(Fact_pers, na.rm = TRUE),            # Total ponderado de individuos
    tasa_presencia = ifelse(total_individuos == 0, NA, total_siempre / total_individuos )  # Calcular la tasa como porcentaje
  )

write_xlsx(
  list(
    "Tasa presencia total" = tasa_presencia_total,
    "Tasa presencia" = tasa_presencia
  ),
  "tasa_presencia_peleas_2017.xlsx"
)



## Balacera o Disparos

datos <- datos %>%
  mutate(balas = case_when(
    P11_16_1 %in% 4 ~ 1,  # Recodificar valores 4 como 1
    P11_16_1 %in% 1:3 ~ 0 # Recodificar valores 1 a 3 como 0
  ))

table(datos$enc_region)

etiquetas <- get_labels(datos$enc_rpc)
valores <- get_values(datos$enc_rpc)

tabla_codigos_etiquetas <- data.frame(
  Codigo = valores,
  Etiqueta = etiquetas
)
print(tabla_codigos_etiquetas)

datos <- datos %>%
  mutate(region = case_when(
    enc_rpc %in% c(15101) ~ 15,
    enc_rpc %in% c(1101, 1107) ~ 1,
    enc_rpc %in% c(2101, 2201, 2301) ~ 2,
    enc_rpc %in% c(3101, 3301) ~ 3,
    enc_rpc %in% c(4101, 4102, 4201, 4203, 4301) ~ 4,
    enc_rpc %in% c(5101, 5103, 5109, 5301, 5501, 5502, 5601, 5701, 5801, 5802, 5804) ~ 5,
    enc_rpc %in% c(6101, 6115, 6117, 6301) ~ 6,
    enc_rpc %in% c(7101, 7102, 7201, 7301, 7304, 7401) ~ 7,
    enc_rpc %in% c(8101, 8102, 8103, 8106, 8107, 8107, 8108, 8110, 8111, 8112, 8301) ~ 8,
    enc_rpc %in% c(8401, 8416) ~ 16,
    enc_rpc %in% c(9101, 9112, 9120, 9201) ~ 9,
    enc_rpc %in% c(10101, 10201, 10202, 10301) ~ 10,
    enc_rpc %in% c(11101, 11201) ~ 11,
    enc_rpc %in% c(12101) ~ 12,
    enc_rpc %in% c(13101, 13102, 13103, 13104, 13105, 13106, 13107, 13108, 13109, 13110, 13111, 13112, 13113, 13114, 13115, 13116, 13117, 13118, 13119, 13120, 13121, 13122, 13123, 13124, 13125, 13126, 13127, 13128, 13129, 13130, 13131, 13132, 13201, 13301, 13302, 13401, 13402, 13404, 13501, 13601, 13604, 13605) ~ 13,
    enc_rpc %in% c(14101, 14201) ~ 14,
    # Agrega aquí las demás enc_rpcs con sus respectivas regiones
    TRUE ~ NA_real_ 
  ))# Para asignar NA si una enc_rpc no tiene región definida


datos <- datos %>%
  mutate(region = factor(enc_region, 
                         levels = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16), 
                         labels = c("Tarapacá", "Antofagasta", "Atacama", "Coquimbo", "Valparaíso", 
                                    "Bernardo O'Higgins", "Maule", "Biobío", "Araucanía", 
                                    "Los Lagos", "Aysén", "Magallanes", "RM", "Los Ríos", 
                                    "Arica y Parinacota", "Ñuble")
  ))

tasa_presencia <- datos %>%
  group_by(region) %>%
  summarise(
    total_siempre = sum(balas * Fact_pers, na.rm = TRUE),  # Ignorar NA en los cálculos
    total_individuos = sum(Fact_pers, na.rm = TRUE),             # Ignorar NA en los cálculos
    tasa_presencia = ifelse(total_individuos == 0, NA, total_siempre / total_individuos)  # Evitar división por cero
  )

tasa_presencia_total <- datos %>%
  summarise(
    total_siempre = sum(balas * Fact_pers, na.rm = TRUE),   # Total ponderado de consumidores
    total_individuos = sum(Fact_pers, na.rm = TRUE),            # Total ponderado de individuos
    tasa_presencia = ifelse(total_individuos == 0, NA, total_siempre / total_individuos )  # Calcular la tasa como porcentaje
  )

write_xlsx(
  list(
    "Tasa presencia total" = tasa_presencia_total,
    "Tasa presencia" = tasa_presencia
  ),
  "tasa_presencia_balas_2017.xlsx"
)


## Robo con violencia

datos <- datos %>%
  mutate(robo = case_when(
    A1_1_1 %in% 1 ~ 1,  # Recodificar valores 5 como 1
    A1_1_1 %in% 2 ~ 0 # Recodificar valores 1 a 4 como 0
  ))

table(datos$enc_region)

etiquetas <- get_labels(datos$enc_rpc)
valores <- get_values(datos$enc_rpc)

tabla_codigos_etiquetas <- data.frame(
  Codigo = valores,
  Etiqueta = etiquetas
)
print(tabla_codigos_etiquetas)

datos <- datos %>%
  mutate(region = case_when(
    enc_rpc %in% c(15101) ~ 15,
    enc_rpc %in% c(1101, 1107) ~ 1,
    enc_rpc %in% c(2101, 2201, 2301) ~ 2,
    enc_rpc %in% c(3101, 3301) ~ 3,
    enc_rpc %in% c(4101, 4102, 4201, 4203, 4301) ~ 4,
    enc_rpc %in% c(5101, 5103, 5109, 5301, 5501, 5502, 5601, 5701, 5801, 5802, 5804) ~ 5,
    enc_rpc %in% c(6101, 6115, 6117, 6301) ~ 6,
    enc_rpc %in% c(7101, 7102, 7201, 7301, 7304, 7401) ~ 7,
    enc_rpc %in% c(8101, 8102, 8103, 8106, 8107, 8107, 8108, 8110, 8111, 8112, 8301) ~ 8,
    enc_rpc %in% c(8401, 8416) ~ 16,
    enc_rpc %in% c(9101, 9112, 9120, 9201) ~ 9,
    enc_rpc %in% c(10101, 10201, 10202, 10301) ~ 10,
    enc_rpc %in% c(11101, 11201) ~ 11,
    enc_rpc %in% c(12101) ~ 12,
    enc_rpc %in% c(13101, 13102, 13103, 13104, 13105, 13106, 13107, 13108, 13109, 13110, 13111, 13112, 13113, 13114, 13115, 13116, 13117, 13118, 13119, 13120, 13121, 13122, 13123, 13124, 13125, 13126, 13127, 13128, 13129, 13130, 13131, 13132, 13201, 13301, 13302, 13401, 13402, 13404, 13501, 13601, 13604, 13605) ~ 13,
    enc_rpc %in% c(14101, 14201) ~ 14,
    # Agrega aquí las demás enc_rpcs con sus respectivas regiones
    TRUE ~ NA_real_ 
  ))# Para asignar NA si una enc_rpc no tiene región definida


datos <- datos %>%
  mutate(region = factor(enc_region, 
                         levels = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16), 
                         labels = c("Tarapacá", "Antofagasta", "Atacama", "Coquimbo", "Valparaíso", 
                                    "Bernardo O'Higgins", "Maule", "Biobío", "Araucanía", 
                                    "Los Lagos", "Aysén", "Magallanes", "RM", "Los Ríos", 
                                    "Arica y Parinacota", "Ñuble")
  ))

tasa_presencia <- datos %>%
  group_by(region) %>%
  summarise(
    total_siempre = sum(robo * Fact_pers, na.rm = TRUE),  # Ignorar NA en los cálculos
    total_individuos = sum(Fact_pers, na.rm = TRUE),             # Ignorar NA en los cálculos
    tasa_presencia = ifelse(total_individuos == 0, NA, total_siempre / total_individuos)  # Evitar división por cero
  )

tasa_presencia_total <- datos %>%
  summarise(
    total_siempre = sum(robo * Fact_pers, na.rm = TRUE),   # Total ponderado de consumidores
    total_individuos = sum(Fact_pers, na.rm = TRUE),            # Total ponderado de individuos
    tasa_presencia = ifelse(total_individuos == 0, NA, total_siempre / total_individuos )  # Calcular la tasa como porcentaje
  )

write_xlsx(
  list(
    "Tasa presencia total" = tasa_presencia_total,
    "Tasa presencia" = tasa_presencia
  ),
  "tasa_presencia_robo_2017.xlsx"
)


#2016

rm(list = ls())
datos <- read_sav("bbdd_2016.sav")

## Pandillas violentas

datos <- datos %>%
  mutate(pandillas = case_when(
    P11_10_1 %in% 4 ~ 1,  # Recodificar valores 5 como 1
    P11_10_1 %in% 1:3 ~ 0 # Recodificar valores 1 a 4 como 0
  ))

table(datos$enc_region)

etiquetas <- get_labels(datos$enc_rpc)
valores <- get_values(datos$enc_rpc)

tabla_codigos_etiquetas <- data.frame(
  Codigo = valores,
  Etiqueta = etiquetas
)
print(tabla_codigos_etiquetas)

datos <- datos %>%
  mutate(region = case_when(
    enc_rpc %in% c(15101) ~ 15,
    enc_rpc %in% c(1101, 1107) ~ 1,
    enc_rpc %in% c(2101, 2201, 2301) ~ 2,
    enc_rpc %in% c(3101, 3301) ~ 3,
    enc_rpc %in% c(4101, 4102, 4201, 4203, 4301) ~ 4,
    enc_rpc %in% c(5101, 5103, 5109, 5301, 5501, 5502, 5601, 5701, 5801, 5802, 5804) ~ 5,
    enc_rpc %in% c(6101, 6115, 6117, 6301) ~ 6,
    enc_rpc %in% c(7101, 7102, 7201, 7301, 7304, 7401) ~ 7,
    enc_rpc %in% c(8101, 8102, 8103, 8106, 8107, 8107, 8108, 8110, 8111, 8112, 8301) ~ 8,
    enc_rpc %in% c(8401, 8416) ~ 16,
    enc_rpc %in% c(9101, 9112, 9120, 9201) ~ 9,
    enc_rpc %in% c(10101, 10201, 10202, 10301) ~ 10,
    enc_rpc %in% c(11101, 11201) ~ 11,
    enc_rpc %in% c(12101) ~ 12,
    enc_rpc %in% c(13101, 13102, 13103, 13104, 13105, 13106, 13107, 13108, 13109, 13110, 13111, 13112, 13113, 13114, 13115, 13116, 13117, 13118, 13119, 13120, 13121, 13122, 13123, 13124, 13125, 13126, 13127, 13128, 13129, 13130, 13131, 13132, 13201, 13301, 13302, 13401, 13402, 13404, 13501, 13601, 13604, 13605) ~ 13,
    enc_rpc %in% c(14101, 14201) ~ 14,
    # Agrega aquí las demás enc_rpcs con sus respectivas regiones
    TRUE ~ NA_real_ 
  ))# Para asignar NA si una enc_rpc no tiene región definida


datos <- datos %>%
  mutate(region = factor(enc_region, 
                         levels = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16), 
                         labels = c("Tarapacá", "Antofagasta", "Atacama", "Coquimbo", "Valparaíso", 
                                    "Bernardo O'Higgins", "Maule", "Biobío", "Araucanía", 
                                    "Los Lagos", "Aysén", "Magallanes", "RM", "Los Ríos", 
                                    "Arica y Parinacota", "Ñuble")
  ))

tasa_presencia <- datos %>%
  group_by(region) %>%
  summarise(
    total_siempre = sum(pandillas * Fact_pers, na.rm = TRUE),  # Ignorar NA en los cálculos
    total_individuos = sum(Fact_pers, na.rm = TRUE),             # Ignorar NA en los cálculos
    tasa_presencia = ifelse(total_individuos == 0, NA, total_siempre / total_individuos)  # Evitar división por cero
  )

tasa_presencia_total <- datos %>%
  summarise(
    total_siempre = sum(pandillas * Fact_pers, na.rm = TRUE),   # Total ponderado de consumidores
    total_individuos = sum(Fact_pers, na.rm = TRUE),            # Total ponderado de individuos
    tasa_presencia = ifelse(total_individuos == 0, NA, total_siempre / total_individuos )  # Calcular la tasa como porcentaje
  )

write_xlsx(
  list(
    "Tasa presencia total" = tasa_presencia_total,
    "Tasa presencia" = tasa_presencia
  ),
  "tasa_presencia_pandillas_2016.xlsx"
)


## Peleas calljeras con arma blanco o de fuego 

datos <- datos %>%
  mutate(peleas = case_when(
    P11_11_1 %in% 4 ~ 1,  # Recodificar valores 4 como 1
    P11_11_1 %in% 1:1 ~ 0 # Recodificar valores 1 a 3 como 0
  ))

table(datos$enc_region)

etiquetas <- get_labels(datos$enc_rpc)
valores <- get_values(datos$enc_rpc)

tabla_codigos_etiquetas <- data.frame(
  Codigo = valores,
  Etiqueta = etiquetas
)
print(tabla_codigos_etiquetas)

datos <- datos %>%
  mutate(region = case_when(
    enc_rpc %in% c(15101) ~ 15,
    enc_rpc %in% c(1101, 1107) ~ 1,
    enc_rpc %in% c(2101, 2201, 2301) ~ 2,
    enc_rpc %in% c(3101, 3301) ~ 3,
    enc_rpc %in% c(4101, 4102, 4201, 4203, 4301) ~ 4,
    enc_rpc %in% c(5101, 5103, 5109, 5301, 5501, 5502, 5601, 5701, 5801, 5802, 5804) ~ 5,
    enc_rpc %in% c(6101, 6115, 6117, 6301) ~ 6,
    enc_rpc %in% c(7101, 7102, 7201, 7301, 7304, 7401) ~ 7,
    enc_rpc %in% c(8101, 8102, 8103, 8106, 8107, 8107, 8108, 8110, 8111, 8112, 8301) ~ 8,
    enc_rpc %in% c(8401, 8416) ~ 16,
    enc_rpc %in% c(9101, 9112, 9120, 9201) ~ 9,
    enc_rpc %in% c(10101, 10201, 10202, 10301) ~ 10,
    enc_rpc %in% c(11101, 11201) ~ 11,
    enc_rpc %in% c(12101) ~ 12,
    enc_rpc %in% c(13101, 13102, 13103, 13104, 13105, 13106, 13107, 13108, 13109, 13110, 13111, 13112, 13113, 13114, 13115, 13116, 13117, 13118, 13119, 13120, 13121, 13122, 13123, 13124, 13125, 13126, 13127, 13128, 13129, 13130, 13131, 13132, 13201, 13301, 13302, 13401, 13402, 13404, 13501, 13601, 13604, 13605) ~ 13,
    enc_rpc %in% c(14101, 14201) ~ 14,
    # Agrega aquí las demás enc_rpcs con sus respectivas regiones
    TRUE ~ NA_real_ 
  ))# Para asignar NA si una enc_rpc no tiene región definida


datos <- datos %>%
  mutate(region = factor(enc_region, 
                         levels = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16), 
                         labels = c("Tarapacá", "Antofagasta", "Atacama", "Coquimbo", "Valparaíso", 
                                    "Bernardo O'Higgins", "Maule", "Biobío", "Araucanía", 
                                    "Los Lagos", "Aysén", "Magallanes", "RM", "Los Ríos", 
                                    "Arica y Parinacota", "Ñuble")
  ))

tasa_presencia <- datos %>%
  group_by(region) %>%
  summarise(
    total_siempre = sum(peleas * Fact_pers, na.rm = TRUE),  # Ignorar NA en los cálculos
    total_individuos = sum(Fact_pers, na.rm = TRUE),             # Ignorar NA en los cálculos
    tasa_presencia = ifelse(total_individuos == 0, NA, total_siempre / total_individuos)  # Evitar división por cero
  )

tasa_presencia_total <- datos %>%
  summarise(
    total_siempre = sum(peleas * Fact_pers, na.rm = TRUE),   # Total ponderado de consumidores
    total_individuos = sum(Fact_pers, na.rm = TRUE),            # Total ponderado de individuos
    tasa_presencia = ifelse(total_individuos == 0, NA, total_siempre / total_individuos )  # Calcular la tasa como porcentaje
  )

write_xlsx(
  list(
    "Tasa presencia total" = tasa_presencia_total,
    "Tasa presencia" = tasa_presencia
  ),
  "tasa_presencia_peleas_2016.xlsx"
)



## Balacera o Disparos

datos <- datos %>%
  mutate(balas = case_when(
    P11_16_1 %in% 4 ~ 1,  # Recodificar valores 4 como 1
    P11_16_1 %in% 1:3 ~ 0 # Recodificar valores 1 a 3 como 0
  ))

table(datos$enc_region)

etiquetas <- get_labels(datos$enc_rpc)
valores <- get_values(datos$enc_rpc)

tabla_codigos_etiquetas <- data.frame(
  Codigo = valores,
  Etiqueta = etiquetas
)
print(tabla_codigos_etiquetas)

datos <- datos %>%
  mutate(region = case_when(
    enc_rpc %in% c(15101) ~ 15,
    enc_rpc %in% c(1101, 1107) ~ 1,
    enc_rpc %in% c(2101, 2201, 2301) ~ 2,
    enc_rpc %in% c(3101, 3301) ~ 3,
    enc_rpc %in% c(4101, 4102, 4201, 4203, 4301) ~ 4,
    enc_rpc %in% c(5101, 5103, 5109, 5301, 5501, 5502, 5601, 5701, 5801, 5802, 5804) ~ 5,
    enc_rpc %in% c(6101, 6115, 6117, 6301) ~ 6,
    enc_rpc %in% c(7101, 7102, 7201, 7301, 7304, 7401) ~ 7,
    enc_rpc %in% c(8101, 8102, 8103, 8106, 8107, 8107, 8108, 8110, 8111, 8112, 8301) ~ 8,
    enc_rpc %in% c(8401, 8416) ~ 16,
    enc_rpc %in% c(9101, 9112, 9120, 9201) ~ 9,
    enc_rpc %in% c(10101, 10201, 10202, 10301) ~ 10,
    enc_rpc %in% c(11101, 11201) ~ 11,
    enc_rpc %in% c(12101) ~ 12,
    enc_rpc %in% c(13101, 13102, 13103, 13104, 13105, 13106, 13107, 13108, 13109, 13110, 13111, 13112, 13113, 13114, 13115, 13116, 13117, 13118, 13119, 13120, 13121, 13122, 13123, 13124, 13125, 13126, 13127, 13128, 13129, 13130, 13131, 13132, 13201, 13301, 13302, 13401, 13402, 13404, 13501, 13601, 13604, 13605) ~ 13,
    enc_rpc %in% c(14101, 14201) ~ 14,
    # Agrega aquí las demás enc_rpcs con sus respectivas regiones
    TRUE ~ NA_real_ 
  ))# Para asignar NA si una enc_rpc no tiene región definida


datos <- datos %>%
  mutate(region = factor(enc_region, 
                         levels = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16), 
                         labels = c("Tarapacá", "Antofagasta", "Atacama", "Coquimbo", "Valparaíso", 
                                    "Bernardo O'Higgins", "Maule", "Biobío", "Araucanía", 
                                    "Los Lagos", "Aysén", "Magallanes", "RM", "Los Ríos", 
                                    "Arica y Parinacota", "Ñuble")
  ))

tasa_presencia <- datos %>%
  group_by(region) %>%
  summarise(
    total_siempre = sum(balas * Fact_pers, na.rm = TRUE),  # Ignorar NA en los cálculos
    total_individuos = sum(Fact_pers, na.rm = TRUE),             # Ignorar NA en los cálculos
    tasa_presencia = ifelse(total_individuos == 0, NA, total_siempre / total_individuos)  # Evitar división por cero
  )

tasa_presencia_total <- datos %>%
  summarise(
    total_siempre = sum(balas * Fact_pers, na.rm = TRUE),   # Total ponderado de consumidores
    total_individuos = sum(Fact_pers, na.rm = TRUE),            # Total ponderado de individuos
    tasa_presencia = ifelse(total_individuos == 0, NA, total_siempre / total_individuos )  # Calcular la tasa como porcentaje
  )

write_xlsx(
  list(
    "Tasa presencia total" = tasa_presencia_total,
    "Tasa presencia" = tasa_presencia
  ),
  "tasa_presencia_balas_2016.xlsx"
)


## Robo con violencia

datos <- datos %>%
  mutate(robo = case_when(
    A1_1_1 %in% 1 ~ 1,  # Recodificar valores 5 como 1
    A1_1_1 %in% 2 ~ 0 # Recodificar valores 1 a 4 como 0
  ))

table(datos$enc_region)

etiquetas <- get_labels(datos$enc_rpc)
valores <- get_values(datos$enc_rpc)

tabla_codigos_etiquetas <- data.frame(
  Codigo = valores,
  Etiqueta = etiquetas
)
print(tabla_codigos_etiquetas)

datos <- datos %>%
  mutate(region = case_when(
    enc_rpc %in% c(15101) ~ 15,
    enc_rpc %in% c(1101, 1107) ~ 1,
    enc_rpc %in% c(2101, 2201, 2301) ~ 2,
    enc_rpc %in% c(3101, 3301) ~ 3,
    enc_rpc %in% c(4101, 4102, 4201, 4203, 4301) ~ 4,
    enc_rpc %in% c(5101, 5103, 5109, 5301, 5501, 5502, 5601, 5701, 5801, 5802, 5804) ~ 5,
    enc_rpc %in% c(6101, 6115, 6117, 6301) ~ 6,
    enc_rpc %in% c(7101, 7102, 7201, 7301, 7304, 7401) ~ 7,
    enc_rpc %in% c(8101, 8102, 8103, 8106, 8107, 8107, 8108, 8110, 8111, 8112, 8301) ~ 8,
    enc_rpc %in% c(8401, 8416) ~ 16,
    enc_rpc %in% c(9101, 9112, 9120, 9201) ~ 9,
    enc_rpc %in% c(10101, 10201, 10202, 10301) ~ 10,
    enc_rpc %in% c(11101, 11201) ~ 11,
    enc_rpc %in% c(12101) ~ 12,
    enc_rpc %in% c(13101, 13102, 13103, 13104, 13105, 13106, 13107, 13108, 13109, 13110, 13111, 13112, 13113, 13114, 13115, 13116, 13117, 13118, 13119, 13120, 13121, 13122, 13123, 13124, 13125, 13126, 13127, 13128, 13129, 13130, 13131, 13132, 13201, 13301, 13302, 13401, 13402, 13404, 13501, 13601, 13604, 13605) ~ 13,
    enc_rpc %in% c(14101, 14201) ~ 14,
    # Agrega aquí las demás enc_rpcs con sus respectivas regiones
    TRUE ~ NA_real_ 
  ))# Para asignar NA si una enc_rpc no tiene región definida


datos <- datos %>%
  mutate(region = factor(enc_region, 
                         levels = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16), 
                         labels = c("Tarapacá", "Antofagasta", "Atacama", "Coquimbo", "Valparaíso", 
                                    "Bernardo O'Higgins", "Maule", "Biobío", "Araucanía", 
                                    "Los Lagos", "Aysén", "Magallanes", "RM", "Los Ríos", 
                                    "Arica y Parinacota", "Ñuble")
  ))

tasa_presencia <- datos %>%
  group_by(region) %>%
  summarise(
    total_siempre = sum(robo * Fact_pers, na.rm = TRUE),  # Ignorar NA en los cálculos
    total_individuos = sum(Fact_pers, na.rm = TRUE),             # Ignorar NA en los cálculos
    tasa_presencia = ifelse(total_individuos == 0, NA, total_siempre / total_individuos)  # Evitar división por cero
  )

tasa_presencia_total <- datos %>%
  summarise(
    total_siempre = sum(robo * Fact_pers, na.rm = TRUE),   # Total ponderado de consumidores
    total_individuos = sum(Fact_pers, na.rm = TRUE),            # Total ponderado de individuos
    tasa_presencia = ifelse(total_individuos == 0, NA, total_siempre / total_individuos )  # Calcular la tasa como porcentaje
  )

write_xlsx(
  list(
    "Tasa presencia total" = tasa_presencia_total,
    "Tasa presencia" = tasa_presencia
  ),
  "tasa_presencia_robo_2016.xlsx"
)



#2015

rm(list = ls())
datos <- read_sav("bbdd_2015.sav")

# Pandillas violentas 
datos <- datos %>%
  mutate(pandillas = case_when(
    P10_10_1 %in% 4 ~ 1,  # Recodificar valores 5 como 1
    P10_10_1 %in% 1:3 ~ 0 # Recodificar valores 1 a 4 como 0
  ))

table(datos$enc_region)

etiquetas <- get_labels(datos$enc_rpc)
valores <- get_values(datos$enc_rpc)

tabla_codigos_etiquetas <- data.frame(
  Codigo = valores,
  Etiqueta = etiquetas
)
print(tabla_codigos_etiquetas)

datos <- datos %>%
  mutate(region = case_when(
    enc_rpc %in% c(15101) ~ 15,
    enc_rpc %in% c(1101, 1107) ~ 1,
    enc_rpc %in% c(2101, 2201, 2301) ~ 2,
    enc_rpc %in% c(3101, 3301) ~ 3,
    enc_rpc %in% c(4101, 4102, 4201, 4203, 4301) ~ 4,
    enc_rpc %in% c(5101, 5103, 5106, 5108, 5109, 5301, 5501, 5502, 5505, 5601, 5701, 5801, 5802, 5804) ~ 5,
    enc_rpc %in% c(6101, 6115, 6117, 6301) ~ 6,
    enc_rpc %in% c(7101, 7102, 7201, 7301, 7304, 7401) ~ 7,
    enc_rpc %in% c(8101, 8102, 8103, 8106, 8107, 8107, 8108, 8110, 8111, 8112, 8301) ~ 8,
    enc_rpc %in% c(8401, 8416) ~ 16,
    enc_rpc %in% c(9101, 9112, 9120, 9201) ~ 9,
    enc_rpc %in% c(10101, 10201, 10202, 10301) ~ 10,
    enc_rpc %in% c(11101, 11201) ~ 11,
    enc_rpc %in% c(12101) ~ 12,
    enc_rpc %in% c(13101, 13102, 13103, 13104, 13105, 13106, 13107, 13108, 13109, 13110, 13111, 13112, 13113, 13114, 13115, 13116, 13117, 13118, 13119, 13120, 13121, 13122, 13123, 13124, 13125, 13126, 13127, 13128, 13129, 13130, 13131, 13132, 13201, 13301, 13302, 13401, 13402, 13404, 13501, 13601, 13604, 13605) ~ 13,
    enc_rpc %in% c(14101, 14201) ~ 14,
    # Agrega aquí las demás enc_rpcs con sus respectivas regiones
    TRUE ~ NA_real_ 
  ))# Para asignar NA si una enc_rpc no tiene región definida


datos <- datos %>%
  mutate(region = factor(enc_region, 
                         levels = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16), 
                         labels = c("Tarapacá", "Antofagasta", "Atacama", "Coquimbo", "Valparaíso", 
                                    "Bernardo O'Higgins", "Maule", "Biobío", "Araucanía", 
                                    "Los Lagos", "Aysén", "Magallanes", "RM", "Los Ríos", 
                                    "Arica y Parinacota", "Ñuble")
  ))

tasa_presencia <- datos %>%
  group_by(region) %>%
  summarise(
    total_siempre = sum(pandillas * Fact_pers_15reg_nuevo, na.rm = TRUE),  # Ignorar NA en los cálculos
    total_individuos = sum(Fact_pers_15reg_nuevo, na.rm = TRUE),             # Ignorar NA en los cálculos
    tasa_presencia = ifelse(total_individuos == 0, NA, total_siempre / total_individuos)  # Evitar división por cero
  )

tasa_presencia_total <- datos %>%
  summarise(
    total_siempre = sum(pandillas * Fact_pers_15reg_nuevo, na.rm = TRUE),   # Total ponderado de consumidores
    total_individuos = sum(Fact_pers_15reg_nuevo, na.rm = TRUE),            # Total ponderado de individuos
    tasa_presencia = ifelse(total_individuos == 0, NA, total_siempre / total_individuos )  # Calcular la tasa como porcentaje
  )

write_xlsx(
  list(
    "Tasa presencia total" = tasa_presencia_total,
    "Tasa presencia" = tasa_presencia
  ),
  "tasa_presencia_pandillas_2015.xlsx"
)

## Peleas calljeras con arma blanco o de fuego 

datos <- datos %>%
  mutate(peleas = case_when(
    P10_11_1 %in% 4 ~ 1,  # Recodificar valores 4 como 1
    P10_11_1 %in% 1:1 ~ 0 # Recodificar valores 1 a 3 como 0
  ))

table(datos$enc_region)

etiquetas <- get_labels(datos$enc_rpc)
valores <- get_values(datos$enc_rpc)

tabla_codigos_etiquetas <- data.frame(
  Codigo = valores,
  Etiqueta = etiquetas
)
print(tabla_codigos_etiquetas)

datos <- datos %>%
  mutate(region = case_when(
    enc_rpc %in% c(15101) ~ 15,
    enc_rpc %in% c(1101, 1107) ~ 1,
    enc_rpc %in% c(2101, 2201, 2301) ~ 2,
    enc_rpc %in% c(3101, 3301) ~ 3,
    enc_rpc %in% c(4101, 4102, 4201, 4203, 4301) ~ 4,
    enc_rpc %in% c(5101, 5103, 5109, 5301, 5501, 5502, 5601, 5701, 5801, 5802, 5804) ~ 5,
    enc_rpc %in% c(6101, 6115, 6117, 6301) ~ 6,
    enc_rpc %in% c(7101, 7102, 7201, 7301, 7304, 7401) ~ 7,
    enc_rpc %in% c(8101, 8102, 8103, 8106, 8107, 8107, 8108, 8110, 8111, 8112, 8301) ~ 8,
    enc_rpc %in% c(8401, 8416) ~ 16,
    enc_rpc %in% c(9101, 9112, 9120, 9201) ~ 9,
    enc_rpc %in% c(10101, 10201, 10202, 10301) ~ 10,
    enc_rpc %in% c(11101, 11201) ~ 11,
    enc_rpc %in% c(12101) ~ 12,
    enc_rpc %in% c(13101, 13102, 13103, 13104, 13105, 13106, 13107, 13108, 13109, 13110, 13111, 13112, 13113, 13114, 13115, 13116, 13117, 13118, 13119, 13120, 13121, 13122, 13123, 13124, 13125, 13126, 13127, 13128, 13129, 13130, 13131, 13132, 13201, 13301, 13302, 13401, 13402, 13404, 13501, 13601, 13604, 13605) ~ 13,
    enc_rpc %in% c(14101, 14201) ~ 14,
    # Agrega aquí las demás enc_rpcs con sus respectivas regiones
    TRUE ~ NA_real_ 
  ))# Para asignar NA si una enc_rpc no tiene región definida


datos <- datos %>%
  mutate(region = factor(enc_region, 
                         levels = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16), 
                         labels = c("Tarapacá", "Antofagasta", "Atacama", "Coquimbo", "Valparaíso", 
                                    "Bernardo O'Higgins", "Maule", "Biobío", "Araucanía", 
                                    "Los Lagos", "Aysén", "Magallanes", "RM", "Los Ríos", 
                                    "Arica y Parinacota", "Ñuble")
  ))

tasa_presencia <- datos %>%
  group_by(region) %>%
  summarise(
    total_siempre = sum(peleas * Fact_pers, na.rm = TRUE),  # Ignorar NA en los cálculos
    total_individuos = sum(Fact_pers, na.rm = TRUE),             # Ignorar NA en los cálculos
    tasa_presencia = ifelse(total_individuos == 0, NA, total_siempre / total_individuos)  # Evitar división por cero
  )

tasa_presencia_total <- datos %>%
  summarise(
    total_siempre = sum(peleas * Fact_pers, na.rm = TRUE),   # Total ponderado de consumidores
    total_individuos = sum(Fact_pers, na.rm = TRUE),            # Total ponderado de individuos
    tasa_presencia = ifelse(total_individuos == 0, NA, total_siempre / total_individuos )  # Calcular la tasa como porcentaje
  )

write_xlsx(
  list(
    "Tasa presencia total" = tasa_presencia_total,
    "Tasa presencia" = tasa_presencia
  ),
  "tasa_presencia_peleas_2015.xlsx"
)



## Balacera o Disparos

datos <- datos %>%
  mutate(balas = case_when(
    P10_16_1 %in% 4 ~ 1,  # Recodificar valores 4 como 1
    P10_16_1 %in% 1:3 ~ 0 # Recodificar valores 1 a 3 como 0
  ))

table(datos$enc_region)

etiquetas <- get_labels(datos$enc_rpc)
valores <- get_values(datos$enc_rpc)

tabla_codigos_etiquetas <- data.frame(
  Codigo = valores,
  Etiqueta = etiquetas
)
print(tabla_codigos_etiquetas)

datos <- datos %>%
  mutate(region = case_when(
    enc_rpc %in% c(15101) ~ 15,
    enc_rpc %in% c(1101, 1107) ~ 1,
    enc_rpc %in% c(2101, 2201, 2301) ~ 2,
    enc_rpc %in% c(3101, 3301) ~ 3,
    enc_rpc %in% c(4101, 4102, 4201, 4203, 4301) ~ 4,
    enc_rpc %in% c(5101, 5103, 5109, 5301, 5501, 5502, 5601, 5701, 5801, 5802, 5804) ~ 5,
    enc_rpc %in% c(6101, 6115, 6117, 6301) ~ 6,
    enc_rpc %in% c(7101, 7102, 7201, 7301, 7304, 7401) ~ 7,
    enc_rpc %in% c(8101, 8102, 8103, 8106, 8107, 8107, 8108, 8110, 8111, 8112, 8301) ~ 8,
    enc_rpc %in% c(8401, 8416) ~ 16,
    enc_rpc %in% c(9101, 9112, 9120, 9201) ~ 9,
    enc_rpc %in% c(10101, 10201, 10202, 10301) ~ 10,
    enc_rpc %in% c(11101, 11201) ~ 11,
    enc_rpc %in% c(12101) ~ 12,
    enc_rpc %in% c(13101, 13102, 13103, 13104, 13105, 13106, 13107, 13108, 13109, 13110, 13111, 13112, 13113, 13114, 13115, 13116, 13117, 13118, 13119, 13120, 13121, 13122, 13123, 13124, 13125, 13126, 13127, 13128, 13129, 13130, 13131, 13132, 13201, 13301, 13302, 13401, 13402, 13404, 13501, 13601, 13604, 13605) ~ 13,
    enc_rpc %in% c(14101, 14201) ~ 14,
    # Agrega aquí las demás enc_rpcs con sus respectivas regiones
    TRUE ~ NA_real_ 
  ))# Para asignar NA si una enc_rpc no tiene región definida


datos <- datos %>%
  mutate(region = factor(enc_region, 
                         levels = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16), 
                         labels = c("Tarapacá", "Antofagasta", "Atacama", "Coquimbo", "Valparaíso", 
                                    "Bernardo O'Higgins", "Maule", "Biobío", "Araucanía", 
                                    "Los Lagos", "Aysén", "Magallanes", "RM", "Los Ríos", 
                                    "Arica y Parinacota", "Ñuble")
  ))

tasa_presencia <- datos %>%
  group_by(region) %>%
  summarise(
    total_siempre = sum(balas * Fact_pers, na.rm = TRUE),  # Ignorar NA en los cálculos
    total_individuos = sum(Fact_pers, na.rm = TRUE),             # Ignorar NA en los cálculos
    tasa_presencia = ifelse(total_individuos == 0, NA, total_siempre / total_individuos)  # Evitar división por cero
  )

tasa_presencia_total <- datos %>%
  summarise(
    total_siempre = sum(balas * Fact_pers, na.rm = TRUE),   # Total ponderado de consumidores
    total_individuos = sum(Fact_pers, na.rm = TRUE),            # Total ponderado de individuos
    tasa_presencia = ifelse(total_individuos == 0, NA, total_siempre / total_individuos )  # Calcular la tasa como porcentaje
  )

write_xlsx(
  list(
    "Tasa presencia total" = tasa_presencia_total,
    "Tasa presencia" = tasa_presencia
  ),
  "tasa_presencia_balas_2015.xlsx"
)


## Robo con violencia

datos <- datos %>%
  mutate(robo = case_when(
    A1_1_1 %in% 1 ~ 1,  # Recodificar valores 5 como 1
    A1_1_1 %in% 2 ~ 0 # Recodificar valores 1 a 4 como 0
  ))

table(datos$enc_region)

etiquetas <- get_labels(datos$enc_rpc)
valores <- get_values(datos$enc_rpc)

tabla_codigos_etiquetas <- data.frame(
  Codigo = valores,
  Etiqueta = etiquetas
)
print(tabla_codigos_etiquetas)

datos <- datos %>%
  mutate(region = case_when(
    enc_rpc %in% c(15101) ~ 15,
    enc_rpc %in% c(1101, 1107) ~ 1,
    enc_rpc %in% c(2101, 2201, 2301) ~ 2,
    enc_rpc %in% c(3101, 3301) ~ 3,
    enc_rpc %in% c(4101, 4102, 4201, 4203, 4301) ~ 4,
    enc_rpc %in% c(5101, 5103, 5109, 5301, 5501, 5502, 5601, 5701, 5801, 5802, 5804) ~ 5,
    enc_rpc %in% c(6101, 6115, 6117, 6301) ~ 6,
    enc_rpc %in% c(7101, 7102, 7201, 7301, 7304, 7401) ~ 7,
    enc_rpc %in% c(8101, 8102, 8103, 8106, 8107, 8107, 8108, 8110, 8111, 8112, 8301) ~ 8,
    enc_rpc %in% c(8401, 8416) ~ 16,
    enc_rpc %in% c(9101, 9112, 9120, 9201) ~ 9,
    enc_rpc %in% c(10101, 10201, 10202, 10301) ~ 10,
    enc_rpc %in% c(11101, 11201) ~ 11,
    enc_rpc %in% c(12101) ~ 12,
    enc_rpc %in% c(13101, 13102, 13103, 13104, 13105, 13106, 13107, 13108, 13109, 13110, 13111, 13112, 13113, 13114, 13115, 13116, 13117, 13118, 13119, 13120, 13121, 13122, 13123, 13124, 13125, 13126, 13127, 13128, 13129, 13130, 13131, 13132, 13201, 13301, 13302, 13401, 13402, 13404, 13501, 13601, 13604, 13605) ~ 13,
    enc_rpc %in% c(14101, 14201) ~ 14,
    # Agrega aquí las demás enc_rpcs con sus respectivas regiones
    TRUE ~ NA_real_ 
  ))# Para asignar NA si una enc_rpc no tiene región definida


datos <- datos %>%
  mutate(region = factor(enc_region, 
                         levels = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16), 
                         labels = c("Tarapacá", "Antofagasta", "Atacama", "Coquimbo", "Valparaíso", 
                                    "Bernardo O'Higgins", "Maule", "Biobío", "Araucanía", 
                                    "Los Lagos", "Aysén", "Magallanes", "RM", "Los Ríos", 
                                    "Arica y Parinacota", "Ñuble")
  ))

tasa_presencia <- datos %>%
  group_by(region) %>%
  summarise(
    total_siempre = sum(robo * Fact_pers, na.rm = TRUE),  # Ignorar NA en los cálculos
    total_individuos = sum(Fact_pers, na.rm = TRUE),             # Ignorar NA en los cálculos
    tasa_presencia = ifelse(total_individuos == 0, NA, total_siempre / total_individuos)  # Evitar división por cero
  )

tasa_presencia_total <- datos %>%
  summarise(
    total_siempre = sum(robo * Fact_pers, na.rm = TRUE),   # Total ponderado de consumidores
    total_individuos = sum(Fact_pers, na.rm = TRUE),            # Total ponderado de individuos
    tasa_presencia = ifelse(total_individuos == 0, NA, total_siempre / total_individuos )  # Calcular la tasa como porcentaje
  )

write_xlsx(
  list(
    "Tasa presencia total" = tasa_presencia_total,
    "Tasa presencia" = tasa_presencia
  ),
  "tasa_presencia_robo_2015.xlsx"
)
