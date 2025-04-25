setwd("C:/Users/cjara/OneDrive - Universidad Católica de Chile/Mercado drogas/ENUSC/2023")
rm(list = ls())
library(haven)
library(dplyr)
library(sjlabelled)
library(writexl)
options(scipen = 999)

# Carga el archivo SPSS
datos <- read_sav("C:/Users/cjara/OneDrive - Universidad Católica de Chile/Mercado drogas/ENUSC/2017/bbdd.sav")
summary(datos)

#2023

setwd("C:/Users/cjara/OneDrive - Universidad Católica de Chile/Mercado drogas/ENUSC/2023")
datos <- read_sav("C:/Users/cjara/OneDrive - Universidad Católica de Chile/Mercado drogas/ENUSC/2023/bbdd2023.sav")
summary(datos)

datos <- datos %>%
  mutate(trafico = case_when(
    PRESENCIA_TRAFICO %in% 5:4 ~ 1,  # Recodificar valores 5 como 1
    PRESENCIA_TRAFICO %in% 1:3 ~ 0 # Recodificar valores 1 a 4 como 0
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
    total_siempre = sum(trafico * Fact_Pers_Com, na.rm = TRUE),  # Ignorar NA en los cálculos
    total_individuos = sum(Fact_Pers_Com, na.rm = TRUE),             # Ignorar NA en los cálculos
    tasa_presencia = ifelse(total_individuos == 0, NA, total_siempre / total_individuos)  # Evitar división por cero
  )


tasa_presencia2 <- datos %>%
  group_by(enc_region) %>%
  summarise(
    total_siempre = sum(trafico * Fact_Pers_Reg, na.rm = TRUE),  # Ignorar NA en los cálculos
    total_individuos = sum(Fact_Pers_Reg, na.rm = TRUE),             # Ignorar NA en los cálculos
    tasa_presencia = ifelse(total_individuos == 0, NA, total_siempre / total_individuos)  # Evitar división por cero
  )

tasa_presencia_total <- datos %>%
  summarise(
    total_siempre = sum(trafico * Fact_Pers_Com, na.rm = TRUE),   # Total ponderado de consumidores
    total_individuos = sum(Fact_Pers_Com, na.rm = TRUE),            # Total ponderado de individuos
    tasa_presencia = ifelse(total_individuos == 0, NA, total_siempre / total_individuos )  # Calcular la tasa como porcentaje
  )

write_xlsx(
  list(
    "Tasa presencia" = tasa_presencia,
    "Tasa presencia total" = tasa_presencia_total
  ),
  "tasa_presencia.xlsx"
)

#2022

rm(list = ls())
setwd("C:/Users/cjara/OneDrive - Universidad Católica de Chile/Mercado drogas/ENUSC/2022")
datos <- read_sav("C:/Users/cjara/OneDrive - Universidad Católica de Chile/Mercado drogas/ENUSC/2022/bbdd.sav")
summary(datos)

datos <- datos %>%
  mutate(trafico = case_when(
    P6_14_1 %in% 4:3 ~ 1,  # Recodificar valores 5 como 1
    P6_14_1 %in% 1:2 ~ 0 # Recodificar valores 1 a 4 como 0
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
    total_siempre = sum(trafico * Fact_Pers, na.rm = TRUE),  # Ignorar NA en los cálculos
    total_individuos = sum(Fact_Pers, na.rm = TRUE),             # Ignorar NA en los cálculos
    tasa_presencia = ifelse(total_individuos == 0, NA, total_siempre / total_individuos)  # Evitar división por cero
  )

tasa_presencia_total <- datos %>%
  summarise(
    total_siempre = sum(trafico * Fact_Pers, na.rm = TRUE),   # Total ponderado de consumidores
    total_individuos = sum(Fact_Pers, na.rm = TRUE),            # Total ponderado de individuos
    tasa_presencia = ifelse(total_individuos == 0, NA, total_siempre / total_individuos )  # Calcular la tasa como porcentaje
  )

write_xlsx(
  list(
    "Tasa presencia" = tasa_presencia,
    "Tasa presencia total" = tasa_presencia_total
  ),
  "tasa_presencia.xlsx"
)

#2020

rm(list = ls())
setwd("C:/Users/cjara/OneDrive - Universidad Católica de Chile/Mercado drogas/ENUSC/2020")
datos <- read_sav("C:/Users/cjara/OneDrive - Universidad Católica de Chile/Mercado drogas/ENUSC/2020/bbdd.sav")
summary(datos)

datos <- datos %>%
  mutate(trafico = case_when(
    P5_14_1 %in% 4:3 ~ 1,  # Recodificar valores 5 como 1
    P5_14_1 %in% 1:2 ~ 0 # Recodificar valores 1 a 4 como 0
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
    total_siempre = sum(trafico * Fact_Pers, na.rm = TRUE),  # Ignorar NA en los cálculos
    total_individuos = sum(Fact_Pers, na.rm = TRUE),             # Ignorar NA en los cálculos
    tasa_presencia = ifelse(total_individuos == 0, NA, total_siempre / total_individuos)  # Evitar división por cero
  )

tasa_presencia_total <- datos %>%
  summarise(
    total_siempre = sum(trafico * Fact_Pers, na.rm = TRUE),   # Total ponderado de consumidores
    total_individuos = sum(Fact_Pers, na.rm = TRUE),            # Total ponderado de individuos
    tasa_presencia = ifelse(total_individuos == 0, NA, total_siempre / total_individuos )  # Calcular la tasa como porcentaje
  )

write_xlsx(
  list(
    "Tasa presencia" = tasa_presencia,
    "Tasa presencia total" = tasa_presencia_total
  ),
  "tasa_presencia.xlsx"
)

#2019

rm(list = ls())
setwd("C:/Users/cjara/OneDrive - Universidad Católica de Chile/Mercado drogas/ENUSC/2019")
datos <- read_sav("C:/Users/cjara/OneDrive - Universidad Católica de Chile/Mercado drogas/ENUSC/2019/bbdd.sav")

datos <- datos %>%
  mutate(trafico = case_when(
    P11_14_1 %in% 4:3 ~ 1,  # Recodificar valores 5 como 1
    P11_14_1 %in% 1:2 ~ 0 # Recodificar valores 1 a 4 como 0
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
    total_siempre = sum(trafico * Fact_Pers, na.rm = TRUE),  # Ignorar NA en los cálculos
    total_individuos = sum(Fact_Pers, na.rm = TRUE),             # Ignorar NA en los cálculos
    tasa_presencia = ifelse(total_individuos == 0, NA, total_siempre / total_individuos)  # Evitar división por cero
  )

tasa_presencia_total <- datos %>%
  summarise(
    total_siempre = sum(trafico * Fact_Pers, na.rm = TRUE),   # Total ponderado de consumidores
    total_individuos = sum(Fact_Pers, na.rm = TRUE),            # Total ponderado de individuos
    tasa_presencia = ifelse(total_individuos == 0, NA, total_siempre / total_individuos )  # Calcular la tasa como porcentaje
  )

write_xlsx(
  list(
    "Tasa presencia total" = tasa_presencia_total,
    "Tasa presencia" = tasa_presencia
  ),
  "tasa_presencia.xlsx"
)

#2018

rm(list = ls())
setwd("C:/Users/cjara/OneDrive - Universidad Católica de Chile/Mercado drogas/ENUSC/2018")
datos <- read_sav("C:/Users/cjara/OneDrive - Universidad Católica de Chile/Mercado drogas/ENUSC/2018/bbdd.sav")

datos <- datos %>%
  mutate(trafico = case_when(
    P11_14_1 %in% 4:3 ~ 1,  # Recodificar valores 5 como 1
    P11_14_1 %in% 1:2 ~ 0 # Recodificar valores 1 a 4 como 0
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
    total_siempre = sum(trafico * Fact_pers, na.rm = TRUE),  # Ignorar NA en los cálculos
    total_individuos = sum(Fact_pers, na.rm = TRUE),             # Ignorar NA en los cálculos
    tasa_presencia = ifelse(total_individuos == 0, NA, total_siempre / total_individuos)  # Evitar división por cero
  )

tasa_presencia_total <- datos %>%
  summarise(
    total_siempre = sum(trafico * Fact_pers, na.rm = TRUE),   # Total ponderado de consumidores
    total_individuos = sum(Fact_pers, na.rm = TRUE),            # Total ponderado de individuos
    tasa_presencia = ifelse(total_individuos == 0, NA, total_siempre / total_individuos )  # Calcular la tasa como porcentaje
  )

write_xlsx(
  list(
    "Tasa presencia total" = tasa_presencia_total,
    "Tasa presencia" = tasa_presencia
  ),
  "tasa_presencia.xlsx"
)

#2017

rm(list = ls())
setwd("C:/Users/cjara/OneDrive - Universidad Católica de Chile/Mercado drogas/ENUSC/2017")
datos <- read_sav("C:/Users/cjara/OneDrive - Universidad Católica de Chile/Mercado drogas/ENUSC/2017/bbdd.sav")

datos <- datos %>%
  mutate(trafico = case_when(
    P11_14_1 %in% 4:3 ~ 1,  # Recodificar valores 5 como 1
    P11_14_1 %in% 1:2 ~ 0 # Recodificar valores 1 a 4 como 0
  ))

table(datos$enc_region)

etiquetas <- get_labels(datos$enc_rpc)
valores <- get_values(datos$enc_rpc)

tabla_codigos_etiquetas <- data.frame(
  Codigo = valores,
  Etiqueta = etiquetas
)
print(tabla_codigos_etiquetas)
table(datos$enc_rpc)

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
    TRUE ~ NA_real_ 
    ))


datos <- datos %>%
  mutate(region = factor(region, 
                         levels = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16), 
                         labels = c("Tarapacá", "Antofagasta", "Atacama", "Coquimbo", "Valparaíso", 
                                    "Bernardo O'Higgins", "Maule", "Biobío", "Araucanía", 
                                    "Los Lagos", "Aysén", "Magallanes", "RM", "Los Ríos", 
                                    "Arica y Parinacota", "Ñuble")
  ))

table(datos$region)

tasa_presencia <- datos %>%
  group_by(region) %>%
  summarise(
    total_siempre = sum(trafico * Fact_pers, na.rm = TRUE),  # Ignorar NA en los cálculos
    total_individuos = sum(Fact_pers, na.rm = TRUE),             # Ignorar NA en los cálculos
    tasa_presencia = ifelse(total_individuos == 0, NA, total_siempre / total_individuos)  # Evitar división por cero
  )

tasa_presencia_total <- datos %>%
  summarise(
    total_siempre = sum(trafico * Fact_pers, na.rm = TRUE),   # Total ponderado de consumidores
    total_individuos = sum(Fact_pers, na.rm = TRUE),            # Total ponderado de individuos
    tasa_presencia = ifelse(total_individuos == 0, NA, total_siempre / total_individuos )  # Calcular la tasa como porcentaje
  )

write_xlsx(
  list(
    "Tasa presencia total" = tasa_presencia_total,
    "Tasa presencia" = tasa_presencia
  ),
  "tasa_presencia.xlsx"
)

#2016

rm(list = ls())
setwd("C:/Users/cjara/OneDrive - Universidad Católica de Chile/Mercado drogas/ENUSC/2016")
datos <- read_sav("C:/Users/cjara/OneDrive - Universidad Católica de Chile/Mercado drogas/ENUSC/2016/bbdd.sav")

datos <- datos %>%
  mutate(trafico = case_when(
    P11_14_1 %in% 4:3 ~ 1,  # Recodificar valores 5 como 1
    P11_14_1 %in% 1:2 ~ 0 # Recodificar valores 1 a 4 como 0
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
  mutate(region = factor(region, 
                         levels = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16), 
                         labels = c("Tarapacá", "Antofagasta", "Atacama", "Coquimbo", "Valparaíso", 
                                    "Bernardo O'Higgins", "Maule", "Biobío", "Araucanía", 
                                    "Los Lagos", "Aysén", "Magallanes", "RM", "Los Ríos", 
                                    "Arica y Parinacota", "Ñuble")
  ))

tasa_presencia <- datos %>%
  group_by(region) %>%
  summarise(
    total_siempre = sum(trafico * Fact_pers, na.rm = TRUE),  # Ignorar NA en los cálculos
    total_individuos = sum(Fact_pers, na.rm = TRUE),             # Ignorar NA en los cálculos
    tasa_presencia = ifelse(total_individuos == 0, NA, total_siempre / total_individuos)  # Evitar división por cero
  )

tasa_presencia_total <- datos %>%
  summarise(
    total_siempre = sum(trafico * Fact_pers, na.rm = TRUE),   # Total ponderado de consumidores
    total_individuos = sum(Fact_pers, na.rm = TRUE),            # Total ponderado de individuos
    tasa_presencia = ifelse(total_individuos == 0, NA, total_siempre / total_individuos )  # Calcular la tasa como porcentaje
  )

write_xlsx(
  list(
    "Tasa presencia total" = tasa_presencia_total,
    "Tasa presencia" = tasa_presencia
  ),
  "tasa_presencia.xlsx"
)

#2015

rm(list = ls())
setwd("C:/Users/cjara/OneDrive - Universidad Católica de Chile/Mercado drogas/ENUSC/2015")
datos <- read_sav("C:/Users/cjara/OneDrive - Universidad Católica de Chile/Mercado drogas/ENUSC/2015/bbdd.sav")

datos <- datos %>%
  mutate(trafico = case_when(
    P10_14_1 %in% 4:3 ~ 1,  # Recodificar valores 5 como 1
    P10_14_1 %in% 1:2 ~ 0 # Recodificar valores 1 a 4 como 0
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
  mutate(region = factor(region, 
                         levels = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16), 
                         labels = c("Tarapacá", "Antofagasta", "Atacama", "Coquimbo", "Valparaíso", 
                                    "Bernardo O'Higgins", "Maule", "Biobío", "Araucanía", 
                                    "Los Lagos", "Aysén", "Magallanes", "RM", "Los Ríos", 
                                    "Arica y Parinacota", "Ñuble")
  ))

tasa_presencia <- datos %>%
  group_by(region) %>%
  summarise(
    total_siempre = sum(trafico * Fact_pers_15reg_nuevo, na.rm = TRUE),  # Ignorar NA en los cálculos
    total_individuos = sum(Fact_pers_15reg_nuevo, na.rm = TRUE),             # Ignorar NA en los cálculos
    tasa_presencia = ifelse(total_individuos == 0, NA, total_siempre / total_individuos)  # Evitar división por cero
  )

tasa_presencia_total <- datos %>%
  summarise(
    total_siempre = sum(trafico * Fact_pers_15reg_nuevo, na.rm = TRUE),   # Total ponderado de consumidores
    total_individuos = sum(Fact_pers_15reg_nuevo, na.rm = TRUE),            # Total ponderado de individuos
    tasa_presencia = ifelse(total_individuos == 0, NA, total_siempre / total_individuos )  # Calcular la tasa como porcentaje
  )

write_xlsx(
  list(
    "Tasa presencia total" = tasa_presencia_total,
    "Tasa presencia" = tasa_presencia
  ),
  "tasa_presencia.xlsx"
)
