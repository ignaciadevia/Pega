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

table(datos$EDAD)
datos <- datos %>%
  rename(edad = EDAD)

datos <- datos %>%
  filter(edad < 18)

#MARIHUANA

datos <- datos %>%
  mutate(mari = case_when(
    MAR_4 %in% 1:2 ~ 1,  # Recodificar valores 1 o 2 como 1 (Año)
    MAR_4 == 3 ~ 0       # Recodificar valor 3 como 2 (Más de un año)
  ))

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

datos <- datos %>%
  mutate(coca = case_when(
    COC_4 %in% 1:2 ~ 1,  # Recodificar valores 1 o 2 como 1 (Año)
    COC_4 == 3 ~ 0       # Recodificar valor 3 como 2 (Más de un año)
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

datos <- datos %>%
  mutate(pasta = case_when(
    PB_4 %in% 1:2 ~ 1,  # Recodificar valores 1 o 2 como 1 (Año)
    PB_4 == 3 ~ 0       # Recodificar valor 3 como 2 (Más de un año)
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

datos <- datos %>%
  mutate(extasis = case_when(
    OD_2_3_Rp %in% 1:2 ~ 1,  # Recodificar valores 1 o 2 como 1 (Año)
    OD_2_3_Rp == 3 ~ 0       # Recodificar valor 3 como 2 (Más de un año)
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

#HEROINA

datos <- datos %>%
  mutate(heroina = case_when(
    OD_2_5_Rp %in% 1:2 ~ 1,  # Recodificar valores 1 o 2 como 1 (Año)
    OD_2_5_Rp == 3 ~ 0       # Recodificar valor 3 como 2 (Más de un año)
  ))

tasa_consumo_heroina <- datos %>%
  group_by(REGION) %>%
  summarise(
    total_consumidores = sum(heroina * FACTOR_EXPANSION, na.rm = TRUE),  # Ignorar NA en los cálculos
    total_individuos = sum(FACTOR_EXPANSION, na.rm = TRUE),             # Ignorar NA en los cálculos
    tasa_consumo = ifelse(total_individuos == 0, NA, total_consumidores / total_individuos)  # Evitar división por cero
  )

tasa_consumo_heroina_total <- datos %>%
  summarise(
    total_consumidores = sum(heroina * FACTOR_EXPANSION, na.rm = TRUE),   # Total ponderado de consumidores
    total_individuos = sum(FACTOR_EXPANSION, na.rm = TRUE),            # Total ponderado de individuos
    tasa_consumo = ifelse(total_individuos == 0, NA, total_consumidores / total_individuos )  # Calcular la tasa como porcentaje
  )

#CRACK

datos <- datos %>%
  mutate(crack = case_when(
    OD_2_7_Rp %in% 1:2 ~ 1,  # Recodificar valores 1 o 2 como 1 (Año)
    OD_2_7_Rp == 3 ~ 0       # Recodificar valor 3 como 2 (Más de un año)
  ))

tasa_consumo_crack <- datos %>%
  group_by(REGION) %>%
  summarise(
    total_consumidores = sum(crack * FACTOR_EXPANSION, na.rm = TRUE),  # Ignorar NA en los cálculos
    total_individuos = sum(FACTOR_EXPANSION, na.rm = TRUE),             # Ignorar NA en los cálculos
    tasa_consumo = ifelse(total_individuos == 0, NA, total_consumidores / total_individuos)  # Evitar división por cero
  )

tasa_consumo_crack_total <- datos %>%
  summarise(
    total_consumidores = sum(crack * FACTOR_EXPANSION, na.rm = TRUE),   # Total ponderado de consumidores
    total_individuos = sum(FACTOR_EXPANSION, na.rm = TRUE),            # Total ponderado de individuos
    tasa_consumo = ifelse(total_individuos == 0, NA, total_consumidores / total_individuos )  # Calcular la tasa como porcentaje
  )

#POPPER

datos <- datos %>%
  mutate(popper = case_when(
    OD_8_8_Rp %in% 1:2 ~ 1,  # Recodificar valores 1 o 2 como 1 (Año)
    OD_8_8_Rp == 3 ~ 0       # Recodificar valor 3 como 2 (Más de un año)
  ))

tasa_consumo_popper <- datos %>%
  group_by(REGION) %>%
  summarise(
    total_consumidores = sum(popper * FACTOR_EXPANSION, na.rm = TRUE),  # Ignorar NA en los cálculos
    total_individuos = sum(FACTOR_EXPANSION, na.rm = TRUE),             # Ignorar NA en los cálculos
    tasa_consumo = ifelse(total_individuos == 0, NA, total_consumidores / total_individuos)  # Evitar división por cero
  )

tasa_consumo_popper_total <- datos %>%
  summarise(
    total_consumidores = sum(popper * FACTOR_EXPANSION, na.rm = TRUE),   # Total ponderado de consumidores
    total_individuos = sum(FACTOR_EXPANSION, na.rm = TRUE),            # Total ponderado de individuos
    tasa_consumo = ifelse(total_individuos == 0, NA, total_consumidores / total_individuos )  # Calcular la tasa como porcentaje
  )

#LSD

datos <- datos %>%
  mutate(lsd = case_when(
    OD_10_1_Rp %in% 1:2 ~ 1,  # Recodificar valores 1 o 2 como 1 (Año)
    OD_10_1_Rp == 3 ~ 0       # Recodificar valor 3 como 2 (Más de un año)
  ))

tasa_consumo_lsd <- datos %>%
  group_by(REGION) %>%
  summarise(
    total_consumidores = sum(lsd * FACTOR_EXPANSION, na.rm = TRUE),  # Ignorar NA en los cálculos
    total_individuos = sum(FACTOR_EXPANSION, na.rm = TRUE),             # Ignorar NA en los cálculos
    tasa_consumo = ifelse(total_individuos == 0, NA, total_consumidores / total_individuos)  # Evitar división por cero
  )

tasa_consumo_lsd_total <- datos %>%
  summarise(
    total_consumidores = sum(lsd * FACTOR_EXPANSION, na.rm = TRUE),   # Total ponderado de consumidores
    total_individuos = sum(FACTOR_EXPANSION, na.rm = TRUE),            # Total ponderado de individuos
    tasa_consumo = ifelse(total_individuos == 0, NA, total_consumidores / total_individuos )  # Calcular la tasa como porcentaje
  )

#TUSI

datos <- datos %>%
  mutate(tusi = case_when(
    OD_10_4_Rp %in% 1:2 ~ 1,  # Recodificar valores 1 o 2 como 1 (Año)
    OD_10_4_Rp == 3 ~ 0       # Recodificar valor 3 como 2 (Más de un año)
  ))

tasa_consumo_tusi <- datos %>%
  group_by(REGION) %>%
  summarise(
    total_consumidores = sum(tusi * FACTOR_EXPANSION, na.rm = TRUE),  # Ignorar NA en los cálculos
    total_individuos = sum(FACTOR_EXPANSION, na.rm = TRUE),             # Ignorar NA en los cálculos
    tasa_consumo = ifelse(total_individuos == 0, NA, total_consumidores / total_individuos)  # Evitar división por cero
  )

tasa_consumo_tusi_total <- datos %>%
  summarise(
    total_consumidores = sum(tusi * FACTOR_EXPANSION, na.rm = TRUE),   # Total ponderado de consumidores
    total_individuos = sum(FACTOR_EXPANSION, na.rm = TRUE),            # Total ponderado de individuos
    tasa_consumo = ifelse(total_individuos == 0, NA, total_consumidores / total_individuos )  # Calcular la tasa como porcentaje
  )

#HONGOS

datos <- datos %>%
  mutate(hongos = case_when(
    OD_10_7_Rp %in% 1:2 ~ 1,  # Recodificar valores 1 o 2 como 1 (Año)
    OD_10_7_Rp == 3 ~ 0       # Recodificar valor 3 como 2 (Más de un año)
  ))

tasa_consumo_hongos <- datos %>%
  group_by(REGION) %>%
  summarise(
    total_consumidores = sum(hongos * FACTOR_EXPANSION, na.rm = TRUE),  # Ignorar NA en los cálculos
    total_individuos = sum(FACTOR_EXPANSION, na.rm = TRUE),             # Ignorar NA en los cálculos
    tasa_consumo = ifelse(total_individuos == 0, NA, total_consumidores / total_individuos)  # Evitar división por cero
  )

tasa_consumo_hongos_total <- datos %>%
  summarise(
    total_consumidores = sum(hongos * FACTOR_EXPANSION, na.rm = TRUE),   # Total ponderado de consumidores
    total_individuos = sum(FACTOR_EXPANSION, na.rm = TRUE),            # Total ponderado de individuos
    tasa_consumo = ifelse(total_individuos == 0, NA, total_consumidores / total_individuos )  # Calcular la tasa como porcentaje
  )

#SAN PEDRO

datos <- datos %>%
  mutate(sanpedro = case_when(
    OD_10_5_Rp %in% 1:2 ~ 1,  # Recodificar valores 1 o 2 como 1 (Año)
    OD_10_5_Rp == 3 ~ 0       # Recodificar valor 3 como 2 (Más de un año)
  ))

tasa_consumo_sanpedro <- datos %>%
  group_by(REGION) %>%
  summarise(
    total_consumidores = sum(sanpedro * FACTOR_EXPANSION, na.rm = TRUE),  # Ignorar NA en los cálculos
    total_individuos = sum(FACTOR_EXPANSION, na.rm = TRUE),             # Ignorar NA en los cálculos
    tasa_consumo = ifelse(total_individuos == 0, NA, total_consumidores / total_individuos)  # Evitar división por cero
  )

tasa_consumo_sanpedro_total <- datos %>%
  summarise(
    total_consumidores = sum(sanpedro * FACTOR_EXPANSION, na.rm = TRUE),   # Total ponderado de consumidores
    total_individuos = sum(FACTOR_EXPANSION, na.rm = TRUE),            # Total ponderado de individuos
    tasa_consumo = ifelse(total_individuos == 0, NA, total_consumidores / total_individuos )  # Calcular la tasa como porcentaje
  )

#POLVO DE ÁNGEL

datos <- datos %>%
  mutate(polvo = case_when(
    OD_10_2_Rp %in% 1:2 ~ 1,  # Recodificar valores 1 o 2 como 1 (Año)
    OD_10_2_Rp == 3 ~ 0       # Recodificar valor 3 como 2 (Más de un año)
  ))

tasa_consumo_polvo <- datos %>%
  group_by(REGION) %>%
  summarise(
    total_consumidores = sum(polvo * FACTOR_EXPANSION, na.rm = TRUE),  # Ignorar NA en los cálculos
    total_individuos = sum(FACTOR_EXPANSION, na.rm = TRUE),             # Ignorar NA en los cálculos
    tasa_consumo = ifelse(total_individuos == 0, NA, total_consumidores / total_individuos)  # Evitar división por cero
  )

tasa_consumo_polvo_total <- datos %>%
  summarise(
    total_consumidores = sum(polvo * FACTOR_EXPANSION, na.rm = TRUE),   # Total ponderado de consumidores
    total_individuos = sum(FACTOR_EXPANSION, na.rm = TRUE),            # Total ponderado de individuos
    tasa_consumo = ifelse(total_individuos == 0, NA, total_consumidores / total_individuos )  # Calcular la tasa como porcentaje
  )

#TRANQUILIZANTES

datos <- datos %>% 
  mutate(T_TRANS_2_1 = ifelse(T_TRANS_2_1 %in% c(1, 2), 1, 0),
         T_TRANS_2_2 = ifelse(T_TRANS_2_2 %in% c(1, 2), 1, 0),
         T_TRANS_2_3 = ifelse(T_TRANS_2_3 %in% c(1, 2), 1, 0),
         T_TRANS_2_4 = ifelse(T_TRANS_2_4 %in% c(1, 2), 1, 0),
         T_TRANS_2_5 = ifelse(T_TRANS_2_5 %in% c(1, 2), 1, 0),
         T_TRANS_2_6 = ifelse(T_TRANS_2_6 %in% c(1, 2), 1, 0),
         T_TRANS_2_7 = ifelse(T_TRANS_2_7 %in% c(1, 2), 1, 0),
         T_TRANS_2_8 = ifelse(T_TRANS_2_8 %in% c(1, 2), 1, 0),
         T_TRANS_2_9 = ifelse(T_TRANS_2_9 %in% c(1, 2), 1, 0)) %>%
  mutate(tranquilizantes = case_when(
    T_TRANS_2_1 == 1 | T_TRANS_2_2 == 1 | T_TRANS_2_3 == 1 | T_TRANS_2_4 == 1 | 
      T_TRANS_2_5 == 1 | T_TRANS_2_6 == 1 | T_TRANS_2_7 == 1 | T_TRANS_2_8 == 1 | T_TRANS_2_9 == 1 ~ 1,
    TRUE ~ 0
  ))


tasa_consumo_tranquilizantes <- datos %>%
  group_by(REGION) %>%
  summarise(
    total_consumidores = sum(tranquilizantes * FACTOR_EXPANSION, na.rm = TRUE),  # Ignorar NA en los cálculos
    total_individuos = sum(FACTOR_EXPANSION, na.rm = TRUE),             # Ignorar NA en los cálculos
    tasa_consumo = ifelse(total_individuos == 0, NA, total_consumidores / total_individuos)  # Evitar división por cero
  )

tasa_consumo_tranquilizantes_total <- datos %>%
  summarise(
    total_consumidores = sum(tranquilizantes * FACTOR_EXPANSION, na.rm = TRUE),   # Total ponderado de consumidores
    total_individuos = sum(FACTOR_EXPANSION, na.rm = TRUE),            # Total ponderado de individuos
    tasa_consumo = ifelse(total_individuos == 0, NA, total_consumidores / total_individuos )  # Calcular la tasa como porcentaje
  )

#ESTIMULANTES

datos <- datos %>% 
  mutate(OD_4_1_Rp = ifelse(OD_4_1_Rp %in% c(1, 2), 1, 0),
         OD_4_2_Rp = ifelse(OD_4_2_Rp %in% c(1, 2), 1, 0),
         OD_4_3_Rp = ifelse(OD_4_3_Rp %in% c(1, 2), 1, 0),
         OD_4_4_Rp = ifelse(OD_4_4_Rp %in% c(1, 2), 1, 0),
         OD_4_6_Rp = ifelse(OD_4_6_Rp %in% c(1, 2), 1, 0),
         OD_4_7_Rp = ifelse(OD_4_7_Rp %in% c(1, 2), 1, 0),
         OD_4_5_Rp = ifelse(OD_4_5_Rp %in% c(1, 2), 1, 0)) %>%
  mutate(estimulantes = case_when(
    OD_4_1_Rp == 1 | OD_4_2_Rp == 1 | OD_4_3_Rp == 1 | OD_4_4_Rp == 1 | 
      OD_4_5_Rp == 1 | OD_4_6_Rp == 1 | OD_4_7_Rp == 1 ~ 1,
    TRUE ~ 0
  ))


tasa_consumo_estimulantes <- datos %>%
  group_by(REGION) %>%
  summarise(
    total_consumidores = sum(estimulantes * FACTOR_EXPANSION, na.rm = TRUE),  # Ignorar NA en los cálculos
    total_individuos = sum(FACTOR_EXPANSION, na.rm = TRUE),             # Ignorar NA en los cálculos
    tasa_consumo = ifelse(total_individuos == 0, NA, total_consumidores / total_individuos)  # Evitar división por cero
  )

tasa_consumo_estimulantes_total <- datos %>%
  summarise(
    total_consumidores = sum(estimulantes * FACTOR_EXPANSION, na.rm = TRUE),   # Total ponderado de consumidores
    total_individuos = sum(FACTOR_EXPANSION, na.rm = TRUE),            # Total ponderado de individuos
    tasa_consumo = ifelse(total_individuos == 0, NA, total_consumidores / total_individuos )  # Calcular la tasa como porcentaje
  )

# COCAINAS
datos <- datos %>%
  mutate(cocainas = if_else(coca == 1 | pasta == 1 | crack == 1, 1, 0))
table(datos$cocainas)

tasa_cocainas <- datos %>%
  group_by(REGION) %>%
  summarise(
    total_consumidores = sum(cocainas * FACTOR_EXPANSION, na.rm = TRUE),  # Ignorar NA en los cálculos
    total_individuos = sum(FACTOR_EXPANSION, na.rm = TRUE),             # Ignorar NA en los cálculos
    tasa_consumo = ifelse(total_individuos == 0, NA, total_consumidores / total_individuos)  # Evitar división por cero
  )

tasa_cocainas_total <- datos %>%
  summarise(
    total_consumidores = sum(cocainas * FACTOR_EXPANSION, na.rm = TRUE),   # Total ponderado de consumidores
    total_individuos = sum(FACTOR_EXPANSION, na.rm = TRUE),            # Total ponderado de individuos
    tasa_consumo = ifelse(total_individuos == 0, NA, total_consumidores / total_individuos )  # Calcular la tasa como porcentaje
  )


# ALUCINOGENOS

datos <- datos %>%
  mutate(alucinogenos = if_else(lsd == 1 | hongos == 1 | tusi == 1 | sanpedro == 1 |polvo == 1 , 1, 0))
table(datos$alucinogenos)

tasa_alucinogenos <- datos %>%
  group_by(REGION) %>%
  summarise(
    total_consumidores = sum(alucinogenos * FACTOR_EXPANSION, na.rm = TRUE),  # Ignorar NA en los cálculos
    total_individuos = sum(FACTOR_EXPANSION, na.rm = TRUE),             # Ignorar NA en los cálculos
    tasa_consumo = ifelse(total_individuos == 0, NA, total_consumidores / total_individuos)  # Evitar división por cero
  )

tasa_alucinogenos_total <- datos %>%
  summarise(
    total_consumidores = sum(alucinogenos * FACTOR_EXPANSION, na.rm = TRUE),   # Total ponderado de consumidores
    total_individuos = sum(FACTOR_EXPANSION, na.rm = TRUE),            # Total ponderado de individuos
    tasa_consumo = ifelse(total_individuos == 0, NA, total_consumidores / total_individuos )  # Calcular la tasa como porcentaje
  )

# OTRAS

datos <- datos %>%
  mutate(otras = if_else(lsd == 1 | hongos == 1 | tusi == 1 | sanpedro == 1 |polvo == 1 | coca == 1 | pasta == 1 | crack == 1 | extasis == 1 | heroina == 1 | popper == 1 , 1, 0))
table(datos$otras)

tasa_otras <- datos %>%
  group_by(REGION) %>%
  summarise(
    total_consumidores = sum(otras * FACTOR_EXPANSION, na.rm = TRUE),  # Ignorar NA en los cálculos
    total_individuos = sum(FACTOR_EXPANSION, na.rm = TRUE),             # Ignorar NA en los cálculos
    tasa_consumo = ifelse(total_individuos == 0, NA, total_consumidores / total_individuos)  # Evitar división por cero
  )

tasa_otras_total <- datos %>%
  summarise(
    total_consumidores = sum(otras * FACTOR_EXPANSION, na.rm = TRUE),   # Total ponderado de consumidores
    total_individuos = sum(FACTOR_EXPANSION, na.rm = TRUE),            # Total ponderado de individuos
    tasa_consumo = ifelse(total_individuos == 0, NA, total_consumidores / total_individuos )  # Calcular la tasa como porcentaje
  )

#EXCEL 

write_xlsx(
  list(
    "Tasa mari" = tasa_consumo_mari,
    "Tasa mari total" = tasa_consumo_mari_total,
    "Tasa coca" = tasa_consumo_coca,
    "Tasa coca total" = tasa_consumo_coca_total,
    "Tasa pasta" = tasa_consumo_pasta,
    "Tasa pasta total" = tasa_consumo_pasta_total,
    "Tasa extasis" = tasa_consumo_extasis,
    "Tasa extasis total" = tasa_consumo_extasis_total,
    "Tasa heroina" = tasa_consumo_heroina,
    "Tasa heroina total" = tasa_consumo_heroina_total,
    "Tasa crack" = tasa_consumo_crack,
    "Tasa crack total" = tasa_consumo_crack_total,
    "Tasa popper" = tasa_consumo_popper,
    "Tasa popper total" = tasa_consumo_popper_total,
    "Tasa lsd" = tasa_consumo_lsd,
    "Tasa lsd total" = tasa_consumo_lsd_total,
    "Tasa tusi" = tasa_consumo_tusi,
    "Tasa tusi total" = tasa_consumo_tusi_total,
    "Tasa hongos" = tasa_consumo_hongos,
    "Tasa hongos total" = tasa_consumo_hongos_total,
    "Tasa san pedro" = tasa_consumo_sanpedro,
    "Tasa san pedro total" = tasa_consumo_sanpedro_total,
    "Tasa polvo de angel" = tasa_consumo_polvo,
    "Tasa polvo de angel total" = tasa_consumo_polvo_total,
    "Tasa tranquilizantes sr" = tasa_consumo_tranquilizantes,
    "Tasa tranquilizantes total" = tasa_consumo_tranquilizantes_total,
    "Tasa estimulantes sr" = tasa_consumo_estimulantes,
    "Tasa estimulantes total" = tasa_consumo_estimulantes_total,
    "Tasa cocainas" = tasa_cocainas,
    "Tasa cocainas total" = tasa_cocainas_total,
    "Tasa alucinogenos" = tasa_alucinogenos,
    "Tasa alucinogenos total" = tasa_alucinogenos_total,
    "Tasa otras" = tasa_otras,
    "Tasa_otras total" = tasa_otras_total
  ),
  "tasa_consumo2.xlsx"
)

#2020
rm(list = ls())
setwd("C:/Users/cjara/OneDrive - Universidad Católica de Chile/Mercado drogas/SENDA/2020")
datos <- read_dta("C:/Users/cjara/OneDrive - Universidad Católica de Chile/Mercado drogas/SENDA/2020/BBDD.dta")

table(datos$S02)
datos <- datos %>%
  rename(edad = S02)

datos <- datos %>%
  filter(edad < 18)

table(datos$REGION)

#MARIHUANA

datos <- datos %>%
  mutate(mari = case_when(
    MAR_4 %in% 1:2 ~ 1,  # Recodificar valores 1 o 2 como 1 (Año)
    MAR_4 == 3 ~ 0       # Recodificar valor 3 como 2 (Más de un año)
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
    COC_4 %in% 1:2 ~ 1,  # Recodificar valores 1 o 2 como 1 (Año)
    COC_4 == 3 ~ 0       # Recodificar valor 3 como 2 (Más de un año)
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
    PB_4 %in% 1:2 ~ 1,  # Recodificar valores 1 o 2 como 1 (Año)
    PB_4 == 3 ~ 0       # Recodificar valor 3 como 2 (Más de un año)
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
    T_OD_2_3 %in% 1:2 ~ 1,  # Recodificar valores 1 o 2 como 1 (Año)
    T_OD_2_3 == 3 ~ 0       # Recodificar valor 3 como 2 (Más de un año)
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

#HEROINA

datos <- datos %>%
  mutate(heroina = case_when(
    T_OD_2_5 %in% 1:2 ~ 1,  # Recodificar valores 1 o 2 como 1 (Año)
    T_OD_2_5 == 3 ~ 0       # Recodificar valor 3 como 2 (Más de un año)
  ))

tasa_consumo_heroina <- datos %>%
  group_by(REGION) %>%
  summarise(
    total_consumidores = sum(heroina * FACT_PERS_COMUNA, na.rm = TRUE),  # Ignorar NA en los cálculos
    total_individuos = sum(FACT_PERS_COMUNA, na.rm = TRUE),             # Ignorar NA en los cálculos
    tasa_consumo = ifelse(total_individuos == 0, NA, total_consumidores / total_individuos)  # Evitar división por cero
  )

tasa_consumo_heroina_total <- datos %>%
  summarise(
    total_consumidores = sum(heroina * FACT_PERS_COMUNA, na.rm = TRUE),   # Total ponderado de consumidores
    total_individuos = sum(FACT_PERS_COMUNA, na.rm = TRUE),            # Total ponderado de individuos
    tasa_consumo = ifelse(total_individuos == 0, NA, total_consumidores / total_individuos )  # Calcular la tasa como porcentaje
  )

#CRACK

datos <- datos %>%
  mutate(crack = case_when(
    T_OD_2_7 %in% 1:2 ~ 1,  # Recodificar valores 1 o 2 como 1 (Año)
    T_OD_2_7 == 3 ~ 0       # Recodificar valor 3 como 2 (Más de un año)
  ))

tasa_consumo_crack <- datos %>%
  group_by(REGION) %>%
  summarise(
    total_consumidores = sum(crack * FACT_PERS_COMUNA, na.rm = TRUE),  # Ignorar NA en los cálculos
    total_individuos = sum(FACT_PERS_COMUNA, na.rm = TRUE),             # Ignorar NA en los cálculos
    tasa_consumo = ifelse(total_individuos == 0, NA, total_consumidores / total_individuos)  # Evitar división por cero
  )

tasa_consumo_crack_total <- datos %>%
  summarise(
    total_consumidores = sum(crack * FACT_PERS_COMUNA, na.rm = TRUE),   # Total ponderado de consumidores
    total_individuos = sum(FACT_PERS_COMUNA, na.rm = TRUE),            # Total ponderado de individuos
    tasa_consumo = ifelse(total_individuos == 0, NA, total_consumidores / total_individuos )  # Calcular la tasa como porcentaje
  )

#POPPER

datos <- datos %>%
  mutate(popper = case_when(
    T_OD_8_8 %in% 1:2 ~ 1,  # Recodificar valores 1 o 2 como 1 (Año)
    T_OD_8_8 == 3 ~ 0       # Recodificar valor 3 como 2 (Más de un año)
  ))

tasa_consumo_popper <- datos %>%
  group_by(REGION) %>%
  summarise(
    total_consumidores = sum(popper * FACT_PERS_COMUNA, na.rm = TRUE),  # Ignorar NA en los cálculos
    total_individuos = sum(FACT_PERS_COMUNA, na.rm = TRUE),             # Ignorar NA en los cálculos
    tasa_consumo = ifelse(total_individuos == 0, NA, total_consumidores / total_individuos)  # Evitar división por cero
  )

tasa_consumo_popper_total <- datos %>%
  summarise(
    total_consumidores = sum(popper * FACT_PERS_COMUNA, na.rm = TRUE),   # Total ponderado de consumidores
    total_individuos = sum(FACT_PERS_COMUNA, na.rm = TRUE),            # Total ponderado de individuos
    tasa_consumo = ifelse(total_individuos == 0, NA, total_consumidores / total_individuos )  # Calcular la tasa como porcentaje
  )

#LSD

datos <- datos %>%
  mutate(lsd = case_when(
    T_OD_10_1 %in% 1:2 ~ 1,  # Recodificar valores 1 o 2 como 1 (Año)
    T_OD_10_1 == 3 ~ 0       # Recodificar valor 3 como 2 (Más de un año)
  ))

tasa_consumo_lsd <- datos %>%
  group_by(REGION) %>%
  summarise(
    total_consumidores = sum(lsd * FACT_PERS_COMUNA, na.rm = TRUE),  # Ignorar NA en los cálculos
    total_individuos = sum(FACT_PERS_COMUNA, na.rm = TRUE),             # Ignorar NA en los cálculos
    tasa_consumo = ifelse(total_individuos == 0, NA, total_consumidores / total_individuos)  # Evitar división por cero
  )

tasa_consumo_lsd_total <- datos %>%
  summarise(
    total_consumidores = sum(lsd * FACT_PERS_COMUNA, na.rm = TRUE),   # Total ponderado de consumidores
    total_individuos = sum(FACT_PERS_COMUNA, na.rm = TRUE),            # Total ponderado de individuos
    tasa_consumo = ifelse(total_individuos == 0, NA, total_consumidores / total_individuos )  # Calcular la tasa como porcentaje
  )

#TUSI

datos <- datos %>%
  mutate(tusi = case_when(
    T_OD_10_4 %in% 1:2 ~ 1,  # Recodificar valores 1 o 2 como 1 (Año)
    T_OD_10_4 == 3 ~ 0       # Recodificar valor 3 como 2 (Más de un año)
  ))

tasa_consumo_tusi <- datos %>%
  group_by(REGION) %>%
  summarise(
    total_consumidores = sum(tusi * FACT_PERS_COMUNA, na.rm = TRUE),  # Ignorar NA en los cálculos
    total_individuos = sum(FACT_PERS_COMUNA, na.rm = TRUE),             # Ignorar NA en los cálculos
    tasa_consumo = ifelse(total_individuos == 0, NA, total_consumidores / total_individuos)  # Evitar división por cero
  )

tasa_consumo_tusi_total <- datos %>%
  summarise(
    total_consumidores = sum(tusi * FACT_PERS_COMUNA, na.rm = TRUE),   # Total ponderado de consumidores
    total_individuos = sum(FACT_PERS_COMUNA, na.rm = TRUE),            # Total ponderado de individuos
    tasa_consumo = ifelse(total_individuos == 0, NA, total_consumidores / total_individuos )  # Calcular la tasa como porcentaje
  )

#HONGOS

datos <- datos %>%
  mutate(hongos = case_when(
    T_OD_10_7 %in% 1:2 ~ 1,  # Recodificar valores 1 o 2 como 1 (Año)
    T_OD_10_7 == 3 ~ 0       # Recodificar valor 3 como 2 (Más de un año)
  ))

tasa_consumo_hongos <- datos %>%
  group_by(REGION) %>%
  summarise(
    total_consumidores = sum(hongos * FACT_PERS_COMUNA, na.rm = TRUE),  # Ignorar NA en los cálculos
    total_individuos = sum(FACT_PERS_COMUNA, na.rm = TRUE),             # Ignorar NA en los cálculos
    tasa_consumo = ifelse(total_individuos == 0, NA, total_consumidores / total_individuos)  # Evitar división por cero
  )

tasa_consumo_hongos_total <- datos %>%
  summarise(
    total_consumidores = sum(hongos * FACT_PERS_COMUNA, na.rm = TRUE),   # Total ponderado de consumidores
    total_individuos = sum(FACT_PERS_COMUNA, na.rm = TRUE),            # Total ponderado de individuos
    tasa_consumo = ifelse(total_individuos == 0, NA, total_consumidores / total_individuos )  # Calcular la tasa como porcentaje
  )

#SAN PEDRO

datos <- datos %>%
  mutate(sanpedro = case_when(
    T_OD_10_5 %in% 1:2 ~ 1,  # Recodificar valores 1 o 2 como 1 (Año)
    T_OD_10_5 == 3 ~ 0       # Recodificar valor 3 como 2 (Más de un año)
  ))

tasa_consumo_sanpedro <- datos %>%
  group_by(REGION) %>%
  summarise(
    total_consumidores = sum(sanpedro * FACT_PERS_COMUNA, na.rm = TRUE),  # Ignorar NA en los cálculos
    total_individuos = sum(FACT_PERS_COMUNA, na.rm = TRUE),             # Ignorar NA en los cálculos
    tasa_consumo = ifelse(total_individuos == 0, NA, total_consumidores / total_individuos)  # Evitar división por cero
  )

tasa_consumo_sanpedro_total <- datos %>%
  summarise(
    total_consumidores = sum(sanpedro * FACT_PERS_COMUNA, na.rm = TRUE),   # Total ponderado de consumidores
    total_individuos = sum(FACT_PERS_COMUNA, na.rm = TRUE),            # Total ponderado de individuos
    tasa_consumo = ifelse(total_individuos == 0, NA, total_consumidores / total_individuos )  # Calcular la tasa como porcentaje
  )

#POLVO DE ÁNGEL

datos <- datos %>%
  mutate(polvo = case_when(
    T_OD_10_2 %in% 1:2 ~ 1,  # Recodificar valores 1 o 2 como 1 (Año)
    T_OD_10_2 == 3 ~ 0       # Recodificar valor 3 como 2 (Más de un año)
  ))

tasa_consumo_polvo <- datos %>%
  group_by(REGION) %>%
  summarise(
    total_consumidores = sum(polvo * FACT_PERS_COMUNA, na.rm = TRUE),  # Ignorar NA en los cálculos
    total_individuos = sum(FACT_PERS_COMUNA, na.rm = TRUE),             # Ignorar NA en los cálculos
    tasa_consumo = ifelse(total_individuos == 0, NA, total_consumidores / total_individuos)  # Evitar división por cero
  )

tasa_consumo_polvo_total <- datos %>%
  summarise(
    total_consumidores = sum(polvo * FACT_PERS_COMUNA, na.rm = TRUE),   # Total ponderado de consumidores
    total_individuos = sum(FACT_PERS_COMUNA, na.rm = TRUE),            # Total ponderado de individuos
    tasa_consumo = ifelse(total_individuos == 0, NA, total_consumidores / total_individuos )  # Calcular la tasa como porcentaje
  )

#TRANQUILIZANTES

datos <- datos %>% 
  mutate(T_TRANS_2_1 = ifelse(T_TRANS_2_1 %in% c(1, 2), 1, 0),
         T_TRANS_2_2 = ifelse(T_TRANS_2_2 %in% c(1, 2), 1, 0),
         T_TRANS_2_3 = ifelse(T_TRANS_2_3 %in% c(1, 2), 1, 0),
         T_TRANS_2_4 = ifelse(T_TRANS_2_4 %in% c(1, 2), 1, 0),
         T_TRANS_2_5 = ifelse(T_TRANS_2_5 %in% c(1, 2), 1, 0),
         T_TRANS_2_6 = ifelse(T_TRANS_2_6 %in% c(1, 2), 1, 0),
         T_TRANS_2_7 = ifelse(T_TRANS_2_7 %in% c(1, 2), 1, 0),
         T_TRANS_2_8 = ifelse(T_TRANS_2_8 %in% c(1, 2), 1, 0),
         T_TRANS_2_9 = ifelse(T_TRANS_2_9 %in% c(1, 2), 1, 0)) %>%
  mutate(tranquilizantes = case_when(
    T_TRANS_2_1 == 1 | T_TRANS_2_2 == 1 | T_TRANS_2_3 == 1 | T_TRANS_2_4 == 1 | 
      T_TRANS_2_5 == 1 | T_TRANS_2_6 == 1 | T_TRANS_2_7 == 1 | T_TRANS_2_8 == 1 | T_TRANS_2_9 == 1 ~ 1,
    TRUE ~ 0
  ))


tasa_consumo_tranquilizantes <- datos %>%
  group_by(REGION) %>%
  summarise(
    total_consumidores = sum(tranquilizantes * FACT_PERS_COMUNA, na.rm = TRUE),  # Ignorar NA en los cálculos
    total_individuos = sum(FACT_PERS_COMUNA, na.rm = TRUE),             # Ignorar NA en los cálculos
    tasa_consumo = ifelse(total_individuos == 0, NA, total_consumidores / total_individuos)  # Evitar división por cero
  )

tasa_consumo_tranquilizantes_total <- datos %>%
  summarise(
    total_consumidores = sum(tranquilizantes * FACT_PERS_COMUNA, na.rm = TRUE),   # Total ponderado de consumidores
    total_individuos = sum(FACT_PERS_COMUNA, na.rm = TRUE),            # Total ponderado de individuos
    tasa_consumo = ifelse(total_individuos == 0, NA, total_consumidores / total_individuos )  # Calcular la tasa como porcentaje
  )

#ESTIMULANTES

datos <- datos %>% 
  mutate(T_OD_4_1 = ifelse(T_OD_4_1 %in% c(1, 2), 1, 0),
         T_OD_4_2 = ifelse(T_OD_4_2 %in% c(1, 2), 1, 0),
         T_OD_4_3 = ifelse(T_OD_4_3 %in% c(1, 2), 1, 0),
         T_OD_4_4 = ifelse(T_OD_4_4 %in% c(1, 2), 1, 0),
         T_OD_4_6 = ifelse(T_OD_4_6 %in% c(1, 2), 1, 0),
         T_OD_4_7 = ifelse(T_OD_4_7 %in% c(1, 2), 1, 0),
         T_OD_4_5 = ifelse(T_OD_4_5 %in% c(1, 2), 1, 0)) %>%
  mutate(estimulantes = case_when(
    T_OD_4_1 == 1 | T_OD_4_2 == 1 | T_OD_4_3 == 1 | T_OD_4_4 == 1 | 
      T_OD_4_5 == 1 | T_OD_4_6 == 1 | T_OD_4_7 == 1 ~ 1,
    TRUE ~ 0
  ))


tasa_consumo_estimulantes <- datos %>%
  group_by(REGION) %>%
  summarise(
    total_consumidores = sum(estimulantes * FACT_PERS_COMUNA, na.rm = TRUE),  # Ignorar NA en los cálculos
    total_individuos = sum(FACT_PERS_COMUNA, na.rm = TRUE),             # Ignorar NA en los cálculos
    tasa_consumo = ifelse(total_individuos == 0, NA, total_consumidores / total_individuos)  # Evitar división por cero
  )

tasa_consumo_estimulantes_total <- datos %>%
  summarise(
    total_consumidores = sum(estimulantes * FACT_PERS_COMUNA, na.rm = TRUE),   # Total ponderado de consumidores
    total_individuos = sum(FACT_PERS_COMUNA, na.rm = TRUE),            # Total ponderado de individuos
    tasa_consumo = ifelse(total_individuos == 0, NA, total_consumidores / total_individuos )  # Calcular la tasa como porcentaje
  )

# COCAINAS
datos <- datos %>%
  mutate(cocainas = if_else(coca == 1 | pasta == 1 | crack == 1, 1, 0))
table(datos$cocainas)

tasa_cocainas <- datos %>%
  group_by(REGION) %>%
  summarise(
    total_consumidores = sum(cocainas * FACT_PERS_COMUNA, na.rm = TRUE),  # Ignorar NA en los cálculos
    total_individuos = sum(FACT_PERS_COMUNA, na.rm = TRUE),             # Ignorar NA en los cálculos
    tasa_consumo = ifelse(total_individuos == 0, NA, total_consumidores / total_individuos)  # Evitar división por cero
  )

tasa_cocainas_total <- datos %>%
  summarise(
    total_consumidores = sum(cocainas * FACT_PERS_COMUNA, na.rm = TRUE),   # Total ponderado de consumidores
    total_individuos = sum(FACT_PERS_COMUNA, na.rm = TRUE),            # Total ponderado de individuos
    tasa_consumo = ifelse(total_individuos == 0, NA, total_consumidores / total_individuos )  # Calcular la tasa como porcentaje
  )


# ALUCINOGENOS

datos <- datos %>%
  mutate(alucinogenos = if_else(lsd == 1 | hongos == 1 | tusi == 1 | sanpedro == 1 |polvo == 1 , 1, 0))
table(datos$alucinogenos)

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

# OTRAS

datos <- datos %>%
  mutate(otras = if_else(lsd == 1 | hongos == 1 | tusi == 1 | sanpedro == 1 |polvo == 1 | coca == 1 | pasta == 1 | crack == 1 | extasis == 1 | heroina == 1 | popper == 1 , 1, 0))
table(datos$otras)

tasa_otras <- datos %>%
  group_by(REGION) %>%
  summarise(
    total_consumidores = sum(otras * FACT_PERS_COMUNA, na.rm = TRUE),  # Ignorar NA en los cálculos
    total_individuos = sum(FACT_PERS_COMUNA, na.rm = TRUE),             # Ignorar NA en los cálculos
    tasa_consumo = ifelse(total_individuos == 0, NA, total_consumidores / total_individuos)  # Evitar división por cero
  )

tasa_otras_total <- datos %>%
  summarise(
    total_consumidores = sum(otras * FACT_PERS_COMUNA, na.rm = TRUE),   # Total ponderado de consumidores
    total_individuos = sum(FACT_PERS_COMUNA, na.rm = TRUE),            # Total ponderado de individuos
    tasa_consumo = ifelse(total_individuos == 0, NA, total_consumidores / total_individuos )  # Calcular la tasa como porcentaje
  )

#EXCEL 

write_xlsx(
  list(
    "Tasa mari" = tasa_consumo_mari,
    "Tasa mari total" = tasa_consumo_mari_total,
    "Tasa coca" = tasa_consumo_coca,
    "Tasa coca total" = tasa_consumo_coca_total,
    "Tasa pasta" = tasa_consumo_pasta,
    "Tasa pasta total" = tasa_consumo_pasta_total,
    "Tasa extasis" = tasa_consumo_extasis,
    "Tasa extasis total" = tasa_consumo_extasis_total,
    "Tasa heroina" = tasa_consumo_heroina,
    "Tasa heroina total" = tasa_consumo_heroina_total,
    "Tasa crack" = tasa_consumo_crack,
    "Tasa crack total" = tasa_consumo_crack_total,
    "Tasa popper" = tasa_consumo_popper,
    "Tasa popper total" = tasa_consumo_popper_total,
    "Tasa lsd" = tasa_consumo_lsd,
    "Tasa lsd total" = tasa_consumo_lsd_total,
    "Tasa tusi" = tasa_consumo_tusi,
    "Tasa tusi total" = tasa_consumo_tusi_total,
    "Tasa hongos" = tasa_consumo_hongos,
    "Tasa hongos total" = tasa_consumo_hongos_total,
    "Tasa san pedro" = tasa_consumo_sanpedro,
    "Tasa san pedro total" = tasa_consumo_sanpedro_total,
    "Tasa polvo de angel" = tasa_consumo_polvo,
    "Tasa polvo de angel total" = tasa_consumo_polvo_total,
    "Tasa tranquilizantes sr" = tasa_consumo_tranquilizantes,
    "Tasa tranquilizantes total" = tasa_consumo_tranquilizantes_total,
    "Tasa estimulantes sr" = tasa_consumo_estimulantes,
    "Tasa estimulantes total" = tasa_consumo_estimulantes_total,
    "Tasa cocainas" = tasa_cocainas,
    "Tasa cocainas total" = tasa_cocainas_total,
    "Tasa alucinogenos" = tasa_alucinogenos,
    "Tasa alucinogenos total" = tasa_alucinogenos_total,
    "Tasa otras" = tasa_otras,
    "Tasa_otras total" = tasa_otras_total
  ),
  "tasa_consumo2.xlsx"
)

#2018

rm(list = ls())
setwd("C:/Users/cjara/OneDrive - Universidad Católica de Chile/Mercado drogas/SENDA/2018")
datos <- read_dta("C:/Users/cjara/OneDrive - Universidad Católica de Chile/Mercado drogas/SENDA/2018/BBDD.dta")

table(datos$S02)
datos <- datos %>%
  rename(edad = S02)

datos <- datos %>%
  filter(edad < 18)

table(datos$Region)

#MARIHUANA

datos <- datos %>%
  mutate(mari = case_when(
    MAR_4 %in% 1:2 ~ 1,  # Recodificar valores 1 o 2 como 1 (Año)
    MAR_4 == 3 ~ 0       # Recodificar valor 3 como 2 (Más de un año)
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
    COC_4 %in% 1:2 ~ 1,  # Recodificar valores 1 o 2 como 1 (Año)
    COC_4 == 3 ~ 0       # Recodificar valor 3 como 2 (Más de un año)
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
    PB_4 %in% 1:2 ~ 1,  # Recodificar valores 1 o 2 como 1 (Año)
    PB_4 == 3 ~ 0       # Recodificar valor 3 como 2 (Más de un año)
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
    T_OD_6_3 %in% 1:2 ~ 1,  # Recodificar valores 1 o 2 como 1 (Año)
    T_OD_6_3 == 3 ~ 0       # Recodificar valor 3 como 2 (Más de un año)
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

#HEROINA

datos <- datos %>%
  mutate(heroina = case_when(
    T_OD_6_5 %in% 1:2 ~ 1,  # Recodificar valores 1 o 2 como 1 (Año)
    T_OD_6_5 == 3 ~ 0       # Recodificar valor 3 como 2 (Más de un año)
  ))

tasa_consumo_heroina <- datos %>%
  group_by(Region) %>%
  summarise(
    total_consumidores = sum(heroina * Fexp, na.rm = TRUE),  # Ignorar NA en los cálculos
    total_individuos = sum(Fexp, na.rm = TRUE),             # Ignorar NA en los cálculos
    tasa_consumo = ifelse(total_individuos == 0, NA, total_consumidores / total_individuos)  # Evitar división por cero
  )

tasa_consumo_heroina_total <- datos %>%
  summarise(
    total_consumidores = sum(heroina * Fexp, na.rm = TRUE),   # Total ponderado de consumidores
    total_individuos = sum(Fexp, na.rm = TRUE),            # Total ponderado de individuos
    tasa_consumo = ifelse(total_individuos == 0, NA, total_consumidores / total_individuos )  # Calcular la tasa como porcentaje
  )

#CRACK

datos <- datos %>%
  mutate(crack = case_when(
    T_OD_6_7 %in% 1:2 ~ 1,  # Recodificar valores 1 o 2 como 1 (Año)
    T_OD_6_7 == 3 ~ 0       # Recodificar valor 3 como 2 (Más de un año)
  ))

tasa_consumo_crack <- datos %>%
  group_by(Region) %>%
  summarise(
    total_consumidores = sum(crack * Fexp, na.rm = TRUE),  # Ignorar NA en los cálculos
    total_individuos = sum(Fexp, na.rm = TRUE),             # Ignorar NA en los cálculos
    tasa_consumo = ifelse(total_individuos == 0, NA, total_consumidores / total_individuos)  # Evitar división por cero
  )

tasa_consumo_crack_total <- datos %>%
  summarise(
    total_consumidores = sum(crack * Fexp, na.rm = TRUE),   # Total ponderado de consumidores
    total_individuos = sum(Fexp, na.rm = TRUE),            # Total ponderado de individuos
    tasa_consumo = ifelse(total_individuos == 0, NA, total_consumidores / total_individuos )  # Calcular la tasa como porcentaje
  )

#POPPER

datos <- datos %>%
  mutate(popper = case_when(
    T_OD_12_8 %in% 1:2 ~ 1,  # Recodificar valores 1 o 2 como 1 (Año)
    T_OD_12_8 == 3 ~ 0       # Recodificar valor 3 como 2 (Más de un año)
  ))

tasa_consumo_popper <- datos %>%
  group_by(Region) %>%
  summarise(
    total_consumidores = sum(popper * Fexp, na.rm = TRUE),  # Ignorar NA en los cálculos
    total_individuos = sum(Fexp, na.rm = TRUE),             # Ignorar NA en los cálculos
    tasa_consumo = ifelse(total_individuos == 0, NA, total_consumidores / total_individuos)  # Evitar división por cero
  )

tasa_consumo_popper_total <- datos %>%
  summarise(
    total_consumidores = sum(popper * Fexp, na.rm = TRUE),   # Total ponderado de consumidores
    total_individuos = sum(Fexp, na.rm = TRUE),            # Total ponderado de individuos
    tasa_consumo = ifelse(total_individuos == 0, NA, total_consumidores / total_individuos )  # Calcular la tasa como porcentaje
  )

#LSD

datos <- datos %>%
  mutate(lsd = case_when(
    T_OD_14_1 %in% 1:2 ~ 1,  # Recodificar valores 1 o 2 como 1 (Año)
    T_OD_14_1 == 3 ~ 0       # Recodificar valor 3 como 2 (Más de un año)
  ))

tasa_consumo_lsd <- datos %>%
  group_by(Region) %>%
  summarise(
    total_consumidores = sum(lsd * Fexp, na.rm = TRUE),  # Ignorar NA en los cálculos
    total_individuos = sum(Fexp, na.rm = TRUE),             # Ignorar NA en los cálculos
    tasa_consumo = ifelse(total_individuos == 0, NA, total_consumidores / total_individuos)  # Evitar división por cero
  )

tasa_consumo_lsd_total <- datos %>%
  summarise(
    total_consumidores = sum(lsd * Fexp, na.rm = TRUE),   # Total ponderado de consumidores
    total_individuos = sum(Fexp, na.rm = TRUE),            # Total ponderado de individuos
    tasa_consumo = ifelse(total_individuos == 0, NA, total_consumidores / total_individuos )  # Calcular la tasa como porcentaje
  )

#TUSI

datos <- datos %>%
  mutate(tusi = case_when(
    T_OD_14_4 %in% 1:2 ~ 1,  # Recodificar valores 1 o 2 como 1 (Año)
    T_OD_14_4 == 3 ~ 0       # Recodificar valor 3 como 2 (Más de un año)
  ))

tasa_consumo_tusi <- datos %>%
  group_by(Region) %>%
  summarise(
    total_consumidores = sum(tusi * Fexp, na.rm = TRUE),  # Ignorar NA en los cálculos
    total_individuos = sum(Fexp, na.rm = TRUE),             # Ignorar NA en los cálculos
    tasa_consumo = ifelse(total_individuos == 0, NA, total_consumidores / total_individuos)  # Evitar división por cero
  )

tasa_consumo_tusi_total <- datos %>%
  summarise(
    total_consumidores = sum(tusi * Fexp, na.rm = TRUE),   # Total ponderado de consumidores
    total_individuos = sum(Fexp, na.rm = TRUE),            # Total ponderado de individuos
    tasa_consumo = ifelse(total_individuos == 0, NA, total_consumidores / total_individuos )  # Calcular la tasa como porcentaje
  )

#HONGOS

datos <- datos %>%
  mutate(hongos = case_when(
    T_OD_14_7 %in% 1:2 ~ 1,  # Recodificar valores 1 o 2 como 1 (Año)
    T_OD_14_7 == 3 ~ 0       # Recodificar valor 3 como 2 (Más de un año)
  ))

tasa_consumo_hongos <- datos %>%
  group_by(Region) %>%
  summarise(
    total_consumidores = sum(hongos * Fexp, na.rm = TRUE),  # Ignorar NA en los cálculos
    total_individuos = sum(Fexp, na.rm = TRUE),             # Ignorar NA en los cálculos
    tasa_consumo = ifelse(total_individuos == 0, NA, total_consumidores / total_individuos)  # Evitar división por cero
  )

tasa_consumo_hongos_total <- datos %>%
  summarise(
    total_consumidores = sum(hongos * Fexp, na.rm = TRUE),   # Total ponderado de consumidores
    total_individuos = sum(Fexp, na.rm = TRUE),            # Total ponderado de individuos
    tasa_consumo = ifelse(total_individuos == 0, NA, total_consumidores / total_individuos )  # Calcular la tasa como porcentaje
  )

#SAN PEDRO

datos <- datos %>%
  mutate(sanpedro = case_when(
    T_OD_14_5 %in% 1:2 ~ 1,  # Recodificar valores 1 o 2 como 1 (Año)
    T_OD_14_5 == 3 ~ 0       # Recodificar valor 3 como 2 (Más de un año)
  ))

tasa_consumo_sanpedro <- datos %>%
  group_by(Region) %>%
  summarise(
    total_consumidores = sum(sanpedro * Fexp, na.rm = TRUE),  # Ignorar NA en los cálculos
    total_individuos = sum(Fexp, na.rm = TRUE),             # Ignorar NA en los cálculos
    tasa_consumo = ifelse(total_individuos == 0, NA, total_consumidores / total_individuos)  # Evitar división por cero
  )

tasa_consumo_sanpedro_total <- datos %>%
  summarise(
    total_consumidores = sum(sanpedro * Fexp, na.rm = TRUE),   # Total ponderado de consumidores
    total_individuos = sum(Fexp, na.rm = TRUE),            # Total ponderado de individuos
    tasa_consumo = ifelse(total_individuos == 0, NA, total_consumidores / total_individuos )  # Calcular la tasa como porcentaje
  )

#POLVO DE ÁNGEL

datos <- datos %>%
  mutate(polvo = case_when(
    T_OD_14_2 %in% 1:2 ~ 1,  # Recodificar valores 1 o 2 como 1 (Año)
    T_OD_14_2 == 3 ~ 0       # Recodificar valor 3 como 2 (Más de un año)
  ))

tasa_consumo_polvo <- datos %>%
  group_by(Region) %>%
  summarise(
    total_consumidores = sum(polvo * Fexp, na.rm = TRUE),  # Ignorar NA en los cálculos
    total_individuos = sum(Fexp, na.rm = TRUE),             # Ignorar NA en los cálculos
    tasa_consumo = ifelse(total_individuos == 0, NA, total_consumidores / total_individuos)  # Evitar división por cero
  )

tasa_consumo_polvo_total <- datos %>%
  summarise(
    total_consumidores = sum(polvo * Fexp, na.rm = TRUE),   # Total ponderado de consumidores
    total_individuos = sum(Fexp, na.rm = TRUE),            # Total ponderado de individuos
    tasa_consumo = ifelse(total_individuos == 0, NA, total_consumidores / total_individuos )  # Calcular la tasa como porcentaje
  )

#TRANQUILIZANTES

datos <- datos %>% 
  mutate(T_TRANS_3_1 = ifelse(T_TRANS_3_1 %in% c(1, 2), 1, 0),
         T_TRANS_3_2 = ifelse(T_TRANS_3_2 %in% c(1, 2), 1, 0),
         T_TRANS_3_3 = ifelse(T_TRANS_3_3 %in% c(1, 2), 1, 0),
         T_TRANS_3_4 = ifelse(T_TRANS_3_4 %in% c(1, 2), 1, 0),
         T_TRANS_3_5 = ifelse(T_TRANS_3_5 %in% c(1, 2), 1, 0),
         T_TRANS_3_6 = ifelse(T_TRANS_3_6 %in% c(1, 2), 1, 0),
         T_TRANS_3_7 = ifelse(T_TRANS_3_7 %in% c(1, 2), 1, 0),
         T_TRANS_3_8 = ifelse(T_TRANS_3_8 %in% c(1, 2), 1, 0),
         T_TRANS_3_9 = ifelse(T_TRANS_3_9 %in% c(1, 2), 1, 0)) %>%
  mutate(tranquilizantes = case_when(
    T_TRANS_3_1 == 1 | T_TRANS_3_2 == 1 | T_TRANS_3_3 == 1 | T_TRANS_3_4 == 1 | 
      T_TRANS_3_5 == 1 | T_TRANS_3_6 == 1 | T_TRANS_3_7 == 1 | T_TRANS_3_8 == 1 | T_TRANS_3_9 == 1 ~ 1,
    TRUE ~ 0
  ))


tasa_consumo_tranquilizantes <- datos %>%
  group_by(Region) %>%
  summarise(
    total_consumidores = sum(tranquilizantes * Fexp, na.rm = TRUE),  # Ignorar NA en los cálculos
    total_individuos = sum(Fexp, na.rm = TRUE),             # Ignorar NA en los cálculos
    tasa_consumo = ifelse(total_individuos == 0, NA, total_consumidores / total_individuos)  # Evitar división por cero
  )

tasa_consumo_tranquilizantes_total <- datos %>%
  summarise(
    total_consumidores = sum(tranquilizantes * Fexp, na.rm = TRUE),   # Total ponderado de consumidores
    total_individuos = sum(Fexp, na.rm = TRUE),            # Total ponderado de individuos
    tasa_consumo = ifelse(total_individuos == 0, NA, total_consumidores / total_individuos )  # Calcular la tasa como porcentaje
  )

#ESTIMULANTES

datos <- datos %>% 
  mutate(T_OD_8_1 = ifelse(T_OD_8_1 %in% c(1, 2), 1, 0),
         T_OD_8_2 = ifelse(T_OD_8_2 %in% c(1, 2), 1, 0),
         T_OD_8_3 = ifelse(T_OD_8_3 %in% c(1, 2), 1, 0),
         T_OD_8_4 = ifelse(T_OD_8_4 %in% c(1, 2), 1, 0),
         T_OD_8_6 = ifelse(T_OD_8_6 %in% c(1, 2), 1, 0),
         T_OD_8_7 = ifelse(T_OD_8_7 %in% c(1, 2), 1, 0),
         T_OD_8_8 = ifelse(T_OD_8_8 %in% c(1, 2), 1, 0),
         T_OD_8_5 = ifelse(T_OD_8_5 %in% c(1, 2), 1, 0)) %>%
  mutate(estimulantes = case_when(
    T_OD_8_1 == 1 | T_OD_8_2 == 1 | T_OD_8_3 == 1 | T_OD_8_4 == 1 | 
      T_OD_8_5 == 1 | T_OD_8_6 == 1 | T_OD_8_7 == 1 | T_OD_8_8 == 1 ~ 1,
    TRUE ~ 0
  ))


tasa_consumo_estimulantes <- datos %>%
  group_by(Region) %>%
  summarise(
    total_consumidores = sum(estimulantes * Fexp, na.rm = TRUE),  # Ignorar NA en los cálculos
    total_individuos = sum(Fexp, na.rm = TRUE),             # Ignorar NA en los cálculos
    tasa_consumo = ifelse(total_individuos == 0, NA, total_consumidores / total_individuos)  # Evitar división por cero
  )

tasa_consumo_estimulantes_total <- datos %>%
  summarise(
    total_consumidores = sum(estimulantes * Fexp, na.rm = TRUE),   # Total ponderado de consumidores
    total_individuos = sum(Fexp, na.rm = TRUE),            # Total ponderado de individuos
    tasa_consumo = ifelse(total_individuos == 0, NA, total_consumidores / total_individuos )  # Calcular la tasa como porcentaje
  )

# COCAINAS
datos <- datos %>%
  mutate(cocainas = if_else(coca == 1 | pasta == 1 | crack == 1, 1, 0))
table(datos$cocainas)

tasa_cocainas <- datos %>%
  group_by(Region) %>%
  summarise(
    total_consumidores = sum(cocainas * Fexp, na.rm = TRUE),  # Ignorar NA en los cálculos
    total_individuos = sum(Fexp, na.rm = TRUE),             # Ignorar NA en los cálculos
    tasa_consumo = ifelse(total_individuos == 0, NA, total_consumidores / total_individuos)  # Evitar división por cero
  )

tasa_cocainas_total <- datos %>%
  summarise(
    total_consumidores = sum(cocainas * Fexp, na.rm = TRUE),   # Total ponderado de consumidores
    total_individuos = sum(Fexp, na.rm = TRUE),            # Total ponderado de individuos
    tasa_consumo = ifelse(total_individuos == 0, NA, total_consumidores / total_individuos )  # Calcular la tasa como porcentaje
  )


# ALUCINOGENOS

datos <- datos %>%
  mutate(alucinogenos = if_else(lsd == 1 | hongos == 1 | tusi == 1 | sanpedro == 1 |polvo == 1 , 1, 0))
table(datos$alucinogenos)

tasa_alucinogenos <- datos %>%
  group_by(Region) %>%
  summarise(
    total_consumidores = sum(alucinogenos * Fexp, na.rm = TRUE),  # Ignorar NA en los cálculos
    total_individuos = sum(Fexp, na.rm = TRUE),             # Ignorar NA en los cálculos
    tasa_consumo = ifelse(total_individuos == 0, NA, total_consumidores / total_individuos)  # Evitar división por cero
  )

tasa_alucinogenos_total <- datos %>%
  summarise(
    total_consumidores = sum(alucinogenos * Fexp, na.rm = TRUE),   # Total ponderado de consumidores
    total_individuos = sum(Fexp, na.rm = TRUE),            # Total ponderado de individuos
    tasa_consumo = ifelse(total_individuos == 0, NA, total_consumidores / total_individuos )  # Calcular la tasa como porcentaje
  )

# OTRAS

datos <- datos %>%
  mutate(otras = if_else(lsd == 1 | hongos == 1 | tusi == 1 | sanpedro == 1 |polvo == 1 | coca == 1 | pasta == 1 | crack == 1 | extasis == 1 | heroina == 1 | popper == 1 , 1, 0))
table(datos$otras)

tasa_otras <- datos %>%
  group_by(Region) %>%
  summarise(
    total_consumidores = sum(otras * Fexp, na.rm = TRUE),  # Ignorar NA en los cálculos
    total_individuos = sum(Fexp, na.rm = TRUE),             # Ignorar NA en los cálculos
    tasa_consumo = ifelse(total_individuos == 0, NA, total_consumidores / total_individuos)  # Evitar división por cero
  )

tasa_otras_total <- datos %>%
  summarise(
    total_consumidores = sum(otras * Fexp, na.rm = TRUE),   # Total ponderado de consumidores
    total_individuos = sum(Fexp, na.rm = TRUE),            # Total ponderado de individuos
    tasa_consumo = ifelse(total_individuos == 0, NA, total_consumidores / total_individuos )  # Calcular la tasa como porcentaje
  )

#EXCEL 

write_xlsx(
  list(
    "Tasa mari" = tasa_consumo_mari,
    "Tasa mari total" = tasa_consumo_mari_total,
    "Tasa coca" = tasa_consumo_coca,
    "Tasa coca total" = tasa_consumo_coca_total,
    "Tasa pasta" = tasa_consumo_pasta,
    "Tasa pasta total" = tasa_consumo_pasta_total,
    "Tasa extasis" = tasa_consumo_extasis,
    "Tasa extasis total" = tasa_consumo_extasis_total,
    "Tasa heroina" = tasa_consumo_heroina,
    "Tasa heroina total" = tasa_consumo_heroina_total,
    "Tasa crack" = tasa_consumo_crack,
    "Tasa crack total" = tasa_consumo_crack_total,
    "Tasa popper" = tasa_consumo_popper,
    "Tasa popper total" = tasa_consumo_popper_total,
    "Tasa lsd" = tasa_consumo_lsd,
    "Tasa lsd total" = tasa_consumo_lsd_total,
    "Tasa tusi" = tasa_consumo_tusi,
    "Tasa tusi total" = tasa_consumo_tusi_total,
    "Tasa hongos" = tasa_consumo_hongos,
    "Tasa hongos total" = tasa_consumo_hongos_total,
    "Tasa san pedro" = tasa_consumo_sanpedro,
    "Tasa san pedro total" = tasa_consumo_sanpedro_total,
    "Tasa polvo de angel" = tasa_consumo_polvo,
    "Tasa polvo de angel total" = tasa_consumo_polvo_total,
    "Tasa tranquilizantes sr" = tasa_consumo_tranquilizantes,
    "Tasa tranquilizantes total" = tasa_consumo_tranquilizantes_total,
    "Tasa estimulantes sr" = tasa_consumo_estimulantes,
    "Tasa estimulantes total" = tasa_consumo_estimulantes_total,
    "Tasa cocainas" = tasa_cocainas,
    "Tasa cocainas total" = tasa_cocainas_total,
    "Tasa alucinogenos" = tasa_alucinogenos,
    "Tasa alucinogenos total" = tasa_alucinogenos_total,
    "Tasa otras" = tasa_otras,
    "Tasa_otras total" = tasa_otras_total
  ),
  "tasa_consumo2.xlsx"
)

#2016
rm(list = ls())
setwd("C:/Users/cjara/OneDrive - Universidad Católica de Chile/Mercado drogas/SENDA/2016")
datos <- read_dta("C:/Users/cjara/OneDrive - Universidad Católica de Chile/Mercado drogas/SENDA/2016/BBDD.dta")

table(datos$edad)

datos <- datos %>%
  filter(edad < 18)

table(datos$región)

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

#MARIHUANA

datos <- datos %>%
  mutate(mari = case_when(
    mar_4 %in% 1:2 ~ 1,  # Recodificar valores 1 o 2 como 1 (Año)
    mar_4 == 3 ~ 0       # Recodificar valor 3 como 2 (Más de un año)
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
    coc_4 %in% 1:2 ~ 1,  # Recodificar valores 1 o 2 como 1 (Año)
    coc_4 == 3 ~ 0       # Recodificar valor 3 como 2 (Más de un año)
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
    pb_4 %in% 1:2 ~ 1,  # Recodificar valores 1 o 2 como 1 (Año)
    pb_4 == 3 ~ 0       # Recodificar valor 3 como 2 (Más de un año)
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
    od_6_c %in% 1:2 ~ 1,  # Recodificar valores 1 o 2 como 1 (Año)
    od_6_c == 3 ~ 0       # Recodificar valor 3 como 2 (Más de un año)
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

#HEROINA

datos <- datos %>%
  mutate(heroina = case_when(
    od_6_e %in% 1:2 ~ 1,  # Recodificar valores 1 o 2 como 1 (Año)
    od_6_e == 3 ~ 0       # Recodificar valor 3 como 2 (Más de un año)
  ))

tasa_consumo_heroina <- datos %>%
  group_by(Region) %>%
  summarise(
    total_consumidores = sum(heroina * Fexp, na.rm = TRUE),  # Ignorar NA en los cálculos
    total_individuos = sum(Fexp, na.rm = TRUE),             # Ignorar NA en los cálculos
    tasa_consumo = ifelse(total_individuos == 0, NA, total_consumidores / total_individuos)  # Evitar división por cero
  )

tasa_consumo_heroina_total <- datos %>%
  summarise(
    total_consumidores = sum(heroina * Fexp, na.rm = TRUE),   # Total ponderado de consumidores
    total_individuos = sum(Fexp, na.rm = TRUE),            # Total ponderado de individuos
    tasa_consumo = ifelse(total_individuos == 0, NA, total_consumidores / total_individuos )  # Calcular la tasa como porcentaje
  )

#CRACK

datos <- datos %>%
  mutate(crack = case_when(
    od_6_f %in% 1:2 ~ 1,  # Recodificar valores 1 o 2 como 1 (Año)
    od_6_f == 3 ~ 0       # Recodificar valor 3 como 2 (Más de un año)
  ))

tasa_consumo_crack <- datos %>%
  group_by(Region) %>%
  summarise(
    total_consumidores = sum(crack * Fexp, na.rm = TRUE),  # Ignorar NA en los cálculos
    total_individuos = sum(Fexp, na.rm = TRUE),             # Ignorar NA en los cálculos
    tasa_consumo = ifelse(total_individuos == 0, NA, total_consumidores / total_individuos)  # Evitar división por cero
  )

tasa_consumo_crack_total <- datos %>%
  summarise(
    total_consumidores = sum(crack * Fexp, na.rm = TRUE),   # Total ponderado de consumidores
    total_individuos = sum(Fexp, na.rm = TRUE),            # Total ponderado de individuos
    tasa_consumo = ifelse(total_individuos == 0, NA, total_consumidores / total_individuos )  # Calcular la tasa como porcentaje
  )

#POPPER

datos <- datos %>%
  mutate(popper = case_when(
    od_12_h %in% 1:2 ~ 1,  # Recodificar valores 1 o 2 como 1 (Año)
    od_12_h == 3 ~ 0       # Recodificar valor 3 como 2 (Más de un año)
  ))

tasa_consumo_popper <- datos %>%
  group_by(Region) %>%
  summarise(
    total_consumidores = sum(popper * Fexp, na.rm = TRUE),  # Ignorar NA en los cálculos
    total_individuos = sum(Fexp, na.rm = TRUE),             # Ignorar NA en los cálculos
    tasa_consumo = ifelse(total_individuos == 0, NA, total_consumidores / total_individuos)  # Evitar división por cero
  )

tasa_consumo_popper_total <- datos %>%
  summarise(
    total_consumidores = sum(popper * Fexp, na.rm = TRUE),   # Total ponderado de consumidores
    total_individuos = sum(Fexp, na.rm = TRUE),            # Total ponderado de individuos
    tasa_consumo = ifelse(total_individuos == 0, NA, total_consumidores / total_individuos )  # Calcular la tasa como porcentaje
  )

#LSD

datos <- datos %>%
  mutate(lsd = case_when(
    od_14_a %in% 1:2 ~ 1,  # Recodificar valores 1 o 2 como 1 (Año)
    od_14_a == 3 ~ 0       # Recodificar valor 3 como 2 (Más de un año)
  ))

tasa_consumo_lsd <- datos %>%
  group_by(Region) %>%
  summarise(
    total_consumidores = sum(lsd * Fexp, na.rm = TRUE),  # Ignorar NA en los cálculos
    total_individuos = sum(Fexp, na.rm = TRUE),             # Ignorar NA en los cálculos
    tasa_consumo = ifelse(total_individuos == 0, NA, total_consumidores / total_individuos)  # Evitar división por cero
  )

tasa_consumo_lsd_total <- datos %>%
  summarise(
    total_consumidores = sum(lsd * Fexp, na.rm = TRUE),   # Total ponderado de consumidores
    total_individuos = sum(Fexp, na.rm = TRUE),            # Total ponderado de individuos
    tasa_consumo = ifelse(total_individuos == 0, NA, total_consumidores / total_individuos )  # Calcular la tasa como porcentaje
  )

#HONGOS

datos <- datos %>%
  mutate(hongos = case_when(
    od_14_f %in% 1:2 ~ 1,  # Recodificar valores 1 o 2 como 1 (Año)
    od_14_f == 3 ~ 0       # Recodificar valor 3 como 2 (Más de un año)
  ))

tasa_consumo_hongos <- datos %>%
  group_by(Region) %>%
  summarise(
    total_consumidores = sum(hongos * Fexp, na.rm = TRUE),  # Ignorar NA en los cálculos
    total_individuos = sum(Fexp, na.rm = TRUE),             # Ignorar NA en los cálculos
    tasa_consumo = ifelse(total_individuos == 0, NA, total_consumidores / total_individuos)  # Evitar división por cero
  )

tasa_consumo_hongos_total <- datos %>%
  summarise(
    total_consumidores = sum(hongos * Fexp, na.rm = TRUE),   # Total ponderado de consumidores
    total_individuos = sum(Fexp, na.rm = TRUE),            # Total ponderado de individuos
    tasa_consumo = ifelse(total_individuos == 0, NA, total_consumidores / total_individuos )  # Calcular la tasa como porcentaje
  )

#SAN PEDRO

datos <- datos %>%
  mutate(sanpedro = case_when(
    od_14_d %in% 1:2 ~ 1,  # Recodificar valores 1 o 2 como 1 (Año)
    od_14_d == 3 ~ 0       # Recodificar valor 3 como 2 (Más de un año)
  ))

tasa_consumo_sanpedro <- datos %>%
  group_by(Region) %>%
  summarise(
    total_consumidores = sum(sanpedro * Fexp, na.rm = TRUE),  # Ignorar NA en los cálculos
    total_individuos = sum(Fexp, na.rm = TRUE),             # Ignorar NA en los cálculos
    tasa_consumo = ifelse(total_individuos == 0, NA, total_consumidores / total_individuos)  # Evitar división por cero
  )

tasa_consumo_sanpedro_total <- datos %>%
  summarise(
    total_consumidores = sum(sanpedro * Fexp, na.rm = TRUE),   # Total ponderado de consumidores
    total_individuos = sum(Fexp, na.rm = TRUE),            # Total ponderado de individuos
    tasa_consumo = ifelse(total_individuos == 0, NA, total_consumidores / total_individuos )  # Calcular la tasa como porcentaje
  )

#POLVO DE ÁNGEL

datos <- datos %>%
  mutate(polvo = case_when(
    od_14_b %in% 1:2 ~ 1,  # Recodificar valores 1 o 2 como 1 (Año)
    od_14_b == 3 ~ 0       # Recodificar valor 3 como 2 (Más de un año)
  ))

tasa_consumo_polvo <- datos %>%
  group_by(Region) %>%
  summarise(
    total_consumidores = sum(polvo * Fexp, na.rm = TRUE),  # Ignorar NA en los cálculos
    total_individuos = sum(Fexp, na.rm = TRUE),             # Ignorar NA en los cálculos
    tasa_consumo = ifelse(total_individuos == 0, NA, total_consumidores / total_individuos)  # Evitar división por cero
  )

tasa_consumo_polvo_total <- datos %>%
  summarise(
    total_consumidores = sum(polvo * Fexp, na.rm = TRUE),   # Total ponderado de consumidores
    total_individuos = sum(Fexp, na.rm = TRUE),            # Total ponderado de individuos
    tasa_consumo = ifelse(total_individuos == 0, NA, total_consumidores / total_individuos )  # Calcular la tasa como porcentaje
  )

#TRANQUILIZANTES

datos <- datos %>% 
  mutate(trans_3_a = ifelse(trans_3_a %in% c(1, 2), 1, 0),
         trans_3_b = ifelse(trans_3_b %in% c(1, 2), 1, 0),
         trans_3_c = ifelse(trans_3_c %in% c(1, 2), 1, 0),
         trans_3_d = ifelse(trans_3_d %in% c(1, 2), 1, 0),
         trans_3_e = ifelse(trans_3_e %in% c(1, 2), 1, 0),
         trans_3_f = ifelse(trans_3_f %in% c(1, 2), 1, 0),
         trans_3_g = ifelse(trans_3_g %in% c(1, 2), 1, 0),
         trans_3_h = ifelse(trans_3_h %in% c(1, 2), 1, 0),
         trans_3_i = ifelse(trans_3_i %in% c(1, 2), 1, 0)) %>%
  mutate(tranquilizantes = case_when(
    trans_3_a == 1 | trans_3_b == 1 | trans_3_c == 1 | trans_3_d == 1 | 
      trans_3_e == 1 | trans_3_f == 1 | trans_3_g == 1 | trans_3_h == 1 | trans_3_i == 1 ~ 1,
    TRUE ~ 0
  ))


tasa_consumo_tranquilizantes <- datos %>%
  group_by(Region) %>%
  summarise(
    total_consumidores = sum(tranquilizantes * Fexp, na.rm = TRUE),  # Ignorar NA en los cálculos
    total_individuos = sum(Fexp, na.rm = TRUE),             # Ignorar NA en los cálculos
    tasa_consumo = ifelse(total_individuos == 0, NA, total_consumidores / total_individuos)  # Evitar división por cero
  )

tasa_consumo_tranquilizantes_total <- datos %>%
  summarise(
    total_consumidores = sum(tranquilizantes * Fexp, na.rm = TRUE),   # Total ponderado de consumidores
    total_individuos = sum(Fexp, na.rm = TRUE),            # Total ponderado de individuos
    tasa_consumo = ifelse(total_individuos == 0, NA, total_consumidores / total_individuos )  # Calcular la tasa como porcentaje
  )

#ESTIMULANTES

datos <- datos %>% 
  mutate(od_8_a = ifelse(od_8_a %in% c(1, 2), 1, 0),
         od_8_b = ifelse(od_8_b %in% c(1, 2), 1, 0),
         od_8_c = ifelse(od_8_c %in% c(1, 2), 1, 0),
         od_8_d = ifelse(od_8_d %in% c(1, 2), 1, 0),
         od_8_e = ifelse(od_8_e %in% c(1, 2), 1, 0),
         od_8_f = ifelse(od_8_f %in% c(1, 2), 1, 0),
         od_8_g = ifelse(od_8_g %in% c(1, 2), 1, 0),
         od_8_h = ifelse(od_8_h %in% c(1, 2), 1, 0)) %>%
  mutate(estimulantes = case_when(
    od_8_a == 1 | od_8_b == 1 | od_8_c == 1 | od_8_d == 1 | 
      od_8_e == 1 | od_8_f == 1 | od_8_g == 1 | od_8_h == 1 ~ 1,
    TRUE ~ 0
  ))


tasa_consumo_estimulantes <- datos %>%
  group_by(Region) %>%
  summarise(
    total_consumidores = sum(estimulantes * Fexp, na.rm = TRUE),  # Ignorar NA en los cálculos
    total_individuos = sum(Fexp, na.rm = TRUE),             # Ignorar NA en los cálculos
    tasa_consumo = ifelse(total_individuos == 0, NA, total_consumidores / total_individuos)  # Evitar división por cero
  )

tasa_consumo_estimulantes_total <- datos %>%
  summarise(
    total_consumidores = sum(estimulantes * Fexp, na.rm = TRUE),   # Total ponderado de consumidores
    total_individuos = sum(Fexp, na.rm = TRUE),            # Total ponderado de individuos
    tasa_consumo = ifelse(total_individuos == 0, NA, total_consumidores / total_individuos )  # Calcular la tasa como porcentaje
  )

# COCAINAS
datos <- datos %>%
  mutate(cocainas = if_else(coca == 1 | pasta == 1 | crack == 1, 1, 0))
table(datos$cocainas)

tasa_cocainas <- datos %>%
  group_by(Region) %>%
  summarise(
    total_consumidores = sum(cocainas * Fexp, na.rm = TRUE),  # Ignorar NA en los cálculos
    total_individuos = sum(Fexp, na.rm = TRUE),             # Ignorar NA en los cálculos
    tasa_consumo = ifelse(total_individuos == 0, NA, total_consumidores / total_individuos)  # Evitar división por cero
  )

tasa_cocainas_total <- datos %>%
  summarise(
    total_consumidores = sum(cocainas * Fexp, na.rm = TRUE),   # Total ponderado de consumidores
    total_individuos = sum(Fexp, na.rm = TRUE),            # Total ponderado de individuos
    tasa_consumo = ifelse(total_individuos == 0, NA, total_consumidores / total_individuos )  # Calcular la tasa como porcentaje
  )


# ALUCINOGENOS

datos <- datos %>%
  mutate(alucinogenos = if_else(lsd == 1 | hongos == 1 | sanpedro == 1 |polvo == 1 , 1, 0))
table(datos$alucinogenos)

tasa_alucinogenos <- datos %>%
  group_by(Region) %>%
  summarise(
    total_consumidores = sum(alucinogenos * Fexp, na.rm = TRUE),  # Ignorar NA en los cálculos
    total_individuos = sum(Fexp, na.rm = TRUE),             # Ignorar NA en los cálculos
    tasa_consumo = ifelse(total_individuos == 0, NA, total_consumidores / total_individuos)  # Evitar división por cero
  )

tasa_alucinogenos_total <- datos %>%
  summarise(
    total_consumidores = sum(alucinogenos * Fexp, na.rm = TRUE),   # Total ponderado de consumidores
    total_individuos = sum(Fexp, na.rm = TRUE),            # Total ponderado de individuos
    tasa_consumo = ifelse(total_individuos == 0, NA, total_consumidores / total_individuos )  # Calcular la tasa como porcentaje
  )

# OTRAS

datos <- datos %>%
  mutate(otras = if_else(lsd == 1 | hongos == 1 | sanpedro == 1 |polvo == 1 | coca == 1 | pasta == 1 | crack == 1 | extasis == 1 | heroina == 1 | popper == 1 , 1, 0))
table(datos$otras)

tasa_otras <- datos %>%
  group_by(Region) %>%
  summarise(
    total_consumidores = sum(otras * Fexp, na.rm = TRUE),  # Ignorar NA en los cálculos
    total_individuos = sum(Fexp, na.rm = TRUE),             # Ignorar NA en los cálculos
    tasa_consumo = ifelse(total_individuos == 0, NA, total_consumidores / total_individuos)  # Evitar división por cero
  )

tasa_otras_total <- datos %>%
  summarise(
    total_consumidores = sum(otras * Fexp, na.rm = TRUE),   # Total ponderado de consumidores
    total_individuos = sum(Fexp, na.rm = TRUE),            # Total ponderado de individuos
    tasa_consumo = ifelse(total_individuos == 0, NA, total_consumidores / total_individuos )  # Calcular la tasa como porcentaje
  )

#EXCEL 

write_xlsx(
  list(
    "Tasa mari" = tasa_consumo_mari,
    "Tasa mari total" = tasa_consumo_mari_total,
    "Tasa coca" = tasa_consumo_coca,
    "Tasa coca total" = tasa_consumo_coca_total,
    "Tasa pasta" = tasa_consumo_pasta,
    "Tasa pasta total" = tasa_consumo_pasta_total,
    "Tasa extasis" = tasa_consumo_extasis,
    "Tasa extasis total" = tasa_consumo_extasis_total,
    "Tasa heroina" = tasa_consumo_heroina,
    "Tasa heroina total" = tasa_consumo_heroina_total,
    "Tasa crack" = tasa_consumo_crack,
    "Tasa crack total" = tasa_consumo_crack_total,
    "Tasa popper" = tasa_consumo_popper,
    "Tasa popper total" = tasa_consumo_popper_total,
    "Tasa lsd" = tasa_consumo_lsd,
    "Tasa lsd total" = tasa_consumo_lsd_total,
    "Tasa hongos" = tasa_consumo_hongos,
    "Tasa hongos total" = tasa_consumo_hongos_total,
    "Tasa san pedro" = tasa_consumo_sanpedro,
    "Tasa san pedro total" = tasa_consumo_sanpedro_total,
    "Tasa polvo de angel" = tasa_consumo_polvo,
    "Tasa polvo de angel total" = tasa_consumo_polvo_total,
    "Tasa tranquilizantes sr" = tasa_consumo_tranquilizantes,
    "Tasa tranquilizantes total" = tasa_consumo_tranquilizantes_total,
    "Tasa estimulantes sr" = tasa_consumo_estimulantes,
    "Tasa estimulantes total" = tasa_consumo_estimulantes_total,
    "Tasa cocainas" = tasa_cocainas,
    "Tasa cocainas total" = tasa_cocainas_total,
    "Tasa alucinogenos" = tasa_alucinogenos,
    "Tasa alucinogenos total" = tasa_alucinogenos_total,
    "Tasa otras" = tasa_otras,
    "Tasa_otras total" = tasa_otras_total
  ),
  "tasa_consumo2.xlsx"
)

#2014

rm(list = ls())
setwd("C:/Users/cjara/OneDrive - Universidad Católica de Chile/Mercado drogas/SENDA/2014")
datos <- read_dta("C:/Users/cjara/OneDrive - Universidad Católica de Chile/Mercado drogas/SENDA/2014/BBDD.dta")

table(datos$edad)

datos <- datos %>%
  filter(edad < 18)

table(datos$Region)

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

#MARIHUANA

datos <- datos %>%
  mutate(mari = case_when(
    mar4 %in% 1:2 ~ 1,  # Recodificar valores 1 o 2 como 1 (Año)
    mar4 == 3 ~ 0       # Recodificar valor 3 como 2 (Más de un año)
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
    coc4 %in% 1:2 ~ 1,  # Recodificar valores 1 o 2 como 1 (Año)
    coc4 == 3 ~ 0       # Recodificar valor 3 como 2 (Más de un año)
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
    pb4 %in% 1:2 ~ 1,  # Recodificar valores 1 o 2 como 1 (Año)
    pb4 == 3 ~ 0       # Recodificar valor 3 como 2 (Más de un año)
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
    od6_3 %in% 1:2 ~ 1,  # Recodificar valores 1 o 2 como 1 (Año)
    od6_3 == 3 ~ 0       # Recodificar valor 3 como 2 (Más de un año)
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

#HEROINA

datos <- datos %>%
  mutate(heroina = case_when(
    od6_4 %in% 1:2 ~ 1,  # Recodificar valores 1 o 2 como 1 (Año)
    od6_4 == 3 ~ 0       # Recodificar valor 3 como 2 (Más de un año)
  ))

tasa_consumo_heroina <- datos %>%
  group_by(Region) %>%
  summarise(
    total_consumidores = sum(heroina * Fexp, na.rm = TRUE),  # Ignorar NA en los cálculos
    total_individuos = sum(Fexp, na.rm = TRUE),             # Ignorar NA en los cálculos
    tasa_consumo = ifelse(total_individuos == 0, NA, total_consumidores / total_individuos)  # Evitar división por cero
  )

tasa_consumo_heroina_total <- datos %>%
  summarise(
    total_consumidores = sum(heroina * Fexp, na.rm = TRUE),   # Total ponderado de consumidores
    total_individuos = sum(Fexp, na.rm = TRUE),            # Total ponderado de individuos
    tasa_consumo = ifelse(total_individuos == 0, NA, total_consumidores / total_individuos )  # Calcular la tasa como porcentaje
  )

#CRACK

datos <- datos %>%
  mutate(crack = case_when(
    od6_5 %in% 1:2 ~ 1,  # Recodificar valores 1 o 2 como 1 (Año)
    od6_5 == 3 ~ 0       # Recodificar valor 3 como 2 (Más de un año)
  ))

tasa_consumo_crack <- datos %>%
  group_by(Region) %>%
  summarise(
    total_consumidores = sum(crack * Fexp, na.rm = TRUE),  # Ignorar NA en los cálculos
    total_individuos = sum(Fexp, na.rm = TRUE),             # Ignorar NA en los cálculos
    tasa_consumo = ifelse(total_individuos == 0, NA, total_consumidores / total_individuos)  # Evitar división por cero
  )

tasa_consumo_crack_total <- datos %>%
  summarise(
    total_consumidores = sum(crack * Fexp, na.rm = TRUE),   # Total ponderado de consumidores
    total_individuos = sum(Fexp, na.rm = TRUE),            # Total ponderado de individuos
    tasa_consumo = ifelse(total_individuos == 0, NA, total_consumidores / total_individuos )  # Calcular la tasa como porcentaje
  )

#POPPER

datos <- datos %>%
  mutate(popper = case_when(
    od12_8 %in% 1:2 ~ 1,  # Recodificar valores 1 o 2 como 1 (Año)
    od12_8 == 3 ~ 0       # Recodificar valor 3 como 2 (Más de un año)
  ))

tasa_consumo_popper <- datos %>%
  group_by(Region) %>%
  summarise(
    total_consumidores = sum(popper * Fexp, na.rm = TRUE),  # Ignorar NA en los cálculos
    total_individuos = sum(Fexp, na.rm = TRUE),             # Ignorar NA en los cálculos
    tasa_consumo = ifelse(total_individuos == 0, NA, total_consumidores / total_individuos)  # Evitar división por cero
  )

tasa_consumo_popper_total <- datos %>%
  summarise(
    total_consumidores = sum(popper * Fexp, na.rm = TRUE),   # Total ponderado de consumidores
    total_individuos = sum(Fexp, na.rm = TRUE),            # Total ponderado de individuos
    tasa_consumo = ifelse(total_individuos == 0, NA, total_consumidores / total_individuos )  # Calcular la tasa como porcentaje
  )

#LSD

datos <- datos %>%
  mutate(lsd = case_when(
    od14_1 %in% 1:2 ~ 1,  # Recodificar valores 1 o 2 como 1 (Año)
    od14_1 == 3 ~ 0       # Recodificar valor 3 como 2 (Más de un año)
  ))

tasa_consumo_lsd <- datos %>%
  group_by(Region) %>%
  summarise(
    total_consumidores = sum(lsd * Fexp, na.rm = TRUE),  # Ignorar NA en los cálculos
    total_individuos = sum(Fexp, na.rm = TRUE),             # Ignorar NA en los cálculos
    tasa_consumo = ifelse(total_individuos == 0, NA, total_consumidores / total_individuos)  # Evitar división por cero
  )

tasa_consumo_lsd_total <- datos %>%
  summarise(
    total_consumidores = sum(lsd * Fexp, na.rm = TRUE),   # Total ponderado de consumidores
    total_individuos = sum(Fexp, na.rm = TRUE),            # Total ponderado de individuos
    tasa_consumo = ifelse(total_individuos == 0, NA, total_consumidores / total_individuos )  # Calcular la tasa como porcentaje
  )


#HONGOS

datos <- datos %>%
  mutate(hongos = case_when(
    od14_6 %in% 1:2 ~ 1,  # Recodificar valores 1 o 2 como 1 (Año)
    od14_6 == 3 ~ 0       # Recodificar valor 3 como 2 (Más de un año)
  ))

tasa_consumo_hongos <- datos %>%
  group_by(Region) %>%
  summarise(
    total_consumidores = sum(hongos * Fexp, na.rm = TRUE),  # Ignorar NA en los cálculos
    total_individuos = sum(Fexp, na.rm = TRUE),             # Ignorar NA en los cálculos
    tasa_consumo = ifelse(total_individuos == 0, NA, total_consumidores / total_individuos)  # Evitar división por cero
  )

tasa_consumo_hongos_total <- datos %>%
  summarise(
    total_consumidores = sum(hongos * Fexp, na.rm = TRUE),   # Total ponderado de consumidores
    total_individuos = sum(Fexp, na.rm = TRUE),            # Total ponderado de individuos
    tasa_consumo = ifelse(total_individuos == 0, NA, total_consumidores / total_individuos )  # Calcular la tasa como porcentaje
  )

#SAN PEDRO

datos <- datos %>%
  mutate(sanpedro = case_when(
    od14_4 %in% 1:2 ~ 1,  # Recodificar valores 1 o 2 como 1 (Año)
    od14_4 == 3 ~ 0       # Recodificar valor 3 como 2 (Más de un año)
  ))

tasa_consumo_sanpedro <- datos %>%
  group_by(Region) %>%
  summarise(
    total_consumidores = sum(sanpedro * Fexp, na.rm = TRUE),  # Ignorar NA en los cálculos
    total_individuos = sum(Fexp, na.rm = TRUE),             # Ignorar NA en los cálculos
    tasa_consumo = ifelse(total_individuos == 0, NA, total_consumidores / total_individuos)  # Evitar división por cero
  )

tasa_consumo_sanpedro_total <- datos %>%
  summarise(
    total_consumidores = sum(sanpedro * Fexp, na.rm = TRUE),   # Total ponderado de consumidores
    total_individuos = sum(Fexp, na.rm = TRUE),            # Total ponderado de individuos
    tasa_consumo = ifelse(total_individuos == 0, NA, total_consumidores / total_individuos )  # Calcular la tasa como porcentaje
  )

#POLVO DE ÁNGEL

datos <- datos %>%
  mutate(polvo = case_when(
    od14_2 %in% 1:2 ~ 1,  # Recodificar valores 1 o 2 como 1 (Año)
    od14_2 == 3 ~ 0       # Recodificar valor 3 como 2 (Más de un año)
  ))

tasa_consumo_polvo <- datos %>%
  group_by(Region) %>%
  summarise(
    total_consumidores = sum(polvo * Fexp, na.rm = TRUE),  # Ignorar NA en los cálculos
    total_individuos = sum(Fexp, na.rm = TRUE),             # Ignorar NA en los cálculos
    tasa_consumo = ifelse(total_individuos == 0, NA, total_consumidores / total_individuos)  # Evitar división por cero
  )

tasa_consumo_polvo_total <- datos %>%
  summarise(
    total_consumidores = sum(polvo * Fexp, na.rm = TRUE),   # Total ponderado de consumidores
    total_individuos = sum(Fexp, na.rm = TRUE),            # Total ponderado de individuos
    tasa_consumo = ifelse(total_individuos == 0, NA, total_consumidores / total_individuos )  # Calcular la tasa como porcentaje
  )

#TRANQUILIZANTES

datos <- datos %>% 
  mutate(trans3_1 = ifelse(trans3_1 %in% c(1, 2), 1, 0),
         trans3_2 = ifelse(trans3_2 %in% c(1, 2), 1, 0),
         trans3_3 = ifelse(trans3_3 %in% c(1, 2), 1, 0),
         trans3_4 = ifelse(trans3_4 %in% c(1, 2), 1, 0),
         trans3_5 = ifelse(trans3_5 %in% c(1, 2), 1, 0),
         trans3_6 = ifelse(trans3_6 %in% c(1, 2), 1, 0),
         trans3_7 = ifelse(trans3_7 %in% c(1, 2), 1, 0),
         trans3_8 = ifelse(trans3_8 %in% c(1, 2), 1, 0),
         trans3_9 = ifelse(trans3_9 %in% c(1, 2), 1, 0)) %>%
  mutate(tranquilizantes = case_when(
    trans3_1 == 1 | trans3_2 == 1 | trans3_3 == 1 | trans3_4 == 1 | 
      trans3_5 == 1 | trans3_6 == 1 | trans3_7 == 1 | trans3_8 == 1 | trans3_9 == 1 ~ 1,
    TRUE ~ 0
  ))


tasa_consumo_tranquilizantes <- datos %>%
  group_by(Region) %>%
  summarise(
    total_consumidores = sum(tranquilizantes * Fexp, na.rm = TRUE),  # Ignorar NA en los cálculos
    total_individuos = sum(Fexp, na.rm = TRUE),             # Ignorar NA en los cálculos
    tasa_consumo = ifelse(total_individuos == 0, NA, total_consumidores / total_individuos)  # Evitar división por cero
  )

tasa_consumo_tranquilizantes_total <- datos %>%
  summarise(
    total_consumidores = sum(tranquilizantes * Fexp, na.rm = TRUE),   # Total ponderado de consumidores
    total_individuos = sum(Fexp, na.rm = TRUE),            # Total ponderado de individuos
    tasa_consumo = ifelse(total_individuos == 0, NA, total_consumidores / total_individuos )  # Calcular la tasa como porcentaje
  )

#ESTIMULANTES

datos <- datos %>% 
  mutate(od8_1 = ifelse(od8_1 %in% c(1, 2), 1, 0),
         od8_2 = ifelse(od8_2 %in% c(1, 2), 1, 0),
         od8_3 = ifelse(od8_3 %in% c(1, 2), 1, 0),
         od8_4 = ifelse(od8_4 %in% c(1, 2), 1, 0),
         od8_6 = ifelse(od8_6 %in% c(1, 2), 1, 0),
         od8_7 = ifelse(od8_7 %in% c(1, 2), 1, 0),
         od8_8 = ifelse(od8_8 %in% c(1, 2), 1, 0),
         od8_5 = ifelse(od8_5 %in% c(1, 2), 1, 0)) %>%
  mutate(estimulantes = case_when(
    od8_1 == 1 | od8_2 == 1 | od8_3 == 1 | od8_4 == 1 | 
      od8_5 == 1 | od8_6 == 1 | od8_7 == 1 | od8_8 == 1 ~ 1,
    TRUE ~ 0
  ))


tasa_consumo_estimulantes <- datos %>%
  group_by(Region) %>%
  summarise(
    total_consumidores = sum(estimulantes * Fexp, na.rm = TRUE),  # Ignorar NA en los cálculos
    total_individuos = sum(Fexp, na.rm = TRUE),             # Ignorar NA en los cálculos
    tasa_consumo = ifelse(total_individuos == 0, NA, total_consumidores / total_individuos)  # Evitar división por cero
  )

tasa_consumo_estimulantes_total <- datos %>%
  summarise(
    total_consumidores = sum(estimulantes * Fexp, na.rm = TRUE),   # Total ponderado de consumidores
    total_individuos = sum(Fexp, na.rm = TRUE),            # Total ponderado de individuos
    tasa_consumo = ifelse(total_individuos == 0, NA, total_consumidores / total_individuos )  # Calcular la tasa como porcentaje
  )

# COCAINAS
datos <- datos %>%
  mutate(cocainas = if_else(coca == 1 | pasta == 1 | crack == 1, 1, 0))
table(datos$cocainas)

tasa_cocainas <- datos %>%
  group_by(Region) %>%
  summarise(
    total_consumidores = sum(cocainas * Fexp, na.rm = TRUE),  # Ignorar NA en los cálculos
    total_individuos = sum(Fexp, na.rm = TRUE),             # Ignorar NA en los cálculos
    tasa_consumo = ifelse(total_individuos == 0, NA, total_consumidores / total_individuos)  # Evitar división por cero
  )

tasa_cocainas_total <- datos %>%
  summarise(
    total_consumidores = sum(cocainas * Fexp, na.rm = TRUE),   # Total ponderado de consumidores
    total_individuos = sum(Fexp, na.rm = TRUE),            # Total ponderado de individuos
    tasa_consumo = ifelse(total_individuos == 0, NA, total_consumidores / total_individuos )  # Calcular la tasa como porcentaje
  )


# ALUCINOGENOS

datos <- datos %>%
  mutate(alucinogenos = if_else(lsd == 1 | hongos == 1 | sanpedro == 1 |polvo == 1 , 1, 0))
table(datos$alucinogenos)

tasa_alucinogenos <- datos %>%
  group_by(Region) %>%
  summarise(
    total_consumidores = sum(alucinogenos * Fexp, na.rm = TRUE),  # Ignorar NA en los cálculos
    total_individuos = sum(Fexp, na.rm = TRUE),             # Ignorar NA en los cálculos
    tasa_consumo = ifelse(total_individuos == 0, NA, total_consumidores / total_individuos)  # Evitar división por cero
  )

tasa_alucinogenos_total <- datos %>%
  summarise(
    total_consumidores = sum(alucinogenos * Fexp, na.rm = TRUE),   # Total ponderado de consumidores
    total_individuos = sum(Fexp, na.rm = TRUE),            # Total ponderado de individuos
    tasa_consumo = ifelse(total_individuos == 0, NA, total_consumidores / total_individuos )  # Calcular la tasa como porcentaje
  )

# OTRAS

datos <- datos %>%
  mutate(otras = if_else(lsd == 1 | hongos == 1 | sanpedro == 1 |polvo == 1 | coca == 1 | pasta == 1 | crack == 1 | extasis == 1 | heroina == 1 | popper == 1 , 1, 0))
table(datos$otras)

tasa_otras <- datos %>%
  group_by(Region) %>%
  summarise(
    total_consumidores = sum(otras * Fexp, na.rm = TRUE),  # Ignorar NA en los cálculos
    total_individuos = sum(Fexp, na.rm = TRUE),             # Ignorar NA en los cálculos
    tasa_consumo = ifelse(total_individuos == 0, NA, total_consumidores / total_individuos)  # Evitar división por cero
  )

tasa_otras_total <- datos %>%
  summarise(
    total_consumidores = sum(otras * Fexp, na.rm = TRUE),   # Total ponderado de consumidores
    total_individuos = sum(Fexp, na.rm = TRUE),            # Total ponderado de individuos
    tasa_consumo = ifelse(total_individuos == 0, NA, total_consumidores / total_individuos )  # Calcular la tasa como porcentaje
  )

#EXCEL 

write_xlsx(
  list(
    "Tasa mari" = tasa_consumo_mari,
    "Tasa mari total" = tasa_consumo_mari_total,
    "Tasa coca" = tasa_consumo_coca,
    "Tasa coca total" = tasa_consumo_coca_total,
    "Tasa pasta" = tasa_consumo_pasta,
    "Tasa pasta total" = tasa_consumo_pasta_total,
    "Tasa extasis" = tasa_consumo_extasis,
    "Tasa extasis total" = tasa_consumo_extasis_total,
    "Tasa heroina" = tasa_consumo_heroina,
    "Tasa heroina total" = tasa_consumo_heroina_total,
    "Tasa crack" = tasa_consumo_crack,
    "Tasa crack total" = tasa_consumo_crack_total,
    "Tasa popper" = tasa_consumo_popper,
    "Tasa popper total" = tasa_consumo_popper_total,
    "Tasa lsd" = tasa_consumo_lsd,
    "Tasa lsd total" = tasa_consumo_lsd_total,
    "Tasa hongos" = tasa_consumo_hongos,
    "Tasa hongos total" = tasa_consumo_hongos_total,
    "Tasa san pedro" = tasa_consumo_sanpedro,
    "Tasa san pedro total" = tasa_consumo_sanpedro_total,
    "Tasa polvo de angel" = tasa_consumo_polvo,
    "Tasa polvo de angel total" = tasa_consumo_polvo_total,
    "Tasa tranquilizantes sr" = tasa_consumo_tranquilizantes,
    "Tasa tranquilizantes total" = tasa_consumo_tranquilizantes_total,
    "Tasa estimulantes sr" = tasa_consumo_estimulantes,
    "Tasa estimulantes total" = tasa_consumo_estimulantes_total,
    "Tasa cocainas" = tasa_cocainas,
    "Tasa cocainas total" = tasa_cocainas_total,
    "Tasa alucinogenos" = tasa_alucinogenos,
    "Tasa alucinogenos total" = tasa_alucinogenos_total,
    "Tasa otras" = tasa_otras,
    "Tasa_otras total" = tasa_otras_total
  ),
  "tasa_consumo2.xlsx"
)

#2012

rm(list = ls())
setwd("C:/Users/cjara/OneDrive - Universidad Católica de Chile/Mercado drogas/SENDA/2012")
datos <- read_dta("C:/Users/cjara/OneDrive - Universidad Católica de Chile/Mercado drogas/SENDA/2012/BBDD.dta")

table(datos$edad)

datos <- datos %>%
  filter(edad < 18)

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

#MARIHUANA

datos <- datos %>%
  mutate(mari = case_when(
    p38 %in% 1:2 ~ 1,  # Recodificar valores 1 o 2 como 1 (Año)
    p38 == 3 ~ 0       # Recodificar valor 3 como 2 (Más de un año)
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
    p83 %in% 1:2 ~ 1,  # Recodificar valores 1 o 2 como 1 (Año)
    p83 == 3 ~ 0       # Recodificar valor 3 como 2 (Más de un año)
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
    p61 %in% 1:2 ~ 1,  # Recodificar valores 1 o 2 como 1 (Año)
    p61 == 3 ~ 0       # Recodificar valor 3 como 2 (Más de un año)
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
    p107_2 %in% 1:2 ~ 1,  # Recodificar valores 1 o 2 como 1 (Año)
    p107_2 == 3 ~ 0       # Recodificar valor 3 como 2 (Más de un año)
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

#HEROINA

datos <- datos %>%
  mutate(heroina = case_when(
    p107_3 %in% 1:2 ~ 1,  # Recodificar valores 1 o 2 como 1 (Año)
    p107_3 == 3 ~ 0       # Recodificar valor 3 como 2 (Más de un año)
  ))

tasa_consumo_heroina <- datos %>%
  group_by(Region) %>%
  summarise(
    total_consumidores = sum(heroina * Fexp, na.rm = TRUE),  # Ignorar NA en los cálculos
    total_individuos = sum(Fexp, na.rm = TRUE),             # Ignorar NA en los cálculos
    tasa_consumo = ifelse(total_individuos == 0, NA, total_consumidores / total_individuos)  # Evitar división por cero
  )

tasa_consumo_heroina_total <- datos %>%
  summarise(
    total_consumidores = sum(heroina * Fexp, na.rm = TRUE),   # Total ponderado de consumidores
    total_individuos = sum(Fexp, na.rm = TRUE),            # Total ponderado de individuos
    tasa_consumo = ifelse(total_individuos == 0, NA, total_consumidores / total_individuos )  # Calcular la tasa como porcentaje
  )

#CRACK

datos <- datos %>%
  mutate(crack = case_when(
    p107_4 %in% 1:2 ~ 1,  # Recodificar valores 1 o 2 como 1 (Año)
    p107_4 == 3 ~ 0       # Recodificar valor 3 como 2 (Más de un año)
  ))

tasa_consumo_crack <- datos %>%
  group_by(Region) %>%
  summarise(
    total_consumidores = sum(crack * Fexp, na.rm = TRUE),  # Ignorar NA en los cálculos
    total_individuos = sum(Fexp, na.rm = TRUE),             # Ignorar NA en los cálculos
    tasa_consumo = ifelse(total_individuos == 0, NA, total_consumidores / total_individuos)  # Evitar división por cero
  )

tasa_consumo_crack_total <- datos %>%
  summarise(
    total_consumidores = sum(crack * Fexp, na.rm = TRUE),   # Total ponderado de consumidores
    total_individuos = sum(Fexp, na.rm = TRUE),            # Total ponderado de individuos
    tasa_consumo = ifelse(total_individuos == 0, NA, total_consumidores / total_individuos )  # Calcular la tasa como porcentaje
  )

#POPPER

datos <- datos %>%
  mutate(popper = case_when(
    p113_8 %in% 1:2 ~ 1,  # Recodificar valores 1 o 2 como 1 (Año)
    p113_8 == 3 ~ 0       # Recodificar valor 3 como 2 (Más de un año)
  ))

tasa_consumo_popper <- datos %>%
  group_by(Region) %>%
  summarise(
    total_consumidores = sum(popper * Fexp, na.rm = TRUE),  # Ignorar NA en los cálculos
    total_individuos = sum(Fexp, na.rm = TRUE),             # Ignorar NA en los cálculos
    tasa_consumo = ifelse(total_individuos == 0, NA, total_consumidores / total_individuos)  # Evitar división por cero
  )

tasa_consumo_popper_total <- datos %>%
  summarise(
    total_consumidores = sum(popper * Fexp, na.rm = TRUE),   # Total ponderado de consumidores
    total_individuos = sum(Fexp, na.rm = TRUE),            # Total ponderado de individuos
    tasa_consumo = ifelse(total_individuos == 0, NA, total_consumidores / total_individuos )  # Calcular la tasa como porcentaje
  )

#LSD

datos <- datos %>%
  mutate(lsd = case_when(
    p115_1 %in% 1:2 ~ 1,  # Recodificar valores 1 o 2 como 1 (Año)
    p115_1 == 3 ~ 0       # Recodificar valor 3 como 2 (Más de un año)
  ))

tasa_consumo_lsd <- datos %>%
  group_by(Region) %>%
  summarise(
    total_consumidores = sum(lsd * Fexp, na.rm = TRUE),  # Ignorar NA en los cálculos
    total_individuos = sum(Fexp, na.rm = TRUE),             # Ignorar NA en los cálculos
    tasa_consumo = ifelse(total_individuos == 0, NA, total_consumidores / total_individuos)  # Evitar división por cero
  )

tasa_consumo_lsd_total <- datos %>%
  summarise(
    total_consumidores = sum(lsd * Fexp, na.rm = TRUE),   # Total ponderado de consumidores
    total_individuos = sum(Fexp, na.rm = TRUE),            # Total ponderado de individuos
    tasa_consumo = ifelse(total_individuos == 0, NA, total_consumidores / total_individuos )  # Calcular la tasa como porcentaje
  )


#HONGOS

datos <- datos %>%
  mutate(hongos = case_when(
    p115_5 %in% 1:2 ~ 1,  # Recodificar valores 1 o 2 como 1 (Año)
    p115_5 == 3 ~ 0       # Recodificar valor 3 como 2 (Más de un año)
  ))

tasa_consumo_hongos <- datos %>%
  group_by(Region) %>%
  summarise(
    total_consumidores = sum(hongos * Fexp, na.rm = TRUE),  # Ignorar NA en los cálculos
    total_individuos = sum(Fexp, na.rm = TRUE),             # Ignorar NA en los cálculos
    tasa_consumo = ifelse(total_individuos == 0, NA, total_consumidores / total_individuos)  # Evitar división por cero
  )

tasa_consumo_hongos_total <- datos %>%
  summarise(
    total_consumidores = sum(hongos * Fexp, na.rm = TRUE),   # Total ponderado de consumidores
    total_individuos = sum(Fexp, na.rm = TRUE),            # Total ponderado de individuos
    tasa_consumo = ifelse(total_individuos == 0, NA, total_consumidores / total_individuos )  # Calcular la tasa como porcentaje
  )

#SAN PEDRO

datos <- datos %>%
  mutate(sanpedro = case_when(
    p115_3 %in% 1:2 ~ 1,  # Recodificar valores 1 o 2 como 1 (Año)
    p115_3 == 3 ~ 0       # Recodificar valor 3 como 2 (Más de un año)
  ))

tasa_consumo_sanpedro <- datos %>%
  group_by(Region) %>%
  summarise(
    total_consumidores = sum(sanpedro * Fexp, na.rm = TRUE),  # Ignorar NA en los cálculos
    total_individuos = sum(Fexp, na.rm = TRUE),             # Ignorar NA en los cálculos
    tasa_consumo = ifelse(total_individuos == 0, NA, total_consumidores / total_individuos)  # Evitar división por cero
  )

tasa_consumo_sanpedro_total <- datos %>%
  summarise(
    total_consumidores = sum(sanpedro * Fexp, na.rm = TRUE),   # Total ponderado de consumidores
    total_individuos = sum(Fexp, na.rm = TRUE),            # Total ponderado de individuos
    tasa_consumo = ifelse(total_individuos == 0, NA, total_consumidores / total_individuos )  # Calcular la tasa como porcentaje
  )

#POLVO DE ÁNGEL

datos <- datos %>%
  mutate(polvo = case_when(
    p115_2 %in% 1:2 ~ 1,  # Recodificar valores 1 o 2 como 1 (Año)
    p115_2 == 3 ~ 0       # Recodificar valor 3 como 2 (Más de un año)
  ))

tasa_consumo_polvo <- datos %>%
  group_by(Region) %>%
  summarise(
    total_consumidores = sum(polvo * Fexp, na.rm = TRUE),  # Ignorar NA en los cálculos
    total_individuos = sum(Fexp, na.rm = TRUE),             # Ignorar NA en los cálculos
    tasa_consumo = ifelse(total_individuos == 0, NA, total_consumidores / total_individuos)  # Evitar división por cero
  )

tasa_consumo_polvo_total <- datos %>%
  summarise(
    total_consumidores = sum(polvo * Fexp, na.rm = TRUE),   # Total ponderado de consumidores
    total_individuos = sum(Fexp, na.rm = TRUE),            # Total ponderado de individuos
    tasa_consumo = ifelse(total_individuos == 0, NA, total_consumidores / total_individuos )  # Calcular la tasa como porcentaje
  )

#TRANQUILIZANTES

datos <- datos %>% 
  mutate(p119_1 = ifelse(p119_1 %in% c(1, 2), 1, 0),
         p119_2 = ifelse(p119_2 %in% c(1, 2), 1, 0),
         p119_3 = ifelse(p119_3 %in% c(1, 2), 1, 0),
         p119_4 = ifelse(p119_4 %in% c(1, 2), 1, 0),
         p119_5 = ifelse(p119_5 %in% c(1, 2), 1, 0),
         p119_6 = ifelse(p119_6 %in% c(1, 2), 1, 0),
         p119_7 = ifelse(p119_7 %in% c(1, 2), 1, 0),
         p119_8 = ifelse(p119_8 %in% c(1, 2), 1, 0),
         p119_9 = ifelse(p119_9 %in% c(1, 2), 1, 0)) %>%
  mutate(tranquilizantes = case_when(
    p119_1 == 1 | p119_2 == 1 | p119_3 == 1 | p119_4 == 1 | 
      p119_5 == 1 | p119_6 == 1 | p119_7 == 1 | p119_8 == 1 | p119_9 == 1 ~ 1,
    TRUE ~ 0
  ))


tasa_consumo_tranquilizantes <- datos %>%
  group_by(Region) %>%
  summarise(
    total_consumidores = sum(tranquilizantes * Fexp, na.rm = TRUE),  # Ignorar NA en los cálculos
    total_individuos = sum(Fexp, na.rm = TRUE),             # Ignorar NA en los cálculos
    tasa_consumo = ifelse(total_individuos == 0, NA, total_consumidores / total_individuos)  # Evitar división por cero
  )

tasa_consumo_tranquilizantes_total <- datos %>%
  summarise(
    total_consumidores = sum(tranquilizantes * Fexp, na.rm = TRUE),   # Total ponderado de consumidores
    total_individuos = sum(Fexp, na.rm = TRUE),            # Total ponderado de individuos
    tasa_consumo = ifelse(total_individuos == 0, NA, total_consumidores / total_individuos )  # Calcular la tasa como porcentaje
  )

#ESTIMULANTES

datos <- datos %>% 
  mutate(p109_1 = ifelse(p109_1 %in% c(1, 2), 1, 0),
         p109_2 = ifelse(p109_2 %in% c(1, 2), 1, 0),
         p109_3 = ifelse(p109_3 %in% c(1, 2), 1, 0),
         p109_4 = ifelse(p109_4 %in% c(1, 2), 1, 0),
         p109_6 = ifelse(p109_6 %in% c(1, 2), 1, 0),
         p109_7 = ifelse(p109_7 %in% c(1, 2), 1, 0),
         p109_8 = ifelse(p109_8 %in% c(1, 2), 1, 0),
         p109_5 = ifelse(p109_5 %in% c(1, 2), 1, 0)) %>%
  mutate(estimulantes = case_when(
    p109_1 == 1 | p109_2 == 1 | p109_3 == 1 | p109_4 == 1 | 
      p109_5 == 1 | p109_6 == 1 | p109_7 == 1 | p109_8 == 1 ~ 1,
    TRUE ~ 0
  ))


tasa_consumo_estimulantes <- datos %>%
  group_by(Region) %>%
  summarise(
    total_consumidores = sum(estimulantes * Fexp, na.rm = TRUE),  # Ignorar NA en los cálculos
    total_individuos = sum(Fexp, na.rm = TRUE),             # Ignorar NA en los cálculos
    tasa_consumo = ifelse(total_individuos == 0, NA, total_consumidores / total_individuos)  # Evitar división por cero
  )

tasa_consumo_estimulantes_total <- datos %>%
  summarise(
    total_consumidores = sum(estimulantes * Fexp, na.rm = TRUE),   # Total ponderado de consumidores
    total_individuos = sum(Fexp, na.rm = TRUE),            # Total ponderado de individuos
    tasa_consumo = ifelse(total_individuos == 0, NA, total_consumidores / total_individuos )  # Calcular la tasa como porcentaje
  )

# COCAINAS
datos <- datos %>%
  mutate(cocainas = if_else(coca == 1 | pasta == 1 | crack == 1, 1, 0))
table(datos$cocainas)

tasa_cocainas <- datos %>%
  group_by(Region) %>%
  summarise(
    total_consumidores = sum(cocainas * Fexp, na.rm = TRUE),  # Ignorar NA en los cálculos
    total_individuos = sum(Fexp, na.rm = TRUE),             # Ignorar NA en los cálculos
    tasa_consumo = ifelse(total_individuos == 0, NA, total_consumidores / total_individuos)  # Evitar división por cero
  )

tasa_cocainas_total <- datos %>%
  summarise(
    total_consumidores = sum(cocainas * Fexp, na.rm = TRUE),   # Total ponderado de consumidores
    total_individuos = sum(Fexp, na.rm = TRUE),            # Total ponderado de individuos
    tasa_consumo = ifelse(total_individuos == 0, NA, total_consumidores / total_individuos )  # Calcular la tasa como porcentaje
  )


# ALUCINOGENOS

datos <- datos %>%
  mutate(alucinogenos = if_else(lsd == 1 | hongos == 1 | sanpedro == 1 |polvo == 1 , 1, 0))
table(datos$alucinogenos)

tasa_alucinogenos <- datos %>%
  group_by(Region) %>%
  summarise(
    total_consumidores = sum(alucinogenos * Fexp, na.rm = TRUE),  # Ignorar NA en los cálculos
    total_individuos = sum(Fexp, na.rm = TRUE),             # Ignorar NA en los cálculos
    tasa_consumo = ifelse(total_individuos == 0, NA, total_consumidores / total_individuos)  # Evitar división por cero
  )

tasa_alucinogenos_total <- datos %>%
  summarise(
    total_consumidores = sum(alucinogenos * Fexp, na.rm = TRUE),   # Total ponderado de consumidores
    total_individuos = sum(Fexp, na.rm = TRUE),            # Total ponderado de individuos
    tasa_consumo = ifelse(total_individuos == 0, NA, total_consumidores / total_individuos )  # Calcular la tasa como porcentaje
  )

# OTRAS

datos <- datos %>%
  mutate(otras = if_else(lsd == 1 | hongos == 1 | sanpedro == 1 |polvo == 1 | coca == 1 | pasta == 1 | crack == 1 | extasis == 1 | heroina == 1 | popper == 1 , 1, 0))
table(datos$otras)

tasa_otras <- datos %>%
  group_by(Region) %>%
  summarise(
    total_consumidores = sum(otras * Fexp, na.rm = TRUE),  # Ignorar NA en los cálculos
    total_individuos = sum(Fexp, na.rm = TRUE),             # Ignorar NA en los cálculos
    tasa_consumo = ifelse(total_individuos == 0, NA, total_consumidores / total_individuos)  # Evitar división por cero
  )

tasa_otras_total <- datos %>%
  summarise(
    total_consumidores = sum(otras * Fexp, na.rm = TRUE),   # Total ponderado de consumidores
    total_individuos = sum(Fexp, na.rm = TRUE),            # Total ponderado de individuos
    tasa_consumo = ifelse(total_individuos == 0, NA, total_consumidores / total_individuos )  # Calcular la tasa como porcentaje
  )

#EXCEL 

write_xlsx(
  list(
    "Tasa mari" = tasa_consumo_mari,
    "Tasa mari total" = tasa_consumo_mari_total,
    "Tasa coca" = tasa_consumo_coca,
    "Tasa coca total" = tasa_consumo_coca_total,
    "Tasa pasta" = tasa_consumo_pasta,
    "Tasa pasta total" = tasa_consumo_pasta_total,
    "Tasa extasis" = tasa_consumo_extasis,
    "Tasa extasis total" = tasa_consumo_extasis_total,
    "Tasa heroina" = tasa_consumo_heroina,
    "Tasa heroina total" = tasa_consumo_heroina_total,
    "Tasa crack" = tasa_consumo_crack,
    "Tasa crack total" = tasa_consumo_crack_total,
    "Tasa popper" = tasa_consumo_popper,
    "Tasa popper total" = tasa_consumo_popper_total,
    "Tasa lsd" = tasa_consumo_lsd,
    "Tasa lsd total" = tasa_consumo_lsd_total,
    "Tasa hongos" = tasa_consumo_hongos,
    "Tasa hongos total" = tasa_consumo_hongos_total,
    "Tasa san pedro" = tasa_consumo_sanpedro,
    "Tasa san pedro total" = tasa_consumo_sanpedro_total,
    "Tasa polvo de angel" = tasa_consumo_polvo,
    "Tasa polvo de angel total" = tasa_consumo_polvo_total,
    "Tasa tranquilizantes sr" = tasa_consumo_tranquilizantes,
    "Tasa tranquilizantes total" = tasa_consumo_tranquilizantes_total,
    "Tasa estimulantes sr" = tasa_consumo_estimulantes,
    "Tasa estimulantes total" = tasa_consumo_estimulantes_total,
    "Tasa cocainas" = tasa_cocainas,
    "Tasa cocainas total" = tasa_cocainas_total,
    "Tasa alucinogenos" = tasa_alucinogenos,
    "Tasa alucinogenos total" = tasa_alucinogenos_total,
    "Tasa otras" = tasa_otras,
    "Tasa_otras total" = tasa_otras_total
  ),
  "tasa_consumo2.xlsx"
)

#2010

rm(list = ls())
setwd("C:/Users/cjara/OneDrive - Universidad Católica de Chile/Mercado drogas/SENDA/2010")
datos <- read_dta("C:/Users/cjara/OneDrive - Universidad Católica de Chile/Mercado drogas/SENDA/2010/BBDD.dta")

table(datos$pedadent)
datos <- datos %>%
  rename(edad = pedadent)

datos <- datos %>%
  filter(edad < 18)

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

datos <- datos %>%
  mutate(mari = case_when(
    p036 %in% 1:2 ~ 1,  # Recodificar valores 1 o 2 como 1 (Año)
    p036 == 3 ~ 0       # Recodificar valor 3 como 2 (Más de un año)
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
    p080 %in% 1:2 ~ 1,  # Recodificar valores 1 o 2 como 1 (Año)
    p080 == 3 ~ 0       # Recodificar valor 3 como 2 (Más de un año)
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
    p058 %in% 1:2 ~ 1,  # Recodificar valores 1 o 2 como 1 (Año)
    p058 == 3 ~ 0       # Recodificar valor 3 como 2 (Más de un año)
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
    p106 %in% 1:2 ~ 1,  # Recodificar valores 1 o 2 como 1 (Año)
    p106 == 3 ~ 0       # Recodificar valor 3 como 2 (Más de un año)
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

#HEROINA

datos <- datos %>%
  mutate(heroina = case_when(
    p104 %in% 1:2 ~ 1,  # Recodificar valores 1 o 2 como 1 (Año)
    p104 == 3 ~ 0       # Recodificar valor 3 como 2 (Más de un año)
  ))

tasa_consumo_heroina <- datos %>%
  group_by(Region) %>%
  summarise(
    total_consumidores = sum(heroina * Fexp, na.rm = TRUE),  # Ignorar NA en los cálculos
    total_individuos = sum(Fexp, na.rm = TRUE),             # Ignorar NA en los cálculos
    tasa_consumo = ifelse(total_individuos == 0, NA, total_consumidores / total_individuos)  # Evitar división por cero
  )

tasa_consumo_heroina_total <- datos %>%
  summarise(
    total_consumidores = sum(heroina * Fexp, na.rm = TRUE),   # Total ponderado de consumidores
    total_individuos = sum(Fexp, na.rm = TRUE),            # Total ponderado de individuos
    tasa_consumo = ifelse(total_individuos == 0, NA, total_consumidores / total_individuos )  # Calcular la tasa como porcentaje
  )

#CRACK

datos <- datos %>%
  mutate(crack = case_when(
    p100 %in% 1:2 ~ 1,  # Recodificar valores 1 o 2 como 1 (Año)
    p100 == 3 ~ 0       # Recodificar valor 3 como 2 (Más de un año)
  ))

tasa_consumo_crack <- datos %>%
  group_by(Region) %>%
  summarise(
    total_consumidores = sum(crack * Fexp, na.rm = TRUE),  # Ignorar NA en los cálculos
    total_individuos = sum(Fexp, na.rm = TRUE),             # Ignorar NA en los cálculos
    tasa_consumo = ifelse(total_individuos == 0, NA, total_consumidores / total_individuos)  # Evitar división por cero
  )

tasa_consumo_crack_total <- datos %>%
  summarise(
    total_consumidores = sum(crack * Fexp, na.rm = TRUE),   # Total ponderado de consumidores
    total_individuos = sum(Fexp, na.rm = TRUE),            # Total ponderado de individuos
    tasa_consumo = ifelse(total_individuos == 0, NA, total_consumidores / total_individuos )  # Calcular la tasa como porcentaje
  )

#POPPER

datos <- datos %>%
  mutate(popper = case_when(
    p136 %in% 1:2 ~ 1,  # Recodificar valores 1 o 2 como 1 (Año)
    p136 == 3 ~ 0       # Recodificar valor 3 como 2 (Más de un año)
  ))

tasa_consumo_popper <- datos %>%
  group_by(Region) %>%
  summarise(
    total_consumidores = sum(popper * Fexp, na.rm = TRUE),  # Ignorar NA en los cálculos
    total_individuos = sum(Fexp, na.rm = TRUE),             # Ignorar NA en los cálculos
    tasa_consumo = ifelse(total_individuos == 0, NA, total_consumidores / total_individuos)  # Evitar división por cero
  )

tasa_consumo_popper_total <- datos %>%
  summarise(
    total_consumidores = sum(popper * Fexp, na.rm = TRUE),   # Total ponderado de consumidores
    total_individuos = sum(Fexp, na.rm = TRUE),            # Total ponderado de individuos
    tasa_consumo = ifelse(total_individuos == 0, NA, total_consumidores / total_individuos )  # Calcular la tasa como porcentaje
  )

#LSD

datos <- datos %>%
  mutate(lsd = case_when(
    p146 %in% 1:2 ~ 1,  # Recodificar valores 1 o 2 como 1 (Año)
    p146 == 3 ~ 0       # Recodificar valor 3 como 2 (Más de un año)
  ))

tasa_consumo_lsd <- datos %>%
  group_by(Region) %>%
  summarise(
    total_consumidores = sum(lsd * Fexp, na.rm = TRUE),  # Ignorar NA en los cálculos
    total_individuos = sum(Fexp, na.rm = TRUE),             # Ignorar NA en los cálculos
    tasa_consumo = ifelse(total_individuos == 0, NA, total_consumidores / total_individuos)  # Evitar división por cero
  )

tasa_consumo_lsd_total <- datos %>%
  summarise(
    total_consumidores = sum(lsd * Fexp, na.rm = TRUE),   # Total ponderado de consumidores
    total_individuos = sum(Fexp, na.rm = TRUE),            # Total ponderado de individuos
    tasa_consumo = ifelse(total_individuos == 0, NA, total_consumidores / total_individuos )  # Calcular la tasa como porcentaje
  )


#HONGOS

datos <- datos %>%
  mutate(hongos = case_when(
    p154 %in% 1:2 ~ 1,  # Recodificar valores 1 o 2 como 1 (Año)
    p154  == 3 ~ 0       # Recodificar valor 3 como 2 (Más de un año)
  ))

tasa_consumo_hongos <- datos %>%
  group_by(Region) %>%
  summarise(
    total_consumidores = sum(hongos * Fexp, na.rm = TRUE),  # Ignorar NA en los cálculos
    total_individuos = sum(Fexp, na.rm = TRUE),             # Ignorar NA en los cálculos
    tasa_consumo = ifelse(total_individuos == 0, NA, total_consumidores / total_individuos)  # Evitar división por cero
  )

tasa_consumo_hongos_total <- datos %>%
  summarise(
    total_consumidores = sum(hongos * Fexp, na.rm = TRUE),   # Total ponderado de consumidores
    total_individuos = sum(Fexp, na.rm = TRUE),            # Total ponderado de individuos
    tasa_consumo = ifelse(total_individuos == 0, NA, total_consumidores / total_individuos )  # Calcular la tasa como porcentaje
  )

#SAN PEDRO

datos <- datos %>%
  mutate(sanpedro = case_when(
    p150 %in% 1:2 ~ 1,  # Recodificar valores 1 o 2 como 1 (Año)
    p150 == 3 ~ 0       # Recodificar valor 3 como 2 (Más de un año)
  ))

tasa_consumo_sanpedro <- datos %>%
  group_by(Region) %>%
  summarise(
    total_consumidores = sum(sanpedro * Fexp, na.rm = TRUE),  # Ignorar NA en los cálculos
    total_individuos = sum(Fexp, na.rm = TRUE),             # Ignorar NA en los cálculos
    tasa_consumo = ifelse(total_individuos == 0, NA, total_consumidores / total_individuos)  # Evitar división por cero
  )

tasa_consumo_sanpedro_total <- datos %>%
  summarise(
    total_consumidores = sum(sanpedro * Fexp, na.rm = TRUE),   # Total ponderado de consumidores
    total_individuos = sum(Fexp, na.rm = TRUE),            # Total ponderado de individuos
    tasa_consumo = ifelse(total_individuos == 0, NA, total_consumidores / total_individuos )  # Calcular la tasa como porcentaje
  )

#POLVO DE ÁNGEL

datos <- datos %>%
  mutate(polvo = case_when(
    p148 %in% 1:2 ~ 1,  # Recodificar valores 1 o 2 como 1 (Año)
    p148 == 3 ~ 0       # Recodificar valor 3 como 2 (Más de un año)
  ))

tasa_consumo_polvo <- datos %>%
  group_by(Region) %>%
  summarise(
    total_consumidores = sum(polvo * Fexp, na.rm = TRUE),  # Ignorar NA en los cálculos
    total_individuos = sum(Fexp, na.rm = TRUE),             # Ignorar NA en los cálculos
    tasa_consumo = ifelse(total_individuos == 0, NA, total_consumidores / total_individuos)  # Evitar división por cero
  )

tasa_consumo_polvo_total <- datos %>%
  summarise(
    total_consumidores = sum(polvo * Fexp, na.rm = TRUE),   # Total ponderado de consumidores
    total_individuos = sum(Fexp, na.rm = TRUE),            # Total ponderado de individuos
    tasa_consumo = ifelse(total_individuos == 0, NA, total_consumidores / total_individuos )  # Calcular la tasa como porcentaje
  )

#TRANQUILIZANTES

datos <- datos %>% 
  mutate(p170 = ifelse(p170 %in% c(1, 2), 1, 0),
         p172 = ifelse(p172 %in% c(1, 2), 1, 0),
         p174 = ifelse(p174 %in% c(1, 2), 1, 0),
         p176 = ifelse(p176 %in% c(1, 2), 1, 0),
         p178 = ifelse(p178 %in% c(1, 2), 1, 0),
         p180 = ifelse(p180 %in% c(1, 2), 1, 0),
         p182 = ifelse(p182 %in% c(1, 2), 1, 0),
         p184 = ifelse(p184 %in% c(1, 2), 1, 0),
         p186 = ifelse(p186 %in% c(1, 2), 1, 0)) %>%
  mutate(tranquilizantes = case_when(
    p170 == 1 | p172 == 1 | p174 == 1 | p176 == 1 | 
      p178 == 1 | p180 == 1 | p182 == 1 | p184 == 1 | p186 == 1 ~ 1,
    TRUE ~ 0
  ))


tasa_consumo_tranquilizantes <- datos %>%
  group_by(Region) %>%
  summarise(
    total_consumidores = sum(tranquilizantes * Fexp, na.rm = TRUE),  # Ignorar NA en los cálculos
    total_individuos = sum(Fexp, na.rm = TRUE),             # Ignorar NA en los cálculos
    tasa_consumo = ifelse(total_individuos == 0, NA, total_consumidores / total_individuos)  # Evitar división por cero
  )

tasa_consumo_tranquilizantes_total <- datos %>%
  summarise(
    total_consumidores = sum(tranquilizantes * Fexp, na.rm = TRUE),   # Total ponderado de consumidores
    total_individuos = sum(Fexp, na.rm = TRUE),            # Total ponderado de individuos
    tasa_consumo = ifelse(total_individuos == 0, NA, total_consumidores / total_individuos )  # Calcular la tasa como porcentaje
  )

#ESTIMULANTES

datos <- datos %>% 
  mutate(p108 = ifelse(p108 %in% c(1, 2), 1, 0),
         p110 = ifelse(p110 %in% c(1, 2), 1, 0),
         p112 = ifelse(p112 %in% c(1, 2), 1, 0),
         p114 = ifelse(p114 %in% c(1, 2), 1, 0),
         p116 = ifelse(p116 %in% c(1, 2), 1, 0)) %>%
  mutate(estimulantes = case_when(
    p108 == 1 | p110 == 1 | p112 == 1 | p114 == 1 | 
      p116 == 1 ~ 1,
    TRUE ~ 0
  ))


tasa_consumo_estimulantes <- datos %>%
  group_by(Region) %>%
  summarise(
    total_consumidores = sum(estimulantes * Fexp, na.rm = TRUE),  # Ignorar NA en los cálculos
    total_individuos = sum(Fexp, na.rm = TRUE),             # Ignorar NA en los cálculos
    tasa_consumo = ifelse(total_individuos == 0, NA, total_consumidores / total_individuos)  # Evitar división por cero
  )

tasa_consumo_estimulantes_total <- datos %>%
  summarise(
    total_consumidores = sum(estimulantes * Fexp, na.rm = TRUE),   # Total ponderado de consumidores
    total_individuos = sum(Fexp, na.rm = TRUE),            # Total ponderado de individuos
    tasa_consumo = ifelse(total_individuos == 0, NA, total_consumidores / total_individuos )  # Calcular la tasa como porcentaje
  )

# COCAINAS
datos <- datos %>%
  mutate(cocainas = if_else(coca == 1 | pasta == 1 | crack == 1, 1, 0))
table(datos$cocainas)

tasa_cocainas <- datos %>%
  group_by(Region) %>%
  summarise(
    total_consumidores = sum(cocainas * Fexp, na.rm = TRUE),  # Ignorar NA en los cálculos
    total_individuos = sum(Fexp, na.rm = TRUE),             # Ignorar NA en los cálculos
    tasa_consumo = ifelse(total_individuos == 0, NA, total_consumidores / total_individuos)  # Evitar división por cero
  )

tasa_cocainas_total <- datos %>%
  summarise(
    total_consumidores = sum(cocainas * Fexp, na.rm = TRUE),   # Total ponderado de consumidores
    total_individuos = sum(Fexp, na.rm = TRUE),            # Total ponderado de individuos
    tasa_consumo = ifelse(total_individuos == 0, NA, total_consumidores / total_individuos )  # Calcular la tasa como porcentaje
  )


# ALUCINOGENOS

datos <- datos %>%
  mutate(alucinogenos = if_else(lsd == 1 | hongos == 1 | sanpedro == 1 |polvo == 1 , 1, 0))
table(datos$alucinogenos)

tasa_alucinogenos <- datos %>%
  group_by(Region) %>%
  summarise(
    total_consumidores = sum(alucinogenos * Fexp, na.rm = TRUE),  # Ignorar NA en los cálculos
    total_individuos = sum(Fexp, na.rm = TRUE),             # Ignorar NA en los cálculos
    tasa_consumo = ifelse(total_individuos == 0, NA, total_consumidores / total_individuos)  # Evitar división por cero
  )

tasa_alucinogenos_total <- datos %>%
  summarise(
    total_consumidores = sum(alucinogenos * Fexp, na.rm = TRUE),   # Total ponderado de consumidores
    total_individuos = sum(Fexp, na.rm = TRUE),            # Total ponderado de individuos
    tasa_consumo = ifelse(total_individuos == 0, NA, total_consumidores / total_individuos )  # Calcular la tasa como porcentaje
  )

# OTRAS

datos <- datos %>%
  mutate(otras = if_else(lsd == 1 | hongos == 1 | sanpedro == 1 |polvo == 1 | coca == 1 | pasta == 1 | crack == 1 | extasis == 1 | heroina == 1 | popper == 1 , 1, 0))
table(datos$otras)

tasa_otras <- datos %>%
  group_by(Region) %>%
  summarise(
    total_consumidores = sum(otras * Fexp, na.rm = TRUE),  # Ignorar NA en los cálculos
    total_individuos = sum(Fexp, na.rm = TRUE),             # Ignorar NA en los cálculos
    tasa_consumo = ifelse(total_individuos == 0, NA, total_consumidores / total_individuos)  # Evitar división por cero
  )

tasa_otras_total <- datos %>%
  summarise(
    total_consumidores = sum(otras * Fexp, na.rm = TRUE),   # Total ponderado de consumidores
    total_individuos = sum(Fexp, na.rm = TRUE),            # Total ponderado de individuos
    tasa_consumo = ifelse(total_individuos == 0, NA, total_consumidores / total_individuos )  # Calcular la tasa como porcentaje
  )

#EXCEL 

write_xlsx(
  list(
    "Tasa mari" = tasa_consumo_mari,
    "Tasa mari total" = tasa_consumo_mari_total,
    "Tasa coca" = tasa_consumo_coca,
    "Tasa coca total" = tasa_consumo_coca_total,
    "Tasa pasta" = tasa_consumo_pasta,
    "Tasa pasta total" = tasa_consumo_pasta_total,
    "Tasa extasis" = tasa_consumo_extasis,
    "Tasa extasis total" = tasa_consumo_extasis_total,
    "Tasa heroina" = tasa_consumo_heroina,
    "Tasa heroina total" = tasa_consumo_heroina_total,
    "Tasa crack" = tasa_consumo_crack,
    "Tasa crack total" = tasa_consumo_crack_total,
    "Tasa popper" = tasa_consumo_popper,
    "Tasa popper total" = tasa_consumo_popper_total,
    "Tasa lsd" = tasa_consumo_lsd,
    "Tasa lsd total" = tasa_consumo_lsd_total,
    "Tasa hongos" = tasa_consumo_hongos,
    "Tasa hongos total" = tasa_consumo_hongos_total,
    "Tasa san pedro" = tasa_consumo_sanpedro,
    "Tasa san pedro total" = tasa_consumo_sanpedro_total,
    "Tasa polvo de angel" = tasa_consumo_polvo,
    "Tasa polvo de angel total" = tasa_consumo_polvo_total,
    "Tasa tranquilizantes sr" = tasa_consumo_tranquilizantes,
    "Tasa tranquilizantes total" = tasa_consumo_tranquilizantes_total,
    "Tasa estimulantes sr" = tasa_consumo_estimulantes,
    "Tasa estimulantes total" = tasa_consumo_estimulantes_total,
    "Tasa cocainas" = tasa_cocainas,
    "Tasa cocainas total" = tasa_cocainas_total,
    "Tasa alucinogenos" = tasa_alucinogenos,
    "Tasa alucinogenos total" = tasa_alucinogenos_total,
    "Tasa otras" = tasa_otras,
    "Tasa_otras total" = tasa_otras_total
  ),
  "tasa_consumo2.xlsx"
)

#2008

rm(list = ls())
setwd("C:/Users/cjara/OneDrive - Universidad Católica de Chile/Mercado drogas/SENDA/2008")
datos <- read_dta("C:/Users/cjara/OneDrive - Universidad Católica de Chile/Mercado drogas/SENDA/2008/BBDD.dta")

table(datos$edad)

datos <- datos %>%
  filter(edad < 18)

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

datos <- datos %>%
  mutate(mari = case_when(
    q36 %in% 1:2 ~ 1,  # Recodificar valores 1 o 2 como 1 (Año)
    q36 == 3 ~ 0       # Recodificar valor 3 como 2 (Más de un año)
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
    q82 %in% 1:2 ~ 1,  # Recodificar valores 1 o 2 como 1 (Año)
    q82 == 3 ~ 0       # Recodificar valor 3 como 2 (Más de un año)
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
    q59 %in% 1:2 ~ 1,  # Recodificar valores 1 o 2 como 1 (Año)
    q59 == 3 ~ 0       # Recodificar valor 3 como 2 (Más de un año)
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
    q109 %in% 1:2 ~ 1,  # Recodificar valores 1 o 2 como 1 (Año)
    q109 == 3 ~ 0       # Recodificar valor 3 como 2 (Más de un año)
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

#HEROINA

datos <- datos %>%
  mutate(heroina = case_when(
    q107 %in% 1:2 ~ 1,  # Recodificar valores 1 o 2 como 1 (Año)
    q107 == 3 ~ 0       # Recodificar valor 3 como 2 (Más de un año)
  ))

tasa_consumo_heroina <- datos %>%
  group_by(Region) %>%
  summarise(
    total_consumidores = sum(heroina * Fexp, na.rm = TRUE),  # Ignorar NA en los cálculos
    total_individuos = sum(Fexp, na.rm = TRUE),             # Ignorar NA en los cálculos
    tasa_consumo = ifelse(total_individuos == 0, NA, total_consumidores / total_individuos)  # Evitar división por cero
  )

tasa_consumo_heroina_total <- datos %>%
  summarise(
    total_consumidores = sum(heroina * Fexp, na.rm = TRUE),   # Total ponderado de consumidores
    total_individuos = sum(Fexp, na.rm = TRUE),            # Total ponderado de individuos
    tasa_consumo = ifelse(total_individuos == 0, NA, total_consumidores / total_individuos )  # Calcular la tasa como porcentaje
  )

#CRACK

datos <- datos %>%
  mutate(crack = case_when(
    q103 %in% 1:2 ~ 1,  # Recodificar valores 1 o 2 como 1 (Año)
    q103 == 3 ~ 0       # Recodificar valor 3 como 2 (Más de un año)
  ))

tasa_consumo_crack <- datos %>%
  group_by(Region) %>%
  summarise(
    total_consumidores = sum(crack * Fexp, na.rm = TRUE),  # Ignorar NA en los cálculos
    total_individuos = sum(Fexp, na.rm = TRUE),             # Ignorar NA en los cálculos
    tasa_consumo = ifelse(total_individuos == 0, NA, total_consumidores / total_individuos)  # Evitar división por cero
  )

tasa_consumo_crack_total <- datos %>%
  summarise(
    total_consumidores = sum(crack * Fexp, na.rm = TRUE),   # Total ponderado de consumidores
    total_individuos = sum(Fexp, na.rm = TRUE),            # Total ponderado de individuos
    tasa_consumo = ifelse(total_individuos == 0, NA, total_consumidores / total_individuos )  # Calcular la tasa como porcentaje
  )

#POPPER

datos <- datos %>%
  mutate(popper = case_when(
    q122 %in% 1:2 ~ 1,  # Recodificar valores 1 o 2 como 1 (Año)
    q122 == 3 ~ 0       # Recodificar valor 3 como 2 (Más de un año)
  ))

tasa_consumo_popper <- datos %>%
  group_by(Region) %>%
  summarise(
    total_consumidores = sum(popper * Fexp, na.rm = TRUE),  # Ignorar NA en los cálculos
    total_individuos = sum(Fexp, na.rm = TRUE),             # Ignorar NA en los cálculos
    tasa_consumo = ifelse(total_individuos == 0, NA, total_consumidores / total_individuos)  # Evitar división por cero
  )

tasa_consumo_popper_total <- datos %>%
  summarise(
    total_consumidores = sum(popper * Fexp, na.rm = TRUE),   # Total ponderado de consumidores
    total_individuos = sum(Fexp, na.rm = TRUE),            # Total ponderado de individuos
    tasa_consumo = ifelse(total_individuos == 0, NA, total_consumidores / total_individuos )  # Calcular la tasa como porcentaje
  )

#LSD

datos <- datos %>%
  mutate(lsd = case_when(
    q111 %in% 1:2 ~ 1,  # Recodificar valores 1 o 2 como 1 (Año)
    q111 == 3 ~ 0       # Recodificar valor 3 como 2 (Más de un año)
  ))

tasa_consumo_lsd <- datos %>%
  group_by(Region) %>%
  summarise(
    total_consumidores = sum(lsd * Fexp, na.rm = TRUE),  # Ignorar NA en los cálculos
    total_individuos = sum(Fexp, na.rm = TRUE),             # Ignorar NA en los cálculos
    tasa_consumo = ifelse(total_individuos == 0, NA, total_consumidores / total_individuos)  # Evitar división por cero
  )

tasa_consumo_lsd_total <- datos %>%
  summarise(
    total_consumidores = sum(lsd * Fexp, na.rm = TRUE),   # Total ponderado de consumidores
    total_individuos = sum(Fexp, na.rm = TRUE),            # Total ponderado de individuos
    tasa_consumo = ifelse(total_individuos == 0, NA, total_consumidores / total_individuos )  # Calcular la tasa como porcentaje
  )

#SAN PEDRO

datos <- datos %>%
  mutate(sanpedro = case_when(
    q113 %in% 1:2 ~ 1,  # Recodificar valores 1 o 2 como 1 (Año)
    q113 == 3 ~ 0       # Recodificar valor 3 como 2 (Más de un año)
  ))

tasa_consumo_sanpedro <- datos %>%
  group_by(Region) %>%
  summarise(
    total_consumidores = sum(sanpedro * Fexp, na.rm = TRUE),  # Ignorar NA en los cálculos
    total_individuos = sum(Fexp, na.rm = TRUE),             # Ignorar NA en los cálculos
    tasa_consumo = ifelse(total_individuos == 0, NA, total_consumidores / total_individuos)  # Evitar división por cero
  )

tasa_consumo_sanpedro_total <- datos %>%
  summarise(
    total_consumidores = sum(sanpedro * Fexp, na.rm = TRUE),   # Total ponderado de consumidores
    total_individuos = sum(Fexp, na.rm = TRUE),            # Total ponderado de individuos
    tasa_consumo = ifelse(total_individuos == 0, NA, total_consumidores / total_individuos )  # Calcular la tasa como porcentaje
  )

#POLVO DE ÁNGEL

datos <- datos %>%
  mutate(polvo = case_when(
    q112 %in% 1:2 ~ 1,  # Recodificar valores 1 o 2 como 1 (Año)
    q112 == 3 ~ 0       # Recodificar valor 3 como 2 (Más de un año)
  ))

tasa_consumo_polvo <- datos %>%
  group_by(Region) %>%
  summarise(
    total_consumidores = sum(polvo * Fexp, na.rm = TRUE),  # Ignorar NA en los cálculos
    total_individuos = sum(Fexp, na.rm = TRUE),             # Ignorar NA en los cálculos
    tasa_consumo = ifelse(total_individuos == 0, NA, total_consumidores / total_individuos)  # Evitar división por cero
  )

tasa_consumo_polvo_total <- datos %>%
  summarise(
    total_consumidores = sum(polvo * Fexp, na.rm = TRUE),   # Total ponderado de consumidores
    total_individuos = sum(Fexp, na.rm = TRUE),            # Total ponderado de individuos
    tasa_consumo = ifelse(total_individuos == 0, NA, total_consumidores / total_individuos )  # Calcular la tasa como porcentaje
  )

#TRANQUILIZANTES

datos <- datos %>% 
  mutate(q134 = ifelse(q134 %in% c(1, 2), 1, 0),
         q135 = ifelse(q135 %in% c(1, 2), 1, 0),
         q136 = ifelse(q136 %in% c(1, 2), 1, 0),
         q137 = ifelse(q137 %in% c(1, 2), 1, 0),
         q138 = ifelse(q138 %in% c(1, 2), 1, 0),
         q139 = ifelse(q139 %in% c(1, 2), 1, 0),
         q140 = ifelse(q140 %in% c(1, 2), 1, 0),
         q141 = ifelse(q141 %in% c(1, 2), 1, 0),
         q142 = ifelse(q142 %in% c(1, 2), 1, 0),
         q143 = ifelse(q143 %in% c(1, 2), 1, 0)) %>%
  mutate(tranquilizantes = case_when(
    q134 == 1 | q135 == 1 | q136 == 1 | q137 == 1 | 
      q138 == 1 | q139 == 1 | q140 == 1 | q141 == 1 | q142 == 1 | q143 == 1 ~ 1,
    TRUE ~ 0
  ))


tasa_consumo_tranquilizantes <- datos %>%
  group_by(Region) %>%
  summarise(
    total_consumidores = sum(tranquilizantes * Fexp, na.rm = TRUE),  # Ignorar NA en los cálculos
    total_individuos = sum(Fexp, na.rm = TRUE),             # Ignorar NA en los cálculos
    tasa_consumo = ifelse(total_individuos == 0, NA, total_consumidores / total_individuos)  # Evitar división por cero
  )

tasa_consumo_tranquilizantes_total <- datos %>%
  summarise(
    total_consumidores = sum(tranquilizantes * Fexp, na.rm = TRUE),   # Total ponderado de consumidores
    total_individuos = sum(Fexp, na.rm = TRUE),            # Total ponderado de individuos
    tasa_consumo = ifelse(total_individuos == 0, NA, total_consumidores / total_individuos )  # Calcular la tasa como porcentaje
  )

#ESTIMULANTES

datos <- datos %>% 
  mutate(q145 = ifelse(q145 %in% c(1, 2), 1, 0),
         q146 = ifelse(q146 %in% c(1, 2), 1, 0),
         q147 = ifelse(q147 %in% c(1, 2), 1, 0),
         q148 = ifelse(q148 %in% c(1, 2), 1, 0),
         q149 = ifelse(q149 %in% c(1, 2), 1, 0),
         q150 = ifelse(q150 %in% c(1, 2), 1, 0),
         q151 = ifelse(q151 %in% c(1, 2), 1, 0)) %>%
  mutate(estimulantes = case_when(
    q145 == 1 | q146 == 1 | q147 == 1 | q148 == 1 | 
      q149 == 1 | q150 == 1 | q151 == 1 ~ 1,
    TRUE ~ 0
  ))


tasa_consumo_estimulantes <- datos %>%
  group_by(Region) %>%
  summarise(
    total_consumidores = sum(estimulantes * Fexp, na.rm = TRUE),  # Ignorar NA en los cálculos
    total_individuos = sum(Fexp, na.rm = TRUE),             # Ignorar NA en los cálculos
    tasa_consumo = ifelse(total_individuos == 0, NA, total_consumidores / total_individuos)  # Evitar división por cero
  )

tasa_consumo_estimulantes_total <- datos %>%
  summarise(
    total_consumidores = sum(estimulantes * Fexp, na.rm = TRUE),   # Total ponderado de consumidores
    total_individuos = sum(Fexp, na.rm = TRUE),            # Total ponderado de individuos
    tasa_consumo = ifelse(total_individuos == 0, NA, total_consumidores / total_individuos )  # Calcular la tasa como porcentaje
  )

# COCAINAS
datos <- datos %>%
  mutate(cocainas = if_else(coca == 1 | pasta == 1 | crack == 1, 1, 0))
table(datos$cocainas)

tasa_cocainas <- datos %>%
  group_by(Region) %>%
  summarise(
    total_consumidores = sum(cocainas * Fexp, na.rm = TRUE),  # Ignorar NA en los cálculos
    total_individuos = sum(Fexp, na.rm = TRUE),             # Ignorar NA en los cálculos
    tasa_consumo = ifelse(total_individuos == 0, NA, total_consumidores / total_individuos)  # Evitar división por cero
  )

tasa_cocainas_total <- datos %>%
  summarise(
    total_consumidores = sum(cocainas * Fexp, na.rm = TRUE),   # Total ponderado de consumidores
    total_individuos = sum(Fexp, na.rm = TRUE),            # Total ponderado de individuos
    tasa_consumo = ifelse(total_individuos == 0, NA, total_consumidores / total_individuos )  # Calcular la tasa como porcentaje
  )


# ALUCINOGENOS

datos <- datos %>%
  mutate(alucinogenos = if_else(lsd == 1 | sanpedro == 1 |polvo == 1 , 1, 0))
table(datos$alucinogenos)

tasa_alucinogenos <- datos %>%
  group_by(Region) %>%
  summarise(
    total_consumidores = sum(alucinogenos * Fexp, na.rm = TRUE),  # Ignorar NA en los cálculos
    total_individuos = sum(Fexp, na.rm = TRUE),             # Ignorar NA en los cálculos
    tasa_consumo = ifelse(total_individuos == 0, NA, total_consumidores / total_individuos)  # Evitar división por cero
  )

tasa_alucinogenos_total <- datos %>%
  summarise(
    total_consumidores = sum(alucinogenos * Fexp, na.rm = TRUE),   # Total ponderado de consumidores
    total_individuos = sum(Fexp, na.rm = TRUE),            # Total ponderado de individuos
    tasa_consumo = ifelse(total_individuos == 0, NA, total_consumidores / total_individuos )  # Calcular la tasa como porcentaje
  )

# OTRAS

datos <- datos %>%
  mutate(otras = if_else(lsd == 1 | sanpedro == 1 |polvo == 1 | coca == 1 | pasta == 1 | crack == 1 | extasis == 1 | heroina == 1 | popper == 1 , 1, 0))
table(datos$otras)

tasa_otras <- datos %>%
  group_by(Region) %>%
  summarise(
    total_consumidores = sum(otras * Fexp, na.rm = TRUE),  # Ignorar NA en los cálculos
    total_individuos = sum(Fexp, na.rm = TRUE),             # Ignorar NA en los cálculos
    tasa_consumo = ifelse(total_individuos == 0, NA, total_consumidores / total_individuos)  # Evitar división por cero
  )

tasa_otras_total <- datos %>%
  summarise(
    total_consumidores = sum(otras * Fexp, na.rm = TRUE),   # Total ponderado de consumidores
    total_individuos = sum(Fexp, na.rm = TRUE),            # Total ponderado de individuos
    tasa_consumo = ifelse(total_individuos == 0, NA, total_consumidores / total_individuos )  # Calcular la tasa como porcentaje
  )

#EXCEL 

write_xlsx(
  list(
    "Tasa mari" = tasa_consumo_mari,
    "Tasa mari total" = tasa_consumo_mari_total,
    "Tasa coca" = tasa_consumo_coca,
    "Tasa coca total" = tasa_consumo_coca_total,
    "Tasa pasta" = tasa_consumo_pasta,
    "Tasa pasta total" = tasa_consumo_pasta_total,
    "Tasa extasis" = tasa_consumo_extasis,
    "Tasa extasis total" = tasa_consumo_extasis_total,
    "Tasa heroina" = tasa_consumo_heroina,
    "Tasa heroina total" = tasa_consumo_heroina_total,
    "Tasa crack" = tasa_consumo_crack,
    "Tasa crack total" = tasa_consumo_crack_total,
    "Tasa popper" = tasa_consumo_popper,
    "Tasa popper total" = tasa_consumo_popper_total,
    "Tasa lsd" = tasa_consumo_lsd,
    "Tasa lsd total" = tasa_consumo_lsd_total,
    "Tasa san pedro" = tasa_consumo_sanpedro,
    "Tasa san pedro total" = tasa_consumo_sanpedro_total,
    "Tasa polvo de angel" = tasa_consumo_polvo,
    "Tasa polvo de angel total" = tasa_consumo_polvo_total,
    "Tasa tranquilizantes sr" = tasa_consumo_tranquilizantes,
    "Tasa tranquilizantes total" = tasa_consumo_tranquilizantes_total,
    "Tasa estimulantes sr" = tasa_consumo_estimulantes,
    "Tasa estimulantes total" = tasa_consumo_estimulantes_total,
    "Tasa cocainas" = tasa_cocainas,
    "Tasa cocainas total" = tasa_cocainas_total,
    "Tasa alucinogenos" = tasa_alucinogenos,
    "Tasa alucinogenos total" = tasa_alucinogenos_total,
    "Tasa otras" = tasa_otras,
    "Tasa_otras total" = tasa_otras_total
  ),
  "tasa_consumo2.xlsx"
)

#2006

rm(list = ls())
setwd("C:/Users/cjara/OneDrive - Universidad Católica de Chile/Mercado drogas/SENDA/2006")
datos <- read_dta("C:/Users/cjara/OneDrive - Universidad Católica de Chile/Mercado drogas/SENDA/2006/BBDD.dta")

table(datos$edad)

datos <- datos %>%
  filter(edad < 18)

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

datos <- datos %>%
  mutate(mari = case_when(
    p41 %in% 1:2 ~ 1,  # Recodificar valores 1 o 2 como 1 (Año)
    p41 == 3 ~ 0       # Recodificar valor 3 como 2 (Más de un año)
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
    p87 %in% 1:2 ~ 1,  # Recodificar valores 1 o 2 como 1 (Año)
    p87 == 3 ~ 0       # Recodificar valor 3 como 2 (Más de un año)
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
    p64 %in% 1:2 ~ 1,  # Recodificar valores 1 o 2 como 1 (Año)
    p64 == 3 ~ 0       # Recodificar valor 3 como 2 (Más de un año)
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
    p123 %in% 1:2 ~ 1,  # Recodificar valores 1 o 2 como 1 (Año)
    p123 == 3 ~ 0       # Recodificar valor 3 como 2 (Más de un año)
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

#HEROINA

datos <- datos %>%
  mutate(heroina = case_when(
    p109 %in% 1:2 ~ 1,  # Recodificar valores 1 o 2 como 1 (Año)
    p109 == 3 ~ 0       # Recodificar valor 3 como 2 (Más de un año)
  ))

tasa_consumo_heroina <- datos %>%
  group_by(Region) %>%
  summarise(
    total_consumidores = sum(heroina * Fexp, na.rm = TRUE),  # Ignorar NA en los cálculos
    total_individuos = sum(Fexp, na.rm = TRUE),             # Ignorar NA en los cálculos
    tasa_consumo = ifelse(total_individuos == 0, NA, total_consumidores / total_individuos)  # Evitar división por cero
  )

tasa_consumo_heroina_total <- datos %>%
  summarise(
    total_consumidores = sum(heroina * Fexp, na.rm = TRUE),   # Total ponderado de consumidores
    total_individuos = sum(Fexp, na.rm = TRUE),            # Total ponderado de individuos
    tasa_consumo = ifelse(total_individuos == 0, NA, total_consumidores / total_individuos )  # Calcular la tasa como porcentaje
  )

#CRACK

datos <- datos %>%
  mutate(crack = case_when(
    p107 %in% 1:2 ~ 1,  # Recodificar valores 1 o 2 como 1 (Año)
    p107 == 3 ~ 0       # Recodificar valor 3 como 2 (Más de un año)
  ))

tasa_consumo_crack <- datos %>%
  group_by(Region) %>%
  summarise(
    total_consumidores = sum(crack * Fexp, na.rm = TRUE),  # Ignorar NA en los cálculos
    total_individuos = sum(Fexp, na.rm = TRUE),             # Ignorar NA en los cálculos
    tasa_consumo = ifelse(total_individuos == 0, NA, total_consumidores / total_individuos)  # Evitar división por cero
  )

tasa_consumo_crack_total <- datos %>%
  summarise(
    total_consumidores = sum(crack * Fexp, na.rm = TRUE),   # Total ponderado de consumidores
    total_individuos = sum(Fexp, na.rm = TRUE),            # Total ponderado de individuos
    tasa_consumo = ifelse(total_individuos == 0, NA, total_consumidores / total_individuos )  # Calcular la tasa como porcentaje
  )

#POPPER

datos <- datos %>%
  mutate(popper = case_when(
    p113_6 %in% 1:2 ~ 1,  # Recodificar valores 1 o 2 como 1 (Año)
    p113_6 == 3 ~ 0       # Recodificar valor 3 como 2 (Más de un año)
  ))

tasa_consumo_popper <- datos %>%
  group_by(Region) %>%
  summarise(
    total_consumidores = sum(popper * Fexp, na.rm = TRUE),  # Ignorar NA en los cálculos
    total_individuos = sum(Fexp, na.rm = TRUE),             # Ignorar NA en los cálculos
    tasa_consumo = ifelse(total_individuos == 0, NA, total_consumidores / total_individuos)  # Evitar división por cero
  )

tasa_consumo_popper_total <- datos %>%
  summarise(
    total_consumidores = sum(popper * Fexp, na.rm = TRUE),   # Total ponderado de consumidores
    total_individuos = sum(Fexp, na.rm = TRUE),            # Total ponderado de individuos
    tasa_consumo = ifelse(total_individuos == 0, NA, total_consumidores / total_individuos )  # Calcular la tasa como porcentaje
  )

#LSD

datos <- datos %>%
  mutate(lsd = case_when(
    p111_1 %in% 1:2 ~ 1,  # Recodificar valores 1 o 2 como 1 (Año)
    p111_1 == 3 ~ 0       # Recodificar valor 3 como 2 (Más de un año)
  ))

tasa_consumo_lsd <- datos %>%
  group_by(Region) %>%
  summarise(
    total_consumidores = sum(lsd * Fexp, na.rm = TRUE),  # Ignorar NA en los cálculos
    total_individuos = sum(Fexp, na.rm = TRUE),             # Ignorar NA en los cálculos
    tasa_consumo = ifelse(total_individuos == 0, NA, total_consumidores / total_individuos)  # Evitar división por cero
  )

tasa_consumo_lsd_total <- datos %>%
  summarise(
    total_consumidores = sum(lsd * Fexp, na.rm = TRUE),   # Total ponderado de consumidores
    total_individuos = sum(Fexp, na.rm = TRUE),            # Total ponderado de individuos
    tasa_consumo = ifelse(total_individuos == 0, NA, total_consumidores / total_individuos )  # Calcular la tasa como porcentaje
  )

#SAN PEDRO

datos <- datos %>%
  mutate(sanpedro = case_when(
    p111_3 %in% 1:2 ~ 1,  # Recodificar valores 1 o 2 como 1 (Año)
    p111_3 == 3 ~ 0       # Recodificar valor 3 como 2 (Más de un año)
  ))

tasa_consumo_sanpedro <- datos %>%
  group_by(Region) %>%
  summarise(
    total_consumidores = sum(sanpedro * Fexp, na.rm = TRUE),  # Ignorar NA en los cálculos
    total_individuos = sum(Fexp, na.rm = TRUE),             # Ignorar NA en los cálculos
    tasa_consumo = ifelse(total_individuos == 0, NA, total_consumidores / total_individuos)  # Evitar división por cero
  )

tasa_consumo_sanpedro_total <- datos %>%
  summarise(
    total_consumidores = sum(sanpedro * Fexp, na.rm = TRUE),   # Total ponderado de consumidores
    total_individuos = sum(Fexp, na.rm = TRUE),            # Total ponderado de individuos
    tasa_consumo = ifelse(total_individuos == 0, NA, total_consumidores / total_individuos )  # Calcular la tasa como porcentaje
  )

#POLVO DE ÁNGEL

datos <- datos %>%
  mutate(polvo = case_when(
    p111_2 %in% 1:2 ~ 1,  # Recodificar valores 1 o 2 como 1 (Año)
    p111_2 == 3 ~ 0       # Recodificar valor 3 como 2 (Más de un año)
  ))

tasa_consumo_polvo <- datos %>%
  group_by(Region) %>%
  summarise(
    total_consumidores = sum(polvo * Fexp, na.rm = TRUE),  # Ignorar NA en los cálculos
    total_individuos = sum(Fexp, na.rm = TRUE),             # Ignorar NA en los cálculos
    tasa_consumo = ifelse(total_individuos == 0, NA, total_consumidores / total_individuos)  # Evitar división por cero
  )

tasa_consumo_polvo_total <- datos %>%
  summarise(
    total_consumidores = sum(polvo * Fexp, na.rm = TRUE),   # Total ponderado de consumidores
    total_individuos = sum(Fexp, na.rm = TRUE),            # Total ponderado de individuos
    tasa_consumo = ifelse(total_individuos == 0, NA, total_consumidores / total_individuos )  # Calcular la tasa como porcentaje
  )

#TRANQUILIZANTES

datos <- datos %>% 
  mutate(p117_1 = ifelse(p117_1 %in% c(1, 2), 1, 0),
         p117_2 = ifelse(p117_2 %in% c(1, 2), 1, 0),
         p117_3 = ifelse(p117_3 %in% c(1, 2), 1, 0),
         p117_4 = ifelse(p117_4 %in% c(1, 2), 1, 0),
         p117_5 = ifelse(p117_5 %in% c(1, 2), 1, 0),
         p117_6 = ifelse(p117_6 %in% c(1, 2), 1, 0),
         p117_7 = ifelse(p117_7 %in% c(1, 2), 1, 0),
         p117_8 = ifelse(p117_8 %in% c(1, 2), 1, 0),
         p117_9 = ifelse(p117_9 %in% c(1, 2), 1, 0),
         p117_10 = ifelse(p117_10 %in% c(1, 2), 1, 0)) %>%
  mutate(tranquilizantes = case_when(
    p117_1 == 1 | p117_2 == 1 | p117_3 == 1 | p117_4 == 1 | 
      p117_5 == 1 | p117_6 == 1 | p117_7 == 1 | p117_8 == 1 | p117_9 == 1 | p117_10 == 1 ~ 1,
    TRUE ~ 0
  ))


tasa_consumo_tranquilizantes <- datos %>%
  group_by(Region) %>%
  summarise(
    total_consumidores = sum(tranquilizantes * Fexp, na.rm = TRUE),  # Ignorar NA en los cálculos
    total_individuos = sum(Fexp, na.rm = TRUE),             # Ignorar NA en los cálculos
    tasa_consumo = ifelse(total_individuos == 0, NA, total_consumidores / total_individuos)  # Evitar división por cero
  )

tasa_consumo_tranquilizantes_total <- datos %>%
  summarise(
    total_consumidores = sum(tranquilizantes * Fexp, na.rm = TRUE),   # Total ponderado de consumidores
    total_individuos = sum(Fexp, na.rm = TRUE),            # Total ponderado de individuos
    tasa_consumo = ifelse(total_individuos == 0, NA, total_consumidores / total_individuos )  # Calcular la tasa como porcentaje
  )

#ESTIMULANTES

datos <- datos %>% 
  mutate(p119_1 = ifelse(p119_1 %in% c(1, 2), 1, 0),
         p119_2 = ifelse(p119_2 %in% c(1, 2), 1, 0),
         p119_3 = ifelse(p119_3 %in% c(1, 2), 1, 0),
         p119_4 = ifelse(p119_4 %in% c(1, 2), 1, 0),
         p119_5 = ifelse(p119_5 %in% c(1, 2), 1, 0),
         p119_6 = ifelse(p119_6 %in% c(1, 2), 1, 0),
         p119_7 = ifelse(p119_7 %in% c(1, 2), 1, 0),
         p119_8 = ifelse(p119_8 %in% c(1, 2), 1, 0)) %>%
  mutate(estimulantes = case_when(
    p119_1 == 1 | p119_2 == 1 | p119_3 == 1 | p119_4 == 1 | 
      p119_5 == 1 | p119_6 == 1 | p119_7 == 1 | p119_8 == 1 ~ 1,
    TRUE ~ 0
  ))


tasa_consumo_estimulantes <- datos %>%
  group_by(Region) %>%
  summarise(
    total_consumidores = sum(estimulantes * Fexp, na.rm = TRUE),  # Ignorar NA en los cálculos
    total_individuos = sum(Fexp, na.rm = TRUE),             # Ignorar NA en los cálculos
    tasa_consumo = ifelse(total_individuos == 0, NA, total_consumidores / total_individuos)  # Evitar división por cero
  )

tasa_consumo_estimulantes_total <- datos %>%
  summarise(
    total_consumidores = sum(estimulantes * Fexp, na.rm = TRUE),   # Total ponderado de consumidores
    total_individuos = sum(Fexp, na.rm = TRUE),            # Total ponderado de individuos
    tasa_consumo = ifelse(total_individuos == 0, NA, total_consumidores / total_individuos )  # Calcular la tasa como porcentaje
  )

# COCAINAS
datos <- datos %>%
  mutate(cocainas = if_else(coca == 1 | pasta == 1 | crack == 1, 1, 0))
table(datos$cocainas)

tasa_cocainas <- datos %>%
  group_by(Region) %>%
  summarise(
    total_consumidores = sum(cocainas * Fexp, na.rm = TRUE),  # Ignorar NA en los cálculos
    total_individuos = sum(Fexp, na.rm = TRUE),             # Ignorar NA en los cálculos
    tasa_consumo = ifelse(total_individuos == 0, NA, total_consumidores / total_individuos)  # Evitar división por cero
  )

tasa_cocainas_total <- datos %>%
  summarise(
    total_consumidores = sum(cocainas * Fexp, na.rm = TRUE),   # Total ponderado de consumidores
    total_individuos = sum(Fexp, na.rm = TRUE),            # Total ponderado de individuos
    tasa_consumo = ifelse(total_individuos == 0, NA, total_consumidores / total_individuos )  # Calcular la tasa como porcentaje
  )


# ALUCINOGENOS

datos <- datos %>%
  mutate(alucinogenos = if_else(lsd == 1 | sanpedro == 1 |polvo == 1 , 1, 0))
table(datos$alucinogenos)

tasa_alucinogenos <- datos %>%
  group_by(Region) %>%
  summarise(
    total_consumidores = sum(alucinogenos * Fexp, na.rm = TRUE),  # Ignorar NA en los cálculos
    total_individuos = sum(Fexp, na.rm = TRUE),             # Ignorar NA en los cálculos
    tasa_consumo = ifelse(total_individuos == 0, NA, total_consumidores / total_individuos)  # Evitar división por cero
  )

tasa_alucinogenos_total <- datos %>%
  summarise(
    total_consumidores = sum(alucinogenos * Fexp, na.rm = TRUE),   # Total ponderado de consumidores
    total_individuos = sum(Fexp, na.rm = TRUE),            # Total ponderado de individuos
    tasa_consumo = ifelse(total_individuos == 0, NA, total_consumidores / total_individuos )  # Calcular la tasa como porcentaje
  )

# OTRAS

datos <- datos %>%
  mutate(otras = if_else(lsd == 1 | sanpedro == 1 |polvo == 1 | coca == 1 | pasta == 1 | crack == 1 | extasis == 1 | heroina == 1 | popper == 1 , 1, 0))
table(datos$otras)

tasa_otras <- datos %>%
  group_by(Region) %>%
  summarise(
    total_consumidores = sum(otras * Fexp, na.rm = TRUE),  # Ignorar NA en los cálculos
    total_individuos = sum(Fexp, na.rm = TRUE),             # Ignorar NA en los cálculos
    tasa_consumo = ifelse(total_individuos == 0, NA, total_consumidores / total_individuos)  # Evitar división por cero
  )

tasa_otras_total <- datos %>%
  summarise(
    total_consumidores = sum(otras * Fexp, na.rm = TRUE),   # Total ponderado de consumidores
    total_individuos = sum(Fexp, na.rm = TRUE),            # Total ponderado de individuos
    tasa_consumo = ifelse(total_individuos == 0, NA, total_consumidores / total_individuos )  # Calcular la tasa como porcentaje
  )

#EXCEL 

write_xlsx(
  list(
    "Tasa mari" = tasa_consumo_mari,
    "Tasa mari total" = tasa_consumo_mari_total,
    "Tasa coca" = tasa_consumo_coca,
    "Tasa coca total" = tasa_consumo_coca_total,
    "Tasa pasta" = tasa_consumo_pasta,
    "Tasa pasta total" = tasa_consumo_pasta_total,
    "Tasa extasis" = tasa_consumo_extasis,
    "Tasa extasis total" = tasa_consumo_extasis_total,
    "Tasa heroina" = tasa_consumo_heroina,
    "Tasa heroina total" = tasa_consumo_heroina_total,
    "Tasa crack" = tasa_consumo_crack,
    "Tasa crack total" = tasa_consumo_crack_total,
    "Tasa popper" = tasa_consumo_popper,
    "Tasa popper total" = tasa_consumo_popper_total,
    "Tasa lsd" = tasa_consumo_lsd,
    "Tasa lsd total" = tasa_consumo_lsd_total,
    "Tasa san pedro" = tasa_consumo_sanpedro,
    "Tasa san pedro total" = tasa_consumo_sanpedro_total,
    "Tasa polvo de angel" = tasa_consumo_polvo,
    "Tasa polvo de angel total" = tasa_consumo_polvo_total,
    "Tasa tranquilizantes sr" = tasa_consumo_tranquilizantes,
    "Tasa tranquilizantes total" = tasa_consumo_tranquilizantes_total,
    "Tasa estimulantes sr" = tasa_consumo_estimulantes,
    "Tasa estimulantes total" = tasa_consumo_estimulantes_total,
    "Tasa cocainas" = tasa_cocainas,
    "Tasa cocainas total" = tasa_cocainas_total,
    "Tasa alucinogenos" = tasa_alucinogenos,
    "Tasa alucinogenos total" = tasa_alucinogenos_total,
    "Tasa otras" = tasa_otras,
    "Tasa_otras total" = tasa_otras_total
  ),
  "tasa_consumo2.xlsx"
)

#2004

rm(list = ls())
setwd("C:/Users/cjara/OneDrive - Universidad Católica de Chile/Mercado drogas/SENDA/2004")
datos <- read_dta("C:/Users/cjara/OneDrive - Universidad Católica de Chile/Mercado drogas/SENDA/2004/BBDD.dta")

table(datos$edad)

datos <- datos %>%
  filter(edad < 18)

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

datos <- datos %>%
  mutate(mari = case_when(
    p41 %in% 1:2 ~ 1,  # Recodificar valores 1 o 2 como 1 (Año)
    p41 == 3 ~ 0       # Recodificar valor 3 como 2 (Más de un año)
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
    p89 %in% 1:2 ~ 1,  # Recodificar valores 1 o 2 como 1 (Año)
    p89 == 3 ~ 0       # Recodificar valor 3 como 2 (Más de un año)
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
    p65 %in% 1:2 ~ 1,  # Recodificar valores 1 o 2 como 1 (Año)
    p65 == 3 ~ 0       # Recodificar valor 3 como 2 (Más de un año)
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
    p127 %in% 1:2 ~ 1,  # Recodificar valores 1 o 2 como 1 (Año)
    p127 == 3 ~ 0       # Recodificar valor 3 como 2 (Más de un año)
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

#HEROINA

datos <- datos %>%
  mutate(heroina = case_when(
    p112 %in% 1:2 ~ 1,  # Recodificar valores 1 o 2 como 1 (Año)
    p112 == 3 ~ 0       # Recodificar valor 3 como 2 (Más de un año)
  ))

tasa_consumo_heroina <- datos %>%
  group_by(Region) %>%
  summarise(
    total_consumidores = sum(heroina * Fexp, na.rm = TRUE),  # Ignorar NA en los cálculos
    total_individuos = sum(Fexp, na.rm = TRUE),             # Ignorar NA en los cálculos
    tasa_consumo = ifelse(total_individuos == 0, NA, total_consumidores / total_individuos)  # Evitar división por cero
  )

tasa_consumo_heroina_total <- datos %>%
  summarise(
    total_consumidores = sum(heroina * Fexp, na.rm = TRUE),   # Total ponderado de consumidores
    total_individuos = sum(Fexp, na.rm = TRUE),            # Total ponderado de individuos
    tasa_consumo = ifelse(total_individuos == 0, NA, total_consumidores / total_individuos )  # Calcular la tasa como porcentaje
  )

#CRACK

datos <- datos %>%
  mutate(crack = case_when(
    p110 %in% 1:2 ~ 1,  # Recodificar valores 1 o 2 como 1 (Año)
    p110 == 3 ~ 0       # Recodificar valor 3 como 2 (Más de un año)
  ))

tasa_consumo_crack <- datos %>%
  group_by(Region) %>%
  summarise(
    total_consumidores = sum(crack * Fexp, na.rm = TRUE),  # Ignorar NA en los cálculos
    total_individuos = sum(Fexp, na.rm = TRUE),             # Ignorar NA en los cálculos
    tasa_consumo = ifelse(total_individuos == 0, NA, total_consumidores / total_individuos)  # Evitar división por cero
  )

tasa_consumo_crack_total <- datos %>%
  summarise(
    total_consumidores = sum(crack * Fexp, na.rm = TRUE),   # Total ponderado de consumidores
    total_individuos = sum(Fexp, na.rm = TRUE),            # Total ponderado de individuos
    tasa_consumo = ifelse(total_individuos == 0, NA, total_consumidores / total_individuos )  # Calcular la tasa como porcentaje
  )

#POPPER

datos <- datos %>%
  mutate(popper = case_when(
    p116f %in% 1:2 ~ 1,  # Recodificar valores 1 o 2 como 1 (Año)
    p116f == 3 ~ 0       # Recodificar valor 3 como 2 (Más de un año)
  ))

tasa_consumo_popper <- datos %>%
  group_by(Region) %>%
  summarise(
    total_consumidores = sum(popper * Fexp, na.rm = TRUE),  # Ignorar NA en los cálculos
    total_individuos = sum(Fexp, na.rm = TRUE),             # Ignorar NA en los cálculos
    tasa_consumo = ifelse(total_individuos == 0, NA, total_consumidores / total_individuos)  # Evitar división por cero
  )

tasa_consumo_popper_total <- datos %>%
  summarise(
    total_consumidores = sum(popper * Fexp, na.rm = TRUE),   # Total ponderado de consumidores
    total_individuos = sum(Fexp, na.rm = TRUE),            # Total ponderado de individuos
    tasa_consumo = ifelse(total_individuos == 0, NA, total_consumidores / total_individuos )  # Calcular la tasa como porcentaje
  )

#LSD

datos <- datos %>%
  mutate(lsd = case_when(
    p114a %in% 1:2 ~ 1,  # Recodificar valores 1 o 2 como 1 (Año)
    p114a == 3 ~ 0       # Recodificar valor 3 como 2 (Más de un año)
  ))

tasa_consumo_lsd <- datos %>%
  group_by(Region) %>%
  summarise(
    total_consumidores = sum(lsd * Fexp, na.rm = TRUE),  # Ignorar NA en los cálculos
    total_individuos = sum(Fexp, na.rm = TRUE),             # Ignorar NA en los cálculos
    tasa_consumo = ifelse(total_individuos == 0, NA, total_consumidores / total_individuos)  # Evitar división por cero
  )

tasa_consumo_lsd_total <- datos %>%
  summarise(
    total_consumidores = sum(lsd * Fexp, na.rm = TRUE),   # Total ponderado de consumidores
    total_individuos = sum(Fexp, na.rm = TRUE),            # Total ponderado de individuos
    tasa_consumo = ifelse(total_individuos == 0, NA, total_consumidores / total_individuos )  # Calcular la tasa como porcentaje
  )

#SAN PEDRO

datos <- datos %>%
  mutate(sanpedro = case_when(
    p114c %in% 1:2 ~ 1,  # Recodificar valores 1 o 2 como 1 (Año)
    p114c == 3 ~ 0       # Recodificar valor 3 como 2 (Más de un año)
  ))

tasa_consumo_sanpedro <- datos %>%
  group_by(Region) %>%
  summarise(
    total_consumidores = sum(sanpedro * Fexp, na.rm = TRUE),  # Ignorar NA en los cálculos
    total_individuos = sum(Fexp, na.rm = TRUE),             # Ignorar NA en los cálculos
    tasa_consumo = ifelse(total_individuos == 0, NA, total_consumidores / total_individuos)  # Evitar división por cero
  )

tasa_consumo_sanpedro_total <- datos %>%
  summarise(
    total_consumidores = sum(sanpedro * Fexp, na.rm = TRUE),   # Total ponderado de consumidores
    total_individuos = sum(Fexp, na.rm = TRUE),            # Total ponderado de individuos
    tasa_consumo = ifelse(total_individuos == 0, NA, total_consumidores / total_individuos )  # Calcular la tasa como porcentaje
  )

#POLVO DE ÁNGEL

datos <- datos %>%
  mutate(polvo = case_when(
    p114b %in% 1:2 ~ 1,  # Recodificar valores 1 o 2 como 1 (Año)
    p114b == 3 ~ 0       # Recodificar valor 3 como 2 (Más de un año)
  ))

tasa_consumo_polvo <- datos %>%
  group_by(Region) %>%
  summarise(
    total_consumidores = sum(polvo * Fexp, na.rm = TRUE),  # Ignorar NA en los cálculos
    total_individuos = sum(Fexp, na.rm = TRUE),             # Ignorar NA en los cálculos
    tasa_consumo = ifelse(total_individuos == 0, NA, total_consumidores / total_individuos)  # Evitar división por cero
  )

tasa_consumo_polvo_total <- datos %>%
  summarise(
    total_consumidores = sum(polvo * Fexp, na.rm = TRUE),   # Total ponderado de consumidores
    total_individuos = sum(Fexp, na.rm = TRUE),            # Total ponderado de individuos
    tasa_consumo = ifelse(total_individuos == 0, NA, total_consumidores / total_individuos )  # Calcular la tasa como porcentaje
  )

#TRANQUILIZANTES

datos <- datos %>% 
  mutate(p120a = ifelse(p120a %in% c(1, 2), 1, 0),
         p120b = ifelse(p120b %in% c(1, 2), 1, 0),
         p120c = ifelse(p120c %in% c(1, 2), 1, 0),
         p120d = ifelse(p120d %in% c(1, 2), 1, 0),
         p120e = ifelse(p120e %in% c(1, 2), 1, 0),
         p120f = ifelse(p120f %in% c(1, 2), 1, 0),
         p120g = ifelse(p120g %in% c(1, 2), 1, 0),
         p120h = ifelse(p120h %in% c(1, 2), 1, 0),
         p120i = ifelse(p120i %in% c(1, 2), 1, 0),
         p120j = ifelse(p120j %in% c(1, 2), 1, 0)) %>%
  mutate(tranquilizantes = case_when(
    p120a == 1 | p120b == 1 | p120c == 1 | p120d == 1 | 
      p120e == 1 | p120f == 1 | p120g == 1 | p120h == 1 | p120i == 1 | p120j == 1 ~ 1,
    TRUE ~ 0
  ))


tasa_consumo_tranquilizantes <- datos %>%
  group_by(Region) %>%
  summarise(
    total_consumidores = sum(tranquilizantes * Fexp, na.rm = TRUE),  # Ignorar NA en los cálculos
    total_individuos = sum(Fexp, na.rm = TRUE),             # Ignorar NA en los cálculos
    tasa_consumo = ifelse(total_individuos == 0, NA, total_consumidores / total_individuos)  # Evitar división por cero
  )

tasa_consumo_tranquilizantes_total <- datos %>%
  summarise(
    total_consumidores = sum(tranquilizantes * Fexp, na.rm = TRUE),   # Total ponderado de consumidores
    total_individuos = sum(Fexp, na.rm = TRUE),            # Total ponderado de individuos
    tasa_consumo = ifelse(total_individuos == 0, NA, total_consumidores / total_individuos )  # Calcular la tasa como porcentaje
  )

#ESTIMULANTES

datos <- datos %>% 
  mutate(p122a = ifelse(p122a %in% c(1, 2), 1, 0),
         p122b = ifelse(p122b %in% c(1, 2), 1, 0),
         p122c = ifelse(p122c %in% c(1, 2), 1, 0),
         p122d = ifelse(p122d %in% c(1, 2), 1, 0),
         p122e = ifelse(p122e %in% c(1, 2), 1, 0),
         p122f = ifelse(p122f %in% c(1, 2), 1, 0),
         p122g = ifelse(p122g %in% c(1, 2), 1, 0),
         p122h = ifelse(p122h %in% c(1, 2), 1, 0)) %>%
  mutate(estimulantes = case_when(
    p122a == 1 | p122b == 1 | p122c == 1 | p122d == 1 | 
      p122e == 1 | p122f == 1 | p122g == 1 | p122h == 1 ~ 1,
    TRUE ~ 0
  ))

tasa_consumo_estimulantes <- datos %>%
  group_by(Region) %>%
  summarise(
    total_consumidores = sum(estimulantes * Fexp, na.rm = TRUE),  # Ignorar NA en los cálculos
    total_individuos = sum(Fexp, na.rm = TRUE),             # Ignorar NA en los cálculos
    tasa_consumo = ifelse(total_individuos == 0, NA, total_consumidores / total_individuos)  # Evitar división por cero
  )

tasa_consumo_estimulantes_total <- datos %>%
  summarise(
    total_consumidores = sum(estimulantes * Fexp, na.rm = TRUE),   # Total ponderado de consumidores
    total_individuos = sum(Fexp, na.rm = TRUE),            # Total ponderado de individuos
    tasa_consumo = ifelse(total_individuos == 0, NA, total_consumidores / total_individuos )  # Calcular la tasa como porcentaje
  )

# COCAINAS
datos <- datos %>%
  mutate(cocainas = if_else(coca == 1 | pasta == 1 | crack == 1, 1, 0))
table(datos$cocainas)

tasa_cocainas <- datos %>%
  group_by(Region) %>%
  summarise(
    total_consumidores = sum(cocainas * Fexp, na.rm = TRUE),  # Ignorar NA en los cálculos
    total_individuos = sum(Fexp, na.rm = TRUE),             # Ignorar NA en los cálculos
    tasa_consumo = ifelse(total_individuos == 0, NA, total_consumidores / total_individuos)  # Evitar división por cero
  )

tasa_cocainas_total <- datos %>%
  summarise(
    total_consumidores = sum(cocainas * Fexp, na.rm = TRUE),   # Total ponderado de consumidores
    total_individuos = sum(Fexp, na.rm = TRUE),            # Total ponderado de individuos
    tasa_consumo = ifelse(total_individuos == 0, NA, total_consumidores / total_individuos )  # Calcular la tasa como porcentaje
  )


# ALUCINOGENOS

datos <- datos %>%
  mutate(alucinogenos = if_else(lsd == 1 | sanpedro == 1 |polvo == 1 , 1, 0))
table(datos$alucinogenos)

tasa_alucinogenos <- datos %>%
  group_by(Region) %>%
  summarise(
    total_consumidores = sum(alucinogenos * Fexp, na.rm = TRUE),  # Ignorar NA en los cálculos
    total_individuos = sum(Fexp, na.rm = TRUE),             # Ignorar NA en los cálculos
    tasa_consumo = ifelse(total_individuos == 0, NA, total_consumidores / total_individuos)  # Evitar división por cero
  )

tasa_alucinogenos_total <- datos %>%
  summarise(
    total_consumidores = sum(alucinogenos * Fexp, na.rm = TRUE),   # Total ponderado de consumidores
    total_individuos = sum(Fexp, na.rm = TRUE),            # Total ponderado de individuos
    tasa_consumo = ifelse(total_individuos == 0, NA, total_consumidores / total_individuos )  # Calcular la tasa como porcentaje
  )

# OTRAS

datos <- datos %>%
  mutate(otras = if_else(lsd == 1 | sanpedro == 1 |polvo == 1 | coca == 1 | pasta == 1 | crack == 1 | extasis == 1 | heroina == 1 | popper == 1 , 1, 0))
table(datos$otras)

tasa_otras <- datos %>%
  group_by(Region) %>%
  summarise(
    total_consumidores = sum(otras * Fexp, na.rm = TRUE),  # Ignorar NA en los cálculos
    total_individuos = sum(Fexp, na.rm = TRUE),             # Ignorar NA en los cálculos
    tasa_consumo = ifelse(total_individuos == 0, NA, total_consumidores / total_individuos)  # Evitar división por cero
  )

tasa_otras_total <- datos %>%
  summarise(
    total_consumidores = sum(otras * Fexp, na.rm = TRUE),   # Total ponderado de consumidores
    total_individuos = sum(Fexp, na.rm = TRUE),            # Total ponderado de individuos
    tasa_consumo = ifelse(total_individuos == 0, NA, total_consumidores / total_individuos )  # Calcular la tasa como porcentaje
  )

#EXCEL 

write_xlsx(
  list(
    "Tasa mari" = tasa_consumo_mari,
    "Tasa mari total" = tasa_consumo_mari_total,
    "Tasa coca" = tasa_consumo_coca,
    "Tasa coca total" = tasa_consumo_coca_total,
    "Tasa pasta" = tasa_consumo_pasta,
    "Tasa pasta total" = tasa_consumo_pasta_total,
    "Tasa extasis" = tasa_consumo_extasis,
    "Tasa extasis total" = tasa_consumo_extasis_total,
    "Tasa heroina" = tasa_consumo_heroina,
    "Tasa heroina total" = tasa_consumo_heroina_total,
    "Tasa crack" = tasa_consumo_crack,
    "Tasa crack total" = tasa_consumo_crack_total,
    "Tasa popper" = tasa_consumo_popper,
    "Tasa popper total" = tasa_consumo_popper_total,
    "Tasa lsd" = tasa_consumo_lsd,
    "Tasa lsd total" = tasa_consumo_lsd_total,
    "Tasa san pedro" = tasa_consumo_sanpedro,
    "Tasa san pedro total" = tasa_consumo_sanpedro_total,
    "Tasa polvo de angel" = tasa_consumo_polvo,
    "Tasa polvo de angel total" = tasa_consumo_polvo_total,
    "Tasa tranquilizantes sr" = tasa_consumo_tranquilizantes,
    "Tasa tranquilizantes total" = tasa_consumo_tranquilizantes_total,
    "Tasa estimulantes sr" = tasa_consumo_estimulantes,
    "Tasa estimulantes total" = tasa_consumo_estimulantes_total,
    "Tasa cocainas" = tasa_cocainas,
    "Tasa cocainas total" = tasa_cocainas_total,
    "Tasa alucinogenos" = tasa_alucinogenos,
    "Tasa alucinogenos total" = tasa_alucinogenos_total,
    "Tasa otras" = tasa_otras,
    "Tasa_otras total" = tasa_otras_total
  ),
  "tasa_consumo2.xlsx"
)

#2002

rm(list = ls())
setwd("C:/Users/cjara/OneDrive - Universidad Católica de Chile/Mercado drogas/SENDA/2002")
datos <- read_dta("C:/Users/cjara/OneDrive - Universidad Católica de Chile/Mercado drogas/SENDA/2002/BBDD.dta")

table(datos$edad)

datos <- datos %>%
  filter(edad < 18)

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

datos <- datos %>%
  mutate(mari = case_when(
    p40 %in% 1:2 ~ 1,  # Recodificar valores 1 o 2 como 1 (Año)
    p40 == 3 ~ 0       # Recodificar valor 3 como 2 (Más de un año)
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
    p82 %in% 1:2 ~ 1,  # Recodificar valores 1 o 2 como 1 (Año)
    p82 == 3 ~ 0       # Recodificar valor 3 como 2 (Más de un año)
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
    p61 %in% 1:2 ~ 1,  # Recodificar valores 1 o 2 como 1 (Año)
    p61 == 3 ~ 0       # Recodificar valor 3 como 2 (Más de un año)
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
    p116a %in% 1:2 ~ 1,  # Recodificar valores 1 o 2 como 1 (Año)
    p116a == 3 ~ 0       # Recodificar valor 3 como 2 (Más de un año)
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

#HEROINA

datos <- datos %>%
  mutate(heroina = case_when(
    p102 %in% 1:2 ~ 1,  # Recodificar valores 1 o 2 como 1 (Año)
    p102 == 3 ~ 0       # Recodificar valor 3 como 2 (Más de un año)
  ))

tasa_consumo_heroina <- datos %>%
  group_by(Region) %>%
  summarise(
    total_consumidores = sum(heroina * Fexp, na.rm = TRUE),  # Ignorar NA en los cálculos
    total_individuos = sum(Fexp, na.rm = TRUE),             # Ignorar NA en los cálculos
    tasa_consumo = ifelse(total_individuos == 0, NA, total_consumidores / total_individuos)  # Evitar división por cero
  )

tasa_consumo_heroina_total <- datos %>%
  summarise(
    total_consumidores = sum(heroina * Fexp, na.rm = TRUE),   # Total ponderado de consumidores
    total_individuos = sum(Fexp, na.rm = TRUE),            # Total ponderado de individuos
    tasa_consumo = ifelse(total_individuos == 0, NA, total_consumidores / total_individuos )  # Calcular la tasa como porcentaje
  )

#CRACK

datos <- datos %>%
  mutate(crack = case_when(
    p100 %in% 1:2 ~ 1,  # Recodificar valores 1 o 2 como 1 (Año)
    p100 == 3 ~ 0       # Recodificar valor 3 como 2 (Más de un año)
  ))

tasa_consumo_crack <- datos %>%
  group_by(Region) %>%
  summarise(
    total_consumidores = sum(crack * Fexp, na.rm = TRUE),  # Ignorar NA en los cálculos
    total_individuos = sum(Fexp, na.rm = TRUE),             # Ignorar NA en los cálculos
    tasa_consumo = ifelse(total_individuos == 0, NA, total_consumidores / total_individuos)  # Evitar división por cero
  )

tasa_consumo_crack_total <- datos %>%
  summarise(
    total_consumidores = sum(crack * Fexp, na.rm = TRUE),   # Total ponderado de consumidores
    total_individuos = sum(Fexp, na.rm = TRUE),            # Total ponderado de individuos
    tasa_consumo = ifelse(total_individuos == 0, NA, total_consumidores / total_individuos )  # Calcular la tasa como porcentaje
  )

#POPPER

datos <- datos %>%
  mutate(popper = case_when(
    p106f %in% 1:2 ~ 1,  # Recodificar valores 1 o 2 como 1 (Año)
    p106f == 3 ~ 0       # Recodificar valor 3 como 2 (Más de un año)
  ))

tasa_consumo_popper <- datos %>%
  group_by(Region) %>%
  summarise(
    total_consumidores = sum(popper * Fexp, na.rm = TRUE),  # Ignorar NA en los cálculos
    total_individuos = sum(Fexp, na.rm = TRUE),             # Ignorar NA en los cálculos
    tasa_consumo = ifelse(total_individuos == 0, NA, total_consumidores / total_individuos)  # Evitar división por cero
  )

tasa_consumo_popper_total <- datos %>%
  summarise(
    total_consumidores = sum(popper * Fexp, na.rm = TRUE),   # Total ponderado de consumidores
    total_individuos = sum(Fexp, na.rm = TRUE),            # Total ponderado de individuos
    tasa_consumo = ifelse(total_individuos == 0, NA, total_consumidores / total_individuos )  # Calcular la tasa como porcentaje
  )

#LSD

datos <- datos %>%
  mutate(lsd = case_when(
    p104a %in% 1:2 ~ 1,  # Recodificar valores 1 o 2 como 1 (Año)
    p104a == 3 ~ 0       # Recodificar valor 3 como 2 (Más de un año)
  ))

tasa_consumo_lsd <- datos %>%
  group_by(Region) %>%
  summarise(
    total_consumidores = sum(lsd * Fexp, na.rm = TRUE),  # Ignorar NA en los cálculos
    total_individuos = sum(Fexp, na.rm = TRUE),             # Ignorar NA en los cálculos
    tasa_consumo = ifelse(total_individuos == 0, NA, total_consumidores / total_individuos)  # Evitar división por cero
  )

tasa_consumo_lsd_total <- datos %>%
  summarise(
    total_consumidores = sum(lsd * Fexp, na.rm = TRUE),   # Total ponderado de consumidores
    total_individuos = sum(Fexp, na.rm = TRUE),            # Total ponderado de individuos
    tasa_consumo = ifelse(total_individuos == 0, NA, total_consumidores / total_individuos )  # Calcular la tasa como porcentaje
  )

#SAN PEDRO

datos <- datos %>%
  mutate(sanpedro = case_when(
    p104c %in% 1:2 ~ 1,  # Recodificar valores 1 o 2 como 1 (Año)
    p104c == 3 ~ 0       # Recodificar valor 3 como 2 (Más de un año)
  ))

tasa_consumo_sanpedro <- datos %>%
  group_by(Region) %>%
  summarise(
    total_consumidores = sum(sanpedro * Fexp, na.rm = TRUE),  # Ignorar NA en los cálculos
    total_individuos = sum(Fexp, na.rm = TRUE),             # Ignorar NA en los cálculos
    tasa_consumo = ifelse(total_individuos == 0, NA, total_consumidores / total_individuos)  # Evitar división por cero
  )

tasa_consumo_sanpedro_total <- datos %>%
  summarise(
    total_consumidores = sum(sanpedro * Fexp, na.rm = TRUE),   # Total ponderado de consumidores
    total_individuos = sum(Fexp, na.rm = TRUE),            # Total ponderado de individuos
    tasa_consumo = ifelse(total_individuos == 0, NA, total_consumidores / total_individuos )  # Calcular la tasa como porcentaje
  )

#POLVO DE ÁNGEL

datos <- datos %>%
  mutate(polvo = case_when(
    p104b %in% 1:2 ~ 1,  # Recodificar valores 1 o 2 como 1 (Año)
    p104b == 3 ~ 0       # Recodificar valor 3 como 2 (Más de un año)
  ))

tasa_consumo_polvo <- datos %>%
  group_by(Region) %>%
  summarise(
    total_consumidores = sum(polvo * Fexp, na.rm = TRUE),  # Ignorar NA en los cálculos
    total_individuos = sum(Fexp, na.rm = TRUE),             # Ignorar NA en los cálculos
    tasa_consumo = ifelse(total_individuos == 0, NA, total_consumidores / total_individuos)  # Evitar división por cero
  )

tasa_consumo_polvo_total <- datos %>%
  summarise(
    total_consumidores = sum(polvo * Fexp, na.rm = TRUE),   # Total ponderado de consumidores
    total_individuos = sum(Fexp, na.rm = TRUE),            # Total ponderado de individuos
    tasa_consumo = ifelse(total_individuos == 0, NA, total_consumidores / total_individuos )  # Calcular la tasa como porcentaje
  )

#TRANQUILIZANTES

datos <- datos %>% 
  mutate(p110a = ifelse(p110a %in% c(1, 2), 1, 0),
         p110b = ifelse(p110b %in% c(1, 2), 1, 0),
         p110c = ifelse(p110c %in% c(1, 2), 1, 0),
         p110d = ifelse(p110d %in% c(1, 2), 1, 0),
         p110e = ifelse(p110e %in% c(1, 2), 1, 0),
         p110f = ifelse(p110f %in% c(1, 2), 1, 0),
         p110g = ifelse(p110g %in% c(1, 2), 1, 0),
         p110h = ifelse(p110h %in% c(1, 2), 1, 0),
         p110i = ifelse(p110i %in% c(1, 2), 1, 0),
         p110j = ifelse(p110j %in% c(1, 2), 1, 0)) %>%
  mutate(tranquilizantes = case_when(
    p110a == 1 | p110b == 1 | p110c == 1 | p110d == 1 | 
      p110e == 1 | p110f == 1 | p110g == 1 | p110h == 1 | p110i == 1 | p110j == 1 ~ 1,
    TRUE ~ 0
  ))


tasa_consumo_tranquilizantes <- datos %>%
  group_by(Region) %>%
  summarise(
    total_consumidores = sum(tranquilizantes * Fexp, na.rm = TRUE),  # Ignorar NA en los cálculos
    total_individuos = sum(Fexp, na.rm = TRUE),             # Ignorar NA en los cálculos
    tasa_consumo = ifelse(total_individuos == 0, NA, total_consumidores / total_individuos)  # Evitar división por cero
  )

tasa_consumo_tranquilizantes_total <- datos %>%
  summarise(
    total_consumidores = sum(tranquilizantes * Fexp, na.rm = TRUE),   # Total ponderado de consumidores
    total_individuos = sum(Fexp, na.rm = TRUE),            # Total ponderado de individuos
    tasa_consumo = ifelse(total_individuos == 0, NA, total_consumidores / total_individuos )  # Calcular la tasa como porcentaje
  )

#ESTIMULANTES

datos <- datos %>% 
  mutate(p112a = ifelse(p112a %in% c(1, 2), 1, 0),
         p112b = ifelse(p112b %in% c(1, 2), 1, 0),
         p112c = ifelse(p112c %in% c(1, 2), 1, 0),
         p112d = ifelse(p112d %in% c(1, 2), 1, 0),
         p112e = ifelse(p112e %in% c(1, 2), 1, 0),
         p112f = ifelse(p112f %in% c(1, 2), 1, 0),
         p112g = ifelse(p112g %in% c(1, 2), 1, 0)) %>%
  mutate(estimulantes = case_when(
    p112a == 1 | p112b == 1 | p112c == 1 | p112d == 1 | 
      p112e == 1 | p112f == 1 | p112g == 1 ~ 1,
    TRUE ~ 0
  ))

tasa_consumo_estimulantes <- datos %>%
  group_by(Region) %>%
  summarise(
    total_consumidores = sum(estimulantes * Fexp, na.rm = TRUE),  # Ignorar NA en los cálculos
    total_individuos = sum(Fexp, na.rm = TRUE),             # Ignorar NA en los cálculos
    tasa_consumo = ifelse(total_individuos == 0, NA, total_consumidores / total_individuos)  # Evitar división por cero
  )

tasa_consumo_estimulantes_total <- datos %>%
  summarise(
    total_consumidores = sum(estimulantes * Fexp, na.rm = TRUE),   # Total ponderado de consumidores
    total_individuos = sum(Fexp, na.rm = TRUE),            # Total ponderado de individuos
    tasa_consumo = ifelse(total_individuos == 0, NA, total_consumidores / total_individuos )  # Calcular la tasa como porcentaje
  )

# COCAINAS
datos <- datos %>%
  mutate(cocainas = if_else(coca == 1 | pasta == 1 | crack == 1, 1, 0))
table(datos$cocainas)

tasa_cocainas <- datos %>%
  group_by(Region) %>%
  summarise(
    total_consumidores = sum(cocainas * Fexp, na.rm = TRUE),  # Ignorar NA en los cálculos
    total_individuos = sum(Fexp, na.rm = TRUE),             # Ignorar NA en los cálculos
    tasa_consumo = ifelse(total_individuos == 0, NA, total_consumidores / total_individuos)  # Evitar división por cero
  )

tasa_cocainas_total <- datos %>%
  summarise(
    total_consumidores = sum(cocainas * Fexp, na.rm = TRUE),   # Total ponderado de consumidores
    total_individuos = sum(Fexp, na.rm = TRUE),            # Total ponderado de individuos
    tasa_consumo = ifelse(total_individuos == 0, NA, total_consumidores / total_individuos )  # Calcular la tasa como porcentaje
  )


# ALUCINOGENOS

datos <- datos %>%
  mutate(alucinogenos = if_else(lsd == 1 | sanpedro == 1 |polvo == 1 , 1, 0))
table(datos$alucinogenos)

tasa_alucinogenos <- datos %>%
  group_by(Region) %>%
  summarise(
    total_consumidores = sum(alucinogenos * Fexp, na.rm = TRUE),  # Ignorar NA en los cálculos
    total_individuos = sum(Fexp, na.rm = TRUE),             # Ignorar NA en los cálculos
    tasa_consumo = ifelse(total_individuos == 0, NA, total_consumidores / total_individuos)  # Evitar división por cero
  )

tasa_alucinogenos_total <- datos %>%
  summarise(
    total_consumidores = sum(alucinogenos * Fexp, na.rm = TRUE),   # Total ponderado de consumidores
    total_individuos = sum(Fexp, na.rm = TRUE),            # Total ponderado de individuos
    tasa_consumo = ifelse(total_individuos == 0, NA, total_consumidores / total_individuos )  # Calcular la tasa como porcentaje
  )

# OTRAS

datos <- datos %>%
  mutate(otras = if_else(lsd == 1 | sanpedro == 1 |polvo == 1 | coca == 1 | pasta == 1 | crack == 1 | extasis == 1 | heroina == 1 | popper == 1 , 1, 0))
table(datos$otras)

tasa_otras <- datos %>%
  group_by(Region) %>%
  summarise(
    total_consumidores = sum(otras * Fexp, na.rm = TRUE),  # Ignorar NA en los cálculos
    total_individuos = sum(Fexp, na.rm = TRUE),             # Ignorar NA en los cálculos
    tasa_consumo = ifelse(total_individuos == 0, NA, total_consumidores / total_individuos)  # Evitar división por cero
  )

tasa_otras_total <- datos %>%
  summarise(
    total_consumidores = sum(otras * Fexp, na.rm = TRUE),   # Total ponderado de consumidores
    total_individuos = sum(Fexp, na.rm = TRUE),            # Total ponderado de individuos
    tasa_consumo = ifelse(total_individuos == 0, NA, total_consumidores / total_individuos )  # Calcular la tasa como porcentaje
  )

#EXCEL 

write_xlsx(
  list(
    "Tasa mari" = tasa_consumo_mari,
    "Tasa mari total" = tasa_consumo_mari_total,
    "Tasa coca" = tasa_consumo_coca,
    "Tasa coca total" = tasa_consumo_coca_total,
    "Tasa pasta" = tasa_consumo_pasta,
    "Tasa pasta total" = tasa_consumo_pasta_total,
    "Tasa extasis" = tasa_consumo_extasis,
    "Tasa extasis total" = tasa_consumo_extasis_total,
    "Tasa heroina" = tasa_consumo_heroina,
    "Tasa heroina total" = tasa_consumo_heroina_total,
    "Tasa crack" = tasa_consumo_crack,
    "Tasa crack total" = tasa_consumo_crack_total,
    "Tasa popper" = tasa_consumo_popper,
    "Tasa popper total" = tasa_consumo_popper_total,
    "Tasa lsd" = tasa_consumo_lsd,
    "Tasa lsd total" = tasa_consumo_lsd_total,
    "Tasa san pedro" = tasa_consumo_sanpedro,
    "Tasa san pedro total" = tasa_consumo_sanpedro_total,
    "Tasa polvo de angel" = tasa_consumo_polvo,
    "Tasa polvo de angel total" = tasa_consumo_polvo_total,
    "Tasa tranquilizantes sr" = tasa_consumo_tranquilizantes,
    "Tasa tranquilizantes total" = tasa_consumo_tranquilizantes_total,
    "Tasa estimulantes sr" = tasa_consumo_estimulantes,
    "Tasa estimulantes total" = tasa_consumo_estimulantes_total,
    "Tasa cocainas" = tasa_cocainas,
    "Tasa cocainas total" = tasa_cocainas_total,
    "Tasa alucinogenos" = tasa_alucinogenos,
    "Tasa alucinogenos total" = tasa_alucinogenos_total,
    "Tasa otras" = tasa_otras,
    "Tasa_otras total" = tasa_otras_total
  ),
  "tasa_consumo2.xlsx"
)

#2000

rm(list = ls())
setwd("C:/Users/cjara/OneDrive - Universidad Católica de Chile/Mercado drogas/SENDA/2000")
datos <- read_dta("C:/Users/cjara/OneDrive - Universidad Católica de Chile/Mercado drogas/SENDA/2000/BBDD.dta")

table(datos$edad)

datos <- datos %>%
  filter(edad < 18)

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
    cod_area %in% c(45) ~ 11,
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

datos <- datos %>%
  mutate(mari = case_when(
    p31 %in% 1:2 ~ 1,  # Recodificar valores 1 o 2 como 1 (Año)
    p31 == 3 ~ 0       # Recodificar valor 3 como 2 (Más de un año)
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
    p78 %in% 1:2 ~ 1,  # Recodificar valores 1 o 2 como 1 (Año)
    p78 == 3 ~ 0       # Recodificar valor 3 como 2 (Más de un año)
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
    p58 %in% 1:2 ~ 1,  # Recodificar valores 1 o 2 como 1 (Año)
    p58 == 3 ~ 0       # Recodificar valor 3 como 2 (Más de un año)
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

# COCAINAS
datos <- datos %>%
  mutate(cocainas = if_else(coca == 1 | pasta == 1, 1, 0))
table(datos$cocainas)

tasa_cocainas <- datos %>%
  group_by(Region) %>%
  summarise(
    total_consumidores = sum(cocainas * Fexp, na.rm = TRUE),  # Ignorar NA en los cálculos
    total_individuos = sum(Fexp, na.rm = TRUE),             # Ignorar NA en los cálculos
    tasa_consumo = ifelse(total_individuos == 0, NA, total_consumidores / total_individuos)  # Evitar división por cero
  )

tasa_cocainas_total <- datos %>%
  summarise(
    total_consumidores = sum(cocainas * Fexp, na.rm = TRUE),   # Total ponderado de consumidores
    total_individuos = sum(Fexp, na.rm = TRUE),            # Total ponderado de individuos
    tasa_consumo = ifelse(total_individuos == 0, NA, total_consumidores / total_individuos )  # Calcular la tasa como porcentaje
  )

#EXCEL 

write_xlsx(
  list(
    "Tasa mari" = tasa_consumo_mari,
    "Tasa mari total" = tasa_consumo_mari_total,
    "Tasa coca" = tasa_consumo_coca,
    "Tasa coca total" = tasa_consumo_coca_total,
    "Tasa pasta" = tasa_consumo_pasta,
    "Tasa pasta total" = tasa_consumo_pasta_total,
    "Tasa cocainas" = tasa_cocainas,
    "Tasa cocainas total" = tasa_cocainas_total
  ),
  "tasa_consumo2.xlsx"
)

#Etiquetas
etiquetas <- get_labels(datos$cod_area)
valores <- get_values(datos$cod_area)
tabla_codigos_etiquetas <- data.frame(
  Codigo = valores,
  Etiqueta = etiquetas
)
print(tabla_codigos_etiquetas)
