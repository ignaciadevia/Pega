library(haven)
library(dplyr)
library(sjlabelled)
library(writexl)
library(tidyr)
options(scipen = 999)

#2012
rm(list = ls())
setwd("C:/Users/cjara/OneDrive - Universidad Católica de Chile/Mercado drogas/I-ADAM/2012")
datos <- read_dta("C:/Users/cjara/OneDrive - Universidad Católica de Chile/Mercado drogas/I-ADAM/2012/bbdd.dta")

table(datos$comuna)
table(datos$comisaria)

etiquetas <- get_labels(datos$MAR_4)
valores <- get_values(datos$MAR_4)
tabla <- data.frame(
  Codigo = valores,
  Etiqueta = etiquetas
)
print(tabla)

#Delitos

#Coca 
table(datos$p11d)
datos <- datos %>%
  mutate(coca = case_when(
    p11d == 1 ~ 1,  
    p11d == 2 ~ 0,
    p11d == 9 ~ 0,
    p11d == 88 ~ 0,
  ))
table(datos$coca)

tasa_consumo_coca_total <- datos %>%
  summarise(
    total_consumidores = sum(coca * pond4, na.rm = TRUE),   # Total ponderado de consumidores
    total_individuos = sum(pond4, na.rm = TRUE),            # Total ponderado de individuos
    tasa_consumo = ifelse(total_individuos == 0, NA, total_consumidores / total_individuos )  # Calcular la tasa como porcentaje
  )

tasa_consumo_coca_total <- datos %>%
  filter(coca %in% c(0, 1)) %>%  # Filtra solo respuestas válidas (0 = No, 1 = Sí)
  summarise(
    total_consumidores = sum(pond4[coca == 1], na.rm = TRUE),  # Pondera consumidores
    total_respuestas = sum(pond4, na.rm = TRUE),               # Pondera todas las respuestas válidas
    tasa_consumo = ifelse(total_respuestas == 0, NA, total_consumidores / total_respuestas)  # Tasa ajustada
  )

#Marihuana tox
table(datos$mari)
datos <- datos %>%
  mutate(mari = case_when(
    p11b == 1 ~ 1,  
    p11b == 2 ~ 0       
  ))

tasa_consumo_mari_total <- datos %>%
  summarise(
    total_consumidores = sum(mari * pond4, na.rm = TRUE),   # Total ponderado de consumidores
    total_individuos = sum(pond4, na.rm = TRUE),            # Total ponderado de individuos
    tasa_consumo = ifelse(total_individuos == 0, NA, total_consumidores / total_individuos )  # Calcular la tasa como porcentaje
  )



#Meta tox
table(datos$meta)


table(datos$p11b) #Marihuana
table(datos$p11d) #Cocaína
table(datos$p11e) #Alucinógenos
table(datos$p11f) #Extasis
table(datos$p11g) #Chicota
table(datos$p11h) #Metafetaminas
table(datos$p11i) #Pasta
table(datos$p11j) #Inhalables
table(datos$p11k) #Anfetaminas
table(datos$p11l) #Tonaril
table(datos$p11m) #Heroína
table(datos$p11p) #Opiaceos

#Pregunta si ha hecho algo bajo los efectos de la sustancia que le hayan causado problemas con la autoridad: p15c
#Tambien pregunta si se ha visto involucrado en alguna pelea bajo los efectos de la sustancia: p15e

table(datos$p25)
table(datos$p25a)

#2021
rm(list = ls())
setwd("C:/Users/cjara/OneDrive - Universidad Católica de Chile/Mercado drogas/I-ADAM/2021")
datos <- read_dta("C:/Users/cjara/OneDrive - Universidad Católica de Chile/Mercado drogas/I-ADAM/2021/bbdd.dta")

