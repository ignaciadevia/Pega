rm(list = ls())
library(haven)
library(dplyr)
library(tidyverse)
library(tidyr)
library(readxl)
library(lubridate)
library(stringr)
library(writexl)
options(scipen = 999)

setwd("C:/Users/cjara/OneDrive - Universidad Católica de Chile/Mercado drogas/Gendarmería")

#HOJA 1
datos <- read_excel("C:/Users/cjara/OneDrive - Universidad Católica de Chile/Mercado drogas/Gendarmería/Agresiones.xlsx", sheet="2015")

totales1 <- datos %>%
  group_by(reg, Variables) %>%
  summarise(Total_sumado = sum(Total, na.rm = TRUE))

#HOJA 2
datos <- read_excel("C:/Users/cjara/OneDrive - Universidad Católica de Chile/Mercado drogas/Gendarmería/Agresiones.xlsx", sheet="2016")

totales2 <- datos %>%
  group_by(REGION, Variables) %>%
  summarise(Total_sumado = sum(Total, na.rm = TRUE))

#HOJA 3
datos <- read_excel("C:/Users/cjara/OneDrive - Universidad Católica de Chile/Mercado drogas/Gendarmería/Agresiones.xlsx", sheet="2017")

totales3 <- datos %>%
  group_by(REGION, Variables) %>%
  summarise(Total_sumado = sum(Total, na.rm = TRUE))

#HOJA 4
datos <- read_excel("C:/Users/cjara/OneDrive - Universidad Católica de Chile/Mercado drogas/Gendarmería/Agresiones.xlsx", sheet="2018")

totales4 <- datos %>%
  group_by(REGION, Variables) %>%
  summarise(Total_sumado = sum(Total, na.rm = TRUE))

#HOJA 5
datos <- read_excel("C:/Users/cjara/OneDrive - Universidad Católica de Chile/Mercado drogas/Gendarmería/Agresiones.xlsx", sheet="2019")

totales5 <- datos %>%
  group_by(REGION, Variables) %>%
  summarise(Total_sumado = sum(Total, na.rm = TRUE))

#HOJA 6
datos <- read_excel("C:/Users/cjara/OneDrive - Universidad Católica de Chile/Mercado drogas/Gendarmería/Agresiones.xlsx", sheet="2020")

totales6 <- datos %>%
  group_by(REGION, Variables) %>%
  summarise(Total_sumado = sum(Total, na.rm = TRUE))

#HOJA 7
datos <- read_excel("C:/Users/cjara/OneDrive - Universidad Católica de Chile/Mercado drogas/Gendarmería/Agresiones.xlsx", sheet="2021")

totales7 <- datos %>%
  group_by(REGION, Variables) %>%
  summarise(Total_sumado = sum(Total, na.rm = TRUE))

#HOJA 8
datos <- read_excel("C:/Users/cjara/OneDrive - Universidad Católica de Chile/Mercado drogas/Gendarmería/Agresiones.xlsx", sheet="2022")

totales8 <- datos %>%
  group_by(REGION, Variables) %>%
  summarise(Total_sumado = sum(Total, na.rm = TRUE))

#HOJA 9
datos <- read_excel("C:/Users/cjara/OneDrive - Universidad Católica de Chile/Mercado drogas/Gendarmería/Agresiones.xlsx", sheet="2023")

totales9 <- datos %>%
  group_by(REGION, Variables) %>%
  summarise(Total_sumado = sum(Total, na.rm = TRUE))

#HOJA 10
datos <- read_excel("C:/Users/cjara/OneDrive - Universidad Católica de Chile/Mercado drogas/Gendarmería/Agresiones.xlsx", sheet="2024")

totales10 <- datos %>%
  group_by(REGION, Variables) %>%
  summarise(Total_sumado = sum(Total, na.rm = TRUE))

write_xlsx(
  list(
    "2015" = totales1,
    "2016" = totales2,
    "2017" = totales3,
    "2018" = totales4,
    "2019" = totales5,
    "2020" = totales6,
    "2021" = totales7,
    "2022" = totales8,
    "2023" = totales9,
    "2024" = totales10
  ), 
  "agresiones_año.xlsx"
)
getwd()
