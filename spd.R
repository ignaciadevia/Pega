rm(list = ls())
library(haven)
library(dplyr)
library(tidyverse)
library(readxl)
library(writexl)
options(scipen = 999)
setwd("C:/Users/cjara/OneDrive - Universidad Católica de Chile/Mercado drogas")
datos <- read_excel("C:/Users/cjara/OneDrive - Universidad Católica de Chile/Mercado drogas/BBDD_Mercado.xlsx", sheet="Decomisos_SPD")

datos <- datos %>%
  mutate(
    Cocaina_Pct = Cocaina / Total * 100,
    PastaBase_Pct = PastaBase / Total * 100,
    Marihuana_Pct = Marihuana / Total * 100,
    Extasis_Pct = Extasis / Total * 100,
    Heroina_Pct = Heroína / Total * 100
  )

setwd("C:/Users/cjara/OneDrive - Universidad Católica de Chile/Mercado drogas/SPD")
write_xlsx(datos,"decomisos.xlsx")
