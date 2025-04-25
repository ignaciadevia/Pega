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
datos <- read_excel("C:/Users/cjara/OneDrive - Universidad Católica de Chile/Mercado drogas/Gendarmería/base.xlsx", sheet="EGR 2007-2022+")

datos <- datos %>%
  mutate(FECHA_INGR = as.Date(FECHA_INGR), # Convertir a fecha
         Año = year(FECHA_INGR)) # Extraer el año

datos$FECHA_NACI <- as.numeric(datos$FECHA_NACI)
datos$FECHA_NACI <- as.Date(datos$FECHA_NACI, origin = "1899-12-30")

datos <- datos %>%
  mutate(FECHA_INGR = as.Date(FECHA_NACI), # Convertir a fecha
         Naci = year(FECHA_NACI))

datos <- datos %>%
  mutate (Edad = Año - Naci)

#SEPARAR DELITOS
datos <- datos %>%
  separate(TIPO_DELITOS, into = paste0("TIPO_DELITOS", 1:6), sep = ", ", fill = "right", extra = "drop")

datos <- datos %>%
  mutate(DELITOS_RECEP = str_remove_all(DELITOS_RECEP, "\\s*\\(\\d+\\)")) %>%  # Elimina números entre paréntesis
  separate(DELITOS_RECEP, into = paste0("DELITOS_RECEP", 1:11), sep = "; ", fill = "right", extra = "drop")

#DELITOS DROGA
delitos_droga <- c("CONSUMO DE DROGAS (ART. 41)",
                   "CULTIVO/COSECHA ESPECIES VEGETALES PRODUCTORAS ESTUPEFACIENTES (ART.2)",
                   "DESVIO ILICITO DE PRECURSORES Y SUSTANCIAS ESENCIALES (ART. 3.)",
                   "CONTRABANDO",
                   "INFRACCION AL ARTICULO 2° LEY N° 19366",
                   "INFRACCION AL ARTICULO 4 DE LA LEY 20000",
                   "INFRACCION AL ARTICULO 8 DE LA LEY 20000",
                   "INFRACCION LEY Nº 20.000 CONTROL DE MICROTRAFICO",
                   "LEY 19.366 TRAFICO ILICITO DE ESTUPEFACIENTES Y SUSTANCIAS SICOTROPICAS",
                   "LEY 20000 CONTROL DE MICROTRAFICO",
                   "MICROTRAFICO DE ESTUPESFACIENTES",
                   "PORTE DE DROGAS (ART. 41)",
                   "TRAFICO DE ESTUPEFACIENTES (LEY 18.403)",
                   "TRAFICO ILÍCITO DE DROGAS (ART. 3).",
                   "TRAFICO ILICITO DE DROGAS (ART. 5)",
                   "INFRACCION ART. 6 Y 24 LEY 19.366(PRECURSORES)",
                   "INFRACCION ORDENANZA ADUANAS (FRAUDE Y CONTRABANDO)",
                   "OTROS DELITOS DE LA LEY Nº 19.366",
                   "TRAFICO DE PEQUEÑAS CANTIDADES (ART. 4).",
                   "INFRACCION LEY 17.934 (TRAFICO DE ESTUPEFACIENTES)",
                   "INFRACCION LEY Nº 20.000 CONTROL DE MICROTRAFICO",
                   "ARTICULO 41 DE LA LEY N° 19366",
                   "LEY 20000 CONTROL DE MICROTR",
                   "TRAFICO DE ESTUPEFACIENTES (LEY 18.403) (1",
                   "TRAFICO ILICITO DE DROGAS",
                   "LEY 20000 CONTROL DE MICROTRA")

datos <- datos %>%
  mutate(delito_droga = if_else(rowSums(across(starts_with("DELITOS_RECEP"), ~ . %in% delitos_droga)) > 0, 1, 0))

#DELITOS ARMAS
delitos_armas <- c(
  "HURTO DE ARMAS",
  "INFRACCION A LEY 17.798 ARTICULO 2 Y ARTICULO 9",
  "INFRACCION A LEY 17.798 A",
  "INFRACCION A LEY 17.798 ARTICUL",
  "INFRACCION A LEY 17.798 ARTICULO 3 Y 14",
  "INFRACCION A LEY 17.798 ARTICULO 8 INCISO 1",
  "INFRACCION A LEY 17.798 ARTICULO 8",
  "INFRACCION LEY 19.975 PORTE ARMA BLANCA",
  "LEY 17.798 CONTROL DE ARM",
  "LEY 17.798 CONTROL DE ARMAS",
  "OTROS DELITOS CONTEMPLADOS LEY Nº 17.798",
  "PORTE ILEGAL DE ARMA DE FUEGO",
  "TENENCIA ILEGAL A",
  "TENENCIA ILEGAL ARMAS P",
  "TENENCIA ILEGAL ARMAS PROHIBIDAS",
  "TENENCIA ILEGAL Y PORTE ARMAS PROHIBIDAS Y EXPLOSIVOS",
  "TENENCIA ILEGAL Y PORTE D",
  "TENENCIA ILEGAL Y PORTE DE ARMAS",
  "TENENCIA O PORTE DE ARMAS, MUNICIONES Y OTRO",
  "TENENCIA O PORTE DE ARMAS, MUNICIONES Y OTROS",
  "TENENCIA O PORTE DE ARMAS, MUNICIONES Y OTROS;"
)

datos <- datos %>%
  mutate(delito_armas = if_else(rowSums(across(starts_with("DELITOS_RECEP"), ~ . %in% delitos_armas)) > 0, 1, 0))

delitos_droga2 <- c("DELITOS LEY DE DROGAS",
                    "DELITOS LE",
                    "DELITOS LEY DE",
                    "DELITOS LEY DE DROG",
                    "DELITOS LEY DE DROGA")

#CORREGIR CON NA
datos <- datos %>%
  mutate(delito_droga2 = if_else(rowSums(across(starts_with("TIPO_DELITO"), ~ . %in% delitos_droga2)) > 0, 1, 0)) %>%
  mutate(delito_droga2 = factor(delito_droga2, levels = c(0, 1), labels = c("No", "Sí")))

datos <- datos %>%
  mutate(delito_droga = if_else(
    rowSums(across(starts_with("DELITOS_RECEP"), ~ . %in% delitos_droga)) > 0 | 
      (!is.na(delito_droga2) & delito_droga2 == "Sí"),
    1, 
    delito_droga  # Mantiene el valor anterior de delito_droga si no se cumple ninguna condición
  ))

#COMPLEJIDAD CRIMINAL

datos <- datos %>%
  mutate(complejidad2 = if_else(delito_droga == 1 & delito_armas == 1, 1, 0))

#COMUNA Y NACIONALIDAD
datos <- datos %>%
  select(Año, FECHA_NACI, COMUNA_DOMICILIO, REGION, NACIONALIDAD, delito_droga, complejidad2, Edad)

datos <- datos %>%
  filter(!is.na(Año), Año >= 2000)
# Limpiar la columna COMUNA_DOMICILIO
datos <- datos %>%
  mutate(
    COMUNA_DOMICILIO = COMUNA_DOMICILIO %>%
      str_replace_all("[()]", "") %>%          # Elimina solo los paréntesis
      str_remove_all('"') %>%                  # Elimina comillas dobles
      str_replace_all("[,./-]", " ") %>%       # Reemplaza signos por espacio
      str_squish() %>%                         # Elimina espacios duplicados
      str_to_upper()                           # Convierte a mayúsculas
  )


# Recodificar los nombres de las ciudades
datos_filtrados <- datos_filtrados %>%
  mutate(
    COMUNA_DOMICILIO = recode(
      COMUNA_DOMICILIO,
      "",
      "PUNO" = "PUNO PERU", 
      "PUNO (PERU)" = "PUNO PERU",
      "LIMA" = "LIMA PERU", 
      "LIMA (PERU)" = "LIMA PERU",
      "LA PAZ" = "LA PAZ BOLIVIA", 
      "LA PAZ (BOLIVIA)" = "LA PAZ BOLIVIA",
      "ANCA (PERU)" = "ANCAS PERU",
      "ANCAS HUARI" = "ANCAS PERU",
      "ANIMA" = "ANIMAS BOLIVIA",
      "ANIMA POTOSI" = "ANIMAS BOLIVIA",
      "ARAQUIPA" = "AREQUIPA PERU",
      "ARIQUIPA" = "AREQUIPA PERU",
      "AREQUIPA" = "AREQUIPA PERU",
      "ASUNCION" = "ASUNSION PARAGUAY"
      
    )
  )

# Ahora, agrupar por la columna COMUNA_DOMICILIO (ya limpia)
datos2 <- datos %>%
  group_by(COMUNA_DOMICILIO, Año) %>%
  summarise(
    total = n(),                          # Contar la cantidad de registros por comuna
    delito_droga = sum(delito_droga, na.rm = TRUE),  # Sumar delito_droga por comuna
    complejidad2 = sum(complejidad2, na.rm = TRUE)  # Promedio de complejidad2
  ) %>%
  arrange(Año)  # Ordenar por la cantidad total de registros, de mayor a menor

resultados <- datos2 %>%
  group_by(COMUNA_DOMICILIO, Año) %>%
  summarise(
    total_droga = sum(delito_droga, na.rm = TRUE),
    total_complejidad2 = sum(complejidad2, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  group_by(Año) %>%
  mutate(
    porcentaje_droga = (total_droga / sum(total_droga)),
    porcentaje_complejidad2 = (total_complejidad2 / sum(total_complejidad2))
  ) %>%
  ungroup() %>%
  arrange(Año)

resultados2 <- datos %>%
  group_by(NACIONALIDAD, Año) %>%
  summarise(
    total_droga = sum(delito_droga, na.rm = TRUE),
    total_complejidad2 = sum(complejidad2, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  group_by(Año) %>%
  mutate(
    porcentaje_droga = (total_droga / sum(total_droga)),
    porcentaje_complejidad2 = (total_complejidad2 / sum(total_complejidad2))
  ) %>%
  ungroup() %>%
  arrange(Año)

#Fecha nacimiento
resultados3 <- datos %>%
  group_by(Edad, Año) %>%
  summarise(
    total_droga = sum(delito_droga, na.rm = TRUE),
    total_complejidad2 = sum(complejidad2, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  group_by(Año) %>%
  mutate(
    porcentaje_droga = (total_droga / sum(total_droga)),
    porcentaje_complejidad2 = (total_complejidad2 / sum(total_complejidad2))
  ) %>%
  ungroup() %>%
  arrange(Año)


write_xlsx(
  list(
    "Comuna" = resultados,
    "Nacionaliad" = resultados2,
    "Edad" = resultados3
  ), 
  "comuna_nac1.xlsx"
)


#HOJA 2
rm(list = ls())
datos <- read_excel("C:/Users/cjara/OneDrive - Universidad Católica de Chile/Mercado drogas/Gendarmería/base.xlsx", sheet="2007-2012")

datos <- datos %>%
  mutate(FECHA_INGR = as.Date(FECHA_INGR), # Convertir a fecha
         Año = year(FECHA_INGR)) # Extraer el año

datos$FECHA_NACI <- as.numeric(datos$FECHA_NACI)
datos$FECHA_NACI <- as.Date(datos$FECHA_NACI, origin = "1899-12-30")

datos <- datos %>%
  mutate(FECHA_INGR = as.Date(FECHA_NACI), # Convertir a fecha
         Naci = year(FECHA_NACI))

datos <- datos %>%
  mutate (Edad = Año - Naci)

#SEPARAR DELITOS
datos <- datos %>%
  separate(TIPO_DELITOS, into = paste0("TIPO_DELITOS", 1:7), sep = ", ", fill = "right", extra = "drop")

datos <- datos %>%
  mutate(DELITOS_RECEP = str_remove_all(DELITOS_RECEP, "\\s*\\(\\d+\\)")) %>%  # Elimina números entre paréntesis
  separate(DELITOS_RECEP, into = paste0("DELITOS_RECEP", 1:11), sep = "; ", fill = "right", extra = "drop")

#DELITOS DROGA
delitos_droga <- c(
  "ARTICULO 41 DE LA LEY N° 19366",
  "CONSUMO DE DROGAS (ART. 41)",
  "CONSUMO/PORTE DE DROGAS EN LUGARES CALIFICADOS (ART. 51).",
  "CONSUMO/PORTE EN LUG.PUB.O PRIV.C/PREVIO CONCIERTO(ART.50).",
  "CONTRABANDO",
  "CULTIVO/COSECHA ESPECIES VEGETALES PRODUCTORAS ESTUPEFACIENTES (ART.2)",
  "DESVIO ILICITO DE PRECURSORES Y SUSTANCIAS ESENCIALES (ART. 3.)",
  "INFRACCION AL ARTICULO 2° LEY N° 19366",
  "INFRACCION AL ARTICULO 4 DE LA LEY 20000",
  "INFRACCION AL ARTICULO 8 DE LA LEY 20000",
  "INFRACCION AL ARTICULO 8 DE LA LEY 20000;",
  "INFRACCION ART. 6 Y 24 LEY 19.366(PRECURSORES)",
  "INFRACCION LEY 17.934 (TRAFICO DE ESTUPEFACIENTES)",
  "INFRACCION LEY Nº 20.000 CONTROL DE MICROTRAFICO",
  "LEY 19.366 TRAFICO ILICITO DE ESTUPEFACIENTES Y SUSTANCIAS SICOTROPICAS",
  "LEY 20000 CONTROL DE MICROTRAFICO",
  "MICROTRAFICO DE ESTUPESFACIENTES",
  "OTRAS FALTAS DE LA LEY Nº 19.366",
  "OTROS DELITOS DE LA LEY Nº 19.366",
  "PLANTACION  DE ESPECIES VEGETALES DEL GENERO CANNABIS",
  "PORTE DE DROGAS (ART. 41)",
  "PRESCRIPCION INDEBIDA DE SUBSTANCIAS, DROGAS ESTUPEFACIENTES O SICOTROPICAS (ART. 8)",
  "PRODUCCION Y TRAFICO DE PRECURSORES (ART. 6)",
  "TRAFICO DE ESTUPEFACIENTES (LEY 18.403)",
  "TRAFICO DE PEQUEÑAS CANTIDADES (ART. 4).",
  "TRAFICO ILÍCITO DE DROGAS (ART. 3).",
  "TRAFICO ILICITO DE DROGAS (ART. 5)",
  "TRAFICO ILICITO DE DROGAS (ART. 5",
  "LEY 20000 CONTROL DE MICROT",
  "LEY 19.366 TRAFICO ILICITO DE ESTUPEFACIENTES Y SUSTAN",
  "LEY 19.366 TRAFICO ILICITO DE ESTUPEFACIENTES Y SUSTANCIAS SICOTROPI",
  "INFRACCION LEY Nº 20.00"
)

datos <- datos %>%
  mutate(delito_droga = if_else(rowSums(across(starts_with("DELITOS_RECEP"), ~ . %in% delitos_droga)) > 0, 1, 0))

#DELITOS ARMAS
delitos_armas <- c(
  "HURTO DE ARMAS",
  "COAUTOR A INFRACCION A LEY 17.798 ARTICULO 8 INCISO 1",
  "INFRACCION A LEY 17.798 ARTICULO 2",
  "INFRACCION A LEY 17.798 ARTICULO 2 Y ARTICULO 9",
  "INFRACCION A LEY 17.798 ARTICULO 3 Y 14",
  "INFRACCION A LEY 17.798 ARTICULO 8",
  "INFRACCION A LEY 17.798 ARTICULO 8 INCISO 1",
  "INFRACCION LEY 19.975 PORTE ARMA BLANCA",
  "LEY 17.798 CONTROL DE ARMAS",
  "OTROS DELITOS CONTEMPLADOS LEY Nº 17.798",
  "PORTE ILEGAL DE ARMA DE FUEGO",
  "PORTE ILEGAL DE ARMA DE FUEGO (1",
  "PORTE DE ARMA CORTANTE O PUNZANTE (288 BIS)",
  "POSESIÓN O TENENCIA DE ARMAS PROHIBIDAS",
  "POSESIÓN, TENENCIA O PORTE DE ARMAS SUJETAS A CONTROL",
  "TENENCIA ILEGAL ARMAS",
  "TENENCIA ILEGAL ARMAS PROHIBIDAS",
  "TENENCIA ILEGAL DE EXPLOSIVOS",
  "TENENCIA ILEGAL DE EXPLOSIVOS ILICITOS DESCRITOS ART. 2 Y SANCIONADOS ART. 9 AMBOS DE LA LEY 17.798",
  "TENENCIA ILEGAL DE EXP",
  "TENENCIA ILEGAL Y P",
  "TENENCIA ILEGAL Y PORTE ARMAS PROHIBIDAS Y EXPLOSI",
  "TENENCIA ILEGAL Y PORTE ARMAS PROHIBIDAS Y EXPLOSIVOS",
  "TENENCIA ILEGAL Y PORTE ARMAS PROHIBIDAS Y EXPLOSIVOS (",
  "TENENCIA ILEGAL Y PORTE DE ARMAS",
  "TENENCIA O PORTE DE ARMAS, MUNICIONES Y OTROS"
)

datos <- datos %>%
  mutate(delito_armas = if_else(rowSums(across(starts_with("DELITOS_RECEP"), ~ . %in% delitos_armas)) > 0, 1, 0))

delitos_droga2 <- c(
  "DELITOS LEY DE DROGAS",
  "DELITOS L",
  "DELITOS LE",
  "DELITOS LEY",
  "DELITOS LEY D",
  "DELITOS LEY DE",
  "DELITOS LEY DE D",
  "DELITOS LEY DE DR",
  "DELITOS LEY DE DROG",
  "DELITOS LEY DE DROGA",
  "DELITOS LEY DE DROGAS,"
)

#CORREGIR CON NA
datos <- datos %>%
  mutate(delito_droga2 = if_else(rowSums(across(starts_with("TIPO_DELITO"), ~ . %in% delitos_droga2)) > 0, 1, 0)) %>%
  mutate(delito_droga2 = factor(delito_droga2, levels = c(0, 1), labels = c("No", "Sí")))

table(datos$delito_droga, datos$delito_droga2)

datos <- datos %>%
  mutate(delito_droga = if_else(
    rowSums(across(starts_with("DELITOS_RECEP"), ~ . %in% delitos_droga)) > 0 | 
      (!is.na(delito_droga2) & delito_droga2 == "Sí"),
    1, 
    delito_droga  # Mantiene el valor anterior de delito_droga si no se cumple ninguna condición
  ))

#COMPLEJIDAD CRIMINAL

datos <- datos %>%
  mutate(complejidad2 = if_else(delito_droga == 1 & delito_armas == 1, 1, 0))


#COMUNA Y NACIONALIDAD
datos <- datos %>%
  select(Año, FECHA_NACI, COMUNA_DOMICILIO, REGION, NACIONALIDAD, delito_droga, complejidad2, Edad)

datos <- datos %>%
  filter(!is.na(Año), Año >= 2000)
# Limpiar la columna COMUNA_DOMICILIO
datos <- datos %>%
  mutate(
    COMUNA_DOMICILIO = COMUNA_DOMICILIO %>%
      str_replace_all("[()]", "") %>%          # Elimina solo los paréntesis
      str_remove_all('"') %>%                  # Elimina comillas dobles
      str_replace_all("[,./-]", " ") %>%       # Reemplaza signos por espacio
      str_squish() %>%                         # Elimina espacios duplicados
      str_to_upper()                           # Convierte a mayúsculas
  )


# Ahora, agrupar por la columna COMUNA_DOMICILIO (ya limpia)
datos2 <- datos %>%
  group_by(COMUNA_DOMICILIO, Año) %>%
  summarise(
    total = n(),                          # Contar la cantidad de registros por comuna
    delito_droga = sum(delito_droga, na.rm = TRUE),  # Sumar delito_droga por comuna
    complejidad2 = sum(complejidad2, na.rm = TRUE)  # Promedio de complejidad2
  ) %>%
  arrange(Año)  # Ordenar por la cantidad total de registros, de mayor a menor

resultados <- datos2 %>%
  group_by(COMUNA_DOMICILIO, Año) %>%
  summarise(
    total_droga = sum(delito_droga, na.rm = TRUE),
    total_complejidad2 = sum(complejidad2, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  group_by(Año) %>%
  mutate(
    porcentaje_droga = (total_droga / sum(total_droga)),
    porcentaje_complejidad2 = (total_complejidad2 / sum(total_complejidad2))
  ) %>%
  ungroup() %>%
  arrange(Año)

resultados2 <- datos %>%
  group_by(NACIONALIDAD, Año) %>%
  summarise(
    total_droga = sum(delito_droga, na.rm = TRUE),
    total_complejidad2 = sum(complejidad2, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  group_by(Año) %>%
  mutate(
    porcentaje_droga = (total_droga / sum(total_droga)),
    porcentaje_complejidad2 = (total_complejidad2 / sum(total_complejidad2))
  ) %>%
  ungroup() %>%
  arrange(Año)


resultados3 <- datos %>%
  group_by(Edad, Año) %>%
  summarise(
    total_droga = sum(delito_droga, na.rm = TRUE),
    total_complejidad2 = sum(complejidad2, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  group_by(Año) %>%
  mutate(
    porcentaje_droga = (total_droga / sum(total_droga)),
    porcentaje_complejidad2 = (total_complejidad2 / sum(total_complejidad2))
  ) %>%
  ungroup() %>%
  arrange(Año)


write_xlsx(
  list(
    "Comuna" = resultados,
    "Nacionaliad" = resultados2,
    "Edad" = resultados3
  ), 
  "comuna_nac2.xlsx"
)

#HOJA 3
datos <- read_excel("C:/Users/cjara/OneDrive - Universidad Católica de Chile/Mercado drogas/Gendarmería/base.xlsx", sheet="2013 - 2016")

datos <- datos %>%
  mutate(FECHA_INGR = as.Date(FECHA_INGR), # Convertir a fecha
         Año = year(FECHA_INGR)) # Extraer el año

datos$FECHA_NACI <- as.numeric(datos$FECHA_NACI)
datos$FECHA_NACI <- as.Date(datos$FECHA_NACI, origin = "1899-12-30")

datos <- datos %>%
  mutate(FECHA_INGR = as.Date(FECHA_NACI), # Convertir a fecha
         Naci = year(FECHA_NACI))

datos <- datos %>%
  mutate (Edad = Año - Naci)

#SEPARAR DELITOS
datos <- datos %>%
  separate(TIPO_DELITOS, into = paste0("TIPO_DELITOS", 1:8), sep = ", ", fill = "right", extra = "drop")

datos <- datos %>%
  mutate(DELITOS_RECEP = str_remove_all(DELITOS_RECEP, "\\s*\\(\\d+\\)")) %>%  # Elimina números entre paréntesis
  separate(DELITOS_RECEP, into = paste0("DELITOS_RECEP", 1:10), sep = "; ", fill = "right", extra = "drop")

#DELITOS DROGA
delitos_droga <- c(
  "ARTICULO 41 DE LA LEY N° 19366",
  "CONSUMO DE DROGAS (ART. 41)",
  "CONSUMO/PORTE DE DROGAS EN LUGARES CALIFICADOS (ART. 51).",
  "CONSUMO/PORTE DE DROGAS EN LUGARES CALIFICADOS (A",
  "CONSUMO/PORTE EN LUG.PUB.O PRIV.C/PREVIO CONCIERTO(ART.50).",
  "CONTRABANDO",
  "CULTIVO/COSECHA ESPEC.VEGETALES PRODUCTORAS ESTUPEF.(ART.8.",
  "CULTIVO/COSECHA ESPECIES VEGETALES PRODUCTORAS ESTUPEFACIENTES (ART.2)",
  "ELABORACION ILEGAL DE DROGAS O SUSTANCIAS SICOTROPICAS ART.",
  "ELABORACION/PRODUCCION SUSTANCIAS SICOTROPICAS O DROGAS (ART.1)",
  "INFRACCION AL ARTICULO 2° LEY N° 19366",
  "INFRACCION AL ARTICULO 4 DE LA LEY 20000",
  "INFRACCION AL ARTICULO 8 DE LA LEY 20000",
  "INFRACCION ART. 6 Y 24 LEY 19.366(PRECURSORES)",
  "INFRACCION LEY 17.934 (TRAFICO DE ESTUPEFACIENTES)",
  "INFRACCION LEY Nº 20.000 CONTROL DE MICROTRAFICO",
  "INFRACCION LEY Nº 20.000 CONTROL DE MI",
  "LEY 19.366 TRAFICO ILICITO DE ESTUPEFACIENTES Y SUSTANCIAS SICOTROPICAS",
  "LEY 19.366 TRAFICO ILICITO DE ESTUPEFACIENTES Y SUSTANCIAS SICOTROPICAS (",
  "LEY 19.366 TRAFICO ILICITO DE ESTUPEFACIENT",
  "LEY 19.366 TRAFICO ILICI",
  "LEY 19.366 TRAFICO ILICITO DE ESTUPE",
  "LEY 20000 CONTROL DE MICROTRAFICO",
  "MICROTRAFICO DE ESTUPESFACIENTES",
  "OTROS DELITOS DE LA LEY 20.000.",
  "OTROS DELITOS DE LA LEY Nº 19.366",
  "PLANTACION  DE ESPECIES VEGETALES DEL GENERO CANNABIS",
  "PORTE DE DROGAS (ART. 41)",
  "PRODUC Y TRAFICO POR DESVIO DE PRECURSORES ART. 2 LEY 20.000",
  "PRODUCCION Y TRAFICO DE PRECURSORES (ART. 6)",
  "TRAFICO DE ESPECIES VEGETALES ART 10 LEY 20.000",
  "TRAFICO DE ESTUPEFACIENTES (LEY 18.403)",
  "TRAFICO DE ESTUPEFACI",
  "TRAFICO DE PEQUEÑAS CANTIDADES (ART. 4).",
  "TRAFICO DE PEQUEÑAS CANTIDADES (",
  "TRAFICO ILÍCITO DE DROGAS (ART. 3).",
  "TRAFICO ILICITO DE DROGAS (ART. 5)",
  "TRAFICO ILICITO DE DROGAS (ART. 5) ("
)

datos <- datos %>%
  mutate(delito_droga = if_else(rowSums(across(starts_with("DELITOS_RECEP"), ~ . %in% delitos_droga)) > 0, 1, 0))

#DELITOS ARMAS
delitos_armas <- c(
  "DISPAROS INJUSTIF VÍA PÚBLICA (ART. 14 D INC. FINAL)",
  "HURTO DE ARMAS",
  "INFRACCION A LEY 17.798 ARTICUL",
  "INFRACCION A LEY 17.798 ARTICULO 2 Y ARTICULO 9",
  "INFRACCION A LEY 17.798 ARTICULO 3 Y",
  "INFRACCION A LEY 17.798 ARTICULO 3 Y 14",
  "INFRACCION A LEY 17.798 ARTICULO 8",
  "INFRACCION A LEY 17.798 ARTICULO 8 INCISO 1",
  "INFRACCION LEY 19.975 PORTE ARMA BLANCA",
  "LEY 17.798 CONTROL DE ARMAS",
  "OTROS DELITOS CONTEMPLADOS LEY Nº 17.798",
  "OTROS DELITOS DE LA LEY DE CONTROL DE ARMAS (LEY 17.798)",
  "PORTE DE ARMA CORTANTE O PUNZANTE (288 BIS)",
  "PORTE DE ARMA PROHIBIDA (ART. 14 INC. 1°)",
  "PORTE ILEGAL D",
  "PORTE ILEGAL DE ARMA D",
  "PORTE ILEGAL DE ARMA DE FUEGO",
  "PORTE ILEGAL DE ARMA DE FUEGO (",
  "POSESIÓN O TENENCIA DE ARMAS PROHIBIDAS",
  "POSESIÓN, TENENCIA O PORTE DE ARMAS SUJETAS A CONTR",
  "POSESIÓN, TENENCIA O PORTE DE ARMAS SUJETAS A CONTROL",
  "TENENC",
  "TENENCIA",
  "TENENCIA ILEGA",
  "TENENCIA ILEGAL ARMAS PRO",
  "TENENCIA ILEGAL ARMAS PROHIB",
  "TENENCIA ILEGAL ARMAS PROHIBIDAS",
  "TENENCIA ILEGAL DE EXPLOSIVOS",
  "TENENCIA ILEGAL DE EXPLOSIVOS ILICITOS DESCRITOS ART. 2 Y SANCIONADOS ART. 9 AMBOS DE LA LEY 17.798",
  "TENENCIA ILEGAL Y PORTE ARMAS PROHIBIDAS Y EXPLOSIVOS",
  "TENENCIA ILEGAL Y PORTE DE ARMAS",
  "TENENCIA O PORTE DE ARMAS, MUNICI",
  "TENENCIA O PORTE DE ARMAS, MUNICIONES Y OT",
  "TENENCIA O PORTE DE ARMAS, MUNICIONES Y OTROS",
  "TRÁFICO DE ARMAS (ART. 10)"
)

datos <- datos %>%
  mutate(delito_armas = if_else(rowSums(across(starts_with("DELITOS_RECEP"), ~ . %in% delitos_armas)) > 0, 1, 0))

delitos_droga2 <- c(
  "DELITOS LEY DE DROGAS",
  "DELITOS L",
  "DELITOS LE",
  "DELITOS LEY",
  "DELITOS LEY D",
  "DELITOS LEY DE",
  "DELITOS LEY DE D",
  "DELITOS LEY DE DR",
  "DELITOS LEY DE DROG",
  "DELITOS LEY DE DROGA",
  "DELITOS LEY DE DROGAS,"
)

#CORREGIR CON NA
datos <- datos %>%
  mutate(delito_droga2 = if_else(rowSums(across(starts_with("TIPO_DELITO"), ~ . %in% delitos_droga2)) > 0, 1, 0)) %>%
  mutate(delito_droga2 = factor(delito_droga2, levels = c(0, 1), labels = c("No", "Sí")))

table(datos$delito_droga, datos$delito_droga2)

datos <- datos %>%
  mutate(delito_droga = if_else(
    rowSums(across(starts_with("DELITOS_RECEP"), ~ . %in% delitos_droga)) > 0 | 
      (!is.na(delito_droga2) & delito_droga2 == "Sí"),
    1, 
    delito_droga  # Mantiene el valor anterior de delito_droga si no se cumple ninguna condición
  ))

#COMPLEJIDAD CRIMINAL

datos <- datos %>%
  mutate(complejidad2 = if_else(delito_droga == 1 & delito_armas == 1, 1, 0))

#COMUNA Y NACIONALIDAD
datos <- datos %>%
  select(Año, FECHA_NACI, COMUNA_DOMICILIO, REGION, NACIONALIDAD, delito_droga, complejidad2, Edad)

datos <- datos %>%
  filter(!is.na(Año), Año >= 2000)
# Limpiar la columna COMUNA_DOMICILIO
datos <- datos %>%
  mutate(
    COMUNA_DOMICILIO = COMUNA_DOMICILIO %>%
      str_replace_all("[()]", "") %>%          # Elimina solo los paréntesis
      str_remove_all('"') %>%                  # Elimina comillas dobles
      str_replace_all("[,./-]", " ") %>%       # Reemplaza signos por espacio
      str_squish() %>%                         # Elimina espacios duplicados
      str_to_upper()                           # Convierte a mayúsculas
  )


# Ahora, agrupar por la columna COMUNA_DOMICILIO (ya limpia)
datos2 <- datos %>%
  group_by(COMUNA_DOMICILIO, Año) %>%
  summarise(
    total = n(),                          # Contar la cantidad de registros por comuna
    delito_droga = sum(delito_droga, na.rm = TRUE),  # Sumar delito_droga por comuna
    complejidad2 = sum(complejidad2, na.rm = TRUE)  # Promedio de complejidad2
  ) %>%
  arrange(Año)  # Ordenar por la cantidad total de registros, de mayor a menor

resultados <- datos2 %>%
  group_by(COMUNA_DOMICILIO, Año) %>%
  summarise(
    total_droga = sum(delito_droga, na.rm = TRUE),
    total_complejidad2 = sum(complejidad2, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  group_by(Año) %>%
  mutate(
    porcentaje_droga = (total_droga / sum(total_droga)),
    porcentaje_complejidad2 = (total_complejidad2 / sum(total_complejidad2))
  ) %>%
  ungroup() %>%
  arrange(Año)

resultados2 <- datos %>%
  group_by(NACIONALIDAD, Año) %>%
  summarise(
    total_droga = sum(delito_droga, na.rm = TRUE),
    total_complejidad2 = sum(complejidad2, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  group_by(Año) %>%
  mutate(
    porcentaje_droga = (total_droga / sum(total_droga)),
    porcentaje_complejidad2 = (total_complejidad2 / sum(total_complejidad2))
  ) %>%
  ungroup() %>%
  arrange(Año)

resultados3 <- datos %>%
  group_by(Edad, Año) %>%
  summarise(
    total_droga = sum(delito_droga, na.rm = TRUE),
    total_complejidad2 = sum(complejidad2, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  group_by(Año) %>%
  mutate(
    porcentaje_droga = (total_droga / sum(total_droga)),
    porcentaje_complejidad2 = (total_complejidad2 / sum(total_complejidad2))
  ) %>%
  ungroup() %>%
  arrange(Año)


write_xlsx(
  list(
    "Comuna" = resultados,
    "Nacionaliad" = resultados2,
    "Edad" = resultados3
  ), 
  "comuna_nac3.xlsx"
)

#HOJA 4
datos <- read_excel("C:/Users/cjara/OneDrive - Universidad Católica de Chile/Mercado drogas/Gendarmería/base.xlsx", sheet="2017 - 2022")

datos <- datos %>%
  mutate(FECHA_INGR = as.Date(FECHA_INGR), # Convertir a fecha
         Año = year(FECHA_INGR)) # Extraer el año

datos <- datos %>%
  mutate(FECHA_INGR = as.Date(FECHA_NACI), # Convertir a fecha
         Naci = year(FECHA_NACI))

datos <- datos %>%
  mutate (Edad = Año - Naci)

#SEPARAR DELITOS
datos <- datos %>%
  separate(TIPO_DELITOS, into = paste0("TIPO_DELITOS", 1:7), sep = ", ", fill = "right", extra = "drop")

datos <- datos %>%
  mutate(DELITOS_RECEP = str_remove_all(DELITOS_RECEP, "\\s*\\(\\d+\\)")) %>%  # Elimina números entre paréntesis
  separate(DELITOS_RECEP, into = paste0("DELITOS_RECEP", 1:10), sep = "; ", fill = "right", extra = "drop")

#DELITOS DROGA
delitos_droga <- c(
  "ARTICULO 41 DE LA LEY N° 19366",
  "CONSUMO DE DROGAS (ART. 41)",
  "CONSUMO/PORTE EN LUG.PUB.",
  "CONSUMO/PORTE DE DROGAS EN LUGARES CALIFICADOS",
  "CONSUMO/PORTE DE DROGAS EN LUGARES CALIFICADOS (ART. 51)",
  "CONSUMO/PORTE DE DROGAS EN LUGARES CALIFICADOS (ART. 51).",
  "CONSUMO/PORTE EN LUG.PU",
  "CONSUMO/PORTE EN LUG.PUB.O PRIV.C/PREVIO CONCIERTO(ART.50).",
  "CONTRABANDO",
  "CULTIVO/COSECHA ESPEC.VEGETALES PRODUCT",
  "CULTIVO/COSECHA ESPECIES VEGETALES PRODUCTORAS",
  "CULTIVO/COSECHA ESPEC.VEGETALES PRODUCTORAS ESTUPEF.(ART.8.",
  "CULTIVO/COSECHA ESPECIES VEGETALES PRODUCTORAS ESTUPEFACIENTES (ART.2)",
  "ELABORACION ILEGAL DE DROGAS O SUSTANCIAS SICOTROPICAS ART.",
  "ELABORACION/PRODUCCION SUSTANCIAS SICOTROPICAS O DROGAS (ART.1)",
  "ENCUBRIDOR PARA TRAFICO ILICITO DE ESTUPEFACIENTES",
  "FACILITACION DE BIENES AL TRAFICO DE DROGAS ART. 11.",
  "HALLAZGO DE DROGAS.",
  "INFRACCION AL ARTICULO 2° LEY N° 19366",
  "INFRACCION AL ARTICULO 4 DE LA LEY 20",
  "INFRACCION AL ARTICULO 4 DE LA LEY 20000",
  "INFRACCION AL ARTICULO 8 DE LA LEY 20000",
  "INFRACCION LEY N",
  "INFRACCION LEY Nº 20.000 CONTROL DE MICROTRAFICO",
  "INFRACCION LEY 17.934 (TRAFICO DE ESTUPEFACIENTES)",
  "LAVADO DE D",
  "LAVADO DE DI",
  "LAVADO DE DINERO (ART. 12)",
  "LAVADO DE DINERO PERSONA JURIDICA ART. 27  LEY 19.913",
  "LAVADO DE DINERO PERSONA NATURAL ART. 27  L",
  "LAVADO DE DINERO PERSONA NATURAL ART. 27  LEY 19.913",
  "LEY 19.366 TRAFICO ILICITO DE ESTUPEFACIENTES Y SUSTANCIAS SICO",
  "LEY 19.366 TRAFICO ILICITO DE ESTUPEFACIENTES Y SUSTANCIAS SICOTROPICAS",
  "LEY 19.366 TRAFICO ILICITO DE ESTUPEFACIENTES Y SUSTANCIAS SICOTROPICAS;",
  "LEY 20000 CONTROL DE MICROTRAFICO",
  "MICROTRAFICO DE ESTUPESFACIENTES",
  "OTROS DELITOS DE LA LEY 20.000.",
  "PLANTACION  DE ESPECIES VEGETALES DEL GENERO CANNABIS",
  "PORTE DE DROGAS (ART. 41)",
  "PRODUC Y TRAFICO POR DESVIO DE PRECURSORES ART. 2 LEY 20.000",
  "PRODUCCION Y TRAFICO DE PRECURSORES (ART. 6)",
  "TOLERANCIA AL TRAFICO O CONSUMO DE DROGAS ART. 12.",
  "TRAFICO DE ESPECIES VEGETALES ART 10 LEY 20.000",
  "TRAFICO DE ESTUPEFACIENTES (LEY 18.403)",
  "TRAFICO DE PEQUEÑAS CANTIDADES (ART. 4).",
  "TRAFICO ILÍCITO DE DROGAS (ART. 3).",
  "TRAFICO ILICITO DE DROGAS (ART. 5)",
  "TRAF",
  "TRAFICO DE P",
  "TRAFICO DE PEQUEÑAS C",
  "TRAFICO DE PEQUEÑAS CANTI",
  "TRAFICO DE PEQUEÑAS CANTID",
  "TRAFICO DE PEQUEÑAS CANTID",
  "TRAFICO DE PEQUEÑAS CANTIDADE",
  "TRAFICO DE PEQUEÑAS CANTIDADES",
  "TRAFICO DE PEQUEÑAS CANTIDADES (",
  "TRAFICO DE PEQUEÑAS CANTIDADES (ART.",
  "TRAFICO DE PEQUEÑAS CANTIDADES (ART. 4).;",
  "TRAFICO ILÍ",
  "TRAFICO ILÍCITO DE DR",
  "TRAFICO ILÍCITO DE DROGAS",
  "TRAFICO ILICITO DE DROGAS (A",
  "TRAFICO ILÍCITO DE DROGAS (ART",
  "TRAFICO ILÍCITO DE DROGAS (ART."
)

datos <- datos %>%
  mutate(delito_droga = if_else(rowSums(across(starts_with("DELITOS_RECEP"), ~ . %in% delitos_droga)) > 0, 1, 0))

#DELITOS ARMAS
delitos_armas <- c(
  "DISPAROS INJUSTIF VÍA P",
  "DISPAROS INJUSTIF VÍA PÚBLICA (ART",
  "DISPAROS INJUSTIF VÍA PÚBLICA (ART. 1",
  "DISPAROS INJUSTIF VÍA PÚBLICA (ART. 14 D INC",
  "DISPAROS INJUSTIF VÍA PÚBLICA (ART. 14 D INC. FINAL) (",
  "DISPAROS INJUSTIF VÍA PÚBLICA (ART. 14 D INC. FINAL)",
  "INFRACCION A LEY 17.798 ARTICULO 2 Y ARTICULO 9",
  "INFRACCION A LEY 17.798 ARTICULO 3 Y",
  "INFRACCION A LEY 17.798 ARTICULO 3 Y 14",
  "INFRACCION A LEY 17.798 ARTICULO 8",
  "INFRACCION A LEY 17.798 ARTICULO 8 INCISO 1",
  "INFRACCION LEY 19.975 PORTE ARMA BLANCA",
  "LEY 17.798 CONTROL DE ARMAS",
  "OTROS DELITOS CONTEMPLADOS LEY Nº 17.798",
  "OTROS DELITOS DE LA LEY DE",
  "OTROS DELITOS DE LA LEY DE CONTROL DE ARMAS",
  "OTROS DELITOS DE LA LEY DE CONTROL DE ARMAS (LEY 17.798)",
  "PORTE",
  "PORTE DE ARMA CORTANT",
  "PORTE DE ARMA CORTANTE O PUNZ",
  "PORTE DE ARMA CORTANTE O PUNZAN",
  "PORTE DE ARMA CORTANTE O PUNZANTE",
  "PORTE DE ARMA CORTANTE O PUNZANTE (288 BIS)",
  "PORTE DE ARMA P",
  "PORTE DE ARMA PROHIBIDA (A",
  "PORTE DE ARMA PROHIBIDA (ART. 1",
  "PORTE DE ARMA PROHIBIDA (ART. 14 INC.",
  "PORTE DE ARMA PROHIBIDA (ART. 14 INC. 1°",
  "PORTE DE ARMA PROHIBIDA (ART. 14 INC. 1°)",
  "PORTE DE ARMA PROHIBIDA (ART. 14 INC. 1°) (1",
  "PORTE IL",
  "PORTE ILEGAL DE ARMA",
  "PORTE ILEGAL DE ARMA DE FUE",
  "PORTE ILEGAL DE ARMA DE FUEGO",
  "POSES",
  "POSESIÓN",
  "POSESIÓN O TENENCIA DE AR",
  "POSESIÓN O TENENCIA DE ARMAS PROHIBIDAS",
  "POSESIÓN, TENENC",
  "POSESIÓN, TENENCIA O",
  "POSESIÓN, TENENCIA O P",
  "POSESIÓN, TENENCIA O PO",
  "POSESIÓN, TENENCIA O PORTE DE ARM",
  "POSESIÓN, TENENCIA O PORTE DE ARMAS SUJE",
  "POSESIÓN, TENENCIA O PORTE DE ARMAS SUJET",
  "POSESIÓN, TENENCIA O PORTE DE ARMAS SUJETAS A",
  "POSESIÓN, TENENCIA O PORTE DE ARMAS SUJETAS A CO",
  "POSESIÓN, TENENCIA O PORTE DE ARMAS SUJETAS A CON",
  "POSESIÓN, TENENCIA O PORTE DE ARMAS SUJETAS A CONT",
  "POSESIÓN, TENENCIA O PORTE DE ARMAS SUJETAS A CONTR",
  "POSESIÓN, TENENCIA O PORTE DE ARMAS SUJETAS A CONTROL",
  "POSESIÓN, TENENCIA O PORTE DE ARMAS SUJETAS A CONTROL;",
  "TENE",
  "TENENCIA ILEGAL ARMA",
  "TENENCIA ILEGAL ARMAS",
  "TENENCIA ILEGAL ARMAS PROH",
  "TENENCIA ILEGAL ARMAS PROHIBIDAS",
  "TENENCIA ILEGAL DE EXPLOSIVOS",
  "TENENCIA ILEGAL DE EXPLOSIVOS ILICITOS DESCRITOS ART. 2 Y SANCIONADOS ART. 9 AMBOS DE LA LEY 17.798",
  "TENENCIA IL",
  "TENENCIA ILEGAL Y PORTE ARMAS PROHIBIDAS Y EXPLOSIVOS",
  "TENENCIA ILEGAL Y PORTE DE ARMAS",
  "TENENCIA O PORTE DE ARMAS, M",
  "TENENCIA O PORTE DE ARMAS, MUN",
  "TENENCIA O PORTE DE ARMAS, MUNIC",
  "TENENCIA O PORTE DE ARMAS, MUNICIONES Y OTROS",
  "TENENCIA O PORTE DE ARMAS, MUNICIONES Y OTROS (1",
  "TRÁFICO DE ARMAS (ART. 10",
  "TRÁFICO DE ARMAS (ART. 10)"
)

datos <- datos %>%
  mutate(delito_armas = if_else(rowSums(across(starts_with("DELITOS_RECEP"), ~ . %in% delitos_armas)) > 0, 1, 0))

delitos_droga2 <- c(
  "DELITOS LEY DE DROGAS",
  "DELITOS L",
  "DELITOS LE",
  "DELITOS LEY",
  "DELITOS LEY D",
  "DELITOS LEY DE",
  "DELITOS LEY DE D",
  "DELITOS LEY DE DR",
  "DELITOS LEY DE DRO",
  "DELITOS LEY DE DROG",
  "DELITOS LEY DE DROGA",
  "DELITOS LEY DE DROGAS,"
)

#CORREGIR CON NA
datos <- datos %>%
  mutate(delito_droga2 = if_else(rowSums(across(starts_with("TIPO_DELITO"), ~ . %in% delitos_droga2)) > 0, 1, 0)) %>%
  mutate(delito_droga2 = factor(delito_droga2, levels = c(0, 1), labels = c("No", "Sí")))

table(datos$delito_droga, datos$delito_droga2)

datos <- datos %>%
  mutate(delito_droga = if_else(
    rowSums(across(starts_with("DELITOS_RECEP"), ~ . %in% delitos_droga)) > 0 | 
      (!is.na(delito_droga2) & delito_droga2 == "Sí"),
    1, 
    delito_droga  # Mantiene el valor anterior de delito_droga si no se cumple ninguna condición
  ))

#COMPLEJIDAD CRIMINAL
datos <- datos %>%
  mutate(complejidad2 = if_else(delito_droga == 1 & delito_armas == 1, 1, 0))

#COMUNA Y NACIONALIDAD
datos <- datos %>%
  select(Año, FECHA_NACI, COMUNA_DOMICILIO, REGION, NACIONALIDAD, delito_droga, complejidad2, Edad)

datos <- datos %>%
  filter(!is.na(Año), Año >= 2000)
# Limpiar la columna COMUNA_DOMICILIO
datos <- datos %>%
  mutate(
    COMUNA_DOMICILIO = COMUNA_DOMICILIO %>%
      str_replace_all("[()]", "") %>%          # Elimina solo los paréntesis
      str_remove_all('"') %>%                  # Elimina comillas dobles
      str_replace_all("[,./-]", " ") %>%       # Reemplaza signos por espacio
      str_squish() %>%                         # Elimina espacios duplicados
      str_to_upper()                           # Convierte a mayúsculas
  )


# Ahora, agrupar por la columna COMUNA_DOMICILIO (ya limpia)
datos2 <- datos %>%
  group_by(COMUNA_DOMICILIO, Año) %>%
  summarise(
    total = n(),                          # Contar la cantidad de registros por comuna
    delito_droga = sum(delito_droga, na.rm = TRUE),  # Sumar delito_droga por comuna
    complejidad2 = sum(complejidad2, na.rm = TRUE)  # Promedio de complejidad2
  ) %>%
  arrange(Año)  # Ordenar por la cantidad total de registros, de mayor a menor

resultados <- datos2 %>%
  group_by(COMUNA_DOMICILIO, Año) %>%
  summarise(
    total_droga = sum(delito_droga, na.rm = TRUE),
    total_complejidad2 = sum(complejidad2, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  group_by(Año) %>%
  mutate(
    porcentaje_droga = (total_droga / sum(total_droga)),
    porcentaje_complejidad2 = (total_complejidad2 / sum(total_complejidad2))
  ) %>%
  ungroup() %>%
  arrange(Año)

resultados2 <- datos %>%
  group_by(NACIONALIDAD, Año) %>%
  summarise(
    total_droga = sum(delito_droga, na.rm = TRUE),
    total_complejidad2 = sum(complejidad2, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  group_by(Año) %>%
  mutate(
    porcentaje_droga = (total_droga / sum(total_droga)),
    porcentaje_complejidad2 = (total_complejidad2 / sum(total_complejidad2))
  ) %>%
  ungroup() %>%
  arrange(Año)


resultados3 <- datos %>%
  group_by(Edad, Año) %>%
  summarise(
    total_droga = sum(delito_droga, na.rm = TRUE),
    total_complejidad2 = sum(complejidad2, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  group_by(Año) %>%
  mutate(
    porcentaje_droga = (total_droga / sum(total_droga)),
    porcentaje_complejidad2 = (total_complejidad2 / sum(total_complejidad2))
  ) %>%
  ungroup() %>%
  arrange(Año)


write_xlsx(
  list(
    "Comuna" = resultados,
    "Nacionaliad" = resultados2,
    "Edad" = resultados3
  ), 
  "comuna_nac4.xlsx"
)