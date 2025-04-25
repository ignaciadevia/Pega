rm(list = ls())
library(haven)
library(dplyr)
library(sjlabelled)
library(writexl)
library(readxl)
options(scipen = 999)

#2020

setwd("C:/Users/cjara/OneDrive - Universidad Católica de Chile/Mercado drogas/DEIS")
datos <- read_xlsx("C:/Users/cjara/OneDrive - Universidad Católica de Chile/Mercado drogas/DEIS/egreso2020.xlsx")


datos <- datos %>%
  mutate(env.drog = case_when(
    DIAG1 %in% c("T400", "T401", "T402", "T403", "T404", "T405", "T406", "T407", "T408", "T409", "T436", "T438") ~ 1,  # Valores específicos a 1
    TRUE ~ 0  # Todo el resto a 0
  ))
table(datos$env.drog)


datos <- datos %>%
  mutate(env.drog2 = case_when(
    DIAG2 %in% c("T400", "T401", "T402", "T403", "T404", "T405", "T406", "T407", "T408", "T409", "T436", "T438") ~ 1,  # Valores específicos a 1
    TRUE ~ 0  # Todo el resto a 0
  ))
table(datos$env.drog2)

tasa_env <- datos %>%
  group_by(REGION_RESIDENCIA) %>%
  summarise(
    total_env = sum(env.drog, na.rm = TRUE),  # Ignorar NA en los cálculos
    total_individuos = sum(1048575, na.rm = TRUE),             # Ignorar NA en los cálculos
    tasa_consumo = ifelse(total_individuos == 0, NA, total_env / total_individuos)  # Evitar división por cero
  )

tasa_env_total <- datos %>%
  summarise(
    total_env = sum(env.drog, na.rm = TRUE),   # Total ponderado de consumidores
    total_individuos = sum(1048575, na.rm = TRUE),            # Total ponderado de individuos
    tasa_consumo = ifelse(total_individuos == 0, NA, total_env / total_individuos )  # Calcular la tasa como porcentaje
  )

datos <- datos %>%
  mutate(env.opiaceos = case_when(
    DIAG1 %in% c("T400") ~ 1,  # Valores específicos a 1
    TRUE ~ 0  # Todo el resto a 0
  ))
table(datos$env.opiaceos)
#5

datos <- datos %>%
  mutate(env.heroina = case_when(
    DIAG1 %in% c("T401") ~ 1,  # Valores específicos a 1
    TRUE ~ 0  # Todo el resto a 0
  ))
table(datos$env.heroina)
#0

datos <- datos %>%
  mutate(env.otrosopiaceos = case_when(
    DIAG1 %in% c("T402") ~ 1,  # Valores específicos a 1
    TRUE ~ 0  # Todo el resto a 0
  ))
table(datos$env.otrosopiaceos)
#9

datos <- datos %>%
  mutate(env.metadona = case_when(
    DIAG1 %in% c("T403") ~ 1,  # Valores específicos a 1
    TRUE ~ 0  # Todo el resto a 0
  ))
table(datos$env.metadona)
#0

datos <- datos %>%
  mutate(env.narcoticos = case_when(
    DIAG1 %in% c("T404") ~ 1,  # Valores específicos a 1
    TRUE ~ 0  # Todo el resto a 0
  ))
table(datos$env.narcoticos)
#Fentanilo, tramadol
#36

datos <- datos %>%
  mutate(env.cocaina = case_when(
    DIAG1 %in% c("T405") ~ 1,  # Valores específicos a 1
    TRUE ~ 0  # Todo el resto a 0
  ))
table(datos$env.cocaina)
#32

datos <- datos %>%
  mutate(env.narcoticosnoesp = case_when(
    DIAG1 %in% c("T406") ~ 1,  # Valores específicos a 1
    TRUE ~ 0  # Todo el resto a 0
  ))
table(datos$env.narcoticosnoesp)
#7

datos <- datos %>%
  mutate(env.cannabis = case_when(
    DIAG1 %in% c("T407") ~ 1,  # Valores específicos a 1
    TRUE ~ 0  # Todo el resto a 0
  ))
table(datos$env.cannabis)
#17

datos <- datos %>%
  mutate(env.lsd = case_when(
    DIAG1 %in% c("T408") ~ 1,  # Valores específicos a 1
    TRUE ~ 0  # Todo el resto a 0
  ))
table(datos$env.lsd)
#0

datos <- datos %>%
  mutate(env.alucinogenos = case_when(
    DIAG1 %in% c("T409") ~ 1,  # Valores específicos a 1
    TRUE ~ 0  # Todo el resto a 0
  ))
table(datos$env.alucinogenos)
#6

datos <- datos %>%
  mutate(env.psicoestimulantes = case_when(
    DIAG1 %in% c("T436") ~ 1,  # Valores específicos a 1
    TRUE ~ 0  # Todo el resto a 0
  ))
table(datos$env.psicoestimulantes)
#26

datos <- datos %>%
  mutate(env.otrospsicotropicos = case_when(
    DIAG1 %in% c("T438") ~ 1,  # Valores específicos a 1
    TRUE ~ 0  # Todo el resto a 0
  ))
table(datos$env.otrospsicotropicos)
#23

write_xlsx(
  list(
    "Tasa envenentamiento total" = tasa_env_total,
    "Tasa envenenamiento drogas" = tasa_env
  ),
  "tasa_env2020.xlsx"
)

#2019
rm(list = ls())
setwd("C:/Users/cjara/OneDrive - Universidad Católica de Chile/Mercado drogas/DEIS")
datos <- read_xlsx("C:/Users/cjara/OneDrive - Universidad Católica de Chile/Mercado drogas/DEIS/egreso2019.xlsx")

dim(datos)

datos <- datos %>%
  mutate(env.drog = case_when(
    DIAG1 %in% c("T400", "T401", "T402", "T403", "T404", "T405", "T406", "T407", "T408", "T409", "T436", "T438") ~ 1,  # Valores específicos a 1
    TRUE ~ 0  # Todo el resto a 0
  ))
table(datos$env.drog)


datos <- datos %>%
  mutate(env.drog2 = case_when(
    DIAG2 %in% c("T400", "T401", "T402", "T403", "T404", "T405", "T406", "T407", "T408", "T409", "T436", "T438") ~ 1,  # Valores específicos a 1
    TRUE ~ 0  # Todo el resto a 0
  ))
table(datos$env.drog2)

tasa_env <- datos %>%
  group_by(REGION_RESIDENCIA) %>%
  summarise(
    total_env = sum(env.drog, na.rm = TRUE),  # Ignorar NA en los cálculos
    total_individuos = sum(1048575, na.rm = TRUE),             # Ignorar NA en los cálculos
    tasa_consumo = ifelse(total_individuos == 0, NA, total_env / total_individuos)  # Evitar división por cero
  )

tasa_env_total <- datos %>%
  summarise(
    total_env = sum(env.drog, na.rm = TRUE),   # Total ponderado de consumidores
    total_individuos = sum(1048575, na.rm = TRUE),            # Total ponderado de individuos
    tasa_consumo = ifelse(total_individuos == 0, NA, total_env / total_individuos )  # Calcular la tasa como porcentaje
  )

write_xlsx(
  list(
    "Tasa envenentamiento total" = tasa_env_total,
    "Tasa envenenamiento drogas" = tasa_env
  ),
  "tasa_env2019.xlsx"
)

#2018
rm(list = ls())
setwd("C:/Users/cjara/OneDrive - Universidad Católica de Chile/Mercado drogas/DEIS")
datos <- read_xlsx("C:/Users/cjara/OneDrive - Universidad Católica de Chile/Mercado drogas/DEIS/egreso2018.xlsx")

dim(datos)

datos <- datos %>%
  mutate(env.drog = case_when(
    DIAG1 %in% c("T400", "T401", "T402", "T403", "T404", "T405", "T406", "T407", "T408", "T409", "T436", "T438") ~ 1,  # Valores específicos a 1
    TRUE ~ 0  # Todo el resto a 0
  ))
table(datos$env.drog)


datos <- datos %>%
  mutate(env.drog2 = case_when(
    DIAG2 %in% c("T400", "T401", "T402", "T403", "T404", "T405", "T406", "T407", "T408", "T409", "T436", "T438") ~ 1,  # Valores específicos a 1
    TRUE ~ 0  # Todo el resto a 0
  ))
table(datos$env.drog2)

tasa_env <- datos %>%
  group_by(REGION_RESIDENCIA) %>%
  summarise(
    total_env = sum(env.drog, na.rm = TRUE),  # Ignorar NA en los cálculos
    total_individuos = sum(1048575, na.rm = TRUE),             # Ignorar NA en los cálculos
    tasa_consumo = ifelse(total_individuos == 0, NA, total_env / total_individuos)  # Evitar división por cero
  )

tasa_env_total <- datos %>%
  summarise(
    total_env = sum(env.drog, na.rm = TRUE),   # Total ponderado de consumidores
    total_individuos = sum(1048575, na.rm = TRUE),            # Total ponderado de individuos
    tasa_consumo = ifelse(total_individuos == 0, NA, total_env / total_individuos )  # Calcular la tasa como porcentaje
  )

write_xlsx(
  list(
    "Tasa envenentamiento total" = tasa_env_total,
    "Tasa envenenamiento drogas" = tasa_env
  ),
  "tasa_env2018.xlsx"
)

#2017
rm(list = ls())
setwd("C:/Users/cjara/OneDrive - Universidad Católica de Chile/Mercado drogas/DEIS")
datos <- read_xlsx("C:/Users/cjara/OneDrive - Universidad Católica de Chile/Mercado drogas/DEIS/egreso2017.xlsx")

dim(datos)

datos <- datos %>%
  mutate(env.drog = case_when(
    DIAG1 %in% c("T400", "T401", "T402", "T403", "T404", "T405", "T406", "T407", "T408", "T409", "T436", "T438") ~ 1,  # Valores específicos a 1
    TRUE ~ 0  # Todo el resto a 0
  ))
table(datos$env.drog)


datos <- datos %>%
  mutate(env.drog2 = case_when(
    DIAG2 %in% c("T400", "T401", "T402", "T403", "T404", "T405", "T406", "T407", "T408", "T409", "T436", "T438") ~ 1,  # Valores específicos a 1
    TRUE ~ 0  # Todo el resto a 0
  ))
table(datos$env.drog2)

tasa_env <- datos %>%
  group_by(REGION_RESIDENCIA) %>%
  summarise(
    total_env = sum(env.drog, na.rm = TRUE),  # Ignorar NA en los cálculos
    total_individuos = sum(1048575, na.rm = TRUE),             # Ignorar NA en los cálculos
    tasa_consumo = ifelse(total_individuos == 0, NA, total_env / total_individuos)  # Evitar división por cero
  )

tasa_env_total <- datos %>%
  summarise(
    total_env = sum(env.drog, na.rm = TRUE),   # Total ponderado de consumidores
    total_individuos = sum(1048575, na.rm = TRUE),            # Total ponderado de individuos
    tasa_consumo = ifelse(total_individuos == 0, NA, total_env / total_individuos )  # Calcular la tasa como porcentaje
  )

write_xlsx(
  list(
    "Tasa envenentamiento total" = tasa_env_total,
    "Tasa envenenamiento drogas" = tasa_env
  ),
  "tasa_env2017.xlsx"
)

#2016
rm(list = ls())
setwd("C:/Users/cjara/OneDrive - Universidad Católica de Chile/Mercado drogas/DEIS")
datos <- read_xlsx("C:/Users/cjara/OneDrive - Universidad Católica de Chile/Mercado drogas/DEIS/egreso2016.xlsx")

dim(datos)

datos <- datos %>%
  mutate(env.drog = case_when(
    DIAG1 %in% c("T400", "T401", "T402", "T403", "T404", "T405", "T406", "T407", "T408", "T409", "T436", "T438") ~ 1,  # Valores específicos a 1
    TRUE ~ 0  # Todo el resto a 0
  ))
table(datos$env.drog)


datos <- datos %>%
  mutate(env.drog2 = case_when(
    DIAG2 %in% c("T400", "T401", "T402", "T403", "T404", "T405", "T406", "T407", "T408", "T409", "T436", "T438") ~ 1,  # Valores específicos a 1
    TRUE ~ 0  # Todo el resto a 0
  ))
table(datos$env.drog2)

tasa_env <- datos %>%
  group_by(REGION_RESIDENCIA) %>%
  summarise(
    total_env = sum(env.drog, na.rm = TRUE),  # Ignorar NA en los cálculos
    total_individuos = sum(1048575, na.rm = TRUE),             # Ignorar NA en los cálculos
    tasa_consumo = ifelse(total_individuos == 0, NA, total_env / total_individuos)  # Evitar división por cero
  )

tasa_env_total <- datos %>%
  summarise(
    total_env = sum(env.drog, na.rm = TRUE),   # Total ponderado de consumidores
    total_individuos = sum(1048575, na.rm = TRUE),            # Total ponderado de individuos
    tasa_consumo = ifelse(total_individuos == 0, NA, total_env / total_individuos )  # Calcular la tasa como porcentaje
  )

write_xlsx(
  list(
    "Tasa envenentamiento total" = tasa_env_total,
    "Tasa envenenamiento drogas" = tasa_env
  ),
  "tasa_env2016.xlsx"
)

#2015
rm(list = ls())
setwd("C:/Users/cjara/OneDrive - Universidad Católica de Chile/Mercado drogas/DEIS")
datos <- read_xlsx("C:/Users/cjara/OneDrive - Universidad Católica de Chile/Mercado drogas/DEIS/egreso2015.xlsx")

dim(datos)

datos <- datos %>%
  mutate(env.drog = case_when(
    DIAG1 %in% c("T400", "T401", "T402", "T403", "T404", "T405", "T406", "T407", "T408", "T409", "T436", "T438") ~ 1,  # Valores específicos a 1
    TRUE ~ 0  # Todo el resto a 0
  ))
table(datos$env.drog)


datos <- datos %>%
  mutate(env.drog2 = case_when(
    DIAG2 %in% c("T400", "T401", "T402", "T403", "T404", "T405", "T406", "T407", "T408", "T409", "T436", "T438") ~ 1,  # Valores específicos a 1
    TRUE ~ 0  # Todo el resto a 0
  ))
table(datos$env.drog2)

tasa_env <- datos %>%
  group_by(REGION_RESIDENCIA) %>%
  summarise(
    total_env = sum(env.drog, na.rm = TRUE),  # Ignorar NA en los cálculos
    total_individuos = sum(1048575, na.rm = TRUE),             # Ignorar NA en los cálculos
    tasa_consumo = ifelse(total_individuos == 0, NA, total_env / total_individuos)  # Evitar división por cero
  )

tasa_env_total <- datos %>%
  summarise(
    total_env = sum(env.drog, na.rm = TRUE),   # Total ponderado de consumidores
    total_individuos = sum(1048575, na.rm = TRUE),            # Total ponderado de individuos
    tasa_consumo = ifelse(total_individuos == 0, NA, total_env / total_individuos )  # Calcular la tasa como porcentaje
  )

write_xlsx(
  list(
    "Tasa envenentamiento total" = tasa_env_total,
    "Tasa envenenamiento drogas" = tasa_env
  ),
  "tasa_env2015.xlsx"
)

#2014
rm(list = ls())
setwd("C:/Users/cjara/OneDrive - Universidad Católica de Chile/Mercado drogas/DEIS")
datos <- read_xlsx("C:/Users/cjara/OneDrive - Universidad Católica de Chile/Mercado drogas/DEIS/egreso2014.xlsx")

dim(datos)

datos <- datos %>%
  mutate(env.drog = case_when(
    DIAG1 %in% c("T400", "T401", "T402", "T403", "T404", "T405", "T406", "T407", "T408", "T409", "T436", "T438") ~ 1,  # Valores específicos a 1
    TRUE ~ 0  # Todo el resto a 0
  ))
table(datos$env.drog)


datos <- datos %>%
  mutate(env.drog2 = case_when(
    DIAG2 %in% c("T400", "T401", "T402", "T403", "T404", "T405", "T406", "T407", "T408", "T409", "T436", "T438") ~ 1,  # Valores específicos a 1
    TRUE ~ 0  # Todo el resto a 0
  ))
table(datos$env.drog2)

tasa_env <- datos %>%
  group_by(REGION_RESIDENCIA) %>%
  summarise(
    total_env = sum(env.drog, na.rm = TRUE),  # Ignorar NA en los cálculos
    total_individuos = sum(1048575, na.rm = TRUE),             # Ignorar NA en los cálculos
    tasa_consumo = ifelse(total_individuos == 0, NA, total_env / total_individuos)  # Evitar división por cero
  )

tasa_env_total <- datos %>%
  summarise(
    total_env = sum(env.drog, na.rm = TRUE),   # Total ponderado de consumidores
    total_individuos = sum(1048575, na.rm = TRUE),            # Total ponderado de individuos
    tasa_consumo = ifelse(total_individuos == 0, NA, total_env / total_individuos )  # Calcular la tasa como porcentaje
  )

write_xlsx(
  list(
    "Tasa envenentamiento total" = tasa_env_total,
    "Tasa envenenamiento drogas" = tasa_env
  ),
  "tasa_env2014.xlsx"
)

#2013
rm(list = ls())
setwd("C:/Users/cjara/OneDrive - Universidad Católica de Chile/Mercado drogas/DEIS")
datos <- read_xlsx("C:/Users/cjara/OneDrive - Universidad Católica de Chile/Mercado drogas/DEIS/egreso2013.xlsx")

dim(datos)

datos <- datos %>%
  mutate(env.drog = case_when(
    DIAG1 %in% c("T400", "T401", "T402", "T403", "T404", "T405", "T406", "T407", "T408", "T409", "T436", "T438") ~ 1,  # Valores específicos a 1
    TRUE ~ 0  # Todo el resto a 0
  ))
table(datos$env.drog)


datos <- datos %>%
  mutate(env.drog2 = case_when(
    DIAG2 %in% c("T400", "T401", "T402", "T403", "T404", "T405", "T406", "T407", "T408", "T409", "T436", "T438") ~ 1,  # Valores específicos a 1
    TRUE ~ 0  # Todo el resto a 0
  ))
table(datos$env.drog2)

tasa_env <- datos %>%
  group_by(REGION_RESIDENCIA) %>%
  summarise(
    total_env = sum(env.drog, na.rm = TRUE),  # Ignorar NA en los cálculos
    total_individuos = sum(1048575, na.rm = TRUE),             # Ignorar NA en los cálculos
    tasa_consumo = ifelse(total_individuos == 0, NA, total_env / total_individuos)  # Evitar división por cero
  )

tasa_env_total <- datos %>%
  summarise(
    total_env = sum(env.drog, na.rm = TRUE),   # Total ponderado de consumidores
    total_individuos = sum(1048575, na.rm = TRUE),            # Total ponderado de individuos
    tasa_consumo = ifelse(total_individuos == 0, NA, total_env / total_individuos )  # Calcular la tasa como porcentaje
  )

write_xlsx(
  list(
    "Tasa envenentamiento total" = tasa_env_total,
    "Tasa envenenamiento drogas" = tasa_env
  ),
  "tasa_env2013.xlsx"
)

#2012
rm(list = ls())
setwd("C:/Users/cjara/OneDrive - Universidad Católica de Chile/Mercado drogas/DEIS")
datos <- read_xlsx("C:/Users/cjara/OneDrive - Universidad Católica de Chile/Mercado drogas/DEIS/egreso2012.xlsx")

dim(datos)

datos <- datos %>%
  mutate(env.drog = case_when(
    DIAG1 %in% c("T400", "T401", "T402", "T403", "T404", "T405", "T406", "T407", "T408", "T409", "T436", "T438") ~ 1,  # Valores específicos a 1
    TRUE ~ 0  # Todo el resto a 0
  ))
table(datos$env.drog)


datos <- datos %>%
  mutate(env.drog2 = case_when(
    DIAG2 %in% c("T400", "T401", "T402", "T403", "T404", "T405", "T406", "T407", "T408", "T409", "T436", "T438") ~ 1,  # Valores específicos a 1
    TRUE ~ 0  # Todo el resto a 0
  ))
table(datos$env.drog2)

tasa_env <- datos %>%
  group_by(REGION_RESIDENCIA) %>%
  summarise(
    total_env = sum(env.drog, na.rm = TRUE),  # Ignorar NA en los cálculos
    total_individuos = sum(1048575, na.rm = TRUE),             # Ignorar NA en los cálculos
    tasa_consumo = ifelse(total_individuos == 0, NA, total_env / total_individuos)  # Evitar división por cero
  )

tasa_env_total <- datos %>%
  summarise(
    total_env = sum(env.drog, na.rm = TRUE),   # Total ponderado de consumidores
    total_individuos = sum(1048575, na.rm = TRUE),            # Total ponderado de individuos
    tasa_consumo = ifelse(total_individuos == 0, NA, total_env / total_individuos )  # Calcular la tasa como porcentaje
  )

write_xlsx(
  list(
    "Tasa envenentamiento total" = tasa_env_total,
    "Tasa envenenamiento drogas" = tasa_env
  ),
  "tasa_env2012.xlsx"
)

#2011
rm(list = ls())
setwd("C:/Users/cjara/OneDrive - Universidad Católica de Chile/Mercado drogas/DEIS")
datos <- read_xlsx("C:/Users/cjara/OneDrive - Universidad Católica de Chile/Mercado drogas/DEIS/egreso2011.xlsx")

dim(datos)

datos <- datos %>%
  mutate(env.drog = case_when(
    DIAG1 %in% c("T400", "T401", "T402", "T403", "T404", "T405", "T406", "T407", "T408", "T409", "T436", "T438") ~ 1,  # Valores específicos a 1
    TRUE ~ 0  # Todo el resto a 0
  ))
table(datos$env.drog)


datos <- datos %>%
  mutate(env.drog2 = case_when(
    DIAG2 %in% c("T400", "T401", "T402", "T403", "T404", "T405", "T406", "T407", "T408", "T409", "T436", "T438") ~ 1,  # Valores específicos a 1
    TRUE ~ 0  # Todo el resto a 0
  ))
table(datos$env.drog2)

tasa_env <- datos %>%
  group_by(REGION_RESIDENCIA) %>%
  summarise(
    total_env = sum(env.drog, na.rm = TRUE),  # Ignorar NA en los cálculos
    total_individuos = sum(1048575, na.rm = TRUE),             # Ignorar NA en los cálculos
    tasa_consumo = ifelse(total_individuos == 0, NA, total_env / total_individuos)  # Evitar división por cero
  )

tasa_env_total <- datos %>%
  summarise(
    total_env = sum(env.drog, na.rm = TRUE),   # Total ponderado de consumidores
    total_individuos = sum(1048575, na.rm = TRUE),            # Total ponderado de individuos
    tasa_consumo = ifelse(total_individuos == 0, NA, total_env / total_individuos )  # Calcular la tasa como porcentaje
  )

write_xlsx(
  list(
    "Tasa envenentamiento total" = tasa_env_total,
    "Tasa envenenamiento drogas" = tasa_env
  ),
  "tasa_env2011.xlsx"
)

#2010
rm(list = ls())
setwd("C:/Users/cjara/OneDrive - Universidad Católica de Chile/Mercado drogas/DEIS")
datos <- read_xlsx("C:/Users/cjara/OneDrive - Universidad Católica de Chile/Mercado drogas/DEIS/egreso2010.xlsx")

dim(datos)

datos <- datos %>%
  mutate(env.drog = case_when(
    DIAG1 %in% c("T400", "T401", "T402", "T403", "T404", "T405", "T406", "T407", "T408", "T409", "T436", "T438") ~ 1,  # Valores específicos a 1
    TRUE ~ 0  # Todo el resto a 0
  ))
table(datos$env.drog)


datos <- datos %>%
  mutate(env.drog2 = case_when(
    DIAG2 %in% c("T400", "T401", "T402", "T403", "T404", "T405", "T406", "T407", "T408", "T409", "T436", "T438") ~ 1,  # Valores específicos a 1
    TRUE ~ 0  # Todo el resto a 0
  ))
table(datos$env.drog2)

tasa_env <- datos %>%
  group_by(REGION_RESIDENCIA) %>%
  summarise(
    total_env = sum(env.drog, na.rm = TRUE),  # Ignorar NA en los cálculos
    total_individuos = sum(1048575, na.rm = TRUE),             # Ignorar NA en los cálculos
    tasa_consumo = ifelse(total_individuos == 0, NA, total_env / total_individuos)  # Evitar división por cero
  )

tasa_env_total <- datos %>%
  summarise(
    total_env = sum(env.drog, na.rm = TRUE),   # Total ponderado de consumidores
    total_individuos = sum(1048575, na.rm = TRUE),            # Total ponderado de individuos
    tasa_consumo = ifelse(total_individuos == 0, NA, total_env / total_individuos )  # Calcular la tasa como porcentaje
  )

write_xlsx(
  list(
    "Tasa envenentamiento total" = tasa_env_total,
    "Tasa envenenamiento drogas" = tasa_env
  ),
  "tasa_env2010.xlsx"
)

#2009
rm(list = ls())
setwd("C:/Users/cjara/OneDrive - Universidad Católica de Chile/Mercado drogas/DEIS")
datos <- read_xlsx("C:/Users/cjara/OneDrive - Universidad Católica de Chile/Mercado drogas/DEIS/egreso2009.xlsx")

dim(datos)

datos <- datos %>%
  mutate(env.drog = case_when(
    DIAG1 %in% c("T400", "T401", "T402", "T403", "T404", "T405", "T406", "T407", "T408", "T409", "T436", "T438") ~ 1,  # Valores específicos a 1
    TRUE ~ 0  # Todo el resto a 0
  ))
table(datos$env.drog)


datos <- datos %>%
  mutate(env.drog2 = case_when(
    DIAG2 %in% c("T400", "T401", "T402", "T403", "T404", "T405", "T406", "T407", "T408", "T409", "T436", "T438") ~ 1,  # Valores específicos a 1
    TRUE ~ 0  # Todo el resto a 0
  ))
table(datos$env.drog2)

tasa_env <- datos %>%
  group_by(REGION_RESIDENCIA) %>%
  summarise(
    total_env = sum(env.drog, na.rm = TRUE),  # Ignorar NA en los cálculos
    total_individuos = sum(1048575, na.rm = TRUE),             # Ignorar NA en los cálculos
    tasa_consumo = ifelse(total_individuos == 0, NA, total_env / total_individuos)  # Evitar división por cero
  )

tasa_env_total <- datos %>%
  summarise(
    total_env = sum(env.drog, na.rm = TRUE),   # Total ponderado de consumidores
    total_individuos = sum(1048575, na.rm = TRUE),            # Total ponderado de individuos
    tasa_consumo = ifelse(total_individuos == 0, NA, total_env / total_individuos )  # Calcular la tasa como porcentaje
  )

write_xlsx(
  list(
    "Tasa envenentamiento total" = tasa_env_total,
    "Tasa envenenamiento drogas" = tasa_env
  ),
  "tasa_env2009.xlsx"
)

#2008
rm(list = ls())
setwd("C:/Users/cjara/OneDrive - Universidad Católica de Chile/Mercado drogas/DEIS")
datos <- read_xlsx("C:/Users/cjara/OneDrive - Universidad Católica de Chile/Mercado drogas/DEIS/egreso2008.xlsx")

dim(datos)

datos <- datos %>%
  mutate(env.drog = case_when(
    DIAG1 %in% c("T400", "T401", "T402", "T403", "T404", "T405", "T406", "T407", "T408", "T409", "T436", "T438") ~ 1,  # Valores específicos a 1
    TRUE ~ 0  # Todo el resto a 0
  ))
table(datos$env.drog)


datos <- datos %>%
  mutate(env.drog2 = case_when(
    DIAG2 %in% c("T400", "T401", "T402", "T403", "T404", "T405", "T406", "T407", "T408", "T409", "T436", "T438") ~ 1,  # Valores específicos a 1
    TRUE ~ 0  # Todo el resto a 0
  ))
table(datos$env.drog2)

tasa_env <- datos %>%
  group_by(REGION_RESIDENCIA) %>%
  summarise(
    total_env = sum(env.drog, na.rm = TRUE),  # Ignorar NA en los cálculos
    total_individuos = sum(1048575, na.rm = TRUE),             # Ignorar NA en los cálculos
    tasa_consumo = ifelse(total_individuos == 0, NA, total_env / total_individuos)  # Evitar división por cero
  )

tasa_env_total <- datos %>%
  summarise(
    total_env = sum(env.drog, na.rm = TRUE),   # Total ponderado de consumidores
    total_individuos = sum(1048575, na.rm = TRUE),            # Total ponderado de individuos
    tasa_consumo = ifelse(total_individuos == 0, NA, total_env / total_individuos )  # Calcular la tasa como porcentaje
  )

write_xlsx(
  list(
    "Tasa envenentamiento total" = tasa_env_total,
    "Tasa envenenamiento drogas" = tasa_env
  ),
  "tasa_env2008.xlsx"
)

#2007
rm(list = ls())
setwd("C:/Users/cjara/OneDrive - Universidad Católica de Chile/Mercado drogas/DEIS")
datos <- read_xlsx("C:/Users/cjara/OneDrive - Universidad Católica de Chile/Mercado drogas/DEIS/egreso2007.xlsx")

dim(datos)

datos <- datos %>%
  mutate(env.drog = case_when(
    DIAG1 %in% c("T400", "T401", "T402", "T403", "T404", "T405", "T406", "T407", "T408", "T409", "T436", "T438") ~ 1,  # Valores específicos a 1
    TRUE ~ 0  # Todo el resto a 0
  ))
table(datos$env.drog)


datos <- datos %>%
  mutate(env.drog2 = case_when(
    DIAG2 %in% c("T400", "T401", "T402", "T403", "T404", "T405", "T406", "T407", "T408", "T409", "T436", "T438") ~ 1,  # Valores específicos a 1
    TRUE ~ 0  # Todo el resto a 0
  ))
table(datos$env.drog2)

tasa_env <- datos %>%
  group_by(REGION_RESIDENCIA) %>%
  summarise(
    total_env = sum(env.drog, na.rm = TRUE),  # Ignorar NA en los cálculos
    total_individuos = sum(1048575, na.rm = TRUE),             # Ignorar NA en los cálculos
    tasa_consumo = ifelse(total_individuos == 0, NA, total_env / total_individuos)  # Evitar división por cero
  )

tasa_env_total <- datos %>%
  summarise(
    total_env = sum(env.drog, na.rm = TRUE),   # Total ponderado de consumidores
    total_individuos = sum(1048575, na.rm = TRUE),            # Total ponderado de individuos
    tasa_consumo = ifelse(total_individuos == 0, NA, total_env / total_individuos )  # Calcular la tasa como porcentaje
  )

write_xlsx(
  list(
    "Tasa envenentamiento total" = tasa_env_total,
    "Tasa envenenamiento drogas" = tasa_env
  ),
  "tasa_env2007.xlsx"
)

#2006
rm(list = ls())
setwd("C:/Users/cjara/OneDrive - Universidad Católica de Chile/Mercado drogas/DEIS")
datos <- read_xlsx("C:/Users/cjara/OneDrive - Universidad Católica de Chile/Mercado drogas/DEIS/egreso2006.xlsx")

dim(datos)

datos <- datos %>%
  mutate(env.drog = case_when(
    DIAG1 %in% c("T400", "T401", "T402", "T403", "T404", "T405", "T406", "T407", "T408", "T409", "T436", "T438") ~ 1,  # Valores específicos a 1
    TRUE ~ 0  # Todo el resto a 0
  ))
table(datos$env.drog)


datos <- datos %>%
  mutate(env.drog2 = case_when(
    DIAG2 %in% c("T400", "T401", "T402", "T403", "T404", "T405", "T406", "T407", "T408", "T409", "T436", "T438") ~ 1,  # Valores específicos a 1
    TRUE ~ 0  # Todo el resto a 0
  ))
table(datos$env.drog2)

tasa_env <- datos %>%
  group_by(REGION_RESIDENCIA) %>%
  summarise(
    total_env = sum(env.drog, na.rm = TRUE),  # Ignorar NA en los cálculos
    total_individuos = sum(1048575, na.rm = TRUE),             # Ignorar NA en los cálculos
    tasa_consumo = ifelse(total_individuos == 0, NA, total_env / total_individuos)  # Evitar división por cero
  )

tasa_env_total <- datos %>%
  summarise(
    total_env = sum(env.drog, na.rm = TRUE),   # Total ponderado de consumidores
    total_individuos = sum(1048575, na.rm = TRUE),            # Total ponderado de individuos
    tasa_consumo = ifelse(total_individuos == 0, NA, total_env / total_individuos )  # Calcular la tasa como porcentaje
  )

write_xlsx(
  list(
    "Tasa envenentamiento total" = tasa_env_total,
    "Tasa envenenamiento drogas" = tasa_env
  ),
  "tasa_env2006.xlsx"
)

#2005
rm(list = ls())
setwd("C:/Users/cjara/OneDrive - Universidad Católica de Chile/Mercado drogas/DEIS")
datos <- read_xlsx("C:/Users/cjara/OneDrive - Universidad Católica de Chile/Mercado drogas/DEIS/egreso2005.xlsx")

dim(datos)

datos <- datos %>%
  mutate(env.drog = case_when(
    DIAG1 %in% c("T400", "T401", "T402", "T403", "T404", "T405", "T406", "T407", "T408", "T409", "T436", "T438") ~ 1,  # Valores específicos a 1
    TRUE ~ 0  # Todo el resto a 0
  ))
table(datos$env.drog)


datos <- datos %>%
  mutate(env.drog2 = case_when(
    DIAG2 %in% c("T400", "T401", "T402", "T403", "T404", "T405", "T406", "T407", "T408", "T409", "T436", "T438") ~ 1,  # Valores específicos a 1
    TRUE ~ 0  # Todo el resto a 0
  ))
table(datos$env.drog2)

tasa_env <- datos %>%
  group_by(REGION_RESIDENCIA) %>%
  summarise(
    total_env = sum(env.drog, na.rm = TRUE),  # Ignorar NA en los cálculos
    total_individuos = sum(1048575, na.rm = TRUE),             # Ignorar NA en los cálculos
    tasa_consumo = ifelse(total_individuos == 0, NA, total_env / total_individuos)  # Evitar división por cero
  )

tasa_env_total <- datos %>%
  summarise(
    total_env = sum(env.drog, na.rm = TRUE),   # Total ponderado de consumidores
    total_individuos = sum(1048575, na.rm = TRUE),            # Total ponderado de individuos
    tasa_consumo = ifelse(total_individuos == 0, NA, total_env / total_individuos )  # Calcular la tasa como porcentaje
  )

write_xlsx(
  list(
    "Tasa envenentamiento total" = tasa_env_total,
    "Tasa envenenamiento drogas" = tasa_env
  ),
  "tasa_env2005.xlsx"
)

#2004
rm(list = ls())
setwd("C:/Users/cjara/OneDrive - Universidad Católica de Chile/Mercado drogas/DEIS")
datos <- read_xlsx("C:/Users/cjara/OneDrive - Universidad Católica de Chile/Mercado drogas/DEIS/egreso2004.xlsx")

dim(datos)

datos <- datos %>%
  mutate(env.drog = case_when(
    DIAG1 %in% c("T400", "T401", "T402", "T403", "T404", "T405", "T406", "T407", "T408", "T409", "T436", "T438") ~ 1,  # Valores específicos a 1
    TRUE ~ 0  # Todo el resto a 0
  ))
table(datos$env.drog)


datos <- datos %>%
  mutate(env.drog2 = case_when(
    DIAG2 %in% c("T400", "T401", "T402", "T403", "T404", "T405", "T406", "T407", "T408", "T409", "T436", "T438") ~ 1,  # Valores específicos a 1
    TRUE ~ 0  # Todo el resto a 0
  ))
table(datos$env.drog2)

tasa_env <- datos %>%
  group_by(REGION_RESIDENCIA) %>%
  summarise(
    total_env = sum(env.drog, na.rm = TRUE),  # Ignorar NA en los cálculos
    total_individuos = sum(1048575, na.rm = TRUE),             # Ignorar NA en los cálculos
    tasa_consumo = ifelse(total_individuos == 0, NA, total_env / total_individuos)  # Evitar división por cero
  )

tasa_env_total <- datos %>%
  summarise(
    total_env = sum(env.drog, na.rm = TRUE),   # Total ponderado de consumidores
    total_individuos = sum(1048575, na.rm = TRUE),            # Total ponderado de individuos
    tasa_consumo = ifelse(total_individuos == 0, NA, total_env / total_individuos )  # Calcular la tasa como porcentaje
  )

write_xlsx(
  list(
    "Tasa envenentamiento total" = tasa_env_total,
    "Tasa envenenamiento drogas" = tasa_env
  ),
  "tasa_env2004.xlsx"
)

#2003
rm(list = ls())
setwd("C:/Users/cjara/OneDrive - Universidad Católica de Chile/Mercado drogas/DEIS")
datos <- read_xlsx("C:/Users/cjara/OneDrive - Universidad Católica de Chile/Mercado drogas/DEIS/egreso2003.xlsx")

dim(datos)

datos <- datos %>%
  mutate(env.drog = case_when(
    DIAG1 %in% c("T400", "T401", "T402", "T403", "T404", "T405", "T406", "T407", "T408", "T409", "T436", "T438") ~ 1,  # Valores específicos a 1
    TRUE ~ 0  # Todo el resto a 0
  ))
table(datos$env.drog)


datos <- datos %>%
  mutate(env.drog2 = case_when(
    DIAG2 %in% c("T400", "T401", "T402", "T403", "T404", "T405", "T406", "T407", "T408", "T409", "T436", "T438") ~ 1,  # Valores específicos a 1
    TRUE ~ 0  # Todo el resto a 0
  ))
table(datos$env.drog2)

tasa_env <- datos %>%
  group_by(REGION_RESIDENCIA) %>%
  summarise(
    total_env = sum(env.drog, na.rm = TRUE),  # Ignorar NA en los cálculos
    total_individuos = sum(1048575, na.rm = TRUE),             # Ignorar NA en los cálculos
    tasa_consumo = ifelse(total_individuos == 0, NA, total_env / total_individuos)  # Evitar división por cero
  )

tasa_env_total <- datos %>%
  summarise(
    total_env = sum(env.drog, na.rm = TRUE),   # Total ponderado de consumidores
    total_individuos = sum(1048575, na.rm = TRUE),            # Total ponderado de individuos
    tasa_consumo = ifelse(total_individuos == 0, NA, total_env / total_individuos )  # Calcular la tasa como porcentaje
  )

write_xlsx(
  list(
    "Tasa envenentamiento total" = tasa_env_total,
    "Tasa envenenamiento drogas" = tasa_env
  ),
  "tasa_env2003.xlsx"
)

#2002
rm(list = ls())
setwd("C:/Users/cjara/OneDrive - Universidad Católica de Chile/Mercado drogas/DEIS")
datos <- read_xlsx("C:/Users/cjara/OneDrive - Universidad Católica de Chile/Mercado drogas/DEIS/egreso2002.xlsx")

dim(datos)

datos <- datos %>%
  mutate(env.drog = case_when(
    DIAG1 %in% c("T400", "T401", "T402", "T403", "T404", "T405", "T406", "T407", "T408", "T409", "T436", "T438") ~ 1,  # Valores específicos a 1
    TRUE ~ 0  # Todo el resto a 0
  ))
table(datos$env.drog)


datos <- datos %>%
  mutate(env.drog2 = case_when(
    DIAG2 %in% c("T400", "T401", "T402", "T403", "T404", "T405", "T406", "T407", "T408", "T409", "T436", "T438") ~ 1,  # Valores específicos a 1
    TRUE ~ 0  # Todo el resto a 0
  ))
table(datos$env.drog2)

tasa_env <- datos %>%
  group_by(REGION_RESIDENCIA) %>%
  summarise(
    total_env = sum(env.drog, na.rm = TRUE),  # Ignorar NA en los cálculos
    total_individuos = sum(1048575, na.rm = TRUE),             # Ignorar NA en los cálculos
    tasa_consumo = ifelse(total_individuos == 0, NA, total_env / total_individuos)  # Evitar división por cero
  )

tasa_env_total <- datos %>%
  summarise(
    total_env = sum(env.drog, na.rm = TRUE),   # Total ponderado de consumidores
    total_individuos = sum(1048575, na.rm = TRUE),            # Total ponderado de individuos
    tasa_consumo = ifelse(total_individuos == 0, NA, total_env / total_individuos )  # Calcular la tasa como porcentaje
  )

write_xlsx(
  list(
    "Tasa envenentamiento total" = tasa_env_total,
    "Tasa envenenamiento drogas" = tasa_env
  ),
  "tasa_env2002.xlsx"
)

#2001
rm(list = ls())
setwd("C:/Users/cjara/OneDrive - Universidad Católica de Chile/Mercado drogas/DEIS")
datos <- read_xlsx("C:/Users/cjara/OneDrive - Universidad Católica de Chile/Mercado drogas/DEIS/egreso2001.xlsm")

dim(datos)

datos <- datos %>%
  mutate(env.drog = case_when(
    DIAG1 %in% c("T400", "T401", "T402", "T403", "T404", "T405", "T406", "T407", "T408", "T409", "T436", "T438") ~ 1,  # Valores específicos a 1
    TRUE ~ 0  # Todo el resto a 0
  ))
table(datos$env.drog)


datos <- datos %>%
  mutate(env.drog2 = case_when(
    DIAG2 %in% c("T400", "T401", "T402", "T403", "T404", "T405", "T406", "T407", "T408", "T409", "T436", "T438") ~ 1,  # Valores específicos a 1
    TRUE ~ 0  # Todo el resto a 0
  ))
table(datos$env.drog2)

tasa_env <- datos %>%
  group_by(REGION_RESIDENCIA) %>%
  summarise(
    total_env = sum(env.drog, na.rm = TRUE),  # Ignorar NA en los cálculos
    total_individuos = sum(1048575, na.rm = TRUE),             # Ignorar NA en los cálculos
    tasa_consumo = ifelse(total_individuos == 0, NA, total_env / total_individuos)  # Evitar división por cero
  )

tasa_env_total <- datos %>%
  summarise(
    total_env = sum(env.drog, na.rm = TRUE),   # Total ponderado de consumidores
    total_individuos = sum(1048575, na.rm = TRUE),            # Total ponderado de individuos
    tasa_consumo = ifelse(total_individuos == 0, NA, total_env / total_individuos )  # Calcular la tasa como porcentaje
  )

write_xlsx(
  list(
    "Tasa envenentamiento total" = tasa_env_total,
    "Tasa envenenamiento drogas" = tasa_env
  ),
  "tasa_env2001.xlsx"
)
