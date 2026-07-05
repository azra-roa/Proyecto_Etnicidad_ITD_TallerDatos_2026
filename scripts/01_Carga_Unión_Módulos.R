#===========================================================================================
#Proyecto: Análisis de la relacion etnicidad-ITD usando datos de la ENAHO
#Autorxs: Carmen Andonayre y Azra Roa
#Objetivo de este script: Carga de modulos y realizar joins
#Fecha: 04-07-2026
#==========================================================================================

#1. Carga de librerias------------------------
library(rio)
library(janitor)
library(haven) 
library(dplyr) 
library(tidyr)
renv::snapshot()

#2. Importación de datos----------------------

mod200 <- read_dta("datos/crudos/enaho01-2025-200.dta") %>% rename_with(tolower) #Se ha utilizado tolower para transformar todos los nombres de variables a minúsculas
mod300 <- read_dta("datos/crudos/enaho01a-2025-300.dta") %>% rename_with(tolower)
mod500 <- read_dta("datos/crudos/enaho01a-2025-500.dta") %>% rename_with(tolower)

#3. Unión de bases----------------------------

#Se procede primero con la creación de llaves para que sea posible la unión entre los módulos

keys_hogar <- c("año", "mes", "conglome", "vivienda", "hogar", "ubigeo", "dominio", 
                "estrato", "nconglome", "sub_conglome")
keys_persona <- c(keys_hogar, "codperso", "p203", "p204", "p205", 
                  "p206", "p207", "p208a", "p209") 

#Se realiza la unión entre los módulos

enaho_2025 <- mod500 %>% 
  left_join(mod300, by = keys_persona) %>% 
  left_join(mod200, by = keys_persona)

#4. Exportación de la base creada
library(arrow)
renv::snapshot()
write_parquet(enaho_2025, "datos/procesados/enaho_2025.parquet")
