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

