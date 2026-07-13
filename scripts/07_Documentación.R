#===============================================================================
#Proyecto: Análisis de la relacion etnicidad-ITD usando datos de la ENAHO
#Script: Documentación
#Autorxs: Carmen Andonayre y Azra Roa
#Objetivo: Añadir metadatos a la base analítica y generar el codebook final
#Fecha: 12-07-2026
#===============================================================================

# ------------------------------------------------------------------------------
# 0. CONFIGURACIÓN Y PAQUETES---------------------------------------------------
# ------------------------------------------------------------------------------

library(tidyverse)
library(arrow)
library(here)
library(labelled)  # Para inyectar etiquetas y metadatos en las variables
library(codebook)  # Para automatizar el libro de códigos interactivo
renv::snapshot()

# Cargamos nuestra base de datos analítica final (fruto de EXTRAER a CLASIFICAR)
enaho_final <- read_parquet(here("datos", "procesados", "enaho_analitica_2025_12_07_26.parquet"))

# ==============================================================================
# 1. SELECCIÓN DE VARIABLES PARA EL CODEBOOK------------------------------------
# ==============================================================================

#Creamos una base de datos solo con las variables de la exploración analitica y creadas

enaho_codebook <- enaho_final %>%
  select(
    sexo, etnicidad, educacion, edad, 
    tiene_pension, tiene_registro, tiene_contratos, 
    horas_decente, edad_teoria, edad_z, 
    estrato_teo, ingreso_mensual_imp, 
    quintil_ingreso, ingreso_decente, indice_aditivo, ITD
  ) %>%
  mutate(across(where(is.character), as.factor)) #Convertimos las variables de character a factor para que "Codebook" detecte nuestras etiquetas

