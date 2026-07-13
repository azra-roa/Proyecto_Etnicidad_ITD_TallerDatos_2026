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

