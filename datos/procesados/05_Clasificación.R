#===============================================================================
#Proyecto: Análisis de la relacion etnicidad-ITD usando datos de la ENAHO
#Script: Clasificación
#Autorxs: Carmen Andonayre y Azra Roa
#Objetivo: Creación de variables analíticas con las variables escogidas
#Fecha: 11-07-2026
#===============================================================================

# ------------------------------------------------------------------------------
# 0. CONFIGURACIÓN DEL ENTORNO--------------------------------------------------
# ------------------------------------------------------------------------------
library(tidyverse)   # Para la manipulación, exploración y transformación de la base de datos
library(arrow)       # Para el uso y lectura del formato parquet
library(survey)      # Para el uso del factor de expansión
library(srvyr)       # Para el uso de dplyr con encuestas complejas
library(here)        # Para el facil acceso a los datos en el proyecto
library(gtsummary)   # Para la creación de resumenes de la base de datos 
library(flextable)   # Para la creación de tablas 
library(Hmisc)       # Para la creación de edad Z
renv::snapshot()

# Carga de la base de datos acondicionada
enaho_clasificar1 <- read_parquet(here("datos", "procesados", "enaho_2025_12_07_26.parquet"))