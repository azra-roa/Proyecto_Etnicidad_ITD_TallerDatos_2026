# ====================================================================================
#Proyecto: Análisis de la relacion etnicidad-ITD usando datos de la ENAHO 
#Autorxs: Carmen Andonayre y Azra Roa
#Objetivo de este script: Explorar la base de datos acondicionada
#Fecha: 12-07-2026
# =====================================================================================

rm(list = ls())

# ------------------------------------------------------------------------------
# 0. CONFIGURACIÓN Y CARGA DE DATOS
# ------------------------------------------------------------------------------
library(webshot2)
library(tidyverse)
library(arrow)
library(survey)      
library(srvyr)       
library(flextable)   
library(scales)      
library(officer)
library(here)

renv::snapshot()