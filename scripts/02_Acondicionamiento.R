#===========================================================================================
#Proyecto: Análisis de la relacion etnicidad-ITD usando datos de la ENAHO
#Script: Acondicionamiento
#Autorxs: Carmen Andonayre y Azra Roa
#Objetivo: Acondicionamiento de la base de datos (Selección, renombrado,
#          tratamiento de NAs)
#Fecha: 04-07-2026
#==========================================================================================

# ------------------------------------------------------------------------------
# 0. CONFIGURACIÓN DEL ENTORNO--------------------------------------------------
# ------------------------------------------------------------------------------
library(tidyverse)
library(arrow)
library(janitor)
library(naniar)
renv::snapshot()

# ------------------------------------------------------------------------------
# 1. CARGA, SELECCIÓN y RENOMBRADO---------------------------------
# ------------------------------------------------------------------------------
# Carga de la base de datos creada en el script 01 
enaho_raw <- read_parquet("datos/procesados/enaho_2025.parquet")

#Seleccionamos las variables de interes para el proyecto
#Se realizará el renombrado de variables en la misma acción
enaho_seleccion <- enaho_raw %>%
  select(
    #Variables de identificación
    año = año,
    mes = mes,
    conglome = conglome, 
    vivienda = vivienda, 
    hogar    = hogar, 
    codperso  = codperso, 
    ubigeo   = ubigeo, 
    dominio  = dominio, 
    estrato  = estrato,
  
    #Variables para análisis
    sexo = p207,
    etnicidad = p558c,
    educacion = p301a.y,
    edad = p208a,
    pension_no = p558a5,
    horas_sem = i513t,
    temp_pago = p523,
    ing_prin = p524a1,
    tiene_ruc = p510a1,
    tiene_contrato = p511a,
    
    #Factores de expansión
    factor200 = facpob07,
    factor300 = factora07,
    factor500 = fac500a,
  )

# Inspección preliminar de la base
dim(enaho_seleccion)        # Filas y columnas post join y selección
names(enaho_seleccion)      # Verificación de nombres
glimpse(enaho_seleccion)    # Revisión crítica de cómo R interpretó los tipos de datos