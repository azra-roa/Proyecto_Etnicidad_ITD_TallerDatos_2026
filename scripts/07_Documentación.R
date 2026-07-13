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

#Exportamos como la base de datos final de nuestro proyecto
write_parquet(enaho_codebook, here("datos", "procesados", "enaho_final_2025_12_07_26.parquet"))

# ==============================================================================
# 2. ASIGNACIÓN DE METADATOS----------------------------------------------------
# ==============================================================================
# Creación de labels para dar al codebook la etiqueta descriptiva y la fuente original de cada variable
# Se ha usado var_label() para darles un nombre humano y coherente

# A. Variables Base Exploradas (Etiquetadas)
var_label(enaho_codebook$sexo) <- "Sexo del encuestado/a (Fuente: P207)"
var_label(enaho_codebook$etnicidad) <- "Autoidentificación Étnica por Antepasados (Fuente: P558C)"
var_label(enaho_codebook$educacion) <- "Grado de Estudios (Fuente: P301A)"
var_label(enaho_codebook$edad) <- "Edad del encuestado/a (Fuente: P207)"
var_label(enaho_codebook$tiene_pension) <- "Condición de Afiliación a Sistema de Pensiones (Fuente: P558A5)"
var_label(enaho_codebook$tiene_registro) <- "Registro en SUNAT del centro de trabajo (Fuente: P510A1)"
var_label(enaho_codebook$tiene_contratos) <- "Condición de Contratación (Fuente: P511A)"
var_label(enaho_codebook$horas_decente) <- "Horas Semanales Trabajadas (Fuente: I513T)"

# B. Variables Analíticas (Clasificadas)
var_label(enaho_codebook$edad_teoria) <- "Grupo de Edad (Cortes segúm metodología INEI) (Fuente: P208A)"
var_label(enaho_codebook$edad_z) <- "Edad Estandarizada (Puntaje Z)"
var_label(enaho_codebook$estrato_teo) <- "Clasificación Geográfica (Teórica)"
var_label(enaho_codebook$ingreso_mensual_imp) <- "Ingreso Mensual (Imputado)"
var_label(enaho_codebook$quintil_ingreso) <- "Quintil de Ingresos (Fuente: ingreso_mensual_imp)"
var_label(enaho_codebook$ingreso_decente) <- "Ingreso Mensual (En relacion a RMV) (Fuente: ingreso_mensual_imp"
var_label(enaho_codebook$indice_aditivo) <- "Índice de Trabajo Decente simple (del 1-4)"
var_label(enaho_codebook$ITD) <- "Índice de Trabajo Decente (ITD)"
