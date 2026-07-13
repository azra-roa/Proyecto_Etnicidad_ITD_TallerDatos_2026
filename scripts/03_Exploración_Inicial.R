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

enaho_limpia <- read_parquet(here("datos", "procesados", "enaho_2025_12_07_26.parquet"))

# ------------------------------------------------------------------------------
# 1. PREPARACIÓN DE ETIQUETAS--------------------------------------------------- 
# ------------------------------------------------------------------------------
enaho_explorar <- enaho_limpia %>%
  mutate(
    # A. Variables sociodemográficas
    sexo_etiqueta = factor(sexo,
                           levels = c(1, 2),
                           labels = c("Hombre", "Mujer")),
    
    educacion_etiqueta = factor(educacion,
                                levels = 1:12,
                                labels = c("Sin nivel",
                                           "Educación inicial",
                                           "Primaria incompleta",
                                           "Primaria completa",
                                           "Secundaria incompleta",
                                           "Secundaria completa",
                                           "Sup. no univ. incompleta",
                                           "Sup. no univ. completa",
                                           "Sup. univ. incompleta",
                                           "Sup. univ. completa",
                                           "Maestría/Doctorado",
                                           "Básica especial")),
    
    etnicidad_etiqueta = factor(etnicidad,
                                levels = c(1, 2, 3, 4, 5, 6, 7, 9),  # se excluye el 8 ("No sabe/No responde")
                                labels = c("Quechua",
                                           "Aimara",
                                           "Nativo/indígena amazónico",
                                           "Negro/Moreno/Zambo/Mulato/Afroperuano",
                                           "Blanco",
                                           "Mestizo",
                                           "Otro",
                                           "Perteneciente a otro pueblo indígena u originario")),
    # B. Variables laborales
    tiene_ruc_etiqueta = factor(tiene_ruc,
                                levels = c(1, 2, 3),
                                labels = c("Persona Jurídica",
                                           "Persona Natural (con RUC/RUS/RER u otro)",
                                           "No está registrado (no tiene RUC)")),
    
    tiene_contrato_etiqueta = factor(tiene_contrato,
                                     levels = 1:8,
                                     labels = c("Indefinido/nombrado/permanente",
                                                "Plazo fijo (sujeto a modalidad)",
                                                "Período de prueba",
                                                "Convenio de Formación Laboral Juvenil",
                                                "Locación de servicios (honorarios, RUC)",
                                                "CAS",
                                                "Sin contrato",
                                                "Otro")),
    
    temp_pago_etiqueta = factor(temp_pago,
                                levels = c(1, 2, 3, 4),
                                labels = c("Diario",
                                           "Semanal",
                                           "Quincenal",
                                           "Mensual")),
    
    pension_no_etiqueta = factor(pension_no,
                                 levels = c(0, 5),
                                 labels = c("Afiliado",
                                            "No está afiliado")),
  # C. Limpieza Numérica Estricta - verificación de tipo (ya son numeric, se confirma por seguridad)
    factor200 = as.numeric(factor200),
    factor300 = as.numeric(factor300),
    factor500 = as.numeric(factor500)
    
)

write_parquet(enaho_explorar, "datos/procesados/enaho_explorar_2025_12_06_26.parquet")



