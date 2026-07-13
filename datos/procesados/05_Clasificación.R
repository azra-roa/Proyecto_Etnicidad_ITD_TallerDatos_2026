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

# ==============================================================================
# 1. CREACIÓN DE VARIABLES ANALÍTICAS-------------------------------------------
# ==============================================================================

# Cálculo de valores para la creación de edad estandarizada (edad Z)
media_edad_pond <- weighted.mean(enaho_clasificar1$edad, enaho_clasificar1$factor500, na.rm = TRUE)
sd_edad_pond <- sqrt(Hmisc::wtd.var(enaho_clasificar1$edad, enaho_clasificar1$factor500, na.rm = TRUE))

# Construcción de la base analítica 
enaho_clasificar2 <- enaho_clasificar1 %>%
  mutate(
    
    # ------------------------------------------------------------------------------
    # A. RECODIFICACIONES VARIABLE DEMOGRÁFICAS
    # ------------------------------------------------------------------------------
    
    #SEXO, guiado por diccionario ENAHO 2025
    sexo = factor(sexo, levels = c(1, 2), labels = c("Hombre", "Mujer")),
    
    #Edad, guiado por teoría (INEI)
    edad_teoria = case_when(
      edad < 30 ~ "18 a 29 años",
      edad < 45 ~ "30 a 44 años",
      edad < 60 ~ "45 a 59 años",
      TRUE ~ "60 años a más"
    ),
    
    #EDAD estandarizada (EDAD Z)
    edad_z = (edad - media_edad_pond) / sd_edad_pond,
    
    #EDUCACION, agrupado por criterio
    educacion = case_when(
      educacion %in% 1:4 ~ "Primaria o Menor",
      educacion %in% 5:6 ~ "Secundaria",
      educacion %in% 7:8 ~ "Superior No Universitaria",
      educacion %in% 9:11 ~ "Superior Universitaria/Posgrado"
    ),
    
    #ESTRATO, agrupado por teoria
    estrato_teo = case_when(
      estrato %in% 1:5 ~ "Urbano",
      estrato %in% 6:8 ~ "Rural"
    ),
    
    #ETNICIDAD, agrupado por teoria (Garavito)
    etnicidad = case_when(
      etnicidad %in% c(1, 2, 3, 9) ~ "Indígena",
      etnicidad %in% c(4, 5, 6, 7) ~ "No Indígena"
    ),
    
    #INGRESO MENSUAL
    
    #Redoficación de temporalidad de pago
    temp_pago = case_when(
      temp_pago == 1 ~ 30,
      temp_pago == 2 ~ 4,
      temp_pago == 3 ~ 2,
      temp_pago == 4 ~ 1,
    ),
    
    #Creación ingreso mensual
    ingreso_mensual_imp = temp_pago * ing_prin,
    
    #Creación de ingreso por quintiles
    quintil_ingreso = ntile(ingreso_mensual_imp, 5))

# ------------------------------------------------------------------------------
# B. CREACIÓN INDICE DE TRABAJO DECENTE (ITD)
# ------------------------------------------------------------------------------
#Preparación de variables a incluir en el Indice
enaho_clasificar3 <- enaho_clasificar2 %>%
  mutate(
    #SISTEMA DE PENSIONES (Se verificó que aquellas personas que respondian "Pase" contaban con algun sistema de pensiones)
    pension_no = case_when(
      pension_no == 0 ~ 1,
      pension_no == 5 ~ 0,
    ),
    
    #Redodificación para EDA de variables analíticas
    tiene_pension = case_when(
      pension_no == 1 ~ "Tiene pension",
      pension_no == 0 ~ "No tiene pension",
    ),
    
    #REGISTRO SUNAT
    tiene_ruc = case_when(
      tiene_ruc %in% c(1, 2) ~ 1,
      tiene_ruc == 3 ~ 0,
    ),
    
    #Redodificación para EDA de variables analíticas
    tiene_registro = case_when(
      tiene_ruc == 1 ~ "Tiene RUC",
      tiene_ruc == 0 ~ "No tiene RUC",
    ),
    
    #TIENE CONTRATO 
    tiene_contrato = case_when(
      tiene_contrato %in% c(1, 2, 3, 4, 5, 6, 8) ~ 1,
      tiene_contrato == 7 ~ 0,
    ),
    
    #Redodificación para EDA de variables analíticas
    tiene_contratos = case_when(
      tiene_contrato == 1 ~ "Tiene contrato",
      tiene_contrato == 0 ~ "No tiene contrato",
    ),
    
    #INDICADOR MODALIDAD CONTRATACIÓN (Cruce tipo de contrato y registro)
    mod_contratacion = ifelse(
      tiene_contrato == 1 | tiene_ruc == 1, 
      1, 0
    ),
    
    #HORAS TRABAJADAS
    horas_sem = case_when(
      horas_sem <= 48 ~ 1,
      horas_sem > 48 ~ 0,
    ),
    
    #Redodificación para EDA de variables analíticas
    horas_decente = case_when(
      horas_sem == 1 ~ "Menos de 48 horas semanales",
      horas_sem == 0 ~ "Más de 48 horas semanales",
    ),
    
    #INGRESOS
    ingreso_cumple = case_when(
      ingreso_mensual_imp >= 1130 ~ 1,
      ingreso_mensual_imp < 1130 ~ 0,
    ),
    
    #Redodificación para EDA de variables analíticas segun valores de RMV 2025
    ingreso_decente = case_when(
      ingreso_cumple == 1 ~ "Ingreso mensual mayor a RMV",
      ingreso_cumple == 0 ~ "Ingreso mensual menor a RMV",
    ))

#ÍNDICE DE TRABAJO DECENTE

#Creación de indice aditivo sumando las dimensiones de contratacion, sistema de pensiones, horas semanales trabajadas y ingresos en relacion a la RMV
enaho_clasificar3$indice_aditivo <- rowSums(enaho_clasificar3[, c("mod_contratacion", "pension_no", "horas_sem", "ingreso_cumple")], na.rm = TRUE)

#Transformación del índice a una escala de 0-100
min_indice <- min(enaho_clasificar3$indice_aditivo)
max_indice <- max(enaho_clasificar3$indice_aditivo)

enaho_clasificar3$ITD <- ((enaho_clasificar3$indice_aditivo - min_indice) / 
                            (max_indice - min_indice)) * 100

#Recordatorio, se decidió excluir la dimensión seguro de salud dado que desde el 2019 es universal la afiliación al SIS

# ------------------------------------------------------------------------------
# C. EXCLUSIÓN DE VARIABLES DUMMY Y ACTUALIZACIÓN DE DISEÑO MUESTRAL
# ------------------------------------------------------------------------------
enaho_clasificar4 <- enaho_clasificar3 %>% select(-c(ingreso_cumple, mod_contratacion, tiene_contrato, tiene_ruc, temp_pago, pension_no, horas_sem))

# Actualización del diseño muestral con la nueva base analítica
enaho_reporte <- enaho_clasificar4 %>%
  filter(!is.na(factor500)) %>%
  as_survey_design(ids = conglome, strata = estrato, weights = factor500, nest = TRUE)

# ==============================================================================
# 2. EXPORTAR BASE DE DATOS ANALÍTICA
# ==============================================================================

# Guardamos la base con las nuevas variables creadas

df_clean <- as.data.frame(enaho_reporte) 
arrow_table <- as_arrow_table(df_clean)
write_parquet(
  arrow_table, 
  here::here("datos", "procesados", "enaho_analitica_2025_12_07_26.parquet")
)

# ==============================================================================
# 3. REPORTE DE VARIABLES CREADAS-----------------------------------------------
# ==============================================================================

# Construimos el reporte usando el objeto de diseño muestral
reporte_clasificar <- enaho_reporte %>%
  tbl_svysummary(
    # Seleccionamos explícitamente solo las variables NUEVAS que hemos creado
    include = c(
      edad_teoria, edad_z, estrato_teo,
      ingreso_mensual_imp, quintil_ingreso, tiene_pension,
      tiene_registro, tiene_contratos, horas_decente,
      ingreso_decente, indice_aditivo, ITD
    ),
    # Asignamos etiquetas limpias para el reporte
    label = list(
      edad_teoria ~ "Edad (Criterío Teórico",
      edad_z ~ "Edad Estandarizada (Puntaje Z)",
      estrato_teo ~ "Clasificación Geográfica (Teórica)",
      ingreso_mensual_imp ~ "Ingreso Mensual (Imputado)",
      quintil_ingreso ~ "Quintil de Ingreso (Criterio Datos)",
      tiene_pension ~ "Afiliación a Sistema de Pensiones",
      tiene_registro ~ "Registro en SUNAT",
      tiene_contratos ~ "Condición de Contratación",
      horas_decente ~ "Horas Trabajadas a la semana",
      ingreso_decente ~ "Ingreso Mensual (En relacion a RMV)",
      indice_aditivo ~ "Índice de Trabajo Decente simple (del 1-4)",
      ITD ~ "Índice de Trabajo Decente (ITD)"
    ),
    # Configuramos qué estadísticos mostrar
    statistic = list(
      all_categorical() ~ "{n_unweighted} ({p}%)",
      all_continuous() ~ "{mean} ({sd})"
    ),
    digits = all_continuous() ~ 2,
    missing_text = "(Casos perdidos / NA)"
  ) %>%
  modify_header(label = "**Variable Construida / Recodificada**") %>%
  modify_caption("**Reporte de Variables Analíticas de la Fase CLASIFICAR (ENAHO 2025)**") %>%
  bold_labels()

# Imprimir en el visor de RStudio
reporte_clasificar

# Exportar el reporte a Word (usando flextable para mantener el formato)
reporte_clasificar %>%
  as_flex_table() %>%
  flextable::save_as_html(path = here::here("outputs", "CLASIFICAR_Reporte_VariablesCreadas.html"))

