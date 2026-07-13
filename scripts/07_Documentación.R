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

# ==============================================================================
# 3. DOCUMENTACIÓN DE DECISIONES METODOLÓGICAS----------------------------------
# ==============================================================================

# Diccionario de decisiones metodológicas
dict_metadata <- list(
  etnicidad = "El valor 8 se recodificó a NA siguiendo el diccionario INEI. Los casos perdidos (MAR y MCAR) fueron excluidos (listwise). Se renombraron las etiquetas según un criterio teórico (Garavito, 2010)",
  educacion = "Se eliminaron (listwise) los valores perdidos (MCAR). Se renombraron las etiquetas segun criterio propio",
  tiene_pension = "Se considera que una persona esta afiliada a algún sistema de pensiones si respondió 0 a la pregunta P558A5, se considera que no esta afiliada si respondió 5 a dicha pregunta. Esto en linea con lo descrito por Gamero (2012)",
  tiene_registro = "Se eliminaron los valores perdidos (listwise) de la variable fuente (P510A1). Se considera que el negocio tiene registro en SUNAT si respondió 1 o 2 en la pregunta P510A1, se considera que la persona no esta registrada si respondió 3 en dicha pregunta.  Esto en linea con lo descrito por Gamero (2012)",
  tiene_contratos = "Se eliminaron los valores perdidos (liswise) de la variable fuente (P511A). Se considera que la persona tiene algun tipo de contrato si respondió 1, 2, 3, 4, 5, 6 u 8 a la pregunta P511A, se considera que no tiene contrato si respondió 7 a dicha pregunta.  Esto en linea con lo descrito por Gamero (2012)",
  horas_decente = "La pregunta se extrajo imputada desde la base de datos de la INEI. Se volvieron a imputar los valores perdidos restantes por la media (agrupada por educacion). Sobre esa data se recodificó la variable para diferenciar a aquellos que trabajaran mas y menos de 48 horas semanales.  Esto en linea con lo descrito por Gamero (2012)",
  edad_teoria = "Se agruparon en grupos de edad teóricos la variable de edad (P208A)",
  edad_z = "Se estandarizó la edad de la muestra restando la media al valor y dividiendo el resultado por la desviación estándar.",
  estrato_teo = "Se agruparon los estratos Geograficos en Urbano y Rural siguiendo criterios teoricos.",
  ingreso_mensual_imp = "Se calculó el ingreso mensual multiplicando la variable de temporalidad de pago (P523) y la variable de ingreso ocupación principal (P524A1) la cual habia sido previamente imputado por la mediana (agrupada por educación).",
  quintil_ingreso = "Se agruparon los grupos de ingresos en quintiles segun la data",
  ingreso_decente = "Se recodifico la variable de ingreso_mensual_imp en función a si la persona ganaba un monto inferior o superior a la Remuniración Mínima Vital. Esto en linea con lo descrito por Gamero (2012)",
  indice_aditivo = "Se creo un índice aditivo (del 1-4) sumando los valores de las variables dummy de tiene_pension, tiene_registro, horas_decente, tiene_contratos y ingreso decente. Las dimensiones de registro y contrato se consideraron de forma cruzada para una sola dummy. Esto en linea con lo descrito por Gamero (2012)",
  ITD = "Se transformó el Indice de Trabajo Decente a una escala de 0 a 100"
)

# Aplicamos las descripciones iterativamente a las columnas correspondientes
for (var in names(dict_metadata)) {
  attr(enaho_codebook[[var]], "description") <- dict_metadata[[var]]
}

# Agregamos metadatos a nivel de ESTUDIO (Ficha Técnica)
metadata(enaho_codebook)$name <- "Base de Datos Analítica - Proyecto Etnicidad-ITD ENAHO 2025"
metadata(enaho_codebook)$description <- "Submuestra de la Encuesta Nacional de Hogares (2025) restringida a PEA Ocupada, mayores de 14 años, con autoidentificación étnica definida"
metadata(enaho_codebook)$creator <- "Carmen Andonayre y Azra Roa"
