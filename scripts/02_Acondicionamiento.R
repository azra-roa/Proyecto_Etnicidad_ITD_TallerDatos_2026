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
    etnicidad = p558c, # "Por sus antepasados y de acuerdo a sus costumbres, ¿Ud. se considera:"
    educacion = p301a.y, # "¿Cuál es el último año o grado de estudios y nivel que aprobó?"
    edad = p208a,        # Edad en años cumplidos
    pension_no = p558a5, # "¿El Sistema de pensiones al cual Ud. está afiliado es: No está afiliado" (Se usará esta pregunta como dummy de afiliación a sistema de pensiones)
    horas_sem = i513t,  # "Total de horas trabajadas la semana pasada en su ocupación principal" (Importante: pregunta imputada)
    temp_pago = p523,   # "En su ocupación principal, ¿A Ud. le pagan: (diario, semanal, etc)"
    ing_prin = p524a1,  #Ingreso ocupación principal
    tiene_ruc = p510a1,  # "El negocio o empresa donde trabaja, ¿Se encuentra registrado en la SUNAT, como:"
    tiene_contrato = p511a, # Tipo de contrato ocupacion principal
    
    #Factores de expansión
    factor200 = facpob07,    #Factor de expansión módulo 200
    factor300 = factora07,   #Factor de expansión módulo 300
    factor500 = fac500a,     #Factor de expansión módulo 500
  )

# Inspección preliminar de la base
dim(enaho_seleccion)        # Filas y columnas post join y selección
names(enaho_seleccion)      # Verificación de nombres
glimpse(enaho_seleccion)    # Revisión crítica de cómo R interpretó los tipos de datos

# ------------------------------------------------------------------------------
# 2. DIAGNÓSTICO DE NAs Y REPORTE-----------------------------------------------
# ------------------------------------------------------------------------------

# 2.1 Visualización Gráfica (libreria: naniar)

# Barplot de cantidad de NAs por variable
grafico_nas <- gg_miss_var(enaho_seleccion, show_pct = TRUE) +
  labs(
    title = "Porcentaje de Valores Perdidos (NAs) por Variable",
    subtitle = "Proyecto: Análisis de la relacion etnicidad-ITD usando datos de la ENAHO (2025)",
    y = "% de Valores Perdidos",
    x = "Variables"
  ) +
  theme_minimal()

# Mostramos el gráfico en el panel de RStudio
print(grafico_nas)

# Exportamos el gráfico a nuestra carpeta de outputs
ggsave("outputs/Grafico_NAs_Etnicidad_ITC.png", plot = grafico_nas, 
       width = 8, height = 6, bg = "white")

# 2.2 Reporte Tabular 
reporte_nas <- enaho_seleccion %>%
  summarise(across(everything(), ~ round(sum(is.na(.)) / n() * 100, 2))) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "porcentaje_na") %>%
  arrange(desc(porcentaje_na))

#Exportamos la tabla de NAs a la carpeta de outputs
write_csv(reporte_nas, "outputs/Reporte_Datos_Perdidos_ENAHO.csv")
