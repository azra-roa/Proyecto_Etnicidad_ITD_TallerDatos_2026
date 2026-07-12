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
library(haven)
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
    
    #Variables para filtro (PEA ocupada)
    trabajo_semana_pasada = p501, #La persona tuvo trabajo la semana pasada
    empleo_fijo_volvera  = p502,  #Respondió No a la pregunta de trabajo semana pasada pero tiene un empleo fijo al cual volverá
    negocio_volvera  = p503,      #Respondió No a la pregunta de trabajo semana pasada pero tiene un negocio propio al cual volverá
    
    #Factores de expansión
    factor200 = facpob07,    #Factor de expansión módulo 200
    factor300 = factora07,   #Factor de expansión módulo 300
    factor500 = fac500a,     #Factor de expansión módulo 500
  )

# Inspección preliminar de la base
dim(enaho_seleccion)        # Filas y columnas post join y selección
names(enaho_seleccion)      # Verificación de nombres
glimpse(enaho_seleccion)    # Revisión crítica de cómo R interpretó los tipos de datos

#Modificación de variable etnicidad para que incluya NAs correctos
enaho_seleccion <- enaho_seleccion %>%
  mutate(
    etnicidad = ifelse(etnicidad == 8, NA, etnicidad))

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

# ------------------------------------------------------------------------------
# 3. FILTRO DE BASE DE DATOS PARA INCLUIR SOLO A PEA OCUPADA Y NUEVO REPORTE NAS
# ------------------------------------------------------------------------------

# El INEI tiene filtros para las preguntas de trabajo, lo cual genera un caso de 
# missing values MAR por factores estructurales de la encuesta. Considerando el 
# objetivo de este trabajo de comparar accesos al trabajo decente, se ha 
# decidido trabajar con la PEA ocupada, lo cual elimina parcialmente este problema

enaho_seleccion <- zap_labels(enaho_seleccion) #Uso de zap_labels para quedarnos solo con codigos numericos de variables

enaho_tratada_1 <- enaho_seleccion %>%
  # Creación de la variable condición de ocupación
  mutate(
    condicion_ocupacion = ifelse(
      trabajo_semana_pasada == 1 | empleo_fijo_volvera == 1 | negocio_volvera == 1, 
      "PEA Ocupada", 
      "No Ocupado (Desempleado/Inactivo)"
    )
  ) %>% #Filtro para quedarnos solo con aquellos que cumplan alguna de las condiciones
  filter(trabajo_semana_pasada == 1 | empleo_fijo_volvera == 1 | negocio_volvera == 1)  

# 3.1 Visualización Gráfica (libreria: naniar)

#Exclusión de variables de filtro
enaho_tratada_2 <- enaho_tratada_1 %>% select(-c(negocio_volvera, empleo_fijo_volvera, trabajo_semana_pasada, condicion_ocupacion))

# Barplot de cantidad de NAs por variable
grafico_nas_tratada <- gg_miss_var(enaho_tratada_2, show_pct = TRUE) +
  labs(
    title = "Porcentaje de Valores Perdidos (NAs) por Variable",
    subtitle = "Proyecto: Análisis de la relacion etnicidad-ITD usando datos de la ENAHO (2025)",
    y = "% de Valores Perdidos",
    x = "Variables"
  ) +
  theme_minimal()

# Mostramos el gráfico en el panel de RStudio
print(grafico_nas_tratada)

# Exportamos el gráfico a nuestra carpeta de outputs
ggsave("outputs/Grafico_NAs_Etnicidad_ITC_tratada.png", plot = grafico_nas_tratada, 
       width = 8, height = 6, bg = "white")

# 3.2 Reporte Tabular 
reporte_nas <- enaho_tratada_2 %>%
  summarise(across(everything(), ~ round(sum(is.na(.)) / n() * 100, 2))) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "porcentaje_na") %>%
  arrange(desc(porcentaje_na))

#Exportamos la tabla de NAs a la carpeta de outputs
write_csv(reporte_nas, "outputs/Reporte_Datos_Perdidos_ENAHO_tratada.csv")

# ------------------------------------------------------------------------------
# 4. TRATAMIENTO DE NAs---------------------------------------------------------
# ------------------------------------------------------------------------------

# ------------------------------------------------------------------------------
# CASO A: MCAR (Missing Completely At Random) / Ausencia Estructural
# Variable: educación
# Problema: Hay una cantidad pequeña de celdas vacías (NAs= 0.04) que no podemos
# explicar por otra variable.
# Estrategia: Eliminación (listwise)
# ------------------------------------------------------------------------------

# PASO 4.1.1: Diagnóstico educación
# Vemos cuántos NAs reales hay.
diagnostico_educacion <- enaho_tratada_2 %>%
  count(educacion, is.na(educacion)) %>%
  arrange(desc(n))
print(diagnostico_educacion) #22 de 56854 casos son NAs

# PASO 4.1.2: Tratamiento (Eliminación Listwise)
# Luego, como sospechamos que es un casos de MCAR/Estructural, aplicamos la 
# estrategia de Eliminación usando drop_na().
enaho_tratada_3 <- enaho_tratada_2 %>%
  # Eliminación Listwise: Eliminamos de la base a los NAs de la variable
  drop_na(educacion)

sum(is.na(enaho_tratada_3$educacion)) #Al hacer la revisión = 0 NAs en la variable

#A partir de este punto se han eliminado 22 casos de la base de datos.

# ------------------------------------------------------------------------------
# CASO B: MNAR (Missing Not At Random) / Ausencias ligadas al propio valor no observado
# Variables Cuantitativas: ing_prin y temp_pago
# Problema: Variables vinculadas al ingreso y a trabajo generan casos en las que las
# personas tienen incentivos para no responder vinculados a la misma pregunta. 
# Los NAs no se explican por otras variables.
# Estrategia: Imputación Condicionada (Mediana según Nivel Educativo) 
# Nota metodologíca: La estrategia utilizada esta introduciendo sesgo y subestimando la varianza
# ------------------------------------------------------------------------------

# PASO 4.2.1: Diagnóstico ingresos (Variable a tratar = ing_prin)
# Vemos cuántos NAs reales hay.
diagnostico_ingresos <- enaho_tratada_3 %>%
  count(ing_prin, is.na(ing_prin)) %>%
  arrange(desc(n))
print(diagnostico_ingresos) #32170 de 56832 casos son NAs

# PASO 4.2.2: Imputación condicionada por nivel educativo
enaho_tratada_4 <- enaho_tratada_3 %>%
  # Agrupamos a las personas por su nivel educativo para no imputar a ciegas.
  group_by(educacion) %>%
  mutate(
    ing_prin = ifelse(
      is.na(ing_prin), 
      median(ing_prin, na.rm = TRUE), # Imputa la mediana de su propio grupo
      ing_prin
    )
  ) %>%
  ungroup() # Desagrupamos para evitar errores en cruces futuros

sum(is.na(enaho_tratada_4$ing_prin)) #Al hacer la revisión = 0 NAs en la variable

# PASO 4.3.1: Diagnóstico periodicidad de pago (Variable a tratar = temp_pago)
# Vemos cuántos NAs reales hay.
diagnostico_pago <- enaho_tratada_4 %>%
  count(temp_pago, is.na(temp_pago)) %>%
  arrange(desc(n))
print(diagnostico_pago) #31798 de 56832 casos son NAs

# PASO 4.3.2: Imputación condicionada por nivel educativo
enaho_tratada_5 <- enaho_tratada_4 %>%
  # Agrupamos a las personas por su nivel educativo para no imputar a ciegas.
  group_by(educacion) %>%
  mutate(
    temp_pago = ifelse(
      is.na(temp_pago), 
      median(temp_pago, na.rm = TRUE), # Imputa la mediana de su propio grupo
      temp_pago
    )
  ) %>%
  ungroup() # Desagrupamos para evitar errores en cruces futuros

sum(is.na(enaho_tratada_5$temp_pago)) #Al hacer la revisión = 0 NAs en la variable

# ------------------------------------------------------------------------------
# CASO C: MNAR (Missing Not At Random) / Ausencias ligadas al propio valor no observado
# Variables Cualitativas: tiene_contrato y tiene_ruc
# Problema: Como en el caso anterior, variables vinculadas a trabajo generan incentivos
# para no responder vinculados a la misma pregunta. En este caso no podemos imputar por
# la mediana al tratarse de variables categoricas, por lo tanto se asumirá la pérdida de
# poder estadistico y el sesgo introducido y se elliminaran estos casos.
# Estrategia: Eliminación (Listwise)
# Nota metodologíca: La estrategia utilizada esta quitando poder estadístico al modelo
# asi como introduciendo sesgo ya que los casos excluidos no son MCAR. 
# ------------------------------------------------------------------------------

# PASO 4.4.1: Diagnóstico tipo de contrato (Variable a tratar = tiene_contrato)
# Vemos cuántos NAs reales hay.
diagnostico_contrato <- enaho_tratada_5 %>%
  count(tiene_contrato, is.na(tiene_contrato)) %>%
  arrange(desc(n))
print(diagnostico_contrato) #25241 de 56832 casos son NAs

# PASO 4.4.2: Tratamiento (Eliminación Listwise)
# Se ha decidido aplicar la estrategia de Eliminación usando drop_na().
enaho_tratada_6 <- enaho_tratada_5 %>%
  # Eliminación Listwise: Eliminamos de la base a los NAs de la variable
  drop_na(tiene_contrato)

sum(is.na(enaho_tratada_6$tiene_contrato)) #Al hacer la revisión = 0 NAs en la variable

#A partir de este punto se han eliminado 25263 casos de la base de datos entre 
# las dos variables para las que se decidió eliminar NAs.


# PASO 4.5.1: Diagnóstico registro en SUNAT (Variable a tratar = tiene_ruc)
# Vemos cuántos NAs reales hay.
diagnostico_ruc <- enaho_tratada_6 %>%
  count(tiene_ruc, is.na(tiene_ruc)) %>%
  arrange(desc(n))
print(diagnostico_ruc) #6021 de 31591 casos son NAs

# PASO 4.5.2: Tratamiento (Eliminación Listwise)
# Se ha decidido aplicar la estrategia de Eliminación usando drop_na().
enaho_tratada_7 <- enaho_tratada_6 %>%
  # Eliminación Listwise: Eliminamos de la base a los NAs de la variable
  drop_na(tiene_ruc)

sum(is.na(enaho_tratada_7$tiene_ruc)) #Al hacer la revisión = 0 NAs en la variable


#A partir de este punto se han eliminado 31284 casos de la base de datos entre 
# las tres variables para las que se decidió eliminar NAs.


# PASO 4.6.1: Diagnóstico etnicidad (Variable a tratar = autoidentificación etnica por antepasados)
# Vemos cuántos NAs reales hay.
diagnostico_etnicidad <- enaho_tratada_7 %>%
  count(etnicidad, is.na(etnicidad)) %>%
  arrange(desc(n))
print(diagnostico_etnicidad) #1174 de 25570 casos son NAs

# PASO 4.6.2: Tratamiento (Eliminación Listwise)
# Se ha decidido aplicar la estrategia de Eliminación usando drop_na().
enaho_tratada_8 <- enaho_tratada_7 %>%
  # Eliminación Listwise: Eliminamos de la base a los NAs de la variable
  drop_na(etnicidad)

sum(is.na(enaho_tratada_8$etnicidad)) #Al hacer la revisión = 0 NAs en la variable

#A partir de este punto se han eliminado 32458 casos de la base de datos entre 
# las cuatro variables para las que se decidió eliminar NAs.

# ------------------------------------------------------------------------------
# 5. EXPORTANOS NUESTRA BASE DE DATOS-------------------------------------------
# ------------------------------------------------------------------------------

write_parquet(enaho_tratada_8, "datos/procesados/enaho_2025_12_07_26.parquet")
#NOTA SOBRE ESTA BASE:
# - SOLO INCLUYE MAYORES DE 14
# - SOLO INCLUYE PEA OCUPADA
# - SOLO INCLUYE PERSONAS QUE RESPONDIERON A LAS PREGUNTAS DE EDUCACIÓN, REGISTRO
#   EN SUNAT, ETNICIDAD Y TIPO DE CONTRATO
# - IMPUTA INGRESOS Y TEMPORALIDAD DE PAGO POR MEDIANA (AGRUPADAS POR EDUCACIÓN)

