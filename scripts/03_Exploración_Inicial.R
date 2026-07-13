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


# ------------------------------------------------------------------------------
# 2. DISEÑO MUESTRAL------------------------------------------------------------
# ------------------------------------------------------------------------------
# Diseño muestral - Módulo 200 (Miembros del hogar: sexo, edad)
enaho_diseno_200 <- enaho_explorar %>%
  filter(!is.na(factor200)) %>%
  as_survey_design(
    ids = conglome,
    strata = estrato,
    weights = factor200,
    nest = TRUE
  ) # factor200: variables demográficas básicas (sexo, edad)

# Diseño muestral - Módulo 300 (Educación)
enaho_diseno_300 <- enaho_explorar %>%
  filter(!is.na(factor300)) %>%
  as_survey_design(
    ids = conglome,
    strata = estrato,
    weights = factor300,
    nest = TRUE
  ) # factor300: nivel educativo

# Diseño muestral - Módulo 500 (Empleo e ingresos)
enaho_diseno_500 <- enaho_explorar %>%
  filter(!is.na(factor500)) %>%
  as_survey_design(
    ids = conglome,
    strata = estrato,
    weights = factor500,
    nest = TRUE
  ) # factor500: tiene_ruc, tiene_contrato, temp_pago, horas_sem, ing_prin, pension_no, etnicidad

enaho_explorar %>%
  summarise(
    n_factor200 = sum(!is.na(factor200)),
    n_factor300 = sum(!is.na(factor300)),
    n_factor500 = sum(!is.na(factor500))
  )

# ==============================================================================
# 3. EXPLORACIÓN UNIVARIADA: TABLAS DESCRIPTIVAS--------------------------------
# ==============================================================================
# Definimos una función para crear un formato Flextable estandarizado
formato_flextable <- function(tabla, titulo) {
  flextable(tabla) %>%
    add_header_lines(values = titulo) %>%
    add_footer_lines(values = "Fuente: ENAHO 2025. Cálculos expandidos a nivel poblacional.") %>%
    autofit() %>% 
    theme_vanilla() %>% 
    border_inner_h(part = "body", border = officer::fp_border(width = 0)) %>% 
    align(align = "center", part = "all") %>% 
    align(j = 1, align = "left", part = "body") %>% 
    bold(part = "header") %>%
    align(align = "left", part = "footer") %>% 
    fontsize(size = 9, part = "footer") %>% 
    # Aseguramos que la línea final del cuerpo y del pie de página sean correctas
    hline_bottom(part = "body", border = officer::fp_border(width = 1)) %>% 
    hline_bottom(part = "footer", border = officer::fp_border(width = 0))
}

# ------------------------------------------------------------------------------
# 3.1 Sexo------------------------------------------------------------------------
# ------------------------------------------------------------------------------
tabla_sexo <- enaho_diseno_500 %>%
  filter(!is.na(sexo_etiqueta)) %>%
  group_by(sexo_etiqueta) %>%
  summarise(Poblacion = survey_total(vartype = NULL), 
            Porcentaje = survey_mean(vartype = NULL) * 100) %>%
  arrange(desc(Porcentaje)) %>%
  mutate(Poblacion = scales::comma(round(Poblacion, 0)), 
         Porcentaje = paste0(round(Porcentaje, 1), "%")) %>%
  rename(`Sexo` = sexo_etiqueta, 
         `Total (N)` = Poblacion, 
         `%` = Porcentaje)

ft_sexo <- formato_flextable(tabla_sexo, "Tabla 1. Distribución de la PEA ocupada según sexo, Perú, 2025")
print(ft_sexo)

# ------------------------------------------------------------------------------
# 3.2 Etnicidad------------------------------------------------------------------
# ------------------------------------------------------------------------------
tabla_etnicidad <- enaho_diseno_500 %>%
  filter(!is.na(etnicidad_etiqueta)) %>%
  group_by(etnicidad_etiqueta) %>%
  summarise(Poblacion = survey_total(vartype = NULL), 
            Porcentaje = survey_mean(vartype = NULL) * 100) %>%
  arrange(desc(Porcentaje)) %>%
  mutate(Poblacion = scales::comma(round(Poblacion, 0)), 
         Porcentaje = paste0(round(Porcentaje, 1), "%")) %>%
  rename(`Etnicidad` = etnicidad_etiqueta, 
         `Total (N)` = Poblacion, 
         `%` = Porcentaje)

ft_etnicidad <- formato_flextable(tabla_etnicidad, "Tabla 2. Distribución de la PEA ocupada según autoidentificación étnica, Perú, 2025")
print(ft_etnicidad)

# ------------------------------------------------------------------------------
# 3.3 Educación-------------------------------------------------------------------
# ------------------------------------------------------------------------------
tabla_educacion <- enaho_diseno_300 %>%
  filter(!is.na(educacion_etiqueta)) %>%
  group_by(educacion_etiqueta) %>%
  summarise(Poblacion = survey_total(vartype = NULL), 
            Porcentaje = survey_mean(vartype = NULL) * 100) %>%
  arrange(desc(Porcentaje)) %>%
  mutate(Poblacion = scales::comma(round(Poblacion, 0)), 
         Porcentaje = paste0(round(Porcentaje, 1), "%")) %>%
  rename(`Nivel Educativo` = educacion_etiqueta, 
         `Total (N)` = Poblacion, 
         `%` = Porcentaje)

ft_educacion <- formato_flextable(tabla_educacion, "Tabla 3. Distribución de la PEA ocupada según nivel educativo, Perú, 2025")
print(ft_educacion)

# ------------------------------------------------------------------------------
# 3.4 Afiliación al Sistema de Pensiones-----------------------------------------
# ------------------------------------------------------------------------------
tabla_pension_no <- enaho_diseno_500 %>%
  filter(!is.na(pension_no_etiqueta)) %>%
  group_by(pension_no_etiqueta) %>%
  summarise(Poblacion = survey_total(vartype = NULL), 
            Porcentaje = survey_mean(vartype = NULL) * 100) %>%
  arrange(desc(Porcentaje)) %>%
  mutate(Poblacion = scales::comma(round(Poblacion, 0)), 
         Porcentaje = paste0(round(Porcentaje, 1), "%")) %>%
  rename(`Afiliación a Sistema de Pensiones` = pension_no_etiqueta, 
         `Total (N)` = Poblacion, 
         `%` = Porcentaje)

ft_pension_no <- formato_flextable(tabla_pension_no, "Tabla 4 . Distribución de la PEA ocupada según afiliación a sistema de pensiones, Perú, 2025")
print(ft_pension_no)

# ------------------------------------------------------------------------------
# 3.5 Frecuencia de Pago-----------------------------------------------------------
# ------------------------------------------------------------------------------
tabla_temp_pago <- enaho_diseno_500 %>%
  filter(!is.na(temp_pago_etiqueta)) %>%
  group_by(temp_pago_etiqueta) %>%
  summarise(Poblacion = survey_total(vartype = NULL), 
            Porcentaje = survey_mean(vartype = NULL) * 100) %>%
  arrange(desc(Porcentaje)) %>%
  mutate(Poblacion = scales::comma(round(Poblacion, 0)), 
         Porcentaje = paste0(round(Porcentaje, 1), "%")) %>%
  rename(`Frecuencia de Pago` = temp_pago_etiqueta, 
         `Total (N)` = Poblacion, 
         `%` = Porcentaje)

ft_temp_pago <- formato_flextable(tabla_temp_pago, "Tabla 5. Distribución de la PEA ocupada según frecuencia de pago, Perú, 2025")
print(ft_temp_pago)

# ------------------------------------------------------------------------------
# 3.6 Registro en SUNAT (RUC)-----------------------------------------------------
# ------------------------------------------------------------------------------
tabla_tiene_ruc <- enaho_diseno_500 %>%
  filter(!is.na(tiene_ruc_etiqueta)) %>%
  group_by(tiene_ruc_etiqueta) %>%
  summarise(Poblacion = survey_total(vartype = NULL), 
            Porcentaje = survey_mean(vartype = NULL) * 100) %>%
  arrange(desc(Porcentaje)) %>%
  mutate(Poblacion = scales::comma(round(Poblacion, 0)), 
         Porcentaje = paste0(round(Porcentaje, 1), "%")) %>%
  rename(`Registro en SUNAT` = tiene_ruc_etiqueta, 
         `Total (N)` = Poblacion, 
         `%` = Porcentaje)

ft_tiene_ruc <- formato_flextable(tabla_tiene_ruc, "Tabla 6. Distribución de la PEA ocupada según registro en SUNAT, Perú, 2025")
print(ft_tiene_ruc)

# ------------------------------------------------------------------------------
# 3.7 Tipo de Contrato-------------------------------------------------------------
# ------------------------------------------------------------------------------
tabla_tiene_contrato <- enaho_diseno_500 %>%
  filter(!is.na(tiene_contrato_etiqueta)) %>%
  group_by(tiene_contrato_etiqueta) %>%
  summarise(Poblacion = survey_total(vartype = NULL), 
            Porcentaje = survey_mean(vartype = NULL) * 100) %>%
  arrange(desc(Porcentaje)) %>%
  mutate(Poblacion = scales::comma(round(Poblacion, 0)), 
         Porcentaje = paste0(round(Porcentaje, 1), "%")) %>%
  rename(`Tipo de Contrato` = tiene_contrato_etiqueta, 
         `Total (N)` = Poblacion, 
         `%` = Porcentaje)

ft_tiene_contrato <- formato_flextable(tabla_tiene_contrato, "Tabla 7. Distribución de la PEA ocupada según tipo de contrato, Perú, 2025")
print(ft_tiene_contrato)

# ------------------------------------------------------------------------------
# 3.8 Estadísticos de resumen: Edad (Variable Continua)
# ------------------------------------------------------------------------------
stats_edad <- enaho_diseno_200 %>%
  filter(!is.na(edad)) %>%
  summarise(
    `Mínimo` = min(edad, na.rm = TRUE),
    `Percentil 25 (Q1)` = survey_quantile(edad, 0.25, vartype = NULL),
    `Mediana (Q2)` = survey_median(edad, vartype = NULL),
    `Media (Promedio)` = survey_mean(edad, vartype = NULL),
    `Desviación Estándar` = survey_sd(edad, vartype = NULL),
    `Percentil 75 (Q3)` = survey_quantile(edad, 0.75, vartype = NULL),
    `Máximo` = max(edad, na.rm = TRUE)
  ) %>%
  pivot_longer(cols = everything(), names_to = "Estadístico", values_to = "Valor (Años)") %>%
  mutate(
    Estadístico = str_remove(Estadístico, "_q[0-9]+"),
    `Valor (Años)` = scales::comma(round(`Valor (Años)`, 1))
  )
ft_edad <- formato_flextable(stats_edad, "Tabla 8. Edad de la PEA Ocupada (estadísticos de resumen), Perú, 2025")
print(ft_edad)

# ------------------------------------------------------------------------------
# 3.9 Estadísticos de resumen: Ingreso Principal (Variable Continua)
# ------------------------------------------------------------------------------
stats_ing_prin <- enaho_diseno_500 %>%
  filter(!is.na(ing_prin)) %>%
  summarise(
    `Mínimo` = min(ing_prin, na.rm = TRUE),
    `Percentil 25 (Q1)` = survey_quantile(ing_prin, 0.25, vartype = NULL),
    `Mediana (Q2)` = survey_median(ing_prin, vartype = NULL),
    `Media (Promedio)` = survey_mean(ing_prin, vartype = NULL),
    `Desviación Estándar` = survey_sd(ing_prin, vartype = NULL),
    `Percentil 75 (Q3)` = survey_quantile(ing_prin, 0.75, vartype = NULL),
    `Máximo` = max(ing_prin, na.rm = TRUE)
  ) %>%
  pivot_longer(cols = everything(), names_to = "Estadístico", values_to = "Valor (S/.)") %>%
  mutate(
    Estadístico = str_remove(Estadístico, "_q[0-9]+"),
    `Valor (S/.)` = scales::comma(round(`Valor (S/.)`, 1))
  )
ft_ing_prin <- formato_flextable(stats_ing_prin, "Tabla 9. Ingreso Principal de la PEA Ocupada (estadísticos de resumen), Perú, 2025")
print(ft_ing_prin)

# ------------------------------------------------------------------------------
# 3.10 Estadísticos de resumen: Horas Trabajadas Semanales (Variable Continua)
# ------------------------------------------------------------------------------
stats_horas_sem <- enaho_diseno_500 %>%
  filter(!is.na(horas_sem)) %>%
  summarise(
    `Mínimo` = min(horas_sem, na.rm = TRUE),
    `Percentil 25 (Q1)` = survey_quantile(horas_sem, 0.25, vartype = NULL),
    `Mediana (Q2)` = survey_median(horas_sem, vartype = NULL),
    `Media (Promedio)` = survey_mean(horas_sem, vartype = NULL),
    `Desviación Estándar` = survey_sd(horas_sem, vartype = NULL),
    `Percentil 75 (Q3)` = survey_quantile(horas_sem, 0.75, vartype = NULL),
    `Máximo` = max(horas_sem, na.rm = TRUE)
  ) %>%
  pivot_longer(cols = everything(), names_to = "Estadístico", values_to = "Valor (Horas)") %>%
  mutate(
    Estadístico = str_remove(Estadístico, "_q[0-9]+"),
    `Valor (Horas)` = scales::comma(round(`Valor (Horas)`, 1))
  )
ft_horas_sem <- formato_flextable(stats_horas_sem, "Tabla 10. Horas Trabajadas Semanalmente en la Ocupación Principal (estadísticos de resumen), Perú, 2025")
print(ft_horas_sem)
