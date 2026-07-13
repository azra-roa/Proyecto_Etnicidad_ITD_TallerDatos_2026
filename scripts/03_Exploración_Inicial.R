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
