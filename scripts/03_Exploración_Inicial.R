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

# ==============================================================================
# 4. EXPLORACIÓN UNIVARIADA: GRÁFICOS
# ==============================================================================
# 4.1 Histograma: Edad (Ponderado)
plot_edad <- ggplot(enaho_explorar %>% filter(!is.na(edad) & !is.na(factor200)), 
                    aes(x = edad, weight = factor200)) +
  geom_histogram(fill = "#4A7C59", color = "white", binwidth = 2) +
  scale_y_continuous(labels = scales::comma) +
  labs(title = "Gráfico 1. Distribución de edad de la PEA ocupada", 
       x = "Edad (años)", 
       y = "Frecuencia Poblacional", 
       caption = "Fuente: ENAHO 2025. Cálculos ajustados a nivel poblacional") + 
  theme_minimal()
print(plot_edad)

# 4.2 Histograma: Ingreso Principal (Ponderado)
plot_ingreso <- ggplot(enaho_explorar %>% filter(!is.na(ing_prin) & !is.na(factor500)), 
                       aes(x = ing_prin, weight = factor500)) +
  geom_histogram(fill = "#2E5B88", color = "white", bins = 50) +
  coord_cartesian(xlim = c(0, 10000)) + 
  scale_x_continuous(labels = scales::comma) + 
  scale_y_continuous(labels = scales::comma) +
  labs(title = "Gráfico 2. Distribución del ingreso principal", 
       x = "Ingreso (Soles)", 
       y = "Frecuencia Poblacional", 
       caption = "Fuente: ENAHO 2025. Nota: Eje X truncado en S/10,000. Cálculos ajustados a nivel poblacional") + 
  theme_minimal()
print(plot_ingreso)

# 4.3 Histograma: Horas Trabajadas Semanalmente (Ponderado)
plot_horas <- ggplot(enaho_explorar %>% filter(!is.na(horas_sem) & !is.na(factor500)), 
                     aes(x = horas_sem, weight = factor500)) +
  geom_histogram(fill = "#8B5A2B", color = "white", binwidth = 5) +
  scale_y_continuous(labels = scales::comma) +
  labs(title = "Gráfico 3. Distribución de horas trabajadas a la semana", 
       x = "Horas trabajadas (semana)", 
       y = "Frecuencia Poblacional", 
       caption = "Fuente: ENAHO 2025. Cálculos ajustados a nivel poblacional") + 
  theme_minimal()
print(plot_horas)


# 4.4 Gráfico de Barras: Frecuencia de Pago (Ponderado)
prop_temp_pago <- enaho_diseno_500 %>%
  filter(!is.na(temp_pago_etiqueta)) %>%
  group_by(temp_pago_etiqueta) %>%
  summarise(Porcentaje = survey_mean(vartype = NULL) * 100) %>%
  arrange(desc(Porcentaje))

plot_temp_pago <- ggplot(prop_temp_pago, 
                         aes(x = reorder(temp_pago_etiqueta, -Porcentaje), y = Porcentaje)) +
  geom_col(fill = "#E69F00", alpha = 0.85) +
  geom_text(aes(label = paste0(round(Porcentaje, 1), "%")), 
            vjust = -0.5, fontface = "bold", size = 3.5) +
  scale_y_continuous(labels = scales::comma, expand = expansion(mult = c(0, 0.15))) +
  labs(title = "Gráfico 4. Distribución de la PEA ocupada según frecuencia de pago", 
       x = "Frecuencia de Pago", 
       y = "Porcentaje (%)", 
       caption = "Fuente: ENAHO 2025. Cálculos ajustados a nivel poblacional") + 
  theme_minimal()
print(plot_temp_pago)

# ==============================================================================
# 5. EXPLORACIÓN BIVARIADA: RELACIONES ENTRE VARIABLES 
# ==============================================================================
# Se utiliza el factor de expansión del
# módulo 500 (enaho_diseno_500) para todos los cruces bivariados, independientemente
# del módulo de origen de cada variable.

# ------------------------------------------------------------------------------
# 5.1 Categórica vs. Categórica (Tablas de Contingencia)
# ------------------------------------------------------------------------------

# A. Sexo según registro en SUNAT  (Porcentajes por fila)
tabla_sexo_ruc <- enaho_diseno_500 %>%
  filter(!is.na(sexo_etiqueta) & !is.na(tiene_ruc_etiqueta)) %>%
  group_by(sexo_etiqueta, tiene_ruc_etiqueta) %>%
  summarise(Poblacion = survey_total(vartype = NULL)) %>%
  group_by(sexo_etiqueta) %>%
  mutate(
    Porcentaje = (Poblacion / sum(Poblacion)) * 100,
    Celda = paste0(scales::comma(round(Poblacion, 0)), " (", round(Porcentaje, 1), "%)")
  ) %>%
  select(sexo_etiqueta, tiene_ruc_etiqueta, Celda) %>%
  pivot_wider(names_from = tiene_ruc_etiqueta, values_from = Celda) %>%
  rename(`Sexo` = sexo_etiqueta)

ft_sexo_ruc <- formato_flextable(tabla_sexo_ruc, "Tabla 10. Sexo según registro en SUNAT de la PEA ocupada, Perú, 2025")
print(ft_sexo_ruc)

# B. Sexo según Afiliación a Pensiones (Porcentajes por fila)
tabla_sexo_pension <- enaho_diseno_500 %>%
  filter(!is.na(sexo_etiqueta) & !is.na(pension_no_etiqueta)) %>%
  group_by(sexo_etiqueta, pension_no_etiqueta) %>%
  summarise(Poblacion = survey_total(vartype = NULL)) %>%
  group_by(sexo_etiqueta) %>%
  mutate(
    Porcentaje = (Poblacion / sum(Poblacion)) * 100,
    Celda = paste0(scales::comma(round(Poblacion, 0)), " (", round(Porcentaje, 1), "%)")
  ) %>%
  select(sexo_etiqueta, pension_no_etiqueta, Celda) %>%
  pivot_wider(names_from = pension_no_etiqueta, values_from = Celda) %>%
  rename(`Sexo` = sexo_etiqueta)

ft_sexo_pension <- formato_flextable(tabla_sexo_pension, "Tabla 11. Sexo según afiliación a sistema de pensiones de la PEA ocupada, Perú, 2025")
print(ft_sexo_pension)

# C. Sexo según Tipo de Contrato (Porcentajes por fila)
tabla_sexo_contrato <- enaho_diseno_500 %>%
  filter(!is.na(sexo_etiqueta) & !is.na(tiene_contrato_etiqueta)) %>%
  group_by(sexo_etiqueta, tiene_contrato_etiqueta) %>%
  summarise(Poblacion = survey_total(vartype = NULL)) %>%
  group_by(sexo_etiqueta) %>%
  mutate(
    Porcentaje = (Poblacion / sum(Poblacion)) * 100,
    Celda = paste0(scales::comma(round(Poblacion, 0)), " (", round(Porcentaje, 1), "%)")
  ) %>%
  select(sexo_etiqueta, tiene_contrato_etiqueta, Celda) %>%
  pivot_wider(names_from = tiene_contrato_etiqueta, values_from = Celda) %>%
  rename(`Sexo` = sexo_etiqueta)

ft_sexo_contrato <- formato_flextable(tabla_sexo_contrato, "Tabla 12. Tipo de contrato laboral según sexo de la PEA ocupada, Perú, 2025")
print(ft_sexo_contrato)

# D. Registro en SUNAT según Afiliación a Pensiones (Porcentajes por fila)
tabla_ruc_pension <- enaho_diseno_500 %>%
  filter(!is.na(tiene_ruc_etiqueta) & !is.na(pension_no_etiqueta)) %>%
  group_by(pension_no_etiqueta, tiene_ruc_etiqueta) %>%
  summarise(Poblacion = survey_total(vartype = NULL)) %>%
  group_by(pension_no_etiqueta) %>%
  mutate(
    Porcentaje = (Poblacion / sum(Poblacion)) * 100,
    Celda = paste0(scales::comma(round(Poblacion, 0)), " (", round(Porcentaje, 1), "%)")
  ) %>%
  select(pension_no_etiqueta, tiene_ruc_etiqueta, Celda) %>%
  pivot_wider(names_from = tiene_ruc_etiqueta, values_from = Celda) %>%
  rename(`Afiliación a Pensiones` = pension_no_etiqueta)

ft_ruc_pension <- formato_flextable(tabla_ruc_pension, "Tabla 13. Registro en SUNAT según afiliación a sistema de pensiones, PEA ocupada, Perú, 2025")
print(ft_ruc_pension)

# E. Registro en SUNAT según Tipo de Contrato (Porcentajes por fila)
tabla_ruc_contrato <- enaho_diseno_500 %>%
  filter(!is.na(tiene_ruc_etiqueta) & !is.na(tiene_contrato_etiqueta)) %>%
  group_by(tiene_ruc_etiqueta, tiene_contrato_etiqueta) %>%
  summarise(Poblacion = survey_total(vartype = NULL)) %>%
  group_by(tiene_ruc_etiqueta) %>%
  mutate(
    Porcentaje = (Poblacion / sum(Poblacion)) * 100,
    Celda = paste0(scales::comma(round(Poblacion, 0)), " (", round(Porcentaje, 1), "%)")
  ) %>%
  select(tiene_ruc_etiqueta, tiene_contrato_etiqueta, Celda) %>%
  pivot_wider(names_from = tiene_contrato_etiqueta, values_from = Celda) %>%
  rename(`Registro en SUNAT` = tiene_ruc_etiqueta)

ft_ruc_contrato <- formato_flextable(tabla_ruc_contrato, "Tabla 14. Tipo de contrato laboral según registro en SUNAT, PEA ocupada, Perú, 2025")
print(ft_ruc_contrato)

# ------------------------------------------------------------------------------
# 5.2 Categórica vs. Continua (Tablas de Medianas + Boxplots)
# ------------------------------------------------------------------------------
if(!require(quantreg)) install.packages("quantreg")
# A. Ingreso Principal según Sexo (descriptivos completos)
tabla_ing_sexo <- enaho_diseno_500 %>%
  filter(!is.na(sexo_etiqueta) & !is.na(ing_prin)) %>%
  group_by(sexo_etiqueta) %>%
  summarise(
    N       = unweighted(n()),
    Media   = survey_mean(ing_prin, vartype = NULL),
    DE      = survey_sd(ing_prin, vartype = NULL),
    Q1      = survey_quantile(ing_prin, 0.25, vartype = NULL)[[1]],
    Mediana = survey_median(ing_prin, vartype = NULL),
    Q3      = survey_quantile(ing_prin, 0.75, vartype = NULL)[[1]],
    Minimo  = min(ing_prin, na.rm = TRUE),
    Maximo  = max(ing_prin, na.rm = TRUE)
  ) %>%
  mutate(across(c(Media, DE, Q1, Mediana, Q3, Minimo, Maximo), 
                ~ scales::comma(round(.x, 0)))) %>%
  rename(`Sexo` = sexo_etiqueta)

ft_ing_sexo <- formato_flextable(tabla_ing_sexo, "Tabla 15. Estadísticos descriptivos del ingreso principal según sexo, PEA ocupada, Perú, 2025")
print(ft_ing_sexo)

plot_ing_sexo <- ggplot(enaho_explorar %>% filter(!is.na(sexo_etiqueta) & !is.na(ing_prin)), 
                        aes(x = sexo_etiqueta, y = ing_prin, fill = sexo_etiqueta, weight = factor500)) +
  geom_boxplot(alpha = 0.7, outlier.color = "red", outlier.alpha = 0.3) +
  coord_cartesian(ylim = c(0, 5000)) + 
  scale_y_continuous(labels = scales::comma) +
  scale_fill_manual(values = c("Hombre" = "#2E5B88", "Mujer" = "#E69F00")) +
  labs(title = "Gráfico 8. Ingreso principal según sexo, PEA ocupada", 
       x = "Sexo", y = "Ingreso (Soles)",
       caption = "Fuente: ENAHO 2025. Nota: Eje Y truncado en S/5,000.") +
  theme_minimal() + theme(legend.position = "none")
print(plot_ing_sexo)

# B. Horas Semanales según Sexo
tabla_horas_sexo <- enaho_diseno_500 %>%
  filter(!is.na(sexo_etiqueta) & !is.na(horas_sem)) %>%
  group_by(sexo_etiqueta) %>%
  summarise(
    N       = unweighted(n()),
    Media   = survey_mean(horas_sem, vartype = NULL),
    DE      = survey_sd(horas_sem, vartype = NULL),
    Q1      = survey_quantile(horas_sem, 0.25, vartype = NULL)[[1]],
    Mediana = survey_median(horas_sem, vartype = NULL),
    Q3      = survey_quantile(horas_sem, 0.75, vartype = NULL)[[1]],
    Minimo  = min(horas_sem, na.rm = TRUE),
    Maximo  = max(horas_sem, na.rm = TRUE)
  ) %>%
  mutate(across(c(Media, DE, Q1, Mediana, Q3, Minimo, Maximo), 
                ~ round(.x, 1))) %>%
  rename(`Sexo` = sexo_etiqueta)

ft_horas_sexo <- formato_flextable(tabla_horas_sexo, "Tabla 16. Estadísticos descriptivos de horas trabajadas según sexo, PEA ocupada, Perú, 2025")
print(ft_horas_sexo)

plot_horas_sexo <- ggplot(enaho_explorar %>% filter(!is.na(sexo_etiqueta) & !is.na(horas_sem)), 
                          aes(x = sexo_etiqueta, y = horas_sem, fill = sexo_etiqueta, weight = factor500)) +
  geom_boxplot(alpha = 0.7, outlier.color = "red", outlier.alpha = 0.3) +
  scale_fill_manual(values = c("Hombre" = "#2E5B88", "Mujer" = "#E69F00")) +
  labs(title = "Gráfico 9. Horas trabajadas semanalmente según sexo, PEA ocupada", 
       x = "Sexo", y = "Horas semanales",
       caption = "Fuente: ENAHO 2025.") +
  theme_minimal() + theme(legend.position = "none")
print(plot_horas_sexo)

# C. Ingreso Principal según Registro en SUNAT 
tabla_ing_ruc <- enaho_diseno_500 %>%
  filter(!is.na(tiene_ruc_etiqueta) & !is.na(ing_prin)) %>%
  group_by(tiene_ruc_etiqueta) %>%
  summarise(
    N       = unweighted(n()),
    Media   = survey_mean(ing_prin, vartype = NULL),
    DE      = survey_sd(ing_prin, vartype = NULL),
    Mediana = survey_median(ing_prin, vartype = NULL)
  ) %>%
  arrange(desc(Mediana)) %>%
  mutate(across(c(Media, DE, Mediana), 
                ~ scales::comma(round(.x, 0)))) %>%
  rename(`Registro en SUNAT` = tiene_ruc_etiqueta)

ft_ing_ruc <- formato_flextable(tabla_ing_ruc, "Tabla 17. Estadísticos descriptivos del ingreso principal según registro en SUNAT, PEA ocupada, Perú, 2025")
print(ft_ing_ruc)

plot_ing_ruc <- ggplot(enaho_explorar %>% filter(!is.na(tiene_ruc_etiqueta) & !is.na(ing_prin)), 
                       aes(x = tiene_ruc_etiqueta, y = ing_prin, fill = tiene_ruc_etiqueta, weight = factor500)) +
  geom_boxplot(alpha = 0.7, outlier.color = "red", outlier.alpha = 0.3) +
  coord_cartesian(ylim = c(0, 5000)) + 
  scale_y_continuous(labels = scales::comma) +
  labs(title = "Gráfico 10. Ingreso principal según registro en SUNAT, PEA ocupada", 
       x = "Registro en SUNAT", y = "Ingreso (Soles)",
       caption = "Fuente: ENAHO 2025. Nota: Eje Y truncado en S/5,000.") +
  theme_minimal() + theme(legend.position = "none", axis.text.x = element_text(angle = 20, hjust = 1))
print(plot_ing_ruc)

# D. Ingreso Principal según Tipo de Contrato
tabla_ing_contrato <- enaho_diseno_500 %>%
  filter(!is.na(tiene_contrato_etiqueta) & !is.na(ing_prin)) %>%
  group_by(tiene_contrato_etiqueta) %>%
  summarise(
    N       = unweighted(n()),
    Media   = survey_mean(ing_prin, vartype = NULL),
    DE      = survey_sd(ing_prin, vartype = NULL),
    Mediana = survey_median(ing_prin, vartype = NULL)
  ) %>%
  arrange(desc(Mediana)) %>%
  mutate(across(c(Media, DE, Mediana), 
                ~ scales::comma(round(.x, 0)))) %>%
  rename(`Tipo de Contrato` = tiene_contrato_etiqueta)

ft_ing_contrato <- formato_flextable(tabla_ing_contrato, "Tabla 18. Estadísticos descriptivos del ingreso principal según tipo de contrato, PEA ocupada, Perú, 2025")
print(ft_ing_contrato)

plot_ing_contrato <- ggplot(enaho_explorar %>% filter(!is.na(tiene_contrato_etiqueta) & !is.na(ing_prin)), 
                            aes(x = tiene_contrato_etiqueta, y = ing_prin, fill = tiene_contrato_etiqueta, weight = factor500)) +
  geom_boxplot(alpha = 0.7, outlier.color = "red", outlier.alpha = 0.3) +
  coord_cartesian(ylim = c(0, 5000)) + 
  scale_y_continuous(labels = scales::comma) +
  labs(title = "Gráfico 11. Ingreso principal según tipo de contrato, PEA ocupada", 
       x = "Tipo de Contrato", y = "Ingreso (Soles)",
       caption = "Fuente: ENAHO 2025. Nota: Eje Y truncado en S/5,000.") +
  theme_minimal() + theme(legend.position = "none", axis.text.x = element_text(angle = 45, hjust = 1))
print(plot_ing_contrato)

# E. Ingreso Principal según Nivel Educativo 
tabla_ing_educacion <- enaho_diseno_300 %>%
  filter(!is.na(educacion_etiqueta) & !is.na(ing_prin)) %>%
  mutate(
    educacion_agrupada = case_when(
      educacion_etiqueta %in% c("Sin nivel", "Educación inicial", "Básica especial") ~ "Sin nivel / Inicial / Básica especial",
      educacion_etiqueta %in% c("Primaria incompleta", "Primaria completa") ~ "Primaria",
      educacion_etiqueta %in% c("Secundaria incompleta", "Secundaria completa") ~ "Secundaria",
      educacion_etiqueta %in% c("Sup. no univ. incompleta", "Sup. no univ. completa") ~ "Superior no universitaria",
      educacion_etiqueta %in% c("Sup. univ. incompleta", "Sup. univ. completa") ~ "Superior universitaria",
      educacion_etiqueta == "Maestría/Doctorado" ~ "Posgrado",
      TRUE ~ NA_character_
    ),
    educacion_agrupada = factor(educacion_agrupada, levels = c(
      "Sin nivel / Inicial / Básica especial", "Primaria", "Secundaria",
      "Superior no universitaria", "Superior universitaria", "Posgrado"
    ))
  ) %>%
  filter(!is.na(educacion_agrupada)) %>%
  group_by(educacion_agrupada) %>%
  summarise(
    N       = unweighted(n()),
    Media   = survey_mean(ing_prin, vartype = NULL),
    DE      = survey_sd(ing_prin, vartype = NULL),
    Mediana = survey_median(ing_prin, vartype = NULL)
  ) %>%
  mutate(across(c(Media, DE, Mediana), 
                ~ scales::comma(round(.x, 0)))) %>%
  rename(`Nivel Educativo` = educacion_agrupada)

ft_ing_educacion <- formato_flextable(tabla_ing_educacion, "Tabla 19. Estadísticos descriptivos del ingreso principal según nivel educativo (agrupado), PEA ocupada, Perú, 2025")
print(ft_ing_educacion)

# F. Ingreso Principal según Etnicidad (solo tabla, 9 categorías)
tabla_ing_etnicidad <- enaho_diseno_500 %>%
  filter(!is.na(etnicidad_etiqueta) & !is.na(ing_prin)) %>%
  group_by(etnicidad_etiqueta) %>%
  summarise(
    N       = unweighted(n()),
    Media   = survey_mean(ing_prin, vartype = NULL),
    DE      = survey_sd(ing_prin, vartype = NULL),
    Mediana = survey_median(ing_prin, vartype = NULL)
  ) %>%
  arrange(desc(Mediana)) %>%
  mutate(across(c(Media, DE, Mediana), 
                ~ scales::comma(round(.x, 0)))) %>%
  rename(`Etnicidad` = etnicidad_etiqueta)

ft_ing_etnicidad <- formato_flextable(tabla_ing_etnicidad, "Tabla 20. Estadísticos descriptivos del ingreso principal según autoidentificación étnica, PEA ocupada, Perú, 2025")
print(ft_ing_etnicidad)

# ------------------------------------------------------------------------------
# 5.3 Continua vs. Continua (Gráficos de Dispersión)
# ------------------------------------------------------------------------------

# A. Edad vs. Ingreso Principal
plot_edad_ingreso <- ggplot(enaho_explorar %>% filter(!is.na(edad) & ing_prin > 0), 
                            aes(x = edad, y = ing_prin)) +
  geom_jitter(alpha = 0.15, color = "#4A7C59", width = 0.5, height = 0) +
  geom_smooth(method = "gam", color = "red", se = FALSE, linewidth = 1) + 
  scale_y_log10(labels = scales::comma) + 
  labs(title = "Gráfico 12. Relación entre edad e ingreso principal, PEA ocupada", 
       subtitle = "Escala logarítmica (eje Y) con línea de tendencia suavizada", 
       x = "Edad (años)", y = "Ingreso (Soles, escala log10)",
       caption = "Fuente: ENAHO 2025. Nota: Se excluyeron ingresos iguales a cero.") +
  theme_minimal()
print(plot_edad_ingreso)

# B. Horas Semanales vs. Ingreso Principal
plot_horas_ingreso <- ggplot(enaho_explorar %>% filter(!is.na(horas_sem) & ing_prin > 0), 
                             aes(x = horas_sem, y = ing_prin)) +
  geom_jitter(alpha = 0.15, color = "#8B5A2B", width = 0.5, height = 0) +
  geom_smooth(method = "gam", color = "red", se = FALSE, linewidth = 1) + 
  scale_y_log10(labels = scales::comma) + 
  labs(title = "Gráfico 13. Relación entre horas trabajadas e ingreso principal, PEA ocupada", 
       subtitle = "Escala logarítmica (eje Y) con línea de tendencia suavizada", 
       x = "Horas trabajadas (semana)", y = "Ingreso (Soles, escala log10)",
       caption = "Fuente: ENAHO 2025. Nota: Se excluyeron ingresos iguales a cero.") +
  theme_minimal()
print(plot_horas_ingreso)

# =====================================================================================
# 6. EXPORTACIÓN MASIVA (Imágenes para Informe descriptivo que haremos en Markdown)----
# =====================================================================================

# --- Ruta de salida única ---
ruta_salida <- "outputs/outputs_exploracion_inicial"

if (!dir.exists(ruta_salida)) dir.create(ruta_salida, recursive = TRUE)

# ==============================================================================
# EXPORTACIÓN: TABLAS Y GRÁFICOS UNIVARIADOS
# ==============================================================================
save_as_image(ft_sexo,           path = paste0(ruta_salida, "/Tabla1_Sexo.png"))
save_as_image(ft_etnicidad,      path = paste0(ruta_salida, "/Tabla2_Etnicidad.png"))
save_as_image(ft_educacion,      path = paste0(ruta_salida, "/Tabla3_Educacion.png"))
save_as_image(ft_pension_no,     path = paste0(ruta_salida, "/Tabla4_Pension.png"))
save_as_image(ft_temp_pago,      path = paste0(ruta_salida, "/Tabla5_FrecuenciaPago.png"))
save_as_image(ft_tiene_ruc,      path = paste0(ruta_salida, "/Tabla6_RegistroSUNAT.png"))
save_as_image(ft_tiene_contrato, path = paste0(ruta_salida, "/Tabla7_TipoContrato.png"))
save_as_image(ft_edad,           path = paste0(ruta_salida, "/Tabla8_Stats_Edad.png"))
save_as_image(ft_ing_prin,       path = paste0(ruta_salida, "/Tabla9_Stats_Ingreso.png"))
save_as_image(ft_horas_sem,      path = paste0(ruta_salida, "/Tabla10_Stats_HorasSemana.png"))

ggsave(paste0(ruta_salida, "/Grafico1_Edad.png"),           plot = plot_edad,      width = 8, height = 5, bg = "white")
ggsave(paste0(ruta_salida, "/Grafico2_Ingreso.png"),        plot = plot_ingreso,   width = 8, height = 5, bg = "white")
ggsave(paste0(ruta_salida, "/Grafico3_HorasSemana.png"),    plot = plot_horas,     width = 8, height = 5, bg = "white")
ggsave(paste0(ruta_salida, "/Grafico4_FrecuenciaPago.png"), plot = plot_temp_pago, width = 8, height = 5, bg = "white")

# ==============================================================================
# EXPORTACIÓN: TABLAS Y GRÁFICOS BIVARIADOS (numeración según .Rmd)
# ==============================================================================
# --- 5.1 Categórica vs. Categórica ---
save_as_image(ft_sexo_ruc,       path = paste0(ruta_salida, "/Tabla10_RUC_Sexo.png"))
save_as_image(ft_sexo_pension,   path = paste0(ruta_salida, "/Tabla11_Pension_Sexo.png"))
save_as_image(ft_sexo_contrato,  path = paste0(ruta_salida, "/Tabla12_Contrato_Sexo.png"))
save_as_image(ft_ruc_pension,    path = paste0(ruta_salida, "/Tabla13_RUC_Pension.png"))
save_as_image(ft_ruc_contrato,   path = paste0(ruta_salida, "/Tabla14_Contrato_RUC.png"))

# --- 5.2 Categórica vs. Continua ---
save_as_image(ft_ing_sexo,       path = paste0(ruta_salida, "/Tabla15_Ingreso_Sexo.png"))
save_as_image(ft_horas_sexo,     path = paste0(ruta_salida, "/Tabla16_Horas_Sexo.png"))
save_as_image(ft_ing_ruc,        path = paste0(ruta_salida, "/Tabla17_Ingreso_RUC.png"))
save_as_image(ft_ing_contrato,   path = paste0(ruta_salida, "/Tabla18_Ingreso_Contrato.png"))
save_as_image(ft_ing_educacion,  path = paste0(ruta_salida, "/Tabla19_Ingreso_Educacion.png"))
save_as_image(ft_ing_etnicidad,  path = paste0(ruta_salida, "/Tabla20_Ingreso_Etnicidad.png"))

ggsave(paste0(ruta_salida, "/Grafico8_Ingreso_Sexo.png"),      plot = plot_ing_sexo,      width = 8, height = 5, bg = "white")
ggsave(paste0(ruta_salida, "/Grafico9_Horas_Sexo.png"),        plot = plot_horas_sexo,    width = 8, height = 5, bg = "white")
ggsave(paste0(ruta_salida, "/Grafico10_Ingreso_RUC.png"),      plot = plot_ing_ruc,       width = 8, height = 5, bg = "white")
ggsave(paste0(ruta_salida, "/Grafico11_Ingreso_Contrato.png"), plot = plot_ing_contrato,  width = 8, height = 5, bg = "white")

# --- 5.3 Continua vs. Continua ---
ggsave(paste0(ruta_salida, "/Grafico12_Edad_Ingreso.png"),  plot = plot_edad_ingreso,  width = 8, height = 5, bg = "white")
ggsave(paste0(ruta_salida, "/Grafico13_Horas_Ingreso.png"), plot = plot_horas_ingreso, width = 8, height = 5, bg = "white")