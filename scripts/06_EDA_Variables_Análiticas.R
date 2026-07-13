#===============================================================================
#Proyecto: Análisis de la relacion etnicidad-ITD usando datos de la ENAHO 
#Autorxs: Carmen Andonayre y Azra Roa
#Objetivo: Explorar las variables analíticas creadas
#Fecha: 12-07-2026
#===============================================================================

# ------------------------------------------------------------------------------
# 0. CONFIGURACIÓN Y CARGA DE DATOS---------------------------------------------
# ------------------------------------------------------------------------------
library(tidyverse)
library(arrow)
library(survey)      
library(srvyr)       
library(here)
library(gtsummary)
library(flextable)
renv::snapshot()

# Cargamos la base de datos que contiene nuestras variables analíticas
enaho_analitica <- read_parquet(here("datos", "procesados", "enaho_analitica_2025_12_07_26.parquet"))

# ------------------------------------------------------------------------------
# 1. DISEÑO MUESTRAL-------------------------------------------------------------
# ------------------------------------------------------------------------------
enaho_reporte <- enaho_analitica %>%
  filter(!is.na(factor500)) %>%
  as_survey_design(
    ids = conglome,
    strata = estrato,
    weights = factor500,
    nest = TRUE
  )

# ==============================================================================
# 2. EXPLORACIÓN UNIVARIADA DE VARIABLES ANALÍTICAS: TABLAS DESCRIPTIVAS--------
# ==============================================================================
# Reutilizamos la función de formato ya definida en la fase de Explorar (formato_flextable)

# ------------------------------------------------------------------------------
# 2.1 Edad (Criterio Teórico)-----------------------------------------------------
# ------------------------------------------------------------------------------
tabla_edad_teoria <- enaho_reporte %>%
  filter(!is.na(edad_teoria)) %>%
  group_by(edad_teoria) %>%
  summarise(Poblacion = survey_total(vartype = NULL), 
            Porcentaje = survey_mean(vartype = NULL) * 100) %>%
  arrange(desc(Porcentaje)) %>%
  mutate(Poblacion = scales::comma(round(Poblacion, 0)), 
         Porcentaje = paste0(round(Porcentaje, 1), "%")) %>%
  rename(`Grupo Etario (Criterio Teórico)` = edad_teoria, 
         `Total (N)` = Poblacion, 
         `%` = Porcentaje)

ft_edad_teoria <- formato_flextable(tabla_edad_teoria, "Tabla 1. Distribución de la PEA ocupada según grupo etario (criterio teórico INEI), Perú, 2025")
print(ft_edad_teoria)

plot_edad_teoria <- ggplot(enaho_analitica %>% filter(!is.na(edad_teoria)), 
                           aes(x = edad_teoria, weight = factor500)) +
  geom_bar(fill = "#4A7C59", alpha = 0.85) +
  scale_y_continuous(labels = scales::comma) +
  labs(title = "Gráfico 1. Distribución de la PEA ocupada según grupo etario (criterio teórico)", 
       x = "Grupo etario", y = "Frecuencia Poblacional",
       caption = "Fuente: ENAHO 2025. Cálculos ajustados a nivel poblacional.") +
  theme_minimal()
print(plot_edad_teoria)

# ------------------------------------------------------------------------------
# 2.2 Edad Estandarizada (Puntaje Z)----------------------------------------------
# ------------------------------------------------------------------------------
stats_edad_z <- enaho_reporte %>%
  filter(!is.na(edad_z)) %>%
  summarise(
    `Mínimo` = min(edad_z, na.rm = TRUE),
    `Percentil 25 (Q1)` = survey_quantile(edad_z, 0.25, vartype = NULL),
    `Mediana (Q2)` = survey_median(edad_z, vartype = NULL),
    `Media (Promedio)` = survey_mean(edad_z, vartype = NULL),
    `Desviación Estándar` = survey_sd(edad_z, vartype = NULL),
    `Percentil 75 (Q3)` = survey_quantile(edad_z, 0.75, vartype = NULL),
    `Máximo` = max(edad_z, na.rm = TRUE)
  ) %>%
  pivot_longer(cols = everything(), names_to = "Estadístico", values_to = "Valor (Puntaje Z)") %>%
  mutate(
    Estadístico = str_remove(Estadístico, "_q[0-9]+"),
    `Valor (Puntaje Z)` = round(`Valor (Puntaje Z)`, 2)
  )
ft_edad_z <- formato_flextable(stats_edad_z, "Tabla 2. Edad estandarizada (Puntaje Z) de la PEA Ocupada, Perú, 2025")
print(ft_edad_z)

plot_edad_z <- ggplot(enaho_analitica %>% filter(!is.na(edad_z) & !is.na(factor500)), 
                      aes(x = edad_z, weight = factor500)) +
  geom_histogram(fill = "#2E5B88", color = "white", binwidth = 0.25) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "red", linewidth = 0.5) +
  scale_y_continuous(labels = scales::comma) +
  labs(title = "Gráfico 2. Distribución de la edad estandarizada (Puntaje Z) de la PEA ocupada", 
       x = "Edad estandarizada (Puntaje Z)", y = "Frecuencia Poblacional",
       caption = "Fuente: ENAHO 2025. Nota: La línea roja indica la media poblacional (Z = 0).") +
  theme_minimal()
print(plot_edad_z)

# ------------------------------------------------------------------------------
# 2.3 Clasificación Geográfica (Urbano/Rural)-------------------------------------
# ------------------------------------------------------------------------------
tabla_estrato_teo <- enaho_reporte %>%
  filter(!is.na(estrato_teo)) %>%
  group_by(estrato_teo) %>%
  summarise(Poblacion = survey_total(vartype = NULL), 
            Porcentaje = survey_mean(vartype = NULL) * 100) %>%
  arrange(desc(Porcentaje)) %>%
  mutate(Poblacion = scales::comma(round(Poblacion, 0)), 
         Porcentaje = paste0(round(Porcentaje, 1), "%")) %>%
  rename(`Clasificación Geográfica` = estrato_teo, 
         `Total (N)` = Poblacion, 
         `%` = Porcentaje)

ft_estrato_teo <- formato_flextable(tabla_estrato_teo, "Tabla 3. Distribución de la PEA ocupada según clasificación geográfica (Urbano/Rural), Perú, 2025")
print(ft_estrato_teo)

prop_estrato_teo <- enaho_reporte %>%
  filter(!is.na(estrato_teo)) %>%
  group_by(estrato_teo) %>%
  summarise(Porcentaje = survey_mean(vartype = NULL) * 100)

plot_estrato_teo <- ggplot(prop_estrato_teo, aes(x = "", y = Porcentaje, fill = estrato_teo)) +
  geom_col(width = 1, color = "white") +
  coord_polar(theta = "y") +
  geom_text(aes(label = paste0(round(Porcentaje, 1), "%")), 
            position = position_stack(vjust = 0.5), color = "white", fontface = "bold") +
  scale_fill_manual(values = c("Urbano" = "#2E5B88", "Rural" = "#8B5A2B")) +
  labs(title = "Gráfico 3. Distribución de la PEA ocupada según clasificación geográfica", 
       fill = "Zona:",
       caption = "Fuente: ENAHO 2025. Cálculos ajustados a nivel poblacional.") +
  theme_void() +
  theme(plot.title = element_text(hjust = 0.5))
print(plot_estrato_teo)

# ------------------------------------------------------------------------------
# 2.4 Nivel Educativo (Agrupado)---------------------------------------------------
# ------------------------------------------------------------------------------
tabla_educacion_agrup <- enaho_reporte %>%
  filter(!is.na(educacion)) %>%
  group_by(educacion) %>%
  summarise(Poblacion = survey_total(vartype = NULL), 
            Porcentaje = survey_mean(vartype = NULL) * 100) %>%
  arrange(desc(Porcentaje)) %>%
  mutate(Poblacion = scales::comma(round(Poblacion, 0)), 
         Porcentaje = paste0(round(Porcentaje, 1), "%")) %>%
  rename(`Nivel Educativo (Agrupado)` = educacion, 
         `Total (N)` = Poblacion, 
         `%` = Porcentaje)

ft_educacion_agrup <- formato_flextable(tabla_educacion_agrup, "Tabla 4. Distribución de la PEA ocupada según nivel educativo agrupado, Perú, 2025")
print(ft_educacion_agrup)

plot_educacion_agrup <- ggplot(enaho_analitica %>% filter(!is.na(educacion)), 
                               aes(x = fct_infreq(educacion), weight = factor500)) +
  geom_bar(fill = "#E69F00", alpha = 0.85) +
  coord_flip() +
  scale_y_continuous(labels = scales::comma) +
  labs(title = "Gráfico 4. Distribución de la PEA ocupada según nivel educativo agrupado", 
       x = "Nivel Educativo", y = "Frecuencia Poblacional",
       caption = "Fuente: ENAHO 2025. Cálculos ajustados a nivel poblacional.") +
  theme_minimal()
print(plot_educacion_agrup)

# ------------------------------------------------------------------------------
# 2.5 Etnicidad (Agrupada: Indígena/No Indígena)-----------------------------------
# ------------------------------------------------------------------------------
tabla_etnicidad_agrup <- enaho_reporte %>%
  filter(!is.na(etnicidad)) %>%
  group_by(etnicidad) %>%
  summarise(Poblacion = survey_total(vartype = NULL), 
            Porcentaje = survey_mean(vartype = NULL) * 100) %>%
  arrange(desc(Porcentaje)) %>%
  mutate(Poblacion = scales::comma(round(Poblacion, 0)), 
         Porcentaje = paste0(round(Porcentaje, 1), "%")) %>%
  rename(`Etnicidad (Agrupada)` = etnicidad, 
         `Total (N)` = Poblacion, 
         `%` = Porcentaje)

ft_etnicidad_agrup <- formato_flextable(tabla_etnicidad_agrup, "Tabla 5. Distribución de la PEA ocupada según autoidentificación étnica agrupada, Perú, 2025")
print(ft_etnicidad_agrup)

prop_etnicidad_agrup <- enaho_reporte %>%
  filter(!is.na(etnicidad)) %>%
  group_by(etnicidad) %>%
  summarise(Porcentaje = survey_mean(vartype = NULL) * 100)

plot_etnicidad_agrup <- ggplot(prop_etnicidad_agrup, aes(x = "", y = Porcentaje, fill = etnicidad)) +
  geom_col(width = 1, color = "white") +
  coord_polar(theta = "y") +
  geom_text(aes(label = paste0(round(Porcentaje, 1), "%")), 
            position = position_stack(vjust = 0.5), color = "white", fontface = "bold") +
  scale_fill_manual(values = c("Indígena" = "#B33951", "No Indígena" = "#4A7C59")) +
  labs(title = "Gráfico 5. Distribución de la PEA ocupada según autoidentificación étnica agrupada", 
       fill = "Etnicidad:",
       caption = "Fuente: ENAHO 2025. Cálculos ajustados a nivel poblacional.") +
  theme_void() +
  theme(plot.title = element_text(hjust = 0.5))
print(plot_etnicidad_agrup)

# ------------------------------------------------------------------------------
# 2.6 Ingreso Mensual (Imputado)---------------------------------------------------
# ------------------------------------------------------------------------------
stats_ingreso_mensual <- enaho_reporte %>%
  filter(!is.na(ingreso_mensual_imp)) %>%
  summarise(
    `Mínimo` = min(ingreso_mensual_imp, na.rm = TRUE),
    `Percentil 25 (Q1)` = survey_quantile(ingreso_mensual_imp, 0.25, vartype = NULL),
    `Mediana (Q2)` = survey_median(ingreso_mensual_imp, vartype = NULL),
    `Media (Promedio)` = survey_mean(ingreso_mensual_imp, vartype = NULL),
    `Desviación Estándar` = survey_sd(ingreso_mensual_imp, vartype = NULL),
    `Percentil 75 (Q3)` = survey_quantile(ingreso_mensual_imp, 0.75, vartype = NULL),
    `Máximo` = max(ingreso_mensual_imp, na.rm = TRUE)
  ) %>%
  pivot_longer(cols = everything(), names_to = "Estadístico", values_to = "Valor (S/.)") %>%
  mutate(
    Estadístico = str_remove(Estadístico, "_q[0-9]+"),
    `Valor (S/.)` = scales::comma(round(`Valor (S/.)`, 1))
  )
ft_ingreso_mensual <- formato_flextable(stats_ingreso_mensual, "Tabla 6. Ingreso mensual imputado de la PEA Ocupada (estadísticos de resumen), Perú, 2025")
print(ft_ingreso_mensual)

plot_ingreso_mensual <- ggplot(enaho_analitica %>% filter(!is.na(ingreso_mensual_imp) & !is.na(factor500)), 
                               aes(x = ingreso_mensual_imp, weight = factor500)) +
  geom_histogram(fill = "#2E5B88", color = "white", bins = 50) +
  coord_cartesian(xlim = c(0, 10000)) + 
  scale_x_continuous(labels = scales::comma) + 
  scale_y_continuous(labels = scales::comma) +
  labs(title = "Gráfico 6. Distribución del ingreso mensual imputado", 
       x = "Ingreso mensual (Soles)", y = "Frecuencia Poblacional",
       caption = "Fuente: ENAHO 2025. Nota: Eje X truncado en S/10,000. Cálculos ajustados a nivel poblacional.") +
  theme_minimal()
print(plot_ingreso_mensual)

# ------------------------------------------------------------------------------
# 2.7 Quintil de Ingreso-----------------------------------------------------------
# ------------------------------------------------------------------------------
tabla_quintil <- enaho_reporte %>%
  filter(!is.na(quintil_ingreso)) %>%
  mutate(quintil_ingreso = factor(quintil_ingreso, 
                                  levels = 1:5, 
                                  labels = c("Q1 (más bajo)", "Q2", "Q3", "Q4", "Q5 (más alto)"))) %>%
  group_by(quintil_ingreso) %>%
  summarise(Poblacion = survey_total(vartype = NULL), 
            Porcentaje = survey_mean(vartype = NULL) * 100) %>%
  mutate(Poblacion = scales::comma(round(Poblacion, 0)), 
         Porcentaje = paste0(round(Porcentaje, 1), "%")) %>%
  rename(`Quintil de Ingreso` = quintil_ingreso, 
         `Total (N)` = Poblacion, 
         `%` = Porcentaje)

ft_quintil <- formato_flextable(tabla_quintil, "Tabla 7. Distribución de la PEA ocupada según quintil de ingreso, Perú, 2025")
print(ft_quintil)

plot_quintil <- ggplot(enaho_analitica %>% 
                         filter(!is.na(quintil_ingreso)) %>%
                         mutate(quintil_ingreso = factor(quintil_ingreso, 
                                                         levels = 1:5, 
                                                         labels = c("Q1 (más bajo)", "Q2", "Q3", "Q4", "Q5 (más alto)"))), 
                       aes(x = quintil_ingreso, weight = factor500)) +
  geom_bar(fill = "#4A7C59", alpha = 0.85) +
  scale_y_continuous(labels = scales::comma) +
  labs(title = "Gráfico 7. Distribución de la PEA ocupada según quintil de ingreso", 
       x = "Quintil de Ingreso", y = "Frecuencia Poblacional",
       caption = "Fuente: ENAHO 2025. Cálculos ajustados a nivel poblacional.") +
  theme_minimal()
print(plot_quintil)

# ------------------------------------------------------------------------------
# 2.8 Afiliación a Sistema de Pensiones (tiene_pension)---------------------------
# ------------------------------------------------------------------------------
tabla_tiene_pension <- enaho_reporte %>%
  filter(!is.na(tiene_pension)) %>%
  group_by(tiene_pension) %>%
  summarise(Poblacion = survey_total(vartype = NULL), 
            Porcentaje = survey_mean(vartype = NULL) * 100) %>%
  arrange(desc(Porcentaje)) %>%
  mutate(Poblacion = scales::comma(round(Poblacion, 0)), 
         Porcentaje = paste0(round(Porcentaje, 1), "%")) %>%
  rename(`Afiliación a Pensiones` = tiene_pension, `Total (N)` = Poblacion, `%` = Porcentaje)

ft_tiene_pension <- formato_flextable(tabla_tiene_pension, "Tabla 8. Distribución de la PEA ocupada según afiliación a sistema de pensiones, Perú, 2025")
print(ft_tiene_pension)

prop_tiene_pension <- enaho_reporte %>%
  filter(!is.na(tiene_pension)) %>%
  group_by(tiene_pension) %>%
  summarise(Porcentaje = survey_mean(vartype = NULL) * 100)

plot_tiene_pension <- ggplot(prop_tiene_pension, aes(x = "", y = Porcentaje, fill = tiene_pension)) +
  geom_col(width = 1, color = "white") +
  coord_polar(theta = "y") +
  geom_text(aes(label = paste0(round(Porcentaje, 1), "%")), position = position_stack(vjust = 0.5), color = "white", fontface = "bold") +
  scale_fill_manual(values = c("Tiene pension" = "#4A7C59", "No tiene pension" = "#B33951")) +
  labs(title = "Gráfico 8. Distribución de la PEA ocupada según afiliación a sistema de pensiones", 
       fill = "Afiliación:", caption = "Fuente: ENAHO 2025. Cálculos ajustados a nivel poblacional.") +
  theme_void() + theme(plot.title = element_text(hjust = 0.5))
print(plot_tiene_pension)

# ------------------------------------------------------------------------------
# 2.9 Registro en SUNAT (tiene_registro)-------------------------------------------
# ------------------------------------------------------------------------------
tabla_tiene_registro <- enaho_reporte %>%
  filter(!is.na(tiene_registro)) %>%
  group_by(tiene_registro) %>%
  summarise(Poblacion = survey_total(vartype = NULL), 
            Porcentaje = survey_mean(vartype = NULL) * 100) %>%
  arrange(desc(Porcentaje)) %>%
  mutate(Poblacion = scales::comma(round(Poblacion, 0)), 
         Porcentaje = paste0(round(Porcentaje, 1), "%")) %>%
  rename(`Registro en SUNAT` = tiene_registro, `Total (N)` = Poblacion, `%` = Porcentaje)

ft_tiene_registro <- formato_flextable(tabla_tiene_registro, "Tabla 9. Distribución de la PEA ocupada según registro en SUNAT, Perú, 2025")
print(ft_tiene_registro)

prop_tiene_registro <- enaho_reporte %>%
  filter(!is.na(tiene_registro)) %>%
  group_by(tiene_registro) %>%
  summarise(Porcentaje = survey_mean(vartype = NULL) * 100)

plot_tiene_registro <- ggplot(prop_tiene_registro, aes(x = "", y = Porcentaje, fill = tiene_registro)) +
  geom_col(width = 1, color = "white") +
  coord_polar(theta = "y") +
  geom_text(aes(label = paste0(round(Porcentaje, 1), "%")), position = position_stack(vjust = 0.5), color = "white", fontface = "bold") +
  scale_fill_manual(values = c("Tiene RUC" = "#2E5B88", "No tiene RUC" = "#B33951")) +
  labs(title = "Gráfico 9. Distribución de la PEA ocupada según registro en SUNAT", 
       fill = "Registro:", caption = "Fuente: ENAHO 2025. Cálculos ajustados a nivel poblacional.") +
  theme_void() + theme(plot.title = element_text(hjust = 0.5))
print(plot_tiene_registro)

# ------------------------------------------------------------------------------
# 2.10 Condición de Contratación (tiene_contratos)---------------------------------
# ------------------------------------------------------------------------------
tabla_tiene_contratos <- enaho_reporte %>%
  filter(!is.na(tiene_contratos)) %>%
  group_by(tiene_contratos) %>%
  summarise(Poblacion = survey_total(vartype = NULL), 
            Porcentaje = survey_mean(vartype = NULL) * 100) %>%
  arrange(desc(Porcentaje)) %>%
  mutate(Poblacion = scales::comma(round(Poblacion, 0)), 
         Porcentaje = paste0(round(Porcentaje, 1), "%")) %>%
  rename(`Condición de Contratación` = tiene_contratos, `Total (N)` = Poblacion, `%` = Porcentaje)

ft_tiene_contratos <- formato_flextable(tabla_tiene_contratos, "Tabla 10. Distribución de la PEA ocupada según condición de contratación, Perú, 2025")
print(ft_tiene_contratos)

prop_tiene_contratos <- enaho_reporte %>%
  filter(!is.na(tiene_contratos)) %>%
  group_by(tiene_contratos) %>%
  summarise(Porcentaje = survey_mean(vartype = NULL) * 100)

plot_tiene_contratos <- ggplot(prop_tiene_contratos, aes(x = "", y = Porcentaje, fill = tiene_contratos)) +
  geom_col(width = 1, color = "white") +
  coord_polar(theta = "y") +
  geom_text(aes(label = paste0(round(Porcentaje, 1), "%")), position = position_stack(vjust = 0.5), color = "white", fontface = "bold") +
  scale_fill_manual(values = c("Tiene contrato" = "#4A7C59", "No tiene contrato" = "#B33951")) +
  labs(title = "Gráfico 10. Distribución de la PEA ocupada según condición de contratación", 
       fill = "Contratación:", caption = "Fuente: ENAHO 2025. Cálculos ajustados a nivel poblacional.") +
  theme_void() + theme(plot.title = element_text(hjust = 0.5))
print(plot_tiene_contratos)

# ------------------------------------------------------------------------------
# 2.11 Horas Trabajadas (Indicador de Decencia) - horas_decente-------------------
# ------------------------------------------------------------------------------
tabla_horas_decente <- enaho_reporte %>%
  filter(!is.na(horas_decente)) %>%
  group_by(horas_decente) %>%
  summarise(Poblacion = survey_total(vartype = NULL), 
            Porcentaje = survey_mean(vartype = NULL) * 100) %>%
  arrange(desc(Porcentaje)) %>%
  mutate(Poblacion = scales::comma(round(Poblacion, 0)), 
         Porcentaje = paste0(round(Porcentaje, 1), "%")) %>%
  rename(`Horas Trabajadas (Semanal)` = horas_decente, `Total (N)` = Poblacion, `%` = Porcentaje)

ft_horas_decente <- formato_flextable(tabla_horas_decente, "Tabla 11. Distribución de la PEA ocupada según cumplimiento del límite legal de horas semanales, Perú, 2025")
print(ft_horas_decente)

prop_horas_decente <- enaho_reporte %>%
  filter(!is.na(horas_decente)) %>%
  group_by(horas_decente) %>%
  summarise(Porcentaje = survey_mean(vartype = NULL) * 100)

plot_horas_decente <- ggplot(prop_horas_decente, aes(x = "", y = Porcentaje, fill = horas_decente)) +
  geom_col(width = 1, color = "white") +
  coord_polar(theta = "y") +
  geom_text(aes(label = paste0(round(Porcentaje, 1), "%")), position = position_stack(vjust = 0.5), color = "white", fontface = "bold") +
  scale_fill_manual(values = c("Menos de 48 horas semanales" = "#4A7C59", "Más de 48 horas semanales" = "#B33951")) +
  labs(title = "Gráfico 11. Distribución de la PEA ocupada según cumplimiento del límite legal de horas", 
       fill = "Jornada:", caption = "Fuente: ENAHO 2025. Cálculos ajustados a nivel poblacional.") +
  theme_void() + theme(plot.title = element_text(hjust = 0.5))
print(plot_horas_decente)

# ------------------------------------------------------------------------------
# 2.12 Ingreso en relación a la RMV (ingreso_decente)------------------------------
# ------------------------------------------------------------------------------
tabla_ingreso_decente <- enaho_reporte %>%
  filter(!is.na(ingreso_decente)) %>%
  group_by(ingreso_decente) %>%
  summarise(Poblacion = survey_total(vartype = NULL), 
            Porcentaje = survey_mean(vartype = NULL) * 100) %>%
  arrange(desc(Porcentaje)) %>%
  mutate(Poblacion = scales::comma(round(Poblacion, 0)), 
         Porcentaje = paste0(round(Porcentaje, 1), "%")) %>%
  rename(`Ingreso vs. RMV` = ingreso_decente, `Total (N)` = Poblacion, `%` = Porcentaje)

ft_ingreso_decente <- formato_flextable(tabla_ingreso_decente, "Tabla 12. Distribución de la PEA ocupada según cumplimiento de la Remuneración Mínima Vital, Perú, 2025")
print(ft_ingreso_decente)

prop_ingreso_decente <- enaho_reporte %>%
  filter(!is.na(ingreso_decente)) %>%
  group_by(ingreso_decente) %>%
  summarise(Porcentaje = survey_mean(vartype = NULL) * 100)

plot_ingreso_decente <- ggplot(prop_ingreso_decente, aes(x = "", y = Porcentaje, fill = ingreso_decente)) +
  geom_col(width = 1, color = "white") +
  coord_polar(theta = "y") +
  geom_text(aes(label = paste0(round(Porcentaje, 1), "%")), position = position_stack(vjust = 0.5), color = "white", fontface = "bold") +
  scale_fill_manual(values = c("Ingreso mensual mayor a RMV" = "#4A7C59", "Ingreso mensual menor a RMV" = "#B33951")) +
  labs(title = "Gráfico 12. Distribución de la PEA ocupada según cumplimiento de la RMV", 
       fill = "Ingreso vs. RMV:", caption = "Fuente: ENAHO 2025. Nota: RMV 2025 = S/1,130. Cálculos ajustados a nivel poblacional.") +
  theme_void() + theme(plot.title = element_text(hjust = 0.5))
print(plot_ingreso_decente)

# ------------------------------------------------------------------------------
# 2.13 Índice Aditivo (0 a 4 dimensiones cumplidas)--------------------------------
# ------------------------------------------------------------------------------
tabla_indice_aditivo <- enaho_reporte %>%
  filter(!is.na(indice_aditivo)) %>%
  mutate(indice_aditivo_f = factor(indice_aditivo, levels = 0:4)) %>%
  group_by(indice_aditivo_f) %>%
  summarise(Poblacion = survey_total(vartype = NULL), 
            Porcentaje = survey_mean(vartype = NULL) * 100) %>%
  mutate(Poblacion = scales::comma(round(Poblacion, 0)), 
         Porcentaje = paste0(round(Porcentaje, 1), "%")) %>%
  rename(`Dimensiones de Trabajo Decente Cumplidas` = indice_aditivo_f, `Total (N)` = Poblacion, `%` = Porcentaje)

ft_indice_aditivo <- formato_flextable(tabla_indice_aditivo, "Tabla 13. Distribución de la PEA ocupada según número de dimensiones de trabajo decente cumplidas (0 a 4), Perú, 2025")
print(ft_indice_aditivo)

plot_indice_aditivo <- ggplot(enaho_analitica %>% filter(!is.na(indice_aditivo)) %>% mutate(indice_aditivo_f = factor(indice_aditivo, levels = 0:4)), 
                              aes(x = indice_aditivo_f, weight = factor500)) +
  geom_bar(fill = "#8B5A2B", alpha = 0.85) +
  scale_y_continuous(labels = scales::comma) +
  labs(title = "Gráfico 13. Distribución de la PEA ocupada según dimensiones de trabajo decente cumplidas", 
       x = "N.º de dimensiones cumplidas (de 4)", y = "Frecuencia Poblacional",
       caption = "Fuente: ENAHO 2025. Cálculos ajustados a nivel poblacional.") +
  theme_minimal()
print(plot_indice_aditivo)

# ------------------------------------------------------------------------------
# 2.14 Índice de Trabajo Decente (ITD, escala 0-100)-------------------------------
# ------------------------------------------------------------------------------
stats_ITD <- enaho_reporte %>%
  filter(!is.na(ITD)) %>%
  summarise(
    `Mínimo` = min(ITD, na.rm = TRUE),
    `Percentil 25 (Q1)` = survey_quantile(ITD, 0.25, vartype = NULL),
    `Mediana (Q2)` = survey_median(ITD, vartype = NULL),
    `Media (Promedio)` = survey_mean(ITD, vartype = NULL),
    `Desviación Estándar` = survey_sd(ITD, vartype = NULL),
    `Percentil 75 (Q3)` = survey_quantile(ITD, 0.75, vartype = NULL),
    `Máximo` = max(ITD, na.rm = TRUE)
  ) %>%
  pivot_longer(cols = everything(), names_to = "Estadístico", values_to = "Valor (Escala 0-100)") %>%
  mutate(
    Estadístico = str_remove(Estadístico, "_q[0-9]+"),
    `Valor (Escala 0-100)` = round(`Valor (Escala 0-100)`, 1)
  )
ft_ITD <- formato_flextable(stats_ITD, "Tabla 14. Índice de Trabajo Decente (ITD) de la PEA Ocupada (estadísticos de resumen), Perú, 2025")
print(ft_ITD)

plot_ITD <- ggplot(enaho_analitica %>% filter(!is.na(ITD) & !is.na(factor500)), 
                   aes(x = ITD, weight = factor500)) +
  geom_histogram(fill = "#4A7C59", color = "white", binwidth = 5) +
  scale_y_continuous(labels = scales::comma) +
  labs(title = "Gráfico 14. Distribución del Índice de Trabajo Decente (ITD)", 
       x = "ITD (Escala 0-100)", y = "Frecuencia Poblacional",
       caption = "Fuente: ENAHO 2025. Cálculos ajustados a nivel poblacional.") +
  theme_minimal()
print(plot_ITD)

# ==============================================================================
# EXPORTACIÓN: TABLAS Y GRÁFICOS — EDA DE VARIABLES ANALÍTICAS
# ==============================================================================
ruta_salida_analitica <- "outputs/outputs_exploracion_analitica"
if (!dir.exists(ruta_salida_analitica)) {
  dir.create(ruta_salida_analitica, recursive = TRUE)
}

# --- Tablas ---
save_as_image(ft_edad_teoria,      path = paste0(ruta_salida_analitica, "/Tabla1_EdadTeoria.png"))
save_as_image(ft_edad_z,           path = paste0(ruta_salida_analitica, "/Tabla2_EdadZ.png"))
save_as_image(ft_estrato_teo,      path = paste0(ruta_salida_analitica, "/Tabla3_EstratoTeo.png"))
save_as_image(ft_educacion_agrup,  path = paste0(ruta_salida_analitica, "/Tabla4_EducacionAgrupada.png"))
save_as_image(ft_etnicidad_agrup,  path = paste0(ruta_salida_analitica, "/Tabla5_EtnicidadAgrupada.png"))
save_as_image(ft_ingreso_mensual,  path = paste0(ruta_salida_analitica, "/Tabla6_IngresoMensual.png"))
save_as_image(ft_quintil,          path = paste0(ruta_salida_analitica, "/Tabla7_QuintilIngreso.png"))
save_as_image(ft_tiene_pension,    path = paste0(ruta_salida_analitica, "/Tabla8_TienePension.png"))
save_as_image(ft_tiene_registro,   path = paste0(ruta_salida_analitica, "/Tabla9_TieneRegistro.png"))
save_as_image(ft_tiene_contratos,  path = paste0(ruta_salida_analitica, "/Tabla10_TieneContratos.png"))
save_as_image(ft_horas_decente,    path = paste0(ruta_salida_analitica, "/Tabla11_HorasDecente.png"))
save_as_image(ft_ingreso_decente,  path = paste0(ruta_salida_analitica, "/Tabla12_IngresoDecente.png"))
save_as_image(ft_indice_aditivo,   path = paste0(ruta_salida_analitica, "/Tabla13_IndiceAditivo.png"))
save_as_image(ft_ITD,              path = paste0(ruta_salida_analitica, "/Tabla14_ITD.png"))

# --- Gráficos ---
ggsave(paste0(ruta_salida_analitica, "/Grafico1_EdadTeoria.png"),      plot = plot_edad_teoria,      width = 8, height = 5, bg = "white")
ggsave(paste0(ruta_salida_analitica, "/Grafico2_EdadZ.png"),           plot = plot_edad_z,           width = 8, height = 5, bg = "white")
ggsave(paste0(ruta_salida_analitica, "/Grafico3_EstratoTeo.png"),      plot = plot_estrato_teo,      width = 8, height = 5, bg = "white")
ggsave(paste0(ruta_salida_analitica, "/Grafico4_EducacionAgrupada.png"), plot = plot_educacion_agrup, width = 8, height = 5, bg = "white")
ggsave(paste0(ruta_salida_analitica, "/Grafico5_EtnicidadAgrupada.png"), plot = plot_etnicidad_agrup, width = 8, height = 5, bg = "white")
ggsave(paste0(ruta_salida_analitica, "/Grafico6_IngresoMensual.png"),  plot = plot_ingreso_mensual,  width = 8, height = 5, bg = "white")
ggsave(paste0(ruta_salida_analitica, "/Grafico7_QuintilIngreso.png"),  plot = plot_quintil,          width = 8, height = 5, bg = "white")
ggsave(paste0(ruta_salida_analitica, "/Grafico8_TienePension.png"),    plot = plot_tiene_pension,    width = 8, height = 5, bg = "white")
ggsave(paste0(ruta_salida_analitica, "/Grafico9_TieneRegistro.png"),   plot = plot_tiene_registro,   width = 8, height = 5, bg = "white")
ggsave(paste0(ruta_salida_analitica, "/Grafico10_TieneContratos.png"), plot = plot_tiene_contratos,  width = 8, height = 5, bg = "white")
ggsave(paste0(ruta_salida_analitica, "/Grafico11_HorasDecente.png"),   plot = plot_horas_decente,    width = 8, height = 5, bg = "white")
ggsave(paste0(ruta_salida_analitica, "/Grafico12_IngresoDecente.png"), plot = plot_ingreso_decente,  width = 8, height = 5, bg = "white")
ggsave(paste0(ruta_salida_analitica, "/Grafico13_IndiceAditivo.png"),  plot = plot_indice_aditivo,   width = 8, height = 5, bg = "white")
ggsave(paste0(ruta_salida_analitica, "/Grafico14_ITD.png"),            plot = plot_ITD,              width = 8, height = 5, bg = "white")