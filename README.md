# README: Análisis de la relacion entre Etnicidad y Trabajo Decente usando datos de la ENAHO 2025

### Autorxs: Carmen Andonayre y Azra Roa
### Curso: Taller de Procesamiento de Datos
### Encuesta: Encuesta Nacional de Hogares, Instituto Nacional de Estadística e Informática, 2025 (Anual)
### Módulos utilizados: Módulo 200 (Caracteristicas de los miembros del Hogar), Módulo 300 (Educación) y Módulo 500 (Empleo e ingresos) 
### Unidad de análisis: Individuo (Trabajador en PEA ocupada)

## Descripción del proyecto
Este repositorio contiene los scripts y el fluyo de trabajo del proyecto "Análisis de la relacion entre Etnicidad y Trabajo Decente usando datos de la ENAHO 2025", elaborado para el curso Taller de Procesamiento de Datos 2026-1 de la Pontificia Universidad Católica del Perú. El proyecto es una investigación acerca de la relación entre la autoidentificación étnico/racial y el acceso a un trabajo decente, bajo la definición de Gamero (2012). Los datos utilizados pertenecen en su totalidad a la Encuesta Nacional de Hogares (ENAHO) del 2025 (version anual). Esta encuesta es realizada por el Instituto Nacional de Estadística e Informática (INEI) del Perú. El proyecto ha sido trabajado integralmente en el software **R** versión 4.6.0. La versión de todas las librerias ha sido controlada utilizando `renv`

El análisis explora la relación entre la autoidentificación etnica y un Índice de Trabajo Decente creado a partir de las siguientes dimensiones:
- **Modalidad de contratación**: Se divide entre asalariados y no asalariados. Para los primeros considera si tienen algun tipo de contrato, para los segundos si su negocio o actividad esta registrada en la SUNAT
- **Ingresos**: Considera si el ingreso de la persona es mayor o menor que la remuniracion mínima vital vigente
- **Jornada laboral**: Considera si la persona trabaja mas o menos de 48 horas semanales
- **Sistema de pensiones**: Considera si la persona esta afiliada a un sistema de pensiones o no

Este índice ha sido construido adaptando lo trabajado por Julio Gamero Requena (2012).

```text
Proyecto_Etnicidad_ITD_TallerDatos_2026/
├── datos
│   └── procesados
│       ├── enaho_2025_06_07_26.parquet
│       ├── enaho_2025_12_07_26.parquet
│       ├── enaho_2025.parquet
│       ├── enaho_analitica_2025_12_07_26.parquet
│       ├── enaho_explorar_2025_12_06_26.parquet
│       └── enaho_final_2025_12_07_26.parquet
├── docs
│   ├── Diccionario_ENAHO_2025.pdf
│   └── FichaTecnica_ENAHO_2025.pdf
├── figure
│   ├── cb_enaho_codebook_edad_distribution-52-1.pdf
│   ├── cb_enaho_codebook_edad_teoria_distribution-117-1.pdf
│   ├── cb_enaho_codebook_edad_z_distribution-130-1.pdf
│   ├── cb_enaho_codebook_educacion_distribution-39-1.pdf
│   ├── cb_enaho_codebook_estrato_teo_distribution-143-1.pdf
│   ├── cb_enaho_codebook_etnicidad_distribution-26-1.pdf
│   ├── cb_enaho_codebook_horas_decente_distribution-104-1.pdf
│   ├── cb_enaho_codebook_indice_aditivo_distribution-195-1.pdf
│   ├── cb_enaho_codebook_ingreso_decente_distribution-182-1.pdf
│   ├── cb_enaho_codebook_ingreso_mensual_imp_distribution-156-1.pdf
│   ├── cb_enaho_codebook_ITD_distribution-208-1.pdf
│   ├── cb_enaho_codebook_quintil_ingreso_distribution-169-1.pdf
│   ├── cb_enaho_codebook_sexo_distribution-1.pdf
│   ├── cb_enaho_codebook_tiene_contratos_distribution-91-1.pdf
│   ├── cb_enaho_codebook_tiene_pension_distribution-65-1.pdf
│   └── cb_enaho_codebook_tiene_registro_distribution-78-1.pdf
├── outputs
│   ├── outputs_exploracion_analitica
│   │   ├── Grafico1_EdadTeoria.png
│   │   ├── Grafico10_TieneContratos.png
│   │   ├── Grafico11_HorasDecente.png
│   │   ├── Grafico12_IngresoDecente.png
│   │   ├── Grafico13_IndiceAditivo.png
│   │   ├── Grafico14_ITD.png
│   │   ├── Grafico2_EdadZ.png
│   │   ├── Grafico3_EstratoTeo.png
│   │   ├── Grafico4_EducacionAgrupada.png
│   │   ├── Grafico5_EtnicidadAgrupada.png
│   │   ├── Grafico6_IngresoMensual.png
│   │   ├── Grafico7_QuintilIngreso.png
│   │   ├── Grafico8_TienePension.png
│   │   ├── Grafico9_TieneRegistro.png
│   │   ├── Tabla1_EdadTeoria.png
│   │   ├── Tabla10_TieneContratos.png
│   │   ├── Tabla11_HorasDecente.png
│   │   ├── Tabla12_IngresoDecente.png
│   │   ├── Tabla13_IndiceAditivo.png
│   │   ├── Tabla14_ITD.png
│   │   ├── Tabla2_EdadZ.png
│   │   ├── Tabla3_EstratoTeo.png
│   │   ├── Tabla4_EducacionAgrupada.png
│   │   ├── Tabla5_EtnicidadAgrupada.png
│   │   ├── Tabla6_IngresoMensual.png
│   │   ├── Tabla7_QuintilIngreso.png
│   │   ├── Tabla8_TienePension.png
│   │   └── Tabla9_TieneRegistro.png
│   ├── outputs_exploracion_inicial
│   │   ├── Grafico1_Edad.png
│   │   ├── Grafico10_Ingreso_RUC.png
│   │   ├── Grafico11_Ingreso_Contrato.png
│   │   ├── Grafico12_Edad_Ingreso.png
│   │   ├── Grafico13_Horas_Ingreso.png
│   │   ├── Grafico2_Ingreso.png
│   │   ├── Grafico3_HorasSemana.png
│   │   ├── Grafico4_FrecuenciaPago.png
│   │   ├── Grafico8_Ingreso_Sexo.png
│   │   ├── Grafico9_Horas_Sexo.png
│   │   ├── Tabla1_Sexo.png
│   │   ├── Tabla10_RUC_Sexo.png
│   │   ├── Tabla10_Stats_HorasSemana.png
│   │   ├── Tabla11_Pension_Sexo.png
│   │   ├── Tabla11_RUC_Sexo.png
│   │   ├── Tabla12_Contrato_Sexo.png
│   │   ├── Tabla12_Pension_Sexo.png
│   │   ├── Tabla13_Contrato_Sexo.png
│   │   ├── Tabla13_RUC_Pension.png
│   │   ├── Tabla14_Contrato_RUC.png
│   │   ├── Tabla14_RUC_Pension.png
│   │   ├── Tabla15_Contrato_RUC.png
│   │   ├── Tabla15_Ingreso_Sexo.png
│   │   ├── Tabla16_Horas_Sexo.png
│   │   ├── Tabla16_Ingreso_Sexo.png
│   │   ├── Tabla17_Horas_Sexo.png
│   │   ├── Tabla17_Ingreso_RUC.png
│   │   ├── Tabla18_Ingreso_Contrato.png
│   │   ├── Tabla18_Ingreso_RUC.png
│   │   ├── Tabla19_Ingreso_Contrato.png
│   │   ├── Tabla19_Ingreso_Educacion.png
│   │   ├── Tabla2_Etnicidad.png
│   │   ├── Tabla20_Ingreso_Educacion.png
│   │   ├── Tabla20_Ingreso_Etnicidad.png
│   │   ├── Tabla21_Ingreso_Etnicidad.png
│   │   ├── Tabla3_Educacion.png
│   │   ├── Tabla4_Pension.png
│   │   ├── Tabla5_FrecuenciaPago.png
│   │   ├── Tabla6_RegistroSUNAT.png
│   │   ├── Tabla7_TipoContrato.png
│   │   ├── Tabla8_Stats_Edad.png
│   │   └── Tabla9_Stats_Ingreso.png
│   ├── CLASIFICAR_Reporte_VariablesCreadas.html
│   ├── CodeBook.html
│   ├── Grafico_NAs_Etnicidad_ITC_tratada.png
│   ├── Grafico_NAs_Etnicidad_ITC.png
│   ├── Reporte_Datos_Perdidos_ENAHO_tratada.csv
│   └── Reporte_Datos_Perdidos_ENAHO.csv
├── renv
│   ├── .gitignore
│   └── activate.R
├── scripts
│   ├── 01_Carga_Unión_Módulos.R
│   ├── 02_Acondicionamiento.R
│   ├── 03_Exploración_Inicial.R
│   ├── 04_Informe_Exploración_Inicial.Rmd
│   ├── 05_Clasificación.R
│   ├── 06_EDA_Variables_Análiticas.R
│   └── 07_Documentación.R
├── .gitignore
├── .Rprofile
├── Creación de carpetas.R
├── Proyecto_Etnicidad_ITD_TallerDatos_2026.Rproj
├── README.md
└── renv.lock
```
 
En la siguiente sección se especifican las principales acciones y decisones metodológicas tomadas para cada paso del proyecto. De tener dudas con respecto a alguna seccion, favor de consultar los scripts específicos

## EXTRAER
Se descargaron los modulos 200, 300 y 500 de la pagina de Microdatos del INEI (https://proyectos.inei.gob.pe/microdatos/) en su formato anual. Se guardaron las bases de datos (.dta) asi como el diccionario de la encuesta ya la ficha tecnica en las carpetas correspondientes.

## GESTIONAR
Se creó el R.project con el título de la investigación. Se conecto dicho proyecto con Git y Github desde Rstudio, habiendo previamente creado el repositorio desde Github. Se creó un script para la creación de carpetas, las cuales estan estructuradas como se puede apreciar en la sección anterior. Considerando el tamaño de las bases de datos crudas, se incluyo en el archivo ".gitignore" la carpeta de datos crudos para que los archivos subidos a la misma sean ignorados. No obstante, los módulos originales utilizados estan detallados en este README para que en caso se quiera reproducir esta investigación esto sea posible. Para el manejo de librerias en sus versiones utilizadas, se usó el paquete `renv`. 

## ACONDICIONAR
Se unieron los módulos para su posterior exploración mediante joins, cuyo resultado fue la primera base procesada. El proceso seguido esta detallado en el script 01 de unión de módulos.Se prosiguió con el acondicionamiento en el script 02, en el cual se seleccionaron y renombraron las variables a trabajar. El siguiente paso seguido fue una revisión general de los datos para generar un diagnóstico de valores perdidos, del cual se crearon un reporte en forma tabular y otro en forma de gráfico. Los mismos pueden encontrarse en la carpeta de Outputs. Se realizó un filtro a la base de datos para trabajar solo con la Poblacion Economicamente Activa (PEA) ocupada, ya que es la población de interes de este trabajo. Los detalles de las variables utilizadas para dicho filtro pueden ser encontradas en el script 02. Luego de dicho filtro, se realizo un nuevo diagnóstico de valores perdidos para visualizar cuantos de los casos existian por el diseño de la encuesta (existen preguntas que no se hacen a quienes no tienen empleo). El reporte tabular y gráfico de este nuevo diagnostico tambien puede ser encontrado en la carpeta de Outputs. Finalmente, se utilizaron estrategias de tratamiento de valores perdidos diferenciadas dependiendo de los casos de cada variables. Para los casos de Missing Completely at Random (MCAR) se utilizó elmininación (listwise), para aquellos identificados como Missing Not at Random (MNAR) se utilizó imputación por la mediana para las variables cuantitativas y eliminación (listwise) para las variables cualitativas. Se reconoce la introducción de sesgo, subestimación de la varianza y pérdida de poder estadistico que estas decisiones implican para los casos de MNAR. Dada la naturaleza exploratoria de este proyecto, se han asumido dichas limitaciones como aceptables. EL proceso exacto de imputación asi como las variables imputadas pueden encontrarse en el script 02. Finalmente, se exportó la segunda base de datos procesada. Importante: **La base de datos exportada incluye solo a mayores de 14 años, a la PEA ocupada, personas que respondieron las preguntas de educación, registro en SUNAT y tipo de contrato. **

## EXPLORAR
Para el diseño muestral y el manejo de los factores de expansión se construyeron tres diseños muestrales independientes (enaho_diseno_200, enaho_diseno_300, enaho_diseno_500), correspondientes a los tres módulos de la ENAHO utilizados en la investigación (200: hogar/vivienda; 300: educación; 500: empleo e ingresos). Cada uno fue especificado mediante as_survey_design() con ids = conglome, strata = estrato y nest = TRUE, y ponderado con su respectivo factor de expansión (factor200, factor300, factor500). En el análisis univariado, cada variable fue explorada utilizando el diseño muestral correspondiente a su módulo de origen. Para el análisis bivariado, en cambio, se utilizó de manera estandarizada el factor de expansión del módulo 500 (factor500, empleo e ingresos) en todos los cruces. Esta decisión fue confirmada como criterio del curso, bajo la premisa de que mezclar factores de expansión distintos dentro de un mismo cruce generaría estimaciones incoherentes entre sí, dado que cada factor corresponde a una población de referencia distinta. Se optó por trabajar con el factor de expansión del módulo 500 siguiendo un criterio de mayor restricción numérica frente a los otros dos factores, y por tratarse además del núcleo temático de la investigación: empleo e ingresos. Sobre esta misma línea de trabajo con los factores de expansión, en las tablas descriptivas se diferenció explícitamente entre el N muestral (número de encuestados reales que respondieron cada pregunta, obtenido mediante unweighted(n())) y la población estimada (millones de personas representadas mediante la expansión muestral), reportando ambos valores de forma conjunta junto a los estadísticos ponderados (survey_mean(), survey_median(), survey_sd(), survey_quantile()). En cuanto a la construcción y presentación de tablas y gráficos, se estableció un criterio único para decidir el formato de cada variable (tabla, gráfico, o ambos), aplicado de manera consistente en el análisis univariado y bivariado. Las variables continuas se presentan mediante una tabla de estadísticos descriptivos acompañada de un histograma (en el caso univariado) o de un boxplot/gráfico de dispersión (en el caso bivariado). Las variables categóricas dicotómicas o con pocas categorías (3 a 4 niveles) se presentan con tabla y gráfico de manera conjunta, dado que ambos formatos aportan información complementaria: la tabla ofrece precisión numérica, mientras que el gráfico permite una lectura visual inmediata. Las variables categóricas con numerosas categorías (8 o más niveles), en cambio, se presentan únicamente mediante tabla, debido a que un gráfico con ese número de categorías pierde legibilidad. Los cruces entre dos variables continuas se presentan únicamente mediante un gráfico de dispersión, al no ser aplicable una tabla de proporciones en ese caso. Siguiendo este mismo criterio, se decidió no generar ni exportar gráficos para variables cuya distribución ya queda completamente explicada por su tabla correspondiente, considerando que dicha limitación no depende del contexto de uso sino de las características propias de la variable (número de categorías, patrón de distribución). En los casos en que el gráfico sí puede aportar información adicional sobre datos atípicos, la forma de la distribución, picos u otros elementos clave para la interpretación, se optó por incorporarlo. Respecto a la reetiquetación de variables, en el caso de la Afiliación a sistema de pensiones (pension_no) se identificó que el código 0 ("Pase") no corresponde a un valor perdido, sino a una sub-pregunta de selección única dentro de un bloque de opciones de afiliación (P558A1 a P558A5): un valor "Pase" indica que la persona marcó otra opción del bloque, es decir, que sí cuenta con afiliación a algún sistema de pensiones. En consecuencia, se recodificó la variable como 0 = "Afiliado" y 5 = "No está afiliado", en lugar de tratar el código 0 como ausencia de respuesta. De manera similar, en el caso de la variable Educación se mantuvieron las 12 categorías originales del cuestionario para la tabla univariada de distribución educativa. Sin embargo, para la tabla de estadísticos de ingreso según nivel educativo (cruce categórica-continua: Tabla 20), dichas categorías se agruparon en 6 bloques jerárquicos (Sin nivel/Inicial/Básica especial, Primaria, Secundaria, Superior no universitaria, Superior universitaria, Posgrado), con el fin de mantener la legibilidad de la tabla al incorporar múltiples estadísticos (N, media, desviación estándar, mediana) por categoría. Finalmente, para evitar repetir el mismo bloque de formato en cada tabla individual, se definió una función personalizada (formato_flextable()) que aplica un estilo visual homogéneo a la totalidad de las tablas generadas durante la fase de exploración. Construida sobre el paquete flextable, esta función estandariza elementos como el título y la fuente de la tabla, el ajuste automático de columnas, la eliminación de líneas internas del cuerpo, la alineación de texto (centrada para el cuerpo general, izquierda para la primera columna y el pie de página) y el resaltado en negrita de los encabezados. Esta función se invoca en cada tabla del análisis univariado y bivariado, garantizando consistencia visual en los productos exportados a la carpeta de resultados.

## CLASIFICAR
En el script 05, se crean las siguientes variables analíticas: edad_teoria, edad_z, estrato_teo, ingreso_mensual_imp, quintil_ingreso, tiene_pension, tiene_registro, tiene_contrato, horas_decente, ingreso_decente, indice_aditivo, ITD. Para observar el proceso detallado de creación de cada una de las variables, por favor referirse al script 05. Para una definición más formal de cada una de ellas, por favor, referirse al CodeBook presentado en la carpeta "outputs". Como resultado del script 05, se exportó en html un reporte de las variables creadas, así como una nueva base de datos procesada que incluye las nuevas variables. De manera adicional, se utilizó las variables analíticas creadas para hacer un nuevo EDA, que se puede encontrar en el script 06, donde se crean gráficos y tablas exportadas a la carpeta de outputs analíticos .

## DOCUMENTAR
En el script 07, se limpió la base de datos para quedarnos solo con las variables que utilizamos en el EDA de variables analíticas. Se crearon e incluyeron etiquetas descriptivas para cada variable según su fuente (de acuerdo a lo revisado en el diccionario de la ENAHO). Se crearon notas analíticas para detallar la lógica seguida para la creación de cada variable analíticas. A partir de estos datos se creo un libro de códigos para la interpretación del proyecto. El mismo fue creado con el paquete ´codebook´ (CodeBook_codebook). El mismo puede ser encontrado en la carpeta "outputs". En el libro de códigos puede verse una descripción corta de cada variable, las etiquetas de respuesta, la distribución de la misma, las estrategias de imputación y la variable fuente de la ENAHO. La documentación utilizada puede ser encontrada en la carpeta Docs, así como la ficha técnica de la ENAHO 2025. 