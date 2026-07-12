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

Estructura (TBD)
 
En la siguiente sección se especifican las principales acciones y decisones metodológicas tomadas para cada paso del proyecto. De tener dudas con respecto a alguna seccion, favor de consultar los scripts específicos

## EXTRAER
Se descargaron los modulos 200, 300 y 500 de la pagina de Microdatos del INEI (https://proyectos.inei.gob.pe/microdatos/) en su formato anual. Se guardaron las bases de datos (.dta) asi como el diccionario de la encuesta ya la ficha tecnica en las carpetas correspondientes.

## GESTIONAR
Se creó el R.project con el título de la investigación. Se conecto dicho proyecto con Git y Github desde Rstudio, habiendo previamente creado el repositorio desde Github. Se creó un script para la creación de carpetas, las cuales estan estructuradas como se puede apreciar en la sección anterior. Considerando el tamaño de las bases de datos crudas, se incluyo en el archivo ".gitignore" la carpeta de datos crudos para que los archivos subidos a la misma sean ignorados. No obstante, los módulos originales utilizados estan detallados en este README para que en caso se quiera reproducir esta investigación esto sea posible. Para el manejo de librerias en sus versiones utilizadas, se usó el paquete `renv`. 

## ACONDICIONAR
Se unieron los módulos para su posterior exploración mediante joins, cuyo resultado fue la primera base procesada. El proceso seguido esta detallado en el script 01 de unión de módulos.Se prosiguió con el acondicionamiento en el script 02, en el cual se seleccionaron y renombraron las variables a trabajar. El siguiente paso seguido fue una revisión general de los datos para generar un diagnóstico de valores perdidos, del cual se crearon un reporte en forma tabular y otro en forma de gráfico. Los mismos pueden encontrarse en la carpeta de Outputs. Se realizó un filtro a la base de datos para trabajar solo con la Poblacion Economicamente Activa (PEA) ocupada, ya que es la población de interes de este trabajo. Los detalles de las variables utilizadas para dicho filtro pueden ser encontradas en el script 02. Luego de dicho filtro, se realizo un nuevo diagnóstico de valores perdidos para visualizar cuantos de los casos existian por el diseño de la encuesta (existen preguntas que no se hacen a quienes no tienen empleo). El reporte tabular y gráfico de este nuevo diagnostico tambien puede ser encontrado en la carpeta de Outputs. Finalmente, se utilizaron estrategias de tratamiento de valores perdidos diferenciadas dependiendo de los casos de cada variables. Para los casos de Missing Completely at Random (MCAR) se utilizó elmininación (listwise), para aquellos identificados como Missing Not at Random (MNAR) se utilizó imputación por la mediana para las variables cuantitativas y eliminación (listwise) para las variables cualitativas. Se reconoce la introducción de sesgo, subestimación de la varianza y pérdida de poder estadistico que estas decisiones implican para los casos de MNAR. Dada la naturaleza exploratoria de este proyecto, se han asumido dichas limitaciones como aceptables. EL proceso exacto de imputación asi como las variables imputadas pueden encontrarse en el script 02. Finalmente, se exportó la segunda base de datos procesada. Importante: **La base de datos exportada incluye solo a mayores de 14 años, a la PEA ocupada, personas que respondieron las preguntas de educación, registro en SUNAT y tipo de contrato. **
