# Credit Risk Analytics – Costa Rica

Este proyecto analiza el comportamiento de la morosidad PD90+ en el Sistema Financiero Nacional de Costa Rica y su relación con variables macroeconómicas seleccionadas, incluyendo inflación, tasa de política monetaria y tipo de cambio.

El análisis fue desarrollado en R utilizando datos mensuales de SUGEF y BCCR para el período 2007–2023.

## Objetivo del Proyecto

El objetivo de este proyecto es evaluar si las variables macroeconómicas ayudan a explicar el comportamiento de la morosidad severa en el sistema financiero costarricense.

La primera etapa aplica regresión lineal múltiple para identificar relaciones iniciales entre PD90+ y las variables macroeconómicas. La segunda etapa evalúa las limitaciones del modelo de regresión y aplica un enfoque de series de tiempo mediante pruebas de estacionariedad y un modelo VAR.

## Herramientas y Métodos

- R
- Regresión Lineal Múltiple
- Diagnósticos de Regresión
- Prueba Durbin-Watson
- Prueba Breusch-Pagan
- Análisis VIF
- Prueba Dickey-Fuller Aumentada
- Modelo VAR
- Análisis de Series de Tiempo

## Fuentes de Datos

El proyecto utiliza datos públicos de:

- Superintendencia General de Entidades Financieras (SUGEF)
- Banco Central de Costa Rica (BCCR)

Las variables analizadas son:

- Morosidad PD90+
- Tasa de Política Monetaria
- Inflación interanual
- Tipo de cambio

## Resumen del Análisis

El modelo inicial de regresión mostró que la Tasa de Política Monetaria y la inflación se asociaban estadísticamente con la morosidad PD90+, mientras que el tipo de cambio no mostró una relación lineal significativa bajo la especificación inicial.

Sin embargo, las pruebas de diagnóstico evidenciaron problemas como autocorrelación y heterocedasticidad, lo que sugirió que un modelo de regresión simple no era suficiente para representar adecuadamente la dinámica temporal de los datos.

Para abordar esto, el proyecto incorporó un enfoque de series de tiempo. Se aplicaron pruebas de estacionariedad y se estimó un modelo VAR utilizando transformaciones en las variables cuando fue necesario.

Los resultados del modelo VAR sugieren que los cambios de corto plazo en la morosidad PD90+ están asociados principalmente con su propio comportamiento pasado, mientras que las variables macroeconómicas muestran diferentes interacciones dentro del sistema.

## Principales Conclusiones

- La morosidad PD90+ es un indicador relevante de riesgo crediticio para el sistema financiero.
- Las variables macroeconómicas aportan contexto útil, pero no explican por completo el comportamiento de la morosidad por sí solas.
- Los diagnósticos de series de tiempo son importantes al trabajar con datos financieros y macroeconómicos mensuales.
- El modelo VAR ofrece un mejor marco para explorar relaciones dinámicas entre riesgo crediticio y variables macroeconómicas.

## Licencia

Este trabajo está licenciado bajo una 
[Licencia de Creative Commons Atribución-NoComercial 4.0 Internacional (CC BY-NC 4.0)](https://creativecommons.org/licenses/by-nc/4.0/).

![Licencia CC BY-NC 4.0](https://i.creativecommons.org/l/by-nc/4.0/88x31.png)
