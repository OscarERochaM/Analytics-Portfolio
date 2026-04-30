# Reporte analista 1 
# Relación entre PD90+ del SFN y variables macroeconómicas (Inflación, TPM y Tipo de Cambio) — Costa Rica, 2024–2026)

# Paquetes
# install.packages("DataExplorer")
# install.packages("readr")
# install.packages("dplyr")
# install.packages("skimr")
# install.packages("knitr")
# install.packages("broom")
# install.packages("ggplot2")


# Librerías
library(DataExplorer)
library(readr)
library(dplyr)
library(skimr)
library(knitr)
library(broom)
library(ggplot2)

rm(list=ls())

# 1 - Cargar datos y revisión
datos <- read_csv("C:/Users/oscar/Desktop/Backup/Asuntos personales/UCR/2026/2026_I Cuatrimestre/Métodos Cuantitativos en Banca/Reporte/Base de datos_reporte.csv")
View(datos)
skim(datos)

# 2 - Estructura con Data.Explorer y ver relaciones
DataExplorer::plot_str(datos)
DataExplorer::plot_intro(datos)
DataExplorer::plot_missing(datos)
# Excluyendo fecha, dado que es un char
DataExplorer::plot_scatterplot(datos[, c("PD90","TPM","FX","INF_YOY")], by = "PD90")
DataExplorer::plot_histogram(datos[, c("PD90","TPM","FX","INF_YOY")])
DataExplorer::plot_correlation(datos[, c("PD90", "TPM","FX","INF_YOY")])

# Usuando gglot para observar la dispersión un poco más clara
ggplot(datos, aes(INF_YOY, PD90)) + geom_point() + geom_smooth(method="lm", se=FALSE)

# Verifiación de los datos para ver correlación, darse una idea.
cor(datos[, c("PD90","TPM","FX","INF_YOY")], use="complete.obs")

# 3 - Modelo
mod1 <- lm(PD90 ~ TPM + FX + INF_YOY, data=datos)
summary(mod1)

# Variable significativa la inflación p = 0.0417 * 5%
# Dado a eso, se tira el modelo solo explicando PD por la inflación
mod_inf <- lm(PD90 ~ INF_YOY, data = datos)
summary(mod_inf)
#  Solo siendo explicado por inflación, da 10% de significancia

# 4 - Plot para ver tanto mod1 como modelo solo con inflación
par(mfrow=c(2,2))
plot(mod1)
par(mfrow=c(1,1))

par(mfrow=c(2,2))
plot(mod_inf)
par(mfrow=c(1,1))



         


































































