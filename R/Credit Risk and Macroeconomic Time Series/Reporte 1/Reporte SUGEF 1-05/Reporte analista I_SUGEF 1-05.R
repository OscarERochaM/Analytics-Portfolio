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
library(readxl)
library(dplyr)
library(skimr)
library(knitr)
library(broom)

rm(list=ls())

# 1 - Cargar datos y revisión
datos <- read_xlsx("C:/Users/oscar/Desktop/Backup/Asuntos personales/UCR/2026/2026_I Cuatrimestre/Métodos Cuantitativos en Banca/Reporte/Reporte SUGEF 1-05/Base de datos_SUGEF 1-05.xlsx")
skim(datos)

# 2 - Estructura con Data.Explorer y ver relaciones)
DataExplorer::plot_missing(datos)
# Excluyendo fecha
DataExplorer::plot_scatterplot(datos[, c("PD90","TPM","FX","INF_YOY")], by = "PD90")
DataExplorer::plot_histogram(datos[, c("PD90","TPM","FX","INF_YOY")])
DataExplorer::plot_correlation(datos[, c("PD90", "TPM","FX","INF_YOY")])

# Verifiación de los datos para ver correlación, darse una idea no gráfica
cor(datos[, c("PD90","TPM","FX","INF_YOY")], use="complete.obs")

# 3 - Modelo
mod1 <- lm(PD90 ~ TPM + FX + INF_YOY, data=datos)
summary(mod1)

# Variable muy significativa, la inflación, segunda, la TPM
# Dado a eso, se tira el modelo solo explicando PD por la inflación y TPM
mod_inf_TPM <- lm(PD90 ~ INF_YOY + TPM, data = datos)
summary(mod_inf_TPM)
#  Explicado por ambas anteriores, se observa una misma significancia

# 4 - Plot para ver mod1
par(mfrow=c(2,2))
plot(mod1)
par(mfrow=c(1,1))

# En el mod 1, la concentración de los datos sigue siendo dispersa, sin embargo, los residuos estandarizados se miran
# en una buena relación, lo que puede determinar el mejor cumplimiento de los supuestos, y en el summary se observa
# por el % de significancia, que es alta.

# Plot para ver el model de inflacion y TPM
par(mfrow=c(2,2))
plot(mod_inf_TPM)
par(mfrow=c(1,1))



         


































































