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
datos <- read_xlsx("C:/Users/oscar/Desktop/Backup/Asuntos personales/UCR/2026/2026_I Cuatrimestre/Métodos Cuantitativos en Banca/Reporte/Reporte 2/Base de datos_SUGEF 1-05.xlsx")
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


# Reporte 2
rm(list = ls())

# Librerías y paquetes
# install.packages("readxl")
# install.packages("skimr")
# install.packages("lmtest")
# install.packages("car")
# install.packages("vars")
# install.packages("tseries")
library(readxl)
library(skimr)
library(lmtest)
library(car)
library(vars)
library(tseries)


# Cargar los datos de nuevo
datos <- read_xlsx("C:/Users/oscar/Desktop/Backup/Asuntos personales/UCR/2026/2026_I Cuatrimestre/Métodos Cuantitativos en Banca/Reporte/Reporte 2/Base de datos_SUGEF 1-05.xlsx")
skim(datos)

# 1 - Evaluación de supuestos del modelo de regresión lineal
mod1_R2 <- lm(PD90 ~ TPM + FX + INF_YOY, data = datos)

# Pruebas de supuestos del modelo de regresión del Reporte 1
dwtest(mod1_R2)   # autocorrelación
bptest(mod1_R2)   # heterocedasticidad
vif(mod1_R2)      # multicolinealidad

# Gráfico de autocorrelación de residuos
acf(resid(mod1_R2), main = "ACF de residuos del modelo lineal inicial", xlab = "Rezagos", ylab = "Autocorrelación",
col = "black", lwd = 2)

# 2 - Evaluación de estacionariedad
# Base de datos para series de tiempo, eliminando observaciones con NA
datos_st <- na.omit(datos[, c("PD90", "TPM", "FX", "INF_YOY")])

# Pruebas ADF
adf.test(datos_st$PD90)
adf.test(datos_st$TPM)
adf.test(datos_st$FX)
adf.test(datos_st$INF_YOY)

# 3 - Transformación de las variables no estacionarias
# Diferencias
dPD90 <- diff(datos_st$PD90)
dTPM  <- diff(datos_st$TPM)

# Base final para VAR
datos_var <- na.omit(cbind(dPD90, dTPM, FX = datos_st$FX[-1], INF_YOY = datos_st$INF_YOY[-1]))

# 4 - VAR
modelo_var <- VAR(datos_var, p = 3, type = "const")
summary(modelo_var)






















































