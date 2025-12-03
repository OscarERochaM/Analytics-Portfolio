rm(list = ls()) # Limpiar entorno
# Paquetes
install.packages("tidyverse")
install.packages("knitr")
install.packages("broom")
install.packages("pracma")
install.packages("matlib")
# Librerías
library(tidyverse)
library(knitr)
library(broom)
library(pracma)
library(matlib)

# Definir la matriz de coeficientes A y el vector de términos independientes b
A <- matrix(c(8, -6, 2, 10, 5, 15, -3, 6, -15), nrow = 3, byrow = TRUE)
b <- c(-20, 0, 51)

# Mostrar la matriz aumentada Ab
Ab <- cbind(A, b)
cat("Matriz aumentada Ab:\n")
print(Ab)

# Mostrar las ecuaciones de forma simbólica
cat("\nSistema de ecuaciones:\n")
showEqn(A, b)

# Graficar el sistema de ecuaciones
cat("\nGráfico del sistema de ecuaciones:\n")
plotEqn3d(A, b)

# Resolver el sistema usando eliminación gaussiana
gaussiana <- rref(Ab)
cat("\nResultado por eliminación gaussiana:\n")
print(gaussiana)

# Resolver usando la función Solve
solución <- Solve(A, b, fractions = TRUE)
cat("\nSolución del sistema usando Solve:\n")
print(solución)

# Verificación
verificación <- A %*% c(1,4,-2)
cat("\nVerificación: A %*% solución\n")
print(verificación)

