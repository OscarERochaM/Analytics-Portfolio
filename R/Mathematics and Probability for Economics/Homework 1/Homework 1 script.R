rm(list = ls())

# Paquetes
install.packages("lpSolve")
install.packages("lpSolveAPI")
install.packages("MMAC")
install.packages("mosaic")
install.packages("mosaicCalc")
install.packages("manipulate")
install.packages("Ryacas")
# Librerías
library(lpSolve)
library(lpSolveAPI)
library(MMAC)
library(mosaic)
library(mosaicCalc)
library(manipulate)
library(Ryacas)

# Pregunta 1: Límite
x <- ysym("x")
limite <- lim((2*x^2 - 3*x + 1)/(x - 1), x, 1)
cat("Respuesta Pregunta 1 - Límite:\n")
print(limite)

# Pregunta 2: Derivada y evaluación en x=5
f <- makeFun(sqrt(x + 7) ~ x)
df <- D(f(x) ~ x)
valor_derivada <- makeFun(df)(5)
cat("\nRespuesta Pregunta 2 - Derivada evaluada en x = 5:\n")
print(valor_derivada)

# Pregunta 3: Optimización restringida
# Número de variables
model <- make.lp(0, 2)
# Función objetivo (maximizar u = 3x1 + 2x2)
# lpSolve maximiza minimizando la función objetivo, por eso los coeficientes negativos
set.objfn(model, c(-3, -2))
# Restricciones: 5x1 + 10x2 = 1000
add.constraint(model, c(5, 10), "=", 1000)
# Establecer límites de las variables (no negativas, dado a la max)
set.bounds(model, lower = c(0, 0))
# Resolver el modelo
solve(model)
# Obtener valores óptimos
valores_optimos <- get.variables(model)
# Obtener valor óptimo de la función objetivo
valor_objetivo <- get.objective(model)
# Obtener valor de las restricciones
valores_restricciones <- get.constraints(model)

# Mostrar resultados
cat("Valores óptimos:\n")
cat("x1 =", valores_optimos[1], "\n")
cat("x2 =", valores_optimos[2], "\n")
cat("Valor máximo de u =", -valor_objetivo, "\n")  # signo invertido, dado a dar como resultado una maximización
cat("Valores de las restricciones:\n")
print(valores_restricciones)

# Gráficas
# Restricciones
plotFun(100 - 0.5*x ~ x, xlim=range(0,250), ylim=range(0,120), 
        xlab=expression(x[1]), ylab=expression(x[2]), main="Restricción y utilidad", asp=1)
# Utilidad
plotFun((600 - 3*x)/2 ~ x, xlim=range(0,250), add=TRUE, lty=2)