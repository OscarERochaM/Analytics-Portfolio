# Valores dados
capital_trabajo <- 200000
activos_totales <- 1000000
utilidades_retenidas <- 150000
ebit <- 100000
valor_mercado_patrimonio <- 500000
valor_contable_deuda <- 300000
ventas <- 1200000

cat("Datos de entrada:\n")
cat("Capital de trabajo:", capital_trabajo, "\n")
cat("Activos totales:", activos_totales, "\n")
cat("Utilidades retenidas:", utilidades_retenidas, "\n")
cat("EBIT:", ebit, "\n")
cat("Valor mercado patrimonio:", valor_mercado_patrimonio, "\n")
cat("Valor contable deuda:", valor_contable_deuda, "\n")
cat("Ventas:", ventas, "\n\n")

# Calcular variables X1 a X5
X1 <- capital_trabajo / activos_totales
X2 <- utilidades_retenidas / activos_totales
X3 <- ebit / activos_totales
X4 <- valor_mercado_patrimonio / valor_contable_deuda
X5 <- ventas / activos_totales

cat("Variables calculadas:\n")
print(c(X1 = X1, X2 = X2, X3 = X3, X4 = X4, X5 = X5))
cat("\n")

# Vector de coeficientes beta
beta <- c(1.2, 1.4, 3.3, 0.6, 1.0)
cat("Coeficientes beta:\n")
print(beta)
cat("\n")

# Supongamos error = 0 para calcular el Z-score esperado
mu <- 0

# Calcular Z-score
Z <- sum(beta * c(X1, X2, X3, X4, X5)) + mu

cat("El Z-Score calculado es:", round(Z, 3), "\n")

# Interpretación
cat("Interpretación del Z-Score:\n")
if(Z > 2.99) {
  cat("Baja probabilidad de quiebra.\n")
} else if(Z > 1.81 & Z <= 2.99) {
  cat("Zona gris (riesgo medio).\n")
} else {
  cat("Alto riesgo de quiebra.\n")
}

