# librerías
library(ggplot2)
library(readr)  # para mejor manejo de codificación

# Cargar CSV con prueba de codificaciones
cargar_csv <- function(archivo) {
  tryCatch({
    # Intentar cargar con UTF-8
    df <- read_csv(archivo, locale = locale(encoding = "UTF-8"))
    return(df)
  }, error = function(e1) {
    tryCatch({
      # Intentar con Latin1 si falla UTF-8
      df <- read_csv(archivo, locale = locale(encoding = "Latin1"))
      return(df)
    }, error = function(e2) {
      stop(paste("No se pudo cargar el archivo:", archivo))
    })
  })
}

# Carga de datos
ERM <- cargar_csv("ERM.csv")
DP <- cargar_csv("Datos predichos.csv")
ERS <- cargar_csv("ERS.csv")
ERSSolowKeynes <- cargar_csv("ERSSolow - Keynes.csv")
FL <- cargar_csv("FL.csv")

# Modelos de regresión
cat("Resumen modelo ERM1 (PIB ~ FBKF + GID + FL + Desempelo):\n")
ERM1 <- lm(PIB ~ FBKF + GID + FL + Desempelo, data = ERM)
print(summary(ERM1))

cat("\nResumen modelo FBKFlm (PIB ~ FBKF):\n")
FBKFlm <- lm(PIB ~ FBKF, data = ERS)
print(summary(FBKFlm))

cat("\nResumen modelo FLlm (PIB ~ FL):\n")
FLlm <- lm(PIB ~ FL, data = ERS)
print(summary(FLlm))

cat("\nResumen modelo ERSSolowlm (IDH ~ PIBSolow):\n")
ERSSolowlm <- lm(IDH ~ PIBSolow, data = ERSSolowKeynes)
print(summary(ERSSolowlm))

cat("\nResumen modelo ERSKeyneslm (IDH ~ PIBKeynesiano):\n")
ERSKeyneslm <- lm(IDH ~ PIBKeynesiano, data = ERSSolowKeynes)
print(summary(ERSKeyneslm))

# Gráficos básicos
cat("\nGráfico FBKF 2020-2029:\n")
par(mfrow = c(1,1))
plot(DP$Año, DP$FBKF, type = "o", col = "darkgreen", main = "FBKF en millones de colones 2020 - 2029",
     xlab = "Año", ylab = "FBKF")
axis(side = 1, at = 2020:2029, labels = DP$Año)

cat("\nGráfico GID:\n")
par(new = TRUE)
plot(DP$GID, type = "o", axes = FALSE, col = "blue", xlab = "", ylab = "", main = "")
axis(side = 4)
mtext("GID", side = 4, line = 3)

cat("\nGráfico Desempleo 2020-2029:\n")
plot(DP$Año, DP$Desempleo, type = "o", col = "brown", main = "Desempleo 2020 - 2029",
     xlab = "Año", ylab = "Tasa de desempleo", las = 1)
axis(side = 1, at = 2020:2029, labels = DP$Año)

cat("\nGráfico Fuerza Laboral 2020-2029:\n")
plot(DP$Año, DP$FL, type = "o", col = "red", main = "Fuerza Laboral 2020 - 2029",
     xlab = "Año", ylab = "Cantidad de personas")
axis(side = 1, at = 2020:2029, labels = DP$Año)

cat("\nGráfico Índice de Desarrollo Humano 2020-2029:\n")
plot(DP$Año, DP$IDH, type = "o", col = "darkred", main = "Índice de Desarrollo Humano 2020 - 2029",
     xlab = "Año", ylab = "IDH", las = 1)
axis(side = 1, at = 2020:2029, labels = DP$Año)

# Gráfico con ggplot2: PIB vs Año, con colores por FBKF y tamaño por GID
cat("\nGráfico ggplot2: PIB según FBKF y GID:\n")
ERM_clean <- na.omit(ERM)
g <- ggplot() +
  geom_point(data = ERM_clean, aes(x = Año, y = PIB, color = FBKF, size = GID)) +
  geom_smooth(data = ERM_clean, aes(x = Año, y = PIB), method = "lm", se = FALSE, color = "black", linewidth = 1) +
  labs(title = "PIB según FBKF y GID", x = "Año", y = "PIB") +
  theme_minimal()

print(g)
