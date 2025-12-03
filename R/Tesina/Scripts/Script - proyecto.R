# Librerías
library(ggplot2)
library(readr)  # para mejor manejo de codificación

# Función para cargar CSV con prueba de codificaciones
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

# Ruta base relativa a la carpeta raíz del proyecto (Tesina)
ruta_datos <- "Datos/"

# Carga de datos con rutas relativas
cat("Cargando datos...\n")

ERM <- cargar_csv(paste0(ruta_datos, "ERM.csv"))
DP <- cargar_csv(paste0(ruta_datos, "Datos predichos.csv"))
ERS <- cargar_csv(paste0(ruta_datos, "ERS.csv"))
ERSSolowKeynes <- cargar_csv(paste0(ruta_datos, "ERSSolow - Keynes.csv"))
FL <- cargar_csv(paste0(ruta_datos, "FL.csv"))

cat("Datos cargados correctamente.\n\n")

# Modelos de regresión
cat("Resumen modelo ERM1:\n")
ERM1 <- lm(PIB ~ FBKF + GID + FL + Desempelo, data = ERM)
print(summary(ERM1))
cat("\n")

cat("Resumen modelo FBKFlm:\n")
FBKFlm <- lm(PIB ~ FBKF, data = ERS)
print(summary(FBKFlm))
cat("\n")

cat("Resumen modelo FLlm:\n")
FLlm <- lm(PIB ~ FL, data = ERS)
print(summary(FLlm))
cat("\n")

cat("Resumen modelo ERSSolowlm:\n")
ERSSolowlm <- lm(IDH ~ PIBSolow, data = ERSSolowKeynes)
print(summary(ERSSolowlm))
cat("\n")

cat("Resumen modelo ERSKeyneslm:\n")
ERSKeyneslm <- lm(IDH ~ PIBKeynesiano, data = ERSSolowKeynes)
print(summary(ERSKeyneslm))
cat("\n")

# Gráficos básicos
par(mfrow = c(1,1))

plot(DP$Año, DP$FBKF, type = "o", col = "darkgreen", main = "FBKF en millones de colones 2020 - 2029",
     xlab = "Año", ylab = "FBKF")
axis(side = 1, at = 2020:2029, labels = DP$Año)

par(new = TRUE)
plot(DP$GID, type = "o", axes = FALSE, col = "blue", xlab = "", ylab = "", main = "")
axis(side = 4)
mtext("GID", side = 4, line = 3)

plot(DP$Año, DP$Desempleo, type = "o", col = "brown", main = "Desempleo 2020 - 2029",
     xlab = "Año", ylab = "Tasa de desempleo", las = 1)
axis(side = 1, at = 2020:2029, labels = DP$Año)

plot(DP$Año, DP$FL, type = "o", col = "red", main = "Fuerza Laboral 2020 - 2029",
     xlab = "Año", ylab = "Cantidad de personas")
axis(side = 1, at = 2020:2029, labels = DP$Año)

plot(DP$Año, DP$IDH, type = "o", col = "darkred", main = "Índice de Desarrollo Humano 2020 - 2029",
     xlab = "Año", ylab = "IDH", las = 1)
axis(side = 1, at = 2020:2029, labels = DP$Año)

# Gráfico con ggplot2: PIB vs Año, con colores por FBKF y tamaño por GID
ERM_clean <- na.omit(ERM)
ggplot() +
  geom_point(data = ERM_clean, aes(x = Año, y = PIB, color = FBKF, size = GID)) +
  geom_smooth(data = ERM_clean, aes(x = Año, y = PIB), method = "lm", se = FALSE, color = "black", linewidth = 1) +
  labs(title = "PIB según FBKF y GID", x = "Año", y = "PIB") +
  theme_minimal()
