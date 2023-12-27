# Ejercicio 1: Regresion Lineal Simple
# Ingreso de datos
# X: Numero de semanas 
# Y: Ganancia e velocidad (ppm)
x <- c(3, 5, 2, 8, 6, 9, 3, 4)
y <- c(87, 119, 47, 195, 162, 234, 72, 110)

# a) Diagrama de dispersion
plot(x, y, xlab = "N° de semanas", ylab = "Ganancia (ppm)", main = "Diagrama de dispersión")

# b) Recta de regresion
modelo <- lm(y ~ x)
summary(modelo)

# Correlacion
tabla <- data.frame(x, y)
cor(tabla)

# Grafica de linea de ajuste
plot(x, y, xlab = "N° de semanas", ylab = "Ganancia (ppm)", main = "Diagrama de dispersión")
abline(modelo, col = "red")

# c) Estimación: x = 7
predict(modelo, data.frame(x = 7))


