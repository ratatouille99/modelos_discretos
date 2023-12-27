# REGRESION LOGISTICA CON VARIABLE INDEPENDEINTE DICOTOMICA
#EJEMPLO 2 - Grupo etario y Enfermefermedad Cardiaca Coronaria

# Importar datos
#

attach(datos)

# Tabla de contingencia
# Convertimos las variables a factor
datos$GEDAD <- factor(datos$GEDAD, labels = c("< 55", ">= 55"))
datos$ECC <- factor(datos$ECC, labels = c("Ausente", "Presente"))
# head(datos)
tabla2x2 <- table(ECC, GEDAD)
tabla2x2

mosaicplot(tabla2x2, main = "GEDAD vs ECC", color = 2:4, ylab = "Enfermedad Cardiaca Coronaria (ECC)", xlab = "Grupo Etario (GEDAD)")


# Estimación Modelo logistico
modelo <- glm(ECC ~ GEDAD, data = datos, family = "binomial")
summary(modelo)

# Odds-Ratio para el modelo
exp(cbind(OR = coef(modelo), confint(modelo)))
