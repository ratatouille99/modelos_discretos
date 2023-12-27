# REGRESION LOGISTICA CON VARIABLE INDEPENDEINTE POLICOTOMICA
# EJEMPLO 3 - Admision

# Importar datos
#

attach(datos)

# Tabala de contingencia
datos$admit <- factor(datos$admit, labels = c("No admitido", "Admitido"))
datos$proced <- factor(datos$proced, labels = c("Otro", "Religioso", "Privado", "Estatal"))

tabla <- table(admit, proced)
tabla

mosaicplot(tabla, main = "Procedencia vs Admisión", color = 2:7, ylab = "Admision", xlab = "Procendencia")

# Estimación del modelo
modelo <- glm(admit ~ proced, data = datos, family = "binomial")
summary(modelo)

# Solo coeficientes
coefficients(modelo)

# Odds-Ratio para el modelo
exp(cbind(OR = coef(modelo), confint(modelo)))

