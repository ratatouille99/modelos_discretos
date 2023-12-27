# REGRESION LOGISTICA SIMPLE
# EJEMPLO 1 - Enfermefermedad Cardiaca Coronaria y Edad 

# Librerias

# Importar datos
library(readxl)
datos <- read_excel("C:.../Ejmplo1_AGE.xlsx")
View(datos)
attach(datos)

# Diagrama de dispersión
plot(EDAD, ECC, col = "blue", main = "Diagrama de Dispersión")

# Diagrama de cajas - Exploratorio
boxplot(EDAD ~ ECC)

###################################
# Estimación Modelo logistico
###################################

# Convertir la VD a factor
datos$ECC <- factor(datos$ECC, labels = c("Ausente", "Presente"))

modelo <- glm(ECC ~ EDAD, data = datos, family = "binomial")
summary(modelo)

# Odds-Ratio para el modelo
exp(cbind(OR = coef(modelo), confint(modelo)))

# Predicciones: Odds
predict(modelo, data.frame(EDAD = 30))

# Predicciones: Probabilidad
predict(modelo, data.frame(EDAD = 30), type = "response")

# Representación gráfica del modelo
# Graficar con datos originales
# library(ggplot2)

ggplot(data = datos, aes(x = EDAD, y = ECC)) +
  geom_point(aes(color = as.factor(ECC)), shape = 1) +
  stat_function(fun = function(x){predict(modelo, newdata = data.frame(EDAD = x), type = "response")}) +
  theme_bw() + labs(title = "Regresión logística", y = "Probabilidad ECC") +
  theme(legend.position = "none")

###############################
# VALIDACIÓN DE COEFICIENTES
###############################

# Prueba para B (Wald)
p_value_Chi <- 1 - pchisq(21.25340, df = 1)
p_value_Chi

# Devianza
anova(modelo, test = "Chisq")

# Intervalo de Confianza para B
confint(modelo, level = 0.95)
exp(confint(modelo, level = 0.95))

#####################################
# VALIDACIÓN DEL MODELO
#####################################

# Pseudo R2
# install.packages("DescTools")
# library(DescTools)
PseudoR2(modelo, c("CoxSnell", "Nagel"))

# Test Hosmer-Lemeshow
# install.packages("ResourceSelection")
# library(ResourceSelection)
hoslem.test(datos$ECC, fitted(modelo))