# ==============================================================================
# Proyecto: Vectores Autorregresivos (VAR)
# ==============================================================================
# Limpiar espacio de trabajo
rm(list = ls())
# Uso de libreriras para trabajar modelos VAR
library(vars)
library(tidyverse)
library(readxl)


# ------------------------------------------------------------------------------
# Extracción clasica de ddatos
# ------------------------------------------------------------------------------

datos <- read_excel("datos.xlsx", sheet = 3)

var1 <- datos[12:403,2]
var2 <- datos[12:403,3]
var3 <- datos[12:403,4]


mts_data <- ts(cbind(var1, var2, var3), start = c(1993, 1), frequency = 12)

plot.ts(mts_data, main = "Dinámica Multivariada")
# plot.ts(diff(mts_data), main = "Dinámica Multivariada")

#Prueba de cuantos resagos necesita nuesto model0 VAR
lag_selection <- VARselect(mts_data, lag.max = 8, type = "const")
lag_selection$selection
print(lag_selection)

# Estimación del modelo VAR con la librería

var_model <- VAR(mts_data, p = 6, type = "const")

# Muestra las ecuaciones individuales. Explicar R-cuadrado y significancia.
summary(var_model)

# ------------------------------------------------------------------------------
# Diagnóstico del Modelo (Validación)
# ------------------------------------------------------------------------------

#Prueba de raices unitarias
roots(var_model) 


# H0: No hay autocorrelación serial
serial_test <- serial.test(var_model, lags.pt = , type = "PT.asymptotic")
serial_test

# C. Normalidad de los residuos
norm_test <- normality.test(var_model)
norm_test


    

