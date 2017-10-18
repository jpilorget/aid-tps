#Cargo las librerías y elijo el directorio
setwd("~/TP 4")
library(nortest)
library(ADGofTest)
library(MASS)
library(car)

# EJERCICIO 1
tiemposcomp <- read.csv2("tiemposcomp.csv")
#Hago pruebas de normalidad para los tiempos de ambas sucursales 
lillie.test(tiemposcomp[1:20,2])
lillie.test(tiemposcomp[21:40,2])

#Como no es normal, transformo con la variable elevada a la menos 1 y menos 0,5
#Esto está relacionado a las transformaciones de Box & Cox
tiemposcomp$Tiemposexp <- tiemposcomp$Tiempos ^ (-1)
tiemposcomp$Tiemposexp05 <- tiemposcomp$Tiempos ^ (-0.5)

boxcox(aov(tiemposcomp$Tiempos), plotit = T)

#Hago pruebas de normalidad para los tiempos de ambas sucursales
lillie.test(tiemposcomp[1:20,3])
lillie.test(tiemposcomp[21:40,3])

#Hago el test de Student para muestras independientes para cada variable
t.test(tiemposcomp$Tiemposexp[1:20], tiemposcomp$Tiemposexp[21:40])
t.test(tiemposcomp$Tiemposexp05[1:20], tiemposcomp$Tiemposexp05[21:40])

# Cuando no consigo normalidad transformando hago una prueba no paramétrica
# Evaluar la pertinencia de la corrección por continuidad
wilcox.test(x = tiemposcomp$Tiempos[1:20], y= tiemposcomp$Tiempos[21:40], correct = F)

# EJERCICIO 3
supdieta <- read.csv2("suplementosdieta.csv", col.names = c("valor", "suplemento"))

#Tengo que testear que se cumplan los tres supuestos de ANOVA
# Supuestos: homocedasticidad (Levene), independencia (ASUMIDA) y normalidad (Shapiro-Wilks)
aov <- aov(supdieta$valor ~ supdieta$suplemento)

#Luego de comprobar los supuestos hago el test de Tukey
TukeyHSD()

# EJERCICIO 4
#Para trabajar en el punto 6 del ejercicio, con valors ordinales, hago Kruskal-Wallis
#Es un test de varianzas no paramétricas
