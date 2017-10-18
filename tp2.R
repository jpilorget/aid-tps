## TP N° 2 - Análisis de componentes principales ##
## ACLARACIONES: falta el ejercicio 4, no hecho en clase. El ejercicio 7 está en desarrollo.

#EJERCICIO 1
#Cargo la librería que contiene la función traza (tr())
library(psych)
library(readxl)
#Ejercicio 1:
#Creo la matriz
matriz <- matrix(c(3,1,1,1,3,1,1,1,5),nrow=3,ncol=3)
#Calculo la traza
tr(matriz)
#Calculo los autovalores
autoval <- eigen(matriz)$values
#Calculo los autovectores
autovec <- eigen(matriz)$vectors
#La primera componente capta el 54,5%, la segunda el 27,3% y la tercera el 18,2%
sum(autovec[,1] * c(2,2,1))
sum(autovec[,2] * c(2,2,1))

#EJERCICIO 2
#Elijo el directorio y cargo el archivo
setwd("C:/Users/jpilorget/Documents/Maestría en Data Mining - UBA/2017 1C - Análisis Inteligente de Datos")
chalets_total <- read_xls("chalets.xls")
chalets_total <- setNames(chalets_total, c("promotora","dur_hipoteca","precio_medio","sup_cocina"))
chalets_total <- as.data.frame(chalets_total)
chalets <- chalets_total[,2:4]
#Hago el boxplot en valores comunes y normalizados
boxplot(chalets)
chalets_std <- scale(chalets)
boxplot(chalets_std)

#Grafico las correlaciones de a pares
pairs(chalets)

#Calculo las matrices de covarianza y correlación
matcov <- as.matrix(cov(chalets))
matcor <- cov2cor(matcov)

#Calculo la media de cara variable para el vector medio (buscar función mejor)
colMeans(chalets)

#Hallo y grafico las componentes principales
#Calculo los autovectores
avec_chalets <- eigen(matcor)$vectors
aval_chalets <- eigen(matcor)$values
#Calculo la captación de variabilidad de la primera componente 
a <- princomp(chalets, cor = T)
#La primera CP explica el 90,04% de la variabilidad
#Otra alternativa es hacer: aval_chalets/sum(aval_chalets) 
#Grafico las cargas de la primera CP y multiplico por -1 para cambiar el signo
barplot(a$loadings[,1]*-1)
#Se trata de una componente de tamaño, ya que todos sus valores tienen el mismo signo.
#Por tal motivo, se puede ordenar a las promotoras por este valor
chalets_total$cp1 <- round(rowSums(avec_chalets[,1]*chalets_std)*-1,2)
barplot(chalets_total$cp1)

#EJERCICIO 3
#Cargo la matriz
matriz_ej3 <- matrix(c(3,6,5,6,10,12), nrow = 3, ncol = 2, byrow = T)

#Calculo la matriz de covarianza
matcov_ej3 <- as.matrix(cov(matriz_ej3))
matcov_ej3
#Calculo los autovectores y autovalores
aval_ej3 <-eigen(matcov_ej3)$values
avec_ej3 <- eigen(matcov_ej3)$vectors

#Calculo la captación de variabilidad del CP1
aval_ej3/sum(aval_ej3)

#Ahora calculo todo para la matriz de correlaciones
matcor_ej3 <- matrix(cor(matriz_ej3), nrow = 2, ncol = 2)
aval_std_ej3 <-eigen(matcor_ej3)$values
avec__std_ej3 <- eigen(matcor_ej3)$vectors
aval_std_ej3 / sum(aval_std_ej3)

#EJERCICIO 5
#Cargo el archivo de suelos
suelo <- read_xls("suelo.xls")
suelo <- as.data.frame(suelo[,2:6])

matcov_suelo <-as.matrix(cov(suelo))
matcor_suelo <- as.matrix(cor(suelo))

#Calculo la proporción de variabilidad que capta cada componente principal
round(eigen(matcor_suelo)$values,2)
round(eigen(matcov_suelo)$values,2)

#Analizo los loadings de los componentes principales
eigen(matcov_suelo)$vectors
eigen(matcor_suelo)$vectors
#Es mejor usar la matriz de correlación (no la afecta la escala de medida de los atributos)

#EJERCICIO 6
#Cargo el archivo y estandarizo las variables
gorriones <- read.csv2("gorriones.csv", dec = ",")
gorriones <- gorriones[,2:6]
gorriones_std <- scale(gorriones)

#Construyo las matrices de covarianza estandarizada y correlación
matcov_gorriones <- as.matrix(cov(gorriones))
matcor_gorriones <- as.matrix(cor(gorriones))
corrplot::corrplot(matcor_gorriones)

#Calculo los CP y sus loadings
cp_gorriones <- princomp(gorriones, cor = T)
cp_gorriones$loadings

#Calculo el valor del tercer autovalor y cuánto explica
aval_gorriones_std <- eigen(matcor_gorriones)$values
aval_gorriones <- eigen(matcov_gorriones)$values

eigen(matcor_gorriones)$values[1]/sum(eigen(matcor_gorriones)$values)
#El valor del lambda3 es 0,367 y explica el 7,3%

#El gráfico de sedimentación es el siguiente
screeplot(princomp(gorriones, cor = T), type="l")

#Calculo los valores de los CP del gorrión 11
autovec_gorriones <- eigen(matcor_gorriones)$vectors
sum(gorriones_std[11,]*autovec[,1])
sum(gorriones_std[11,]*autovec[,2])
sum(gorriones_std[11,]*autovec[,3])

#Observo los gorriones en los ejes 1-2, 2-3 y 1-3 de CP a través de un biplot 
biplot(cp_gorriones, choices = 1:2, pc.biplot = F, scale = 0.9)
biplot(cp_gorriones, choices = 2:3, pc.biplot = F, scale = 0.9)
biplot(cp_gorriones, choices = c(1,2), pc.biplot = F, scale = 0.9)

#EJERCICIO 7
#Cargo el dataset
hospital <- read.csv2("hospital.csv")

#Chequeo que las dos primeras CP sean los que indica el ejercicio
princomp(hospital[,2:8], cor = T)$loadings[,1:2]

#Observo la proporción de variablidad y el gráfico de sedimentación
cp_hospitales <- princomp(hospital[,2:8], cor = T)
summary(cp_hospitales)
screeplot(cp_hospitales, type = "l")
#Las dos primeras componentes abarcan menos de dos tercios de la variabilidad.
#La tercera es mayor que 1 (criterio de Kaiser), así que la incorporamos.

#Hago el biplot con las primeras dos componentes.
biplot(cp_hospitales, choices = 1:2, pc.biplot = F, scale = 0.9)
