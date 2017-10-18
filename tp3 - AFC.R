# EJERCICIO 2
#Creo la matriz de datos
datos <- matrix(c(14,159,25,152), nrow = 2, ncol = 2)
datos

#Tabla de contingencia
crosstab <- addmargins(datos)
colnames(crosstab) <- c("Masc", "Fem", "Total_fila")
rownames(crosstab) <- c("Usa", "No usa", "Total_col")

#Tabla de distribución conjunta
distr_conjunta <- addmargins(prop.table(datos))

#Tabla de distribución condicional al uso
distr_conduso <- addmargins(prop.table(addmargins(datos,1),1),2)

#Tabla de distribución condicional al sexo
distr_condsexo <- addmargins(prop.table(addmargins(datos,2),2),1)

#Tabla de frecuencia esperada
freq_esp <- chisq.test(datos)$expected

# Test de Chi-cuadrado (sin corrección de continuidad de Yates)
chisq.test(datos, correct = F)

#Elijo el directorio y cargo el archivo
setwd("C:/Users/Usuario/Downloads/AID")
cargo_fumador <- read.csv2("cargo_fumador.csv")

#Creo los perfiles fila y columna
View(cargo_fumador)
install.packages("questionr")
tabla <- wtd.table(cargo_fumador$ï..Cargo, cargo_fumador$Fumador, weights = cargo_fumador$Frecuencia)
tabla <- tabla[,c(3,4,1,2)]
#Perfil columna
perfil_columna <- addmargins(round(prop.table(tabla, 2),2),1)
#Perfil fila
perfil_fila <- addmargins(round(prop.table(tabla, 1),2),2)

#Calculo la inercia (distancia Chi-cuadrado sobre cantidad de datos) y la contribución de Chi sq
parcoord(perfil_columna)
library(ca)
biplot <- ca(tabla, nd=2)
plot(biplot)

#EJERCICIO 8
#Cargo el dataset y construyo la matriz de Burt (disyuntiva por traspuesta)
autos <- read.csv2("Autos.csv")
library(anacor)
autos <- autos[,2:8]
autos_burt <- burtTable(autos)
summary.CA(autos)
summary(autos_burt, nb.dec = 2, ncp = 2)

#Los grados de libertad son i-1 * j - 1 (por eso en una tabla de 4*2 son 3).
#Si el p-valor es menor que alfa entonces cae en la región de rechazo de H0.
#Ergo, las variables no son independientes.
#Si no rechazo H0 digo "No tengo evidencia para asegurar que no son independientes", pero
# nunca digo "Las variables son independientes". H0 nunca se afirma.
# Si bien no hay frecuencias menores a 1 y el 80% son mayores o iguales a 5, no tengo las 
# frecuencias esperadas, por lo que tengo que calcularlas.
