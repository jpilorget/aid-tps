#Elijo el directorio y cargo las librerías
setwd("C:/Users/jpilorget/Documents/Maestría en Data Mining - UBA/2017 1C - Análisis Inteligente de Datos")
library(nortest)
library(ADGofTest)
library(MASS)
library(car)
library(readxl)
library(dplyr)
library(mvnormtest)
library(biotools)
library(rpanel)
library(Hotelling)
library(corpcor)
library(caret)
library(ggplot2)
library(scales)
library(candisc)

##EJERCICIO 1
gorriones <- read.csv2("gorriones.csv")
#i) Analizo las  medias de los grupos
grupo1 <- filter(gorriones, Sobrevida == 1)
grupo2 <- filter(gorriones, Sobrevida ==-1)
summary(grupo1)
summary(grupo2)

#ii) Analizo los vectores medios de los grupos
vecmed1 <- colMeans(grupo1[2:6])
vecmed2 <- colMeans(grupo2[2:6])

#iii) Hago el test de Lawley-Hotelling para ver si tiene sentido hacer análisis discriminante
mangorriones <- manova(cbind(Largo_total,Extension_alar, Largo_picoycabeza, 
                Largo_humero, Largo_esternon) ~Sobrevida, data = gorriones)
summary(mangorriones, test="Hotelling-Lawley")

##EJERCICIO 2
hemofilia <- read.csv2("hemofilia.csv")

#Hago un scatterplot y veo si una línea discrimina
plot(hemofilia$ActividadAHF, hemofilia$AntigenoAHF, col = c( "blue","red"))
abline(0.1,0.8)

#Hago dos boxplot para ver simetría y normalidad
boxplot(hemofilia$ActividadAHF ~ as.factor(hemofilia$Portador), col = c("blue", "red"))
boxplot(hemofilia$AntigenoAHF ~ as.factor(hemofilia$Portador), col = c("blue", "red"))

#Tengo que hacer un test de multinormalidad
hemo_mat <- as.matrix(hemofilia[,2:3])
mshapiro.test(t(hemo_mat)) #Cumple el supuesto de normalidad
#Hago un test de M de Box
boxM(hemofilia[,2:3], hemofilia$Portador)

#Hago un test de Hotelling
hemo_grupo1 <- filter(hemofilia, Portador == 1)[,2:3]
hemo_grupo2 <- filter(hemofilia, Portador == 2)[,2:3]
hot <- hotelling.test(hemo_grupo1, hemo_grupo2)

#Creo dos muestras aleatorias de entrenamiento y validación
set.seed(1)
trainrows <-runif(nrow(hemofilia)) > 0.3
train <- hemofilia[trainrows,]
test <- hemofilia[!trainrows,]


#Aplico el modelo de análisis discriminante lineal (en entrenamiento y validación)
modelo <- lda(Portador ~ ActividadAHF+AntigenoAHF,data=train, prior=c(26/54, 28/54), method = "mle")
prediccion <- predict(object = modelo, newdata = test[,2:3])

#Creo una matrix de confusión
confusionMatrix(prediccion$class, test$Portador)

##EJERCICIO 4
data("iris")

#Ploteo los valores medios de cada especie
vecmed <- aggregate(cbind(Sepal.Length, Sepal.Width, Petal.Length, Petal.Width) ~ Species, iris, mean)
parcoord(vecmed[,2:5], col = c(rainbow(length(vecmed[,2:5]))))

lda <- lda(Species ~ ., iris, prior = c(1,1,1)/3)
plda <- predict(object = lda, newdata = iris)
prop.lda <- lda$svd^2/sum(lda$svd^2)
dataset <- data.frame(species = iris[,"Species"], lda = plda$x)
ggplot(dataset) + geom_point(aes(lda.LD1, lda.LD2, colour = species, shape = species), size = 2.5) + 
    labs(x = paste("LD1 (", percent(prop.lda[1]), ")", sep=""),
         y = paste("LD2 (", percent(prop.lda[2]), ")", sep=""))

