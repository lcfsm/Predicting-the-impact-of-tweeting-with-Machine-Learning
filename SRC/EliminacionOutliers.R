#Se establece el PATH donde esta el dataset final
rm(list = ls())
setwd('C:\\Users\\LAPTOP\\Desktop\\PFM\\src')

DatasetOriginal = read.csv("DataSet_Final.csv")  # read csv file 
DatasetCopy=DatasetOriginal[4:26] #Eliminamos los datos no numericos, no relevantes

#Eliminar las columnas que tienen todos los valores a 0
i <- (colSums(DatasetCopy, na.rm=T) != 0) # T if colSum is not 0, F otherwise

#matnonzero <- DatasetCopy[, i] # all the non-zero columns
#matzeros <- DatasetCopy[, !i]  # all the zero columns

DatasetCopy <- DatasetCopy[, i] # all the non-zero columns

#################### ACP Analisis de componentes principales #####################

#Con Matriz de covarianza
(acp.cov <- prcomp(DatasetCopy))

#Correlaciones entre Variables y Componentes
diag(1/sqrt(diag(cov(DatasetCopy)))) %*% acp.cov$rotation %*% diag(acp.cov$sdev)

#Correlaciones entre Variables y Componentes
diag(1/sqrt(diag(cov(DatasetCopy)))) %*% acp.cov$rotation %*% diag(acp.cov$sdev)

#Con Matriz de correlación
acp <- prcomp(DatasetCopy, scale = TRUE)
acp
summary(acp)

#Para obtener los autovalores
acp$sdev^2

#Correlaciones entre Variables y Componentes
(corvar <- acp$rotation %*% diag(acp$sdev))

#Nuevas coordenadas
acp$x

#Porcentaje de varianza total
barplot(summary(acp)$importance[2, ])

#Correlación enre Variables y CP1 y CP2
plot(-1:1, -1:1, type='n', asp=1, xlab='CP1', ylab='CP2')
abline(h=0, v=0, lty=2, col=8)

## Dibuja un círculo de centro (0,0) y radio 1
symbols(0, 0, 1, inches=F, add=T)
symbols(0, 0, sqrt(.5), inches=F, add=T)

## Dibuja los vectores y coloca los nombres
arrows(0, 0, corvar[,1], corvar[,2], length=.1)
text(corvar[,1], corvar[,2], colnames(DatasetCopy), pos=4, offset=.6, col=2, font=2)

#Gráfico de individuos
plot(acp$x[, 1:2], pch = 19, xlab='CP1', ylab='CP2')
abline(h = 0, v = 0, lty = 2, col = 8)

#Biplot
biplot(acp)

abline(h = 0, v = 0, lty = 2, col = 8)

#Gráfico de barras para Correlación CP y Variables
tweets.acp <- prcomp(DatasetCopy, scale = TRUE)
tweets.cor <- tweets.acp$rotation %*% diag(tweets.acp$sdev)
barplot(t(tweets.cor[, 1:3]), beside = TRUE, ylim = c(-1, 1))

#Matriz de gráficos de dispersión
library(car)
scatterplotMatrix(DatasetCopy, diagonal = "hist")

#Metodo 2#####################################################################

cor(DatasetCopy)
library(FactoMineR)
acp1<-PCA(DatasetCopy)
#En el grafico la suma de los porcentajes dos los ejes menos el 100% 
#nos da la tasa de perdida que estamos teniendo
acp1$eig
acp1$var$coord
acp1$var$cor
acp1$ind$coord
#indices importantes 
acp1$ind$contrib
acp1$ind$cos2
plot(acp1, choix="ind", axes=c(1,3))
plot(acp1, choix="var", axes=c(1,3))

################## Fin ACP #############################################################

################## Deteccion OUTLIERS ############################################################

library(DMwR)
outlier.scores <- lofactor(DatasetCopy, k=5)
plot(density(outlier.scores))
#Se eligen 5 como outliers
outliers <- order(outlier.scores, decreasing=T)[1:5]
#Los outliers serían:
print(outliers)
print(DatasetCopy[outliers,])
#Los pintamos
n <- nrow(DatasetCopy)
labels <- 1:n
labels[-outliers] <- "."
biplot(prcomp(DatasetCopy), cex=.8, xlabs=labels)
pch <- rep(".", n)
pch[outliers] <- "+"
col <- rep("black", n)
col[outliers] <- "red"
pairs(DatasetCopy, pch=pch, col=col)
#Se puede usar clusters basados en funciones de densidad como DBSCAN, por lo que los objetos que no se asignan a otros clusters serían outliers. También se puede usar los k-means. Por ejemplo:
kmeans.result <- kmeans(DatasetCopy, centers=3)
#Centros de los cluster
kmeans.result$centers
#IDs de los clusters
kmeans.result$cluster
#Distancia entre los objetos y los centros
centers <- kmeans.result$centers[kmeans.result$cluster, ]
distances <- sqrt(rowSums((DatasetCopy-centers)^2))
#Cogemos las 5 distancias mayores
outliers <- order(distances, decreasing=T)[1:5]
#Estos son los outliers
print(outliers)
print(DatasetCopy[outliers,])
#Los pintamos
plot(DatasetCopy[,c("impresiones", "interacciones", "tasa.de.interacción")], pch="o", col=kmeans.result$cluster, cex=0.3)
#Y sus centros
points(kmeans.result$centers[,c("impresiones", "interacciones", "tasa.de.interacción")], col=1:3,pch=8, cex=1.5)
#Y los outliers
points(DatasetCopy[outliers, c("impresiones", "interacciones", "tasa.de.interacción")], pch="+", col=4, cex=1.5)

######### Multivariate outliers ##############

#Introducir las variables que saquemos del ACP
summary(ols <- lm(impresiones~., data = DatasetCopy))
opar <- par(mfrow = c(2,2), oma = c(0, 0, 1.1, 0))
plot(ols, las = 1)

#Otra forma multivariate
mod <- lm(impresiones ~ interacciones, data=DatasetCopy)
cooksd <- cooks.distance(mod)
plot(cooksd, pch="*", cex=2, main="Influential Obs by Cooks distance")  # plot cook's distance
abline(h = 4*mean(cooksd, na.rm=T), col="red")  # add cutoff line
text(x=1:length(cooksd)+1, y=cooksd, labels=ifelse(cooksd>4*mean(cooksd, na.rm=T),names(cooksd),""), col="red")  # add labels

########### MISSING VALUES ###############

require(VIM)
require(FactoMineR)
require(tidyr)
require(dplyr)
require(magrittr)
aggr(DatasetOriginal, prop=FALSE,  numbers=TRUE, border=NA, combine=TRUE)
#DatasetOriginal[complete.cases(DatasetOriginal),]
