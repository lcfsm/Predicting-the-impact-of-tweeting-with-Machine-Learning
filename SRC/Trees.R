# PASO 1:   Carga Package y Set de datos
# ---------------------------------------------------------------------------
library(rpart)
library(rpart.plot) 
library(ggplot2)
library(caret)
rm(list = ls())
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
#datos1 = read.csv("Ultimate1.csv")
datos2 = read.csv("Ultimate2.csv")
#datos1[,1] = NULL
datos2[,1] = NULL
#datos2[18:42] = NULL #eliminar palabras

#datos1$impresionesdiscret <- as.factor(datos1$impresionesdiscret)
datos2$impresionesdiscret <- as.factor(datos2$impresionesdiscret)

# Hago que lo que voy a predecir se llame "clase", para simplificar el resto del cÃ³digo
#colnames(datos1)[colnames(datos1)=="impresionesdiscret"]="clase"
colnames(datos2)[colnames(datos2)=="impresionesdiscret"]="clase"

datos = datos2
colclase = which(colnames(datos)=="clase")

#datos$anio=NULL
#datos$Nterminos=NULL #--------------------------------------------------------------------------PRUEBAS

# Creo una funcion para hacer las pruebas... con validacion cruzada
pruebatree = function(datos,sp,ms,md,cp){
  #set.seed(123456)
  indice = createMultiFolds(datos$clase, k = 10, times = 2)
  acierto = c()
  for (i in 1:length(indice)){
    datostra = datos[ indice[[i]],]
    datostst = datos[-indice[[i]],]
    modelo = rpart(clase~., data = datostra,method="class",minsplit=ms,maxdepth=md,cp=cp,parms = list(split = sp))
    prediccion = predict(modelo, datostst, type = "class")
    resultado = confusionMatrix(prediccion, datostst$clase)$overall[[1]]
    acierto = rbind(acierto,c(resultado))
  }
  return(colMeans(acierto))
}

tiposplit = c("information","gain")
valminsplit = c(1,5,10,25,50)
valmaxdepth = c(30) 
valcp       = c(0.001)
todo = data.frame(expand.grid(tiposplit,valminsplit,valcp,valmaxdepth))
#Probar todas estas configuraciones de los parametros de entrada
#View(todo)
todo$resultado = 0
for (i in 1:nrow(todo)){
  tsp = as.character(todo$Var1[i])
  msp = todo$Var2[i]
  cp = todo$Var3[i]
  md = todo$Var4[i]
  res = pruebatree(datos,tsp,msp,md,cp)
  todo$resultado[i] = res
  print(todo$resultado)
}
ggplot(todo,aes(x=Var2,y=resultado,col=Var1))+geom_line()

#Extraer la mejor configuracion y construir un arbol con train y test
mejorconf = todo[which.max(todo$resultado),1:4]
mejorconf[1]
indice = createDataPartition(datos$clase, p = 0.7, times = 1, list=FALSE)
datostra = datos[ indice,]
datostst = datos[-indice,]
modelo = rpart(clase~., data = datostra,minsplit=mejorconf[2],maxdepth=mejorconf[4],cp=mejorconf[3],parms = list(split = mejorconf[1]))

rpart.plot(modelo, type=1, extra=100,cex = .7)

prp(modelo,cex=0.5)
summary(modelo)

library(partykit)
tree_party<-as.party(modelo)
print(tree_party, header=FALSE)

modelo = prune(modelo, cp=modelo$cptable[which.min(modelo$cptable[,"xerror"]),"CP"])
rpart.plot(modelo, type=1, extra=100,cex = .7)

prp(modelo,cex=0.5)
prediccion = predict(modelo, datostst, type = "class")
resultado = confusionMatrix(prediccion, datostst$clase)

plotcp(modelo)
# Plot approximate R-squared and relative error for different splits
layout(matrix(1:2, ncol = 2))
rsq.rpart(modelo)

## save this model-------------------------------------------------------------------
saveRDS(modelo, "arbol.rda")

# Matriz de confusión
mc <- table(prediccion, datostst[,"clase"], dnn = c("Asignado","Real"))
cat("** Árboles de clasificación: rpart\n")
aciertos0 <- sum(diag(mc)) / sum(mc) * 100
cat("\nCorrectamente clasificados:",round(aciertos0,2),"%\n\n\n")

resultado$byClass
resultado$overall

#Probamos cuando acierta en + - 1 ("clase")
datosprueba<-datostst
datosprueba$result <- prediccion
datosprueba$result<-as.numeric(datosprueba$result)
datosprueba$clase<-as.numeric(datosprueba$clase)
sapply(datosprueba, class)
total=0
for (i in 1:nrow(datosprueba)){
  if(datosprueba$clase[i]==datosprueba$result[i] || datosprueba$clase[i]== datosprueba$result[i]+1 || datosprueba$clase[i]== datosprueba$result[i]-1){
    total=total+1
  }
}
aciertosmasmenos = total/nrow(datosprueba) * 100
cat("\nCorrectamente clasificados + 1 -:",round(aciertosmasmenos,.2),"%\n\n\n")

# Vamos a probar a pasarle una matriz de costes asociada a los errores, para
# penalizar mÃ¡s los fallos mÃ¡s grandes
costes = matrix(c(0,1,2,3,4,5,
                  1,0,1,2,3,4,
                  2,1,0,1,2,3,
                  3,2,1,0,1,2,
                  4,3,2,1,0,1,
                  5,4,3,2,1,0),ncol=6)^0.5

modelo = rpart(clase~., data = datostra,minsplit=mejorconf[2],maxdepth=mejorconf[4],cp=mejorconf[3],parms = list(split = mejorconf[4],loss=costes))
prp(modelo,cex=0.5)
prediccion = predict(modelo, datostst, type = "class")
resultado = confusionMatrix(prediccion, datostst$clase)
resultado
rpart.plot(modelo, type=1, extra=100,cex = .7,box.col=c("gray99", "gray88")[modelo$frame$yval])

# Matriz de confusión
mc <- table(prediccion, datostst[,"clase"], dnn = c("Asignado","Real"))
# Ordenar tabla alfabéticamente
#mc <- mc[order(rownames(mc)),order(colnames(mc))]
cat("** Árboles de clasificación: rpart\n")
#print(mc)
# Aciertos en %
aciertos0 <- sum(diag(mc)) / sum(mc) * 100
cat("\nCorrectamente clasificados:",round(aciertos0,2),"%\n\n\n")
resultado$overall["Accuracy"]


