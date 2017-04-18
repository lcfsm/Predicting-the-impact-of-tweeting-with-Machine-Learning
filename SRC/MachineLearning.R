# PASO 1:   Carga Package y Set de datos
# ---------------------------------------------------------------------------
library(rpart)
library(rpart.plot) 
rm(list = ls())
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))  
#setwd('C:\\Users\\LAPTOP\\Desktop\\PFM\\src')
#Dataset = read.csv("Ultimate1.csv")  # read csv file 
Dataset = read.csv("Ultimate2.csv")  # read csv file


Dataset[1] <- NULL
Dataset$impresionesdiscret <- as.factor(Dataset$impresionesdiscret)



#Dataset$Nterminos=NULL
#Dataset$anio=NULL

#Division en train y test
set.seed(101)
alpha     <- 0.8 # percentage of training set
inTrain   <- sample(1:nrow(Dataset), alpha * nrow(Dataset))
Train <- Dataset[inTrain,]
Test <- Dataset[-inTrain,]

# PASO 2:   Crear Arbol de Decision
# ---------------------------------------------------------------------------
modelo.rp<-rpart(impresionesdiscret ~ .,data=Train)
summary(modelo.rp)
#plot(modelo.rp, uniform=TRUE, branch=0.6, margin=0.05)
#text(modelo.rp, all=TRUE, use.n=TRUE)
#title("Training Set's Classification Tree")

#prune.rpart.tree <- prune(modelo.rp, cp=0.02) # pruning the tree
#plot(prune.rpart.tree, uniform=TRUE, branch=0.6)
#text(prune.rpart.tree, all=TRUE, use.n=TRUE)

# PASO 3:  Prediccion en datos de TEST
# -------------------------------------------------------------------------
Prediccion <- predict(modelo.rp, newdata = Test, type = "class") # Prediccción en Test
MC         <- table(Test[, "impresionesdiscret"],Prediccion) # Matriz de Confusión

#treepred <- predict(prune.rpart.tree, newdata = Test, type = "class")
#confusionMatrix(treepred, Test$impresionesdiscret)

# PASO 4: Crear Graficos
# ---------------------------------------------------------------------------

#Visualizacion 1
rpart.plot(modelo.rp, type=1, extra=100,cex = .7,box.col=c("gray99", "gray88")[modelo.rp$frame$yval])

#Visualizacion 2
library(partykit)
rparty.tree <- as.party(modelo.rp)
rparty.tree
plot(rparty.tree)

# PASO 5: Matriz de confusion
# ---------------------------------------------------------------------------

library(caret)
#confusionMatrix(Prediccion, Test$impresionesdiscret)

# Matriz de confusión
mc <- table(Prediccion, Test[,"impresionesdiscret"], dnn = c("Asignado","Real"))
MC         <- table(Test[, "impresionesdiscret"],Prediccion) # Matriz de Confusión
# Ordenar tabla alfabéticamente
# Por algún motivo a veces sale desordenada
#mc <- mc[order(rownames(mc)),order(colnames(mc))]
cat("** Árboles de clasificación: rpart\n")
#print(mc)
# Aciertos en %
aciertos0 <- sum(diag(mc)) / sum(mc) * 100
cat("\nCorrectamente clasificados:",round(aciertos0,2),"%\n\n\n")
#-------------------------------------------------------------------------

# Random Forest ------------------------------------------------------
library(randomForest)
modelo.rf <- randomForest(impresionesdiscret ~ ., data=Train)
## save this model------------------------------------------------------------------- SAVE MODEL
saveRDS(modelo.rf, "./randomforest.rda")
#Visualizacion 
library(partykit)
rparty.tree <- as.party(modelo.rf)
rparty.tree
plot(rparty.tree, type="simple")


pred <- predict(modelo.rf, Test, type="class")
# Matriz de confusión
mc <- table(pred, Test[,"impresionesdiscret"], dnn = c("Asignado","Real"))
# Ordenar tabla alfabéticamente
# Por algún motivo a veces sale desordenada
mc <- mc[order(rownames(mc)),order(colnames(mc))]
cat("Random Forest\n")
print(mc)
# Aciertos en %
aciertos1 <- sum(diag(mc)) / sum(mc) * 100
cat("\nCorrectamente clasificados:",round(aciertos1,2),"%\n\n\n")

# SVM ----------------------------------------------------------------
library(e1071)
modelo.svm <- svm(impresionesdiscret ~ ., data=Train)
pred <- predict(modelo.svm, Test, type="class")
# Matriz de confusión
mc <- table(pred,Test[,"impresionesdiscret"], dnn = c("Asignado","Real"))
# Ordenar tabla alfabéticamente
# Por algún motivo a veces sale desordenada
mc <- mc[order(rownames(mc)),order(colnames(mc))]
cat("** SVM\n")
print(mc)
# Aciertos en %
aciertos2 <- sum(diag(mc)) / sum(mc) * 100
cat("\nCorrectamente clasificados:",round(aciertos2,2),"%\n\n\n")

# Red neuronal -------------------------------------------------------
library(nnet)
parametros <- train(impresionesdiscret ~ ., data=Train, method="nnet", trace=F)
size <- parametros$bestTune$size
decay <- parametros$bestTune$decay
modelo.nnet <- nnet(impresionesdiscret ~ .,trace=F, data=Train, size=size, decay=decay)
pred.nnet <- predict(modelo.nnet, Test, type="class")
# Matriz de confusión
mc <- table(pred.nnet,Test[,"impresionesdiscret"], dnn = c("Asignado","Real"))
# Ordenar tabla alfabéticamente
# Por algún motivo a veces sale desordenada
mc <- mc[order(rownames(mc)),order(colnames(mc))]
cat("** Red neuronal: nnet\n")
print(mc)
# Aciertos en %
aciertos3 <- sum(diag(mc)) / sum(mc) * 100
cat("\nCorrectamente clasificados:",round(aciertos3,2),"%\n\n\n")

# AdaBoost - Adaptative Boosting ------------------------------------------
library(adabag)
modelo.ad <- boosting(impresionesdiscret ~., data=Train)
pred <- predict(modelo.ad, Test, type="class")
# Matriz de confusión
mc <- table(pred$class, Test[,"impresionesdiscret"], dnn = c("Asignado","Real"))
# Ordenar tabla alfabéticamente
# Por algún motivo a veces sale desordenada
mc <- mc[order(rownames(mc)),order(colnames(mc))]
cat("** Adaptative Boosting: boosting\n")
print(mc)
# Aciertos en %
aciertos4 <- sum(diag(mc)) / sum(mc) * 100
cat("\nCorrectamente clasificados:",round(aciertos4,2),"%\n\n\n")

# Bootstrapped Aggregation: Bagging ----------------------------------------
library(ipred)
modelo.ba <- ipred::bagging(impresionesdiscret ~., data=Train)
## save this model------------------------------------------------------------------- SAVE MODEL
saveRDS(modelo.ba, "Bootstrapped.rda")
pred <- predict(modelo.ba, Test, type="class")
# Matriz de confusión
mc <- table(pred,Test[,"impresionesdiscret"], dnn = c("Asignado","Real"))
# Ordenar tabla alfabéticamente
# Por algún motivo a veces sale desordenada
mc <- mc[order(rownames(mc)),order(colnames(mc))]
cat("** Bootstrapped Aggregation: bagging\n")
print(mc)
# Aciertos en %
aciertos5 <- sum(diag(mc)) / sum(mc) * 100
cat("\nCorrectamente clasificados:",round(aciertos5,2),"%\n\n\n")

##################### RESUMEN #####################################################################
resumen <- c(aciertos0,aciertos1,aciertos2,aciertos3,aciertos4,aciertos5)
names(resumen) <- c("rpart","random forest","svm","red neuronal","Adaptative Boosting","Bootstrapped Aggregation")
mod.list <- list(rpart=modelo.rp,randomForest=modelo.rf,svm=modelo.svm,nnet=modelo.nnet,boosting=modelo.ad,bagging=modelo.ba)
resumen <- sort(resumen,decreasing=T)
resumen <- as.matrix(resumen,nc=1)
colnames(resumen) <- "% Aciertos"
cat("Resumen:\n")
print(resumen)

############### VALIDACION CRUZADA #####################################

# PACKAGE Y DATA SET
# ----------------------------------------------------------------------------- 
library(rpart) 

# FOLDS
# ------------------------------------------------------------------------------- 
set.seed(101)
Folds         <- 10            
Dataset$kfold   <- sample(1:Folds, nrow(Dataset), replace = T)

# MODELOS 
# -------------------------------------------------------------------------------- 
Iter   <- data.frame(iteracion = NULL, aciertos = NULL)
for (i in 1:Folds)
{
  Test          <- subset(Dataset, kfold  == i)
  Entrenamiento <- subset(Dataset, !kfold == i) 
  Modelo        <- rpart(impresionesdiscret ~ .,data = Train)       
  Prediccion    <- predict(Modelo, Test, type = "class")  
  MC            <- table(Test[, "impresionesdiscret"],Prediccion)           
  Aciertos      <- MC[1, 1] / (MC[1, 1] + MC[2, 1])
  Iter          <- rbind(Iter, data.frame(Iter = i, acierto = Aciertos))  
}

# GRAFICO
# -------------------------------------------------------------------------------- 
promedio  <- format(mean(Iter$acierto, na.rm=TRUE)*100,digits = 4)
plot(Iter,type = "b", main = "% Prediccion en Cada Iteracion",  
     cex.axis = .7,cex.lab = .7,cex.main = .8, 
     xlab ="No. de Iteraciones", ylab="% Prediccion")
abline(h = mean(Iter$acierto), col = "blue", lty = 2)
legend("topright", legend = paste("Eficiencia de Prediccion =", promedio, "%"),
       col = "blue", lty = 2, lwd = 1, cex=.7, bg=NULL)
