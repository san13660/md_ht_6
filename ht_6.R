# Christopher Sandoval 13660
# Maria Fernanda Estrada 14198


# Paquetes y librerias
install.packages("caret")
install.packages("e1071")
install.packages("dummies")
library(caret)
library(e1071)
library(dummies)


# Juntamos todo el data set de test y training
# Se hace la limpieza de una vez
data_training <- read.csv("train.csv", stringsAsFactors = FALSE)
data_training$Class <- as.factor(ifelse(data_training$SalePrice >= 200000, "Cara", "Economica"))
data_training_filtered <- data_training[, c(2,19,20,35,45,48,52,71,82)]
data_training_filtered <- na.omit(data_training_filtered)
data_test <- read.csv("test.csv", stringsAsFactors = FALSE)
data_test_filtered <- data_test[, c(2,19,20,35,45,48,52,71)]
data_sample <- read.csv("sample_submission.csv", stringsAsFactors = FALSE)
data_sample$Class <- as.factor(ifelse(data_sample$SalePrice >= 200000, "Cara", "Economica"))
data_test_filtered$Class <- data_sample$Class
data_test_filtered <- na.omit(data_test_filtered)
# Set de datos completo
data <- rbind(data_training_filtered,data_test_filtered)


# Agregar columnas dummy
data<-cbind(data,dummy(data$Class,verbose = T))


# Ya separamos los datos nuevamente en training y test aleatoriamente
porcentaje<-0.7
corte <- sample(nrow(data),nrow(data)*porcentaje)
train<-data[corte,]
test<-data[-corte,]


#Queremos saber si una casa es cara o no
modelo<-glm(dataCara~., data = train[,c(1:8,10)],family = binomial(), maxit=100)


# Prediccion Regresion Logistica
pred<-predict(modelo,newdata = test[,c(1:8)], type = "response")
prediccion<-ifelse(pred>=0.6,1,0)
confusionMatrix(as.factor(test$dataCara),as.factor(prediccion))


# Grafica del modelo ejemplos
plot(test$OverallCond, prediccion, pch = 16, xlab = "OverallCond", ylab = "Prediccion")
plot(test$MSSubClass, prediccion, pch = 16, xlab = "MSSubClass", ylab = "Prediccion")
plot(test$YearBuilt, prediccion, pch = 16, xlab = "YearBuilt", ylab = "Prediccion")
