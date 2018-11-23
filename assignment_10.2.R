setwd("C:/Users/CHIRAG/Downloads/ACADgILd")
library(readr)
Weight_lift <- read.csv("Weight lift.csv")
set.seed(12345)
dataTrain<-data[1:4004,]
dataTest<-data[4005:4024,]
head(dataTrain)
head(dataTest)
indexNA <- as.vector(sapply(dataTrain[,1:158],function(x) {length(which(is.na(x)))!=0}))
dataTrain <- dataTrain[,!indexNA]
train_control<- trainControl(method="cv", number=10)
model<- train(classe ~., data=dataTrain,trControl=train_control, method="rf")
model
# make predictions
predictions<- predict(model,dataTrain)
# append predictions
pred<- cbind(dataTrain,predictions)
# summarize results
confusionMatrix<- confusionMatrix(pred$predictions,pred$classe)
confusionMatrix
#how do we create a cross validation scheme
control <- trainControl(method = 'repeatedcv',
                        number = 10,
                        repeats = 3)
seed <-7
metric <- 'Accuracy'
set.seed(seed)
mtry <- sqrt(ncol(dataTrain))
tunegrid <- expand.grid(.mtry=mtry)
rf_default <- train(pitch_belt~.,
                    data = dataTrain,
                    method = 'rf',
                    metric = 0,
                    tuneGrid = tunegrid,
                    trControl = control)
print(rf_default)
#-------------------------------
# make predictions
predictions<- predict(rf_default,dataTest)
# append predictions
pred<- cbind(dataTest,predictions)
# summarize results
confusionMatrix<- confusionMatrix(pred$predictions,pred$classe)
confusionMatrix
varImp(rf_default)
#----------------
# random search for parameters
control <- trainControl(method = 'repeatedcv',
                        number = 10,
                        repeats = 3,
                        search = 'random')
# make predictions
predictions<- predict(rf_default,dataTest)
# append predictions
pred<- cbind(dataTest,predictions)
# summarize results
#confusionMatrix<- confusionMatrix(pred$predictions,pred$classe)
confusionMatrix
varImp(random)
#--------------------
# Grid search
control <- trainControl(method = 'repeatedcv',
                        number = 10,
                        repeats = 3,
                        search = 'grid')
set.seed(seed)
tunegrid <- expand.grid(.mtry=c(1:80))
mtry <- sqrt(ncol(x))
rf_gridsearch <- train(~.,
                       data = dataTrain[1:200,],
                       method = 'rf',
                       metric = 0,
                       tuneGrid = tunegrid,
                       trControl = control)
print(rf_gridsearch)
plot(rf_gridsearch)
# make predictions
predictions<- predict(rf_gridsearch,dataTest)
# append predictions
pred<- cbind(dataTest,predictions)
number = 5,
repeats = 3,
search = 'grid')
seed <- 7
library(C50)
set.seed(seed)
metric <- 'Accuracy'
gbm_mod <- train(pitch_belt~.,
                 data = dataTrain,
                 method = 'gbm',
                 metric = 0,
                 trControl = control)
print(gbm_mod)
plot(gbm_mod)
summary(gbm_mod)
# make predictions
predictions<- predict(gbm_mod,dataTest)
# append predictions
pred<- cbind(dataTest,predictions)
# summarize results
confusionMatrix<- confusionMatrix(pred$predictions,pred$classe)
confusionMatrix