rm(list=ls())
library(caret)
data(oil)
table(oilType)
library(corrplot)
corrplot(cor(fattyAcids), order = "hclust")
fattyAcids <- fattyAcids[,-findCorrelation(cor(fattyAcids))]

#A vs other
type <- ifelse(oilType == "A", "A", "other")
table(type)
dataset <- cbind(type,fattyAcids)
#newDataset <- ROSE(typeC ~ ., data=dataset, seed=123)$data
# check (im)balance of new data
#table(newDataset$typeC)
trainIndex <- createDataPartition(type, p = 0.75,list = FALSE,times = 1)
trainX <-dataset[trainIndex,]
testX <- dataset[-trainIndex,]
trainY <- as.factor(type[trainIndex])
testY <- as.factor(type[-trainIndex])

ctrl <- trainControl(method = "LGOCV",summaryFunction = twoClassSummary,classProbs = TRUE,savePredictions = TRUE)
glmModel <- train(trainX,y = trainY,method = "glm",metric = "ROC",trControl = ctrl)
glmModel
glmPred <- predict(glmModel,testX)
confusionMatrix(data=glmPred,reference = testY)


#B vs other
type <- ifelse(oilType == "B", "B", "other")
table(type)
dataset <- cbind(type,fattyAcids)
#newDataset <- ROSE(typeC ~ ., data=dataset, seed=123)$data
# check (im)balance of new data
#table(newDataset$typeC)
trainIndex <- createDataPartition(type, p = 0.75,list = FALSE,times = 1)
trainX <-dataset[trainIndex,]
testX <- dataset[-trainIndex,]
trainY <- as.factor(type[trainIndex])
testY <- as.factor(type[-trainIndex])

ctrl <- trainControl(method = "LGOCV",summaryFunction = twoClassSummary,classProbs = TRUE,savePredictions = TRUE)
glmModel <- train(trainX,y = trainY,method = "glm",metric = "ROC",trControl = ctrl)
glmModel
glmPred <- predict(glmModel,testX)
confusionMatrix(data=glmPred,reference = testY)


#C vs other
type <- ifelse(oilType == "C", "C", "other")
table(type)
dataset <- cbind(type,fattyAcids)
#newDataset <- ROSE(typeC ~ ., data=dataset, seed=123)$data
# check (im)balance of new data
#table(newDataset$typeC)
trainIndex <- createDataPartition(type, p = 0.75,list = FALSE,times = 1)
trainX <-dataset[trainIndex,]
testX <- dataset[-trainIndex,]
trainY <- as.factor(type[trainIndex])
testY <- as.factor(type[-trainIndex])
table(trainY)
ctrl <- trainControl(method = "LGOCV",summaryFunction = twoClassSummary,classProbs = TRUE,savePredictions = TRUE)
glmModel <- train(trainX,y = trainY,method = "glm",metric = "ROC",trControl = ctrl)
glmModel
glmPred <- predict(glmModel,testX)
confusionMatrix(data=glmPred,reference = testY)

#D vs other
type <- ifelse(oilType == "D", "D", "other")
table(type)
dataset <- cbind(type,fattyAcids)
#newDataset <- ROSE(typeC ~ ., data=dataset, seed=123)$data
# check (im)balance of new data
#table(newDataset$typeC)
trainIndex <- createDataPartition(type, p = 0.75,list = FALSE,times = 1)
trainX <-dataset[trainIndex,]
testX <- dataset[-trainIndex,]
trainY <- as.factor(type[trainIndex])
testY <- as.factor(type[-trainIndex])

ctrl <- trainControl(method = "LGOCV",summaryFunction = twoClassSummary,classProbs = TRUE,savePredictions = TRUE)
glmModel <- train(trainX,y = trainY,method = "glm",metric = "ROC",trControl = ctrl)
glmModel
glmPred <- predict(glmModel,testX)
confusionMatrix(data=glmPred,reference = testY)

#E vs other
type <- ifelse(oilType == "E", "E", "other")
table(type)
dataset <- cbind(type,fattyAcids)
#newDataset <- ROSE(typeC ~ ., data=dataset, seed=123)$data
# check (im)balance of new data
#table(newDataset$typeC)
trainIndex <- createDataPartition(type, p = 0.75,list = FALSE,times = 1)
trainX <-dataset[trainIndex,]
testX <- dataset[-trainIndex,]
trainY <- as.factor(type[trainIndex])
testY <- as.factor(type[-trainIndex])

ctrl <- trainControl(method = "LGOCV",summaryFunction = twoClassSummary,classProbs = TRUE,savePredictions = TRUE)
glmModel <- train(trainX,y = trainY,method = "glm",metric = "ROC",trControl = ctrl)
glmModel
glmPred <- predict(glmModel,testX)
confusionMatrix(data=glmPred,reference = testY)

#A vs other
type <- ifelse(oilType == "F", "F", "other")
table(type)
dataset <- cbind(type,fattyAcids)
#newDataset <- ROSE(typeC ~ ., data=dataset, seed=123)$data
# check (im)balance of new data
#table(newDataset$typeC)
trainIndex <- createDataPartition(type, p = 0.75,list = FALSE,times = 1)
trainX <-dataset[trainIndex,]
testX <- dataset[-trainIndex,]
trainY <- as.factor(type[trainIndex])
testY <- as.factor(type[-trainIndex])

ctrl <- trainControl(method = "LGOCV",summaryFunction = twoClassSummary,classProbs = TRUE,savePredictions = TRUE)
glmModel <- train(trainX,y = trainY,method = "glm",metric = "ROC",trControl = ctrl)
glmModel
glmPred <- predict(glmModel,testX)
confusionMatrix(data=glmPred,reference = testY)

#A vs other
type <- ifelse(oilType == "G", "G", "other")
table(type)
#trainIndex <- createDataPartition(type, p = 0.75,list = FALSE,times = 1)
trainX <-dataset[trainIndex,]
testX <- dataset[-trainIndex,]
trainY <- as.factor(type[trainIndex])
testY <- as.factor(type[-trainIndex])

ctrl <- trainControl(method = "LGOCV",summaryFunction = twoClassSummary,classProbs = TRUE,savePredictions = TRUE)
glmModel <- train(fattyAcids,y = type,method = "glm",metric = "ROC",trControl = ctrl)
glmModel
glmPred <- predict(glmModel,testX)
confusionMatrix(data=glmPred,reference = testY)


#------------lda--------
rm(list=ls())
library(caret)
data(oil)
trainIndex <- createDataPartition(oilType, p = 0.75,list = FALSE,times = 1)
trainX <-fattyAcids[trainIndex,]
trainY <- as.factor(oilType[trainIndex])
testX<-fattyAcids[-trainIndex,]
testY <-as.factor(oilType[-trainIndex])



ldaModel<- train(x = trainX,y = trainY,method = "lda2",metric="kappa")
ldaModel
predLda<-predict(ldaModel,testX)
confusionMatrix(data=predLda,reference = testY)

#--------------pls

plsModel<- train(x = trainX,y = trainY,method = "pls",metric="kappa")
plsModel
predpls<-predict(plsModel,testX)
confusionMatrix(data=predpls,reference = testY)



#------------penalizes--
glmnGrid <- expand.grid(.alpha = c(0, .1, .2, .4, .6, .8, 1),.lambda=seq(0.01,0.2,length=40))
lmnTuned <- train(trainX,y = trainY,method = "glmnet",  tuneGrid = glmnGrid,metric = "kappa",family="multinomial")
lmnTuned
predlmn<-predict(lmnTuned,testX)
confusionMatrix(data=predlmn,reference = testY)

#----------Nearest Shrunken sentroids
library(pamr)
nscGrid <- data.frame(.threshold = 0:25)
nscTuned <- train(x = trainX,
                  y = trainY,
                  method = "pam",
                  tuneGrid = nscGrid,
                  metric = "kappa")

nscTuned

prednsc<-predict(nscTuned,testX)
confusionMatrix(data=prednsc,reference = testY)


