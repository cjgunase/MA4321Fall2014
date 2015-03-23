rm(list=ls())
library(C50)
library(caret)
data(churn)


#Explore Data
dataset <-rbind(churnTrain,churnTest)
X <- dataset[,-20]
nzro <- nearZeroVar(X)
nzro
X <-X[,-6]

numerics <- X[,-c(2,3)]

library(corrplot)
corrplot(cor(numerics),method="circle")
numerics <- numerics[,-findCorrelation(cor(numerics))]

Y <-dataset[,18]


barplot(table(Y))
trainIndex <- createDataPartition(Y, p = .75,list = FALSE,times = 1)
trainX <-numerics[trainIndex,]
trainY <-Y[trainIndex]
testX <- numerics[-trainIndex,]
testY <- Y[-trainIndex]

ctrl <- trainControl(method = "LGOCV",
                     summaryFunction = twoClassSummary,
                     classProbs = TRUE,
                     savePredictions = TRUE)
#glm
library(caret)
set.seed(476)
glmModel <- train(trainX,y = trainY,method = "glm",metric = "ROC",trControl = ctrl,preProc = c("center", "scale"))
glmModel

glmPred <- predict(glmModel,testX)
confusionMatrix(data=glmPred,reference = testY)
glmModel$pred$obs

Roc <- roc(response = glmModel$pred$obs,
                                       predictor = glmModel$pred$yes,
                                       levels = (levels(glmModel$pred$obs)))
plot(Roc, legacy.axes = TRUE)
auc(Roc)

#LDA
ldaModel<- train(x = trainX,y = trainY,method = "lda",metric = "ROC",trControl = ctrl,preProc = c("center", "scale"))
ldaModel
Roc <- roc(response = ldaModel$pred$obs,
           predictor = ldaModel$pred$yes,
           levels = (levels(ldaModel$pred$obs)))
plot(Roc, legacy.axes = TRUE)
auc(Roc)


#plsda
plsdaModel <- train(x = trainX,y = trainY,
                                     method = "pls",
                                     tuneGrid = expand.grid(.ncomp = 1:2),
                                     preProc = c("center","scale"),
                                     metric = "ROC",
                                     trControl = ctrl)

plsdaModel$pred$obs
Roc <- roc(response = plsdaModel$pred$obs,
           predictor = plsdaModel$pred$yes,
           levels = (levels(plsdaModel$pred$obs)))
plot(Roc, legacy.axes = TRUE)
auc(Roc)

plsImpGrant <- varImp(plsdaModel, scale = FALSE)
plsImpGrant

#penalized
glmnGrid <- expand.grid(.alpha = c(0, .1, .2, .4, .6, .8, 1),.lambda=seq(0.01,0.2,length=40))
lmnTuned <- train(trainX,y = trainY,method = "glmnet",  tuneGrid = glmnGrid,metric = "ROC",trControl = ctrl,preProc = c("center", "scale"))
lmnTuned
Roc <- roc(response = lmnTuned$pred$obs,
           predictor = lmnTuned$pred$yes,
           levels = (levels(lmnTuned$pred$obs)))
plot(Roc, legacy.axes = TRUE)
auc(Roc)


#NSC

library(pamr)
nscGrid <- data.frame(.threshold = 0:25)
nscTuned <- train(x = trainX,
                                       y = trainY,
                                       method = "pam",
                                       preProc = c("center", "scale"),
                                       tuneGrid = nscGrid,
                                       metric = "ROC",
                                       trControl = ctrl)


nscTuned
predictors(nscTuned)
varImp(nscTuned, scale = FALSE)



