#chapter 6
rm(list = ls())
library(AppliedPredictiveModeling)
data(solubility)
library(caret) 
trainingData <- solTrainXtrans
## Add the solubility outcome
trainingData$Solubility <- solTrainY

lmFitAllPredictors <- lm(Solubility ~ ., data = trainingData)

summary(lmFitAllPredictors)
#-----------------------------------formula interface-------------------
lmPred1 <- predict(lmFitAllPredictors, solTestXtrans)
head(lmPred1)

lmValues1 <- data.frame(obs = solTestY, pred = lmPred1)
defaultSummary(lmValues1)
#-----------------------------lm using---Non Formula Interface-----------------
ctrl <- trainControl(method = "cv", number = 10)
set.seed(100)
lmFit1 <- train(x = solTrainXtrans, y = solTrainY,method = "lm", trControl = ctrl)



xyplot(solTrainY ~ predict(lmFit1),
          ## plot the points (type = 'p') and a background grid ('g')
          type = c("p", "g"),
          xlab = "Predicted", ylab = "Observed")

xyplot(resid(lmFit1) ~ predict(lmFit1),
        type = c("p", "g"),
        xlab = "Predicted", ylab = "Residuals")


corThresh <- .9
tooHigh <- findCorrelation(cor(solTrainXtrans), corThresh)
corrPred <- names(solTrainXtrans)[tooHigh]
trainXfiltered <- solTrainXtrans[, -tooHigh]

testXfiltered <- solTestXtrans[, -tooHigh]
set.seed(100)
lmFiltered <- train(solTrainXtrans, solTrainY, method = "lm",
                    trControl = ctrl)
lmFiltered


#-----------------------------#Using Robust Linear Regression-------------
library(MASS)
rlmFitAllPredictors <- rlm(Solubility ~ ., data = trainingData)
set.seed(100)
rlmPCA <- train(solTrainXtrans, solTrainY,
                   method = "rlm",
                   preProcess = "pca",
                   trControl = ctrl)
rlmPCA
#------------------------------PLS-------------------------------------------
#http://staff.ustc.edu.cn/~zwp/teach/Reg/Lec8.R
library(pls)
plsFit <-plsr(Solubility~.,data=trainingData,50,validation="CV")
summary(plsFit)
plot(RMSEP(plsFit),legendpos="topright")
plsg2 <-plsr(Solubility~.,data=trainingData,6,validation="CV")
plot(plsg2, ncomp =6, asp = 1, line = TRUE)

rmse <- function (x, y) sqrt (mean ( (x-y)^2 ) )
ypred<-predict(plsg2, ncomp = 6, newdata = trainingData)
ypred <- as.data.frame(ypred)
ypred[,1]
trainingData$Solubility
rmse (ypred,trainingData$Solubility)
lmValues1 <- data.frame(obs =trainingData$Solubility, pred = ypred[,1])
lmValues1
defaultSummary(lmValues1)

ypred<-predict(plsg2, ncomp = 6, newdata = solTestXtrans)
rmse (ypred,solTestY)

