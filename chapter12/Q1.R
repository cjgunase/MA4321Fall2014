rm(list=ls())
data(hepatic)
barplot(table(injury))
nzro <- nearZeroVar(bio)
length(nzro)
dim(bio[,-nzro])
filteredbio <- bio[,-nzro]
filteredbio <- filteredbio[,-findCorrelation(cor(filteredbio))]
comboInfo <- findLinearCombos(filteredbio)
comboInfo$remove
# No linear combinations
preProcValues <- preProcess(filteredbio, method = c("center", "scale"))
Transformedbio <- predict(preProcValues, filteredbio)
trainIndex <- createDataPartition(injury, p = .8,list = FALSE,times = 1)

trainX<-Transformedbio[trainIndex,]
trainY<-factor(injury[trainIndex])
testX<-Transformedbio[-trainIndex,]
testY<-factor(injury[-trainIndex])