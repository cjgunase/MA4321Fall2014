rm(list = ls())
Data <- read.csv("./letter-recognition.data",sep=",",header=FALSE)
#Class Frequency Distribution
y <- Data[,1]
barplot(table(y),xlab="Class",col="purple",main="ClassFrequcy Distribution")
library(corrplot)
library(caret)
x <- Data[,-1]
#Correlations between predictors.
correlations <-cor(x)
corrplot(correlations, order = "hclust")
nearZeroVar(x)
