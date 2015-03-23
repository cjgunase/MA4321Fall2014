rm(list = ls())
set.seed(100)
x <- runif(100, min = 2, max = 10)
y <- sin(x) + rnorm(length(x)) * .25
sinData <- data.frame(x = x, y = y)


plot(x, y,pch=16)
## Create a grid of x values to use for prediction
dataGrid <- data.frame(x = seq(2, 10, length = 100))

#library(kernlab)
plot(x, y,pch=16,main="C=0.1, epsilon=0.1")
rbfSVM <- ksvm(x = x, y = y, data = sinData,
                 kernel ="rbfdot", kpar = "automatic",
                 C = 0.1, epsilon = 0.1)
modelPrediction <- predict(rbfSVM, newdata = dataGrid)
 ## This is a matrix with one column. We can plot the
 ## model predictions by adding points to the previous plot
points(x = dataGrid$x, y = modelPrediction[,1],type = "l", col = "black",lwd="3")

plot(x, y,pch=16,main="C=1, epsilon=0.1")
rbfSVM <- ksvm(x = x, y = y, data = sinData,
               kernel ="rbfdot", kpar = "automatic",
               C = 1, epsilon = 0.1)
modelPrediction <- predict(rbfSVM, newdata = dataGrid)
## This is a matrix with one column. We can plot the
## model predictions by adding points to the previous plot
points(x = dataGrid$x, y = modelPrediction[,1],type = "l", col = "blue",lwd="3")

plot(x, y,pch=16,main="C=10, epsilon=0.1")
rbfSVM <- ksvm(x = x, y = y, data = sinData,
               kernel ="rbfdot", kpar = "automatic",
               C = 10, epsilon = 0.1)
modelPrediction <- predict(rbfSVM, newdata = dataGrid)
## This is a matrix with one column. We can plot the
## model predictions by adding points to the previous plot
points(x = dataGrid$x, y = modelPrediction[,1],type = "l", col = "red",lwd="3")

plot(x, y,pch=16,main="C=100, epsilon=0.1")
rbfSVM <- ksvm(x = x, y = y, data = sinData,
               kernel ="rbfdot", kpar = "automatic",
               C = 100, epsilon = 0.1)
modelPrediction <- predict(rbfSVM, newdata = dataGrid)
## This is a matrix with one column. We can plot the
## model predictions by adding points to the previous plot
points(x = dataGrid$x, y = modelPrediction[,1],type = "l", col = "green",lwd="3")

plot(x, y,pch=16,main="C=1000, epsilon=0.1")
rbfSVM <- ksvm(x = x, y = y, data = sinData,
               kernel ="rbfdot", kpar = "automatic",
               C = 1000, epsilon = 0.1)
modelPrediction <- predict(rbfSVM, newdata = dataGrid)
## This is a matrix with one column. We can plot the
## model predictions by adding points to the previous plot
points(x = dataGrid$x, y = modelPrediction[,1],type = "l", col = "orange",lwd="3")





