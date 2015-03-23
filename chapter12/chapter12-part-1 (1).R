############## chapter 12 ##############

## will use packeges: AppliedPredictiveModeling,
## caret, glmnet, MASS, pamr, pls, pROC, rms, sparseLDA, and subselect

install.packages(c("glmnet", "pamr", "rms", "sparseLDA", "subselect"))

library(AppliedPredictiveModeling)
library(MASS)
## Simulate 4 predictors and a binary outcome (class 1 and class 2).
## X1 and X2 are related with y; cov(X1, X2)=sigma; X3 and X4 are noise;
## X3 is continouse and X4 is binary.

library(MASS)
set.seed(975)
quadFunc=function (n, sigma, pp) 
{
    sigma <- matrix(c(1, sigma, sigma, 2), 2, 2)
    tmpData <- data.frame(mvrnorm(n = n, c(1, 0), sigma), 
                          X3=rnorm(n), X4=sample(0:1, size=n, replace=TRUE,
                          prob=c(1-pp,pp)))
    xSeq <- seq(-4, 4, length = 40)
    plotGrid <- expand.grid(x = xSeq, y = xSeq)
    zFoo <- function(x, y) -1 - 2 * x - 0 * y - 0.2 * x^2 + 2 * 
        y^2
    z2p <- function(x) 1/(1 + exp(-x))
    tmpData$prob <- z2p(zFoo(tmpData$X1, tmpData$X2))
    tmpData$class <- factor(ifelse(runif(length(tmpData$prob)) <= 
        tmpData$prob, "Class1", "Class2"))
    tmpData
}
simulatedTrain <- quadFunc(500, 0.7, 0.1)
simulatedTest <- quadFunc(1000, 0.7, 0.1)
head(simulatedTrain)

############ Logistic Regression ###############

## The glm function (for GLMs) in base R is commonly used to fit 
## logistic regression models. The syntax is

levels(simulatedTrain$class)

modelFit <- glm(class ~ X1,
                data = simulatedTrain, 
                ## 'family' relates to the distribution of the data.
                ## A value of 'binomial' is used for logistic regression
                family = binomial)
modelFit
summary(modelFit)

## The glm function treats the second factor level as the event of interest
## To get the probability of a successful grant, we subtract from one:

successTrainProb <- 1 - predict(modelFit,
                           ## Predict for several days
                           newdata = simulatedTrain,
                           ## glm does not predict the class, but can
                           ## produce the probability of the event
                           type = "response"
                           )
round(successTrainProb,3)
head(successTrainProb)
predictTrainClass <- factor(ifelse(successTrainProb >=0.5, "Class1", "Class2"))
head(predictClass)

sum(predictTrainClass==simulatedTrain$class)/500 ## accuracy rate on training


library(caret)
sensitivity(data = predictTrainClass,
           reference = simulatedTrain$class,
           positive = "Class1")

specificity(data = predictTrainClass,
            reference = simulatedTrain$class,
            negative = "Class2")

confusionMatrix(data =predictTrainClass,
                reference = simulatedTrain$class,
                positive = "Class1")


library(pROC)
rocCurve <- roc(response = simulatedTrain$class,
                predictor = successTrainProb,
                ## This function assumes that the second
                ## class is the event of interest, so we
                ## reverse the labels.
                levels = rev(levels(simulatedTest$class)))

## From this object, we can produce statistics (such as the area 
## under the ROC curve and its confidence interval):

auc(rocCurve)
ci.roc(rocCurve)

## We can also use the plot function to produce the ROC curve itself:

plot(rocCurve, legacy.axes = TRUE)

## To add the nonlinear term for the day of the year,

daySquaredModel <- glm(class ~ X1+X2+X3+X4+I((X1)^2),
                       data = simulatedTrain,
                       family = binomial)
daySquaredModel
summary(daySquaredModel)

## The glm function does not have a non-formula method, so creating models
## with a large number of predictors takes a little more work.
## An alternate solution is shown below.

## Another R function for logistic model is in the package called rms 
## (for Regression Modeling Strategies). The lrm function
## is very similar to glm 
## For a large set of predictors, the formula method for specifying models can
## be cumbersome. As in previous chapters, the train function can efficiently
## fit and validate models. For logistic regression, train provides an interface
## to the glm function that bypasses a model formula, directly produces class
## predictions, and calculates the area under the ROC curve and other metrics.

library(caret)
set.seed(476)
ctrl <- trainControl(summaryFunction = twoClassSummary,
                     classProbs = TRUE)

ctrl <- trainControl(method = "LGOCV",
                     summaryFunction = twoClassSummary,
                     classProbs = TRUE,
                     ##index = list(simulatedTest[,1:4]),
                     savePredictions = TRUE)

lrFull <- train(simulatedTrain[,1:4],
                y = simulatedTrain$class,
                method = "glm",
                metric = "ROC",
                trControl = ctrl)
lrFull


## The predictions for the holdout set (of year
## 2008 grants) is contained in the sub-object pred

head(lrFull$pred)

confusionMatrix(data = lrFull$pred$pred,
               reference = lrFull$pred$obs)
## The ROC curve can also be computed and plotted using the pROC package:
FullRoc <- roc(response = lrFull$pred$obs,
                  predictor = lrFull$pred$Class1,
                  levels = rev(levels(lrFull$pred$obs)))
plot(FullRoc, legacy.axes = TRUE)
auc(FullRoc)


############ Linear Discriminant Analysis #############

## A popular function for creating LDA models is lda in the MASS package. The
## input to this function can either be a formula and data frame or a matrix
## of predictors and a grouping variable as a factor which contains the class
## membership information. 

## We can fit the LDA model as follows:
## Simulate Data Set

set.seed(975)
quadFunc=function (n, sigma, pp) 
{
    sigma <- matrix(c(1, sigma, sigma, 2), 2, 2)
    tmpData <- data.frame(mvrnorm(n = n, c(1, 0), sigma), 
                          X3=rnorm(n), X4=sample(0:1, size=n, replace=TRUE,
                          prob=c(1-pp,pp)))
    xSeq <- seq(-4, 4, length = 40)
    plotGrid <- expand.grid(x = xSeq, y = xSeq)
    zFoo <- function(x, y) -1 - 2 * x - 0 * y - 0.2 * x^2 + 2 * 
        y^2
    z2p <- function(x) 1/(1 + exp(-x))
    tmpData$prob <- z2p(zFoo(tmpData$X1, tmpData$X2))
    tmpData$class <- factor(ifelse(runif(length(tmpData$prob)) <= 
        tmpData$prob, "Class1", "Class2"))
    tmpData
}
simulatedTrain <- quadFunc(500, 0.7, 0.1)
simulatedTest <- quadFunc(1000, 0.7, 0.1)
head(simulatedTrain)

library(MASS)

ldaModel <- lda(x = simulatedTrain[,1:4],
                grouping = simulatedTrain$class)

## For the simulated training data test set, the predictions are produced
## with the syntax:
ldaTrainPredictions <- predict(ldaModel,simulatedTrain[,1:4])
head(ldaTrainPredictions$class)
head(ldaTrainPredictions$posterior)

############## Partial Least Squares Discriminant Analysis ###############

## PLSDA can be performed using the plsr function within the pls package by
## using a categorical matrix which defines the response categories.

## The caret package contains a function (plsda) that can create the appropriate
## dummy variable PLS model for the data and then post-process the raw
## model predictions to return class probabilities. The syntax is very similar to
## the regression model code for PLS given in Sect. 6.3. The main difference is
## a factor variable is used for the outcome.
## For example, to fit the model with the reduced predictor set:

library(caret)

plsdaModel <- plsda(x = simulatedTrain[,1:4],
                    y = simulatedTrain$class,
                    ## The data should be on the same scale for PLS. The
                    ## 'scale' option applies this pre-processing step
                    scale = TRUE,
                    ## Use Bayes method to compute the probabilities
                    probMethod = "Bayes",
                    ## Specify the number of components to model
                    ncomp = 4)

## Predict the 2008 hold-out set
plsPred <- predict(plsdaModel,
                   newdata = simulatedTrain[,1:4])
head(plsPred)

plsProbs <- predict(plsdaModel, 
                    newdata = simulatedTrain[,1:4],
                    type = "prob")
head(plsProbs)

## The train function can also be used with PLS in the classification setting.
## The following code evaluates the first ten PLS components with respect to
## the area under the ROC curve as well as automatically centers and scales the
## predictors prior to model fitting and sample prediction:

set.seed(476)
ctrl <- trainControl(summaryFunction = twoClassSummary,
                     classProbs = TRUE)

plsFit2 <- train(x = simulatedTrain[,1:4],
                 y = simulatedTrain$class,
                 method = "pls",
                 tuneGrid = expand.grid(.ncomp = 1:4),
                 preProc = c("center","scale"),
                 metric = "ROC",
                 trControl = ctrl)
plsFit2

## The basic predict call evaluates new samples, and type = "prob" returns
## the class probabilities. Computing variable importance as illustrated in
## Fig. 12.15 can be done with the following code:

plsImpSim <- varImp(plsFit2, scale = FALSE)
plsImpSim

plot(plsImpSim, top = 20, scales = list(y = list(cex = .95)))

############ Penalized Models ##########

## The primary package for penalized logistic regression is glmnet.
## The glmnet function is very similar to the enet function. 
## The main arguments correspond to the data: x is a matrix of predictors and 
## y is a factor of classes (for logistic regression). 
## For two classes, use family="binomial" corresponds to logistic regression, 
## For three or more classes, use family="multinomial" is appropriate.
## glmnet defaults this parameter to alpha = 1, corresponding to a
## complete lasso penalty.

library(glmnet)

glmnetModel <- glmnet(x = as.matrix(simulatedTrain[,1:4]),
                      y = simulatedTrain$class,
                      family = "binomial")

## Compute predictions for three difference levels of regularization.
## Note that the results are not factors

predict(glmnetModel,
        newx = as.matrix(simulatedTrain[,1:4]),
        s = c(0.05, 0.1, 0.2),
        type = "class")

## Tuning the model using the area under the ROC curve can be accomplished
## with train. For the grant data:
## Specify the tuning values:

glmnGrid <- expand.grid(.alpha = c(0, .1, .2, .4, .6, .8, 1),
                        .lambda = seq(.01, .2, length = 40))
set.seed(476)
glmnTuned <- train(x=simulatedTrain[,1:4],
                   y = simulatedTrain$class,
                   method = "glmnet",
                   tuneGrid = glmnGrid,
                   preProc = c("center", "scale"),
                   metric = "ROC",
                   trControl = ctrl)

## The heat map in the top panel of Fig. 12.16 was produced using the code

plot(glmnTuned, plotType = "level")

## Penalized LDA functions can be found in the sparseLDA and PenalizedLDA packages. 
## The main function in the sparseLDA package is called sda. 
## This function has an argument for the ridge parameter called lambda. 
## The lasso penalty can be stated in two possible ways with the argument stop. 
## The magnitude of the lasso penalty is controlled using a positive number 
## (e.g., stop = 0.01) or, alternatively, the
## number of retained predictors can be chosen using a negative integer (e.g.,
## stop = -6 for six predictors). For example:

library(sparseLDA)
sparseLdaModel <- sda(x=simulatedTrain[,1:4],
                      y = simulatedTrain$class,
                      lambda = 0.01,
                      stop = -6)

## The argument method = "sparseLDA" can be used with train. In this case, train
## will tune the model over lambda and the number of retained predictors.

########### Nearest Shrunken Centroids ###########

## The original R implementation for this model is found in the pamr package.

## The function to train the model is pamr.train, which takes the input data in
## a single list object with components x and y.
## The usual convention for data
## sets is to have samples in rows and different columns for the predictors.
## pamr.train requires the training set predictors to be encoded in the opposite
## format where rows are predictors and columns are samples.

## For the simulated
## data, the input data would be in the format shown below:
## Switch dimensions using the t() function to transpose the data.
## This also implicitly converts the training data frame to a matrix.

inputData <- list(x = t(simulatedTrain[,1:4]), y = simulatedTrain$class)

## The basic syntax to create the model is:

library(pamr)
nscModel <- pamr.train(data = inputData)

## By default, the function chooses 30 appropriate shrinkage values to evaluate.
## There are options to use specific values for the shrinkage amount, the
## prior probabilities and other aspects of the model. The function pamr.predict
## generates predictions on new samples as well as determines which specific
## predictors were used in the model for a given shrinkage value. For example,
## to specify a shrinkage value of 5:

exampleData <- t(simulatedTrain[1:5,1:4])
pamr.predict(nscModel, newx = exampleData, threshold = 2)

## Which predictors were used at this threshold? The predict
## function shows the column numbers for the retained predictors.

thresh2Vars <- pamr.predict(nscModel, newx = exampleData,
                             threshold = 2, type = "nonzero")
thresh2Vars

## The package also contains functions for K-fold cross-validation to choose an
## appropriate amount of shrinkage but is restricted to a single type of resampling
## and tunes the model with overall accuracy. The train syntax is:

## We chose the specific range of tuning parameters here:
nscGrid <- data.frame(.threshold = 0:4)
set.seed(476)
nscTuned <- train(x = simulatedTrain[,1:4], 
                  y = simulatedTrain$class,
                  method = "pam",
                  preProc = c("center", "scale"),
                  tuneGrid = nscGrid,
                  metric = "ROC",
                  trControl = ctrl)

## The predict function
## for train does not require the user to manually specify the shrinkage amount
## (the optimal value determined by the function is automatically used).

## The predictors function will list the predictors used in the prediction equation
## (at the optimal threshold determined by train). In the tuned model, 36 
## were selected:

predictors(nscTuned)

## The function varImp will return the variable importance based on the
## distance between the class centroid and the overall centroid:

varImp(nscTuned, scale = FALSE)
























