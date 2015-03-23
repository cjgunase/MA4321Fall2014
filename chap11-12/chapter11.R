############## chapter 11 ##############

## will use packeges: AppliedPredictiveModeling, caret, klaR, MASS, pROC, and
## randomForest 
install.packages(c("klaR", "pROC", "randomForest"))

## nnet package can be used to creat neural networks

## For illustration, the simulated data set shown in Fig. 11.1 
## will be used in this section. To create these data, 
## the quadBoundaryFunc function in the AppliedPredictiveModeling
## package is used to generate the predictors and outcomes:

library(AppliedPredictiveModeling)
set.seed(975)
simulatedTrain <- quadBoundaryFunc(500)
simulatedTest <- quadBoundaryFunc(1000)
head(simulatedTrain)

## The random forest and quadratic discriminant models will be fit to 
## the data:
library(randomForest)
rfModel <- randomForest(class ~ X1 + X2,
                        data = simulatedTrain,
                        ntree = 2000)
library(MASS) ## for the qda() function
qdaModel <- qda(class ~ X1 + X2, data = simulatedTrain)

## The output of the predict function for qda objects includes 
## both the predicted classes (in a slot called class) and 
## the associated probabilities are in a matrix called posterior.

qdaTrainPred <- predict(qdaModel, simulatedTrain)
names(qdaTrainPred)

head(qdaTrainPred$class)

head(qdaTrainPred$posterior)

qdaTestPred <- predict(qdaModel, simulatedTest)
simulatedTrain$QDAprob <- qdaTrainPred$posterior[,"Class1"]
simulatedTest$QDAprob <- qdaTestPred$posterior[,"Class1"]

## The random forest model requires two calls to the predict function 
## to get the predicted classes and the class probabilities:

rfTestPred <- predict(rfModel, simulatedTest, type = "prob")
head(rfTestPred)

simulatedTest$RFprob <- rfTestPred[,"Class1"]
simulatedTest$RFclass <- predict(rfModel, simulatedTest)

####### Sensitivity and Specificity ###########
## caret has functions for computing sensitivity and specificity. 
## These functions require the user to indicate the role of each
## of the classes:

# Class 1 will be used as the event of interest
library(caret)
sensitivity(data = simulatedTest$RFclass,
           reference = simulatedTest$class,
           positive = "Class1")

specificity(data = simulatedTest$RFclass,
            reference = simulatedTest$class,
            negative = "Class2")

## Predictive values can also be computed either by
## using the prevalence found
## in the data set (46%) or by using prior judgement:
posPredValue(data = simulatedTest$RFclass,
             reference = simulatedTest$class,
             positive = "Class1")

negPredValue(data = simulatedTest$RFclass,
            reference = simulatedTest$class,
            positive = "Class2")

# Change the prevalence manually
posPredValue(data = simulatedTest$RFclass,
             reference = simulatedTest$class,
             positive = "Class1",
             prevalence = .9)

########### Confusion Matrix ###########

## There are several functions in R to create the confusion matrix. 
## The confusionMatrix function in the caret package produces 
## the table and associated statistics:

confusionMatrix(data = simulatedTest$RFclass,
                reference = simulatedTest$class,
                positive = "Class1")

########## Receiver Operating Characteristic Curves #########

## The pROC package (Robin et al. 2011) can create the curve and 
## derive various statistics. First, an R object must be created 
## that contains the relevant information using the pROC function roc. 
## The resulting object is then used to generate the ROC curve 
## or calculate the area under the curve.

library(pROC)
rocCurve <- roc(response = simulatedTest$class,
                predictor = simulatedTest$RFprob,
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

## By default, the x-axis goes backwards, used
## the option legacy.axes = TRUE to get 1-spec
## on the x-axis moving from 0 to 1

## Also, another curve can be added using
## add = TRUE the next time plot.auc is used.

########## Lift Charts ##########

## The lift curve can be created using the lift function in the caret 
## package. It takes a formula as the input where the true class is 
## on the left-hand side of the formula, and one or more columns for model
## class probabilities are on the right. 

## For example, to produce a lift plot for the random forest and QDA
## test set probabilities,

labs <- c(RFprob = "Random Forest",
          QDAprob = "Quadratic Discriminant Analysis")
liftCurve <- lift(class ~ RFprob + QDAprob, data = simulatedTest,
                  labels = labs)
liftCurve

## To plot two lift curves, the xyplot function is used to create a 
## lattice plot:
## Add lattice options to produce a legend on top
xyplot(liftCurve,
       auto.key = list(columns = 2,
       lines = TRUE,
       points = FALSE))

########### Calibrating Probabilities #########
## Calibration plots as described above are available in the 
## calibration.plot function in the PresenceAbsence package and
## in the caret function calibration
## The syntax for the calibration function is similar to the lift
## function:
calCurve <- calibration(class ~ RFprob + QDAprob, data = simulatedTest)
calCurve
xyplot(calCurve, auto.key = list(columns = 2))

## To recalibrate the QDA probabilities, a post-processing model
## is created that models the true outcome as a function of the 
## class probability. 
## To fit a sigmoidal function, a logistic regression model is used 
## via the glm function in base R. 
## To fit the model, the function requires the
## family argument to specify the type of outcome data being modeled. 
## Since our outcome is a discrete category, the binomial distribution 
## is selected:
## The glm() function models the probability of the second factor
## level, so the function relevel() is used to temporarily reverse the
## factors levels.
sigmoidalCal <- glm(relevel(class, ref = "Class2") ~ QDAprob,
                    data = simulatedTrain,
                    family = binomial)
coef(summary(sigmoidalCal))

## The corrected probabilities can be found using the predict
## function
sigmoidProbs <- predict(sigmoidalCal,
                        newdata = simulatedTest[,"QDAprob", drop = FALSE],
                        type = "response")
simulatedTest$QDAsigmoid <- sigmoidProbs

## the naive Bayes model function NaiveBayes in the klaR
## package can be used for the computations:

BayesCal <- NaiveBayes(class ~ QDAprob, data = simulatedTrain,
                       usekernel = TRUE)
## Like qda(), the predict function for this model creates
## both the classes and the probabilities
BayesProbs <- predict(BayesCal,
                      newdata = simulatedTest[, "QDAprob", drop = FALSE])
simulatedTest$QDABayes <- BayesProbs$posterior[, "Class1"]
## The probability values before and after calibration
head(simulatedTest[, c(5:6, 8, 9)])

## These new probabilities are evaluated using another plot:
calCurve2 <- calibration(class ~ QDAprob + QDABayes + QDAsigmoid,
                         data = simulatedTest)
xyplot(calCurve2)




































































