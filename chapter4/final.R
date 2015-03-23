---
title: "Applied Predictive Modeling Ex4"
author: "Chathura J Gunasekara"
date: "Monday, September 29, 2014"
output: word_document
---
4.1
(a).
In this situation, since the sample size is large it is possible to 
set aside a testing data set and training data set.so 
1.simple random spliting ,
2.Stratified random sampling
3.maximum dissimilarity sampling 

can be used. But because the dispropotionality of the classes random sampling should be avoided to prevent creating biased test and train sets.

```{r,echo=FALSE}
music<- read.csv("genresTrain.csv")
attach(music)
library(AppliedPredictiveModeling)
barplot(table(music[192]),main="Frequency Distribution of classes")
library(caret)
library(proxy)
```

(b).
Code for implementing :

1. Using "createDataPartition" function in Caret package.
```{r,echo=FALSE}
set.seed(1);trainingRows <- createDataPartition(music$GENRE,p=0.80,list=FALSE)
head(trainingRows)
train_music<-music[trainingRows,]
train_classes<-GENRE[trainingRows]
test_music <-music[-trainingRows,]
test_classes <- GENRE[-trainingRows]
str(test_classes)


```
2.implementting maximum dissimilarity sampling in caret package. The data will be split on the basis of the predictor values.
```{r, echo=TRUE}

## A random sample of 5 data points
startSet <- sample(1:dim(train_music)[1], 5)
samplePool <- train_music[-startSet, ]
start <- train_music[startSet, ]
newSamp <- maxDissim(start, samplePool, n = 4)
str(newSamp)

```
4.2.
(a)
This data set is small and can not find a frequency distribution because there is no classes in this data set.So it not possible to do stratified sampling or random sampling.
so in this case,
1. Bootstrapping
2. k - fold cross validation 
3.Repeated cross validation should be used.
```{r,echo=FALSE}
library(AppliedPredictiveModeling)
data(permeability)
per_data <-cbind(permeability,fingerprints)
str(per_data)

```
(b).

1.Code for implementation :Repeated stratified sampling
```{r,echo=TRUE}
library(caret)
set.seed(1)
repeatSplits <-createDataPartition(permeability,p=0.8,times=3)
str(repeatSplits)
```
2. Code for implementation :10 fold cross validation.
```{r,echo=TRUE}
set.seed(1)
cvSplits <-createFolds(permeability,k=10,returnTrain=TRUE)

str(cvSplits)
```

4.3.
This data set contains information about a chemical manufacturing process,
in which the goal is to understand the relationship between the process and
the resulting final product yield. The data set consisted of
177 samples of biological material for which 57 characteristics were measured.

(a). A parsimonious model is a model that accomplishes a desired level of explanation or prediction with as few predictor variables as possible.

Follwing plot shows the R-squared and number of PLS components in the model. 

```{r,echo=FALSE}

data(ChemicalManufacturingProcess)
Rsqared <- cbind((1:10),c(0.444,0.5,0.533,0.545,0.542,0.537,0.534,0.534,0.520,0.507))
std.err <- c(0.0272,0.0298,0.0302,0.0308,0.0322,0.0327,0.0333,0.0330,0.0326,0.0324)
plot(Rsqared,xlab="PLS Components",ylab="Resampled R-Squared",main="R-sqred vs No. of components")
lines(Rsqared)
```
Models with components 1 to 4 increase R squared. then after 4 it decreases becasue over fitting.
Numercically optimal value is 0.545 its SD is 0.0308
onestandar_error = 0.545-0.0308 
```{r,echo=TRUE}
0.545-0.0308



print(cbind(Rsqared,std.err))
```
0.533 > 0.5142 , which is within one standard deviation
so Number of PLS Components is enough to model is 3.


b)Computing the toerance values.
```{r,echo=FALSE}
mean <- c(0.444,0.5,0.533,0.545,0.542,0.537,0.534,0.534,0.520,0.507)
tol <- ((mean-0.545)/0.545)*100

table<-data.frame(Rsqared,tol,check.rows=TRUE)
names(table) <- c("PLS Comp","R-Sq","Tol")


```
Optimal number of PLS components if 10% loss in R-squared acceptable is 2

c)
SVM  : 
R Squared is higher in SVM and Random Forests Not much defferece in them, but predictin time is way high for Random forests.so in concering R squared SVM is better.
d)

consider using the simplest model that reasonably approximates the performance of more complex methods with a acceptable prediction time. So in that perspective KNN is better.

4.4
```{r,echo=FALSE}
rm(list = ls())
library(caret)
data(oil)
str(oilType)
barplot(table(oilType),main="oilType")
```
(a)Using Sample function to create a random sample of 60 oils.
```{r,echo=FALSE}
barplot(table(oilType[sample((1:96),60,replace=TRUE,prob=NULL )]),main="oilType in random60 using sample function -01")
barplot(table(oilType[sample((1:96),60,replace=TRUE,prob=NULL )]),main="oilType in random60 using sample function -02")
barplot(table(oilType[sample((1:96),60,replace=TRUE,prob=NULL )]),main="oilType in random60 using sample function -03")
```
Base on the above figures it is observable that the variation in the random sampling. Sometimes very few obeservations of a class can be selected and sometimes an entire class may be not selected.When one class has a disproportionately
small frequency compared to the others, there is a chance that
the distribution of the outcomes may be substantially different between the
training and test sets.
(b)
```{r,echo=FALSE}
x <- createDataPartition(oilType,times=1,p=0.66)
barplot(table(oilType),main="oilType")
barplot(table(oilType[x$Resample1]),main="oilType training set using createDataPartition function - 01")
barplot(table(oilType[x$Resample1]),main="oilType training set using createDataPartition function - 02")

```
This graph does not change as random sample and it preserve the frequency distribution of the original sample In this way, there is a higher likelihood that the outcome distributions will
match.

(c)
A test set should be avoided because the sample size is small , random test set may not be enough give sufficient power or precision to make a judgement. So validation using a single test set can be a poor choise.
Best to use resampling methods such as 
Cross validation which evaluates many alternate versions of the data.
k-Fold Cross-Validation
Repeated Training/Test Splits
The Bootstrap

(d)
Different sample sizes
```{r,echo=FALSE}
library("plotrix")
x <- c(1:4)
F <- c(0.8,0.8,0.8,0.8) 
L <- c(binom.test(8,10)$conf.int[1],binom.test(16,20)$conf.int[1],binom.test(160,200)$conf.int[1],binom.test(1600,2000)$conf.int[1])
U <- c(binom.test(8,10)$conf.int[2],binom.test(16,20)$conf.int[2],binom.test(160,200)$conf.int[2],binom.test(1600,2000)$conf.int[2])
plotCI(x, F, ui=U, li=L,xlab="Different sample sizes",ylab="Prob. of success with CI")
```
When test set size increase uncertainity decrease.


Different accuracy rates
```{r,echo=FALSE}
x <- c(1:4)
F <- c(0.8,0.8,0.8,0.8) 
L <- c(binom.test(16,20,conf.level=.99)$conf.int[1],binom.test(16,20,conf.level=.95)$conf.int[1],binom.test(16,20,conf.level=.9)$conf.int[1],binom.test(16,20,conf.level=.85)$conf.int[1])
U <- c(binom.test(16,20,conf.level=.99)$conf.int[2],binom.test(16,20,conf.level=.95)$conf.int[2],binom.test(16,20,conf.level=.9)$conf.int[2],binom.test(16,20,conf.level=.85)$conf.int[2])
plotCI(x, F, ui=U, li=L,xlab="Different Accuracy rates",ylab="Prob. of success with CI")
```
when accuracy rate is decreases is doesn't make a huge difference in uncertainity.





