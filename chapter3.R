######## chapter 3 ###########

#install.packages(c("caret", "corrplot", "e1071", "lattice", "AppliedPredictiveModeling"))
library(AppliedPredictiveModeling)

## The raw segmentation data set is contained in the AppliedPredictiveModeling
## package.5 To load the data set into R:
data(segmentationOriginal) 

dim(segmentationOriginal)
segmentationOriginal
dim <-segmentationOriginal[1:20,1:20]
name_vec <-names(segmentationOriginal)
names(dim)


## There were fields that identified each cell (called Cell) 
## and a factor vector that indicated which cells were well segmented (Class). 
## The variable Case indicated which cells were originally used for the training and test sets.

## The analysis in this chapter focused on the training set samples, so the data are
## filtered for these cells:
segData <- subset(segmentationOriginal, Case == "Train") 
dim(segData)
segData[1:20, 1:20]
names(segData)

## The Class and Cell fields will be saved into separate vectors, then removed
## from the main object:
cellID <- segData$Cell
class <- segData$Class
case <- segData$Case

# Now remove the columns
segData <- segData[, -(1:3)]

## The original data contained several “status” columns which were binary versions
## of the predictors. To remove these, we find the column names containing
## "Status" and remove them:
statusColNum <- grep("Status", names(segData))
statusColNum   ###segData[,5],...

segData <- segData[, -statusColNum]
dim(segData)
names(segData)
#### Transformations #########

## The skewness function in the e1071 package calculates the sample skewness statistic
## for each predictor:
library(e1071)
grep("AngleCh1",segData)
# For one predictor:
skewness(segData$AngleCh1)
#segData$AngleCh1
# Since all the predictors are numeric columns, the apply function can
# be used to compute the skewness across columns.
skewValues <- apply(segData,2, skewness) #2 is to apply the function to columnts
head(skewValues)  ##skewValues

## To determine which type of transformation should be used, the MASS
## package contains the "boxcox" function. This function estimates lambda, 
## it does not create the transformed variable(s).
## A caret function, BoxCoxTrans,
## can find the appropriate transformation and apply them to the new data:

library(caret)
Ch1AreaTrans <- BoxCoxTrans(segData$AreaCh1) 
Ch1AreaTrans

# The original data
head(segData$AreaCh1)
# After transformation
predict(Ch1AreaTrans, head(segData$AreaCh1))
predict(Ch1AreaTrans, segData$AreaCh1)
## manually checking
(819^(-.9) - 1)/(-.9)

## Another caret function, preProcess, applies this transformation to a set of
## predictors. This function is discussed below. The base R function prcomp can
## be used for PCA. In the code below, the data are centered and scaled prior
## to PCA.

pcaObject <- prcomp(segData, center = TRUE, scale. = TRUE)

# Calculate the cumulative percentage of variance which each component
# accounts for.
percentVariance <- pcaObject$sd^2/sum(pcaObject$sd^2)*100
percentVariance[1:3]

## The transformed values are stored in pcaObject as a sub-object called x:
head(pcaObject$x[, 1:5])

## The another sub-object called rotation stores the variable loadings, where
## rows correspond to predictor variables and columns are associated with the
## components:
head(pcaObject$rotation[, 1:3])

## the caret class preProcess has the ability to transform, center, scale, or impute values,
## as well as apply the spatial sign transformation and feature extraction. The
## function calculates the required quantities for the transformation. After calling
## the preProcess function, the predict method applies the results to a set
## of data. For example, to Box–Cox transform, center, and scale the data, then
## execute PCA for signal extraction, the syntax would be:
> trans <- preProcess(segData, method = c("BoxCox", "center", "scale", "pca"))
> trans

# Apply the transformations:
transformed <- predict(trans, segData)
# These values are different than the previous PCA components since
# they were transformed prior to PCA
head(transformed[, 1:5])

## The order in which the possible transformation are applied is transformation,
## centering, scaling, imputation, feature extraction, and then spatial sign.

##### Filtering #############

## To filter for near-zero variance predictors, the caret package function nearZeroVar
## will return the column numbers of any predictors that fulfill the conditions
## outlined in Sect. 3.5. For the cell segmentation data, there are no problematic
## predictors:
nearZeroVar(segData)
integer(0)
# When predictors should be removed, a vector of integers is
# returned that indicates which columns should be removed.

## Similarly, to filter on between-predictor correlations, the cor function can
## calculate the correlations between predictor variables:
correlations <- cor(segData)
dim(correlations)
correlations[1:4, 1:4]

## To visually examine the correlation structure of the data, the corrplot package
## contains an excellent function of the same name. 
library(corrplot)
corrplot(correlations, order = "hclust")

## To filter based on correlations, the findCorrelation function will apply the
## algorithm in Sect. 3.5. For a given threshold of pairwise correlations, the function
## returns column numbers denoting the predictors that are recommended
## for deletion:
highCorr <- findCorrelation(correlations, cutoff = .75)
length(highCorr)
head(highCorr)
filteredSegData <- segData[, -highCorr]

########## Creating Dummy Variables ###########

## we take a subset of the cars data set in the caret package. 
## For 2005, Kelly Blue Book resale data for 804 GM cars were
## collected (Kuiper 2008). The object of the model was to predict the price of
## the car based on known characteristics. We will focus on the
## price, mileage, and car type (e.g., sedan) for a subset of vehicles:

head(carSubset)
levels(carSubset$Type)

## To model the price as a function of mileage and type of car, 
## we can use the function dummyVars to determine encodings for the predictors. 
## Suppose our first model assumes that the price can be modeled as a simple additive
## function of the mileage and type:
simpleMod <- dummyVars(~Mileage + Type, data = carSubset,
 ## Remove the variable name from the column name
levelsOnly = TRUE)
simpleMod

## To generate the dummy variables for the training set or any new samples,
## the predict method is used in conjunction with the dummyVars object:
predict(simpleMod, head(carSubset))

## The type field was expanded into five variables for five factor levels. 
## The model is simple because it assumes that effect of the mileage is the same for
## every type of car. To fit a more advance model, we could assume that there
## is a joint effect of mileage and car type. This type of effect is referred to as
## an interaction. In the model formula, a colon between factors indicates that
## an interaction should be generated.
withInteraction <- dummyVars(~Mileage + Type + Mileage:Type, data = carSubset,
levelsOnly = TRUE)
withInteraction
predict(withInteraction, head(carSubset))

















