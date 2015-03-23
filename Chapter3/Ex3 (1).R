rm(list=ls())
source("functions.R")
library(mlbench)
data(Glass)
?Glass
#3.1)

#a)

summary(Glass)
glass_red <- Glass[,1:9]
pairs(glass_red,upper.panel=panel.cor,diag.panel=panel.hist)


#The above figure shows the scatter plot of the predictors,histogram and correlation between predictors. 
#The size of the font shows how large the correlation is.
#for example correlation between RI and Ca is high.So we may possibly use one predictor instead of both after
#further analysis with other variables.


#b)

#To visualize outliars, boxplots are used after scaling and centering the dataset.use z-score to see the 
#outliars.
#From this it is observable that some of the predictors are skwed.
boxplot(scale(glass_red, center = TRUE, scale = TRUE))
#Some distributions are skwed.
skewValues <- apply(glass_red, 2, skewness)
skewValues  ##skewValues

#A rule of thumb is -1 to 1 
#RI, Mg, K,Ca,Ba,Fe are skewed.
#c)
library(e1071)
skewValues <- apply(glass_red, 2, skewness)
skewValues  ##skewValues

#A rule of thumb is skewness should be between, -1 to 1 so
#RI, Mg, K,Ca,Ba,Fe are skewed.

#c)

library(caret)
RITrans <- BoxCoxTrans(glass_red$RI) 
RITrans

hist(glass_red$RI)
hist(predict(RITrans,glass_red$RI))

MgTrans <- BoxCoxTrans(glass_red$Mg) 
MgTrans
hist(glass_red$Mg)
hist(predict(MgTrans,glass_red$Mg))

KTrans <- BoxCoxTrans(glass_red$K) 
KTrans
hist(glass_red$K)
hist(predict(KTrans,glass_red$K))

CaTrans <- BoxCoxTrans(glass_red$Ca) 
CaTrans
hist(glass_red$Ca)
hist(predict(CaTrans,glass_red$Ca))

BaTrans <- BoxCoxTrans(glass_red$Ba) 
BaTrans
hist(glass_red$Mg)
hist(predict(MgTrans,glass_red$Mg))

FeTrans <- BoxCoxTrans(glass_red$Fe) 
FeTrans
hist(glass_red$Mg)
hist(predict(MgTrans,glass_red$Mg))

#Lambda could not be estimated; no transformation is applied ?



#########################################################3.2##################################
#3.2
rm(list=ls())
library(mlbench)
data(Soybean)
?Soybean
str(Soybean)

Soybean[seq(1,683,100),] #look at every 100th raw
summary(Soybean)

#a)
#The far
summary(Soybean)
library(caret)
removeColumns <-nearZeroVar(Soybean)
names(Soybean)[removeColumns]

#"leaf.mild" "mycelium"  "sclerotia" these predictors have near zero varience so its possible to remove these
#predictores.







#2 some classes of plants may be rare so can not get any data .yes the pattern of missing data related to the class.

#3 Strategy for missing data






#3.3
library(caret)
data(BloodBrain)

#b)a degenerate distribution is the probability distribution of a discrete random 
#variable whose support consists of only one value. Examples include a two-headed 
#coin and rolling a die whose sides all show the same number. 
#While this distribution does not appear random in the everyday sense of the word, 
#it does satisfy the definition of random variable.

#variance should be zero or very close to zero



str(bbbDescr)
# No Degenerate Distributions
names <- names(bbbDescr)
print (Degenerate predictors variance less than 0.05)
for (i in 1:134 ) {
  #print (var(bbbDescr[,i]))
  if(var(bbbDescr[,i])<0.05){
    print (names[i])
  }
}




#c)

corrrelation <- cor(bbbDescr)
dim(corrrelation)
corrrelation[1:4, 1:4]

library(corrplot)
corrplot(corrrelation, order = "hclust")
     
print("Yes , Theare are high correlations between some predictors based on the correlation plot shows a correlation matrix of the training set. Each pairwise
correlation is computed from the training data and colored according to its
magnitude. This visualization is symmetric: the top and bottom diagonals
show identical information. Dark blue colors indicate strong positive correlations,
dark red is used for strong negative correlations, and white implies
no empirical relationship between the predictors. In this figure, the predictor
variables have been grouped using a clustering technique (Everitt et al. 2011)
so that collinear groups of predictors are adjacent to one another. Looking
along the diagonal, there are blocks of strong positive correlations that indicate
"clusters" of collinearity. Near the center of the diagonal is a large block
of predictors from the first channel.")

#This function searches through a correlation matrix and returns a vector
#of integers corresponding to columns to remove to reduce pair-wise correlations.

highCorr <- findCorrelation(corrrelation, cutoff = .75,verbose=False)

length(highCorr)
head(highCorr)
#removed these predictoros from data set
filteredbbbDescr <- bbbDescr[, -highCorr]
dim(filteredbbbDescr)
#after removing redo the correlation plot
corrrelation <- cor(filteredbbbDescr)
corrplot(corrrelation, order = "hclust")    
     
#As previously mentioned, feature extraction methods (e.g., principal components)
#are another technique for mitigating the effect of strong correlations
#between predictors. However, these techniques make the connection between
#the predictors and the outcome more complex.     
     
     
     
     
     
     
     
############# Functions######################
