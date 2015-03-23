

############# An Introduction to R ############

###### B.1. Start-Up and Getting Help #########

# Comments occur after '#' symbols and are not executed
# Use this command to quit
q()

# Get help on the Sweave function
?Sweave

# apropos will match a keyword against the available functions:
apropos("prop")

# RSiteSearch function conducts an online search of all functions,
# manuals, contributed documentation, the R-Help newsgroup, 
# and other sources for a keyword. 
# For example, to search for different methods to produce
# ROC curves
RSiteSearch("roc")
######## B.2. Packages ############

# The function install.packages can be used to install 
# additional modules
install.packages("randomForest")

# To load a package, the library function is used:
# For example, Load the random forests package
library(randomForest)

# Show the list of currently loaded packages and other information
sessionInfo()

############ B.3. Creating Objects ############

# Anything created in R is an object. 
# Objects can be assigned values using “<-”.
pages <- 97
pages
town <- "Richmond"
print(town)
# Equals also works, but see Section B.9 below

# Another helpful function for understanding the contents 
# of an object is str(for structure).
month.abb  #the abbreviated month names.
str(month.abb)  #a character object with twelve elements.
str(print)  #fuction without data
str(sessionInfo)

########## B.4. Data Types and Basic Structures ###########

# data types: numeric, character, factor, and logical types. 
# Logical data can take on value of TRUE or FALSE. 
if(3 > 2) print("greater") else print("less")
isGreater <- (3 > 2)
isGreater
is.logical(isGreater)

# Numeric data: integers and double precision
x <- 3.6
is.numeric(x)
is.integer(x)
is.double(x)
typeof(x)

# Character strings can be created by putting text 
# inside of quotes:
y <- "your ad here"
typeof(y)
z <- "you can also 'quote' text too"
z

# There are several helpful functions that work on strings. 
# First, char counts the number of characters:
nchar(y)
nchar(z)

# grep function to determine if a substring exists 
# in the character string
grep("ad", y)
grep("my", y)

# If the string is present, return the whole value
grep("too", z, value = TRUE)

# Vector: The most basic method of creating a vector is to use the c function
# (for combine). To create a vector of numeric data:
weights <- c(90, 150, 111, 123)
is.vector(weights)
typeof(weights)
length(weights)
weights + .25

# Many functions work on vectors
mean(weights)
colors <- c("green", "red", "blue", "red", "white")
grep("red", colors)
nchar(colors)

# An alternate method for storing character data in a vector is to use factors.
colors2 <- as.factor(colors)
colors2
levels(colors2)
as.numeric(colors2)

# subset the factor vector by removing the first value using a negative integer value:
colors2[-1]

# work with a subset of a vector, single brackets can be used in different ways:
weights
# positive integers indicate which elements to keep
weights[c(1, 4)]
# negative integers correspond to elements to exclude
weights[-c(1, 4)]
# A vector of logical values can be used 
weights[c(TRUE, TRUE, FALSE, TRUE)]

# Vectors must store the same type of data. An alternative is a list;
both <- list(colors = colors2, weight = weights)
is.vector(both)
is.list(both)
length(both)
names(both)

# Lists can be filtered in a similar manner as vectors. 
# However, double brackets return only the element, 
# while single brackets return another list
both[[1]]
is.list(both[[1]])
both[1]
is.list(both[1])

# We can also subset using the name of the list
both[["colors"]]

# Missing values in R are encoded as NA values:
probabilities <- c(.05, .67, NA, .32, .90)
is.na(probabilities)

# NA is not treated as a character string
probabilities == "NA"

# Most functions propagate missing values...
mean(probabilities)

# ... unless told otherwise
mean(probabilities, na.rm = TRUE)

######### B.5 Working with Rectangular Data Sets ###########

# There are two main structures for rectangular data: matrices and data frames.
# A matrix can only contain data of the same type (e.g., character or numeric)
# data frames must contain columns of the same data type. 
mat <- matrix(1:12, nrow = 3)
mat
rownames(mat) <- c("row 1", "row 2", "row 3")
colnames(mat) <- c("col1", "col2", "col3", "col4")
mat
rownames(mat)
mat[1, 2:3]
mat["row 1", "col3"]
mat[1,3]
mat[1,]
is.matrix(mat[1,])
is.vector(mat[1,])

mat[1,] # dimensions are dropped
mat[1,,drop = FALSE] # to avoid this
is.matrix(mat[1,,drop = FALSE])
is.vector(mat[1,,drop = FALSE])

# Data frames can be created using the data.frame function:
df <- data.frame(colors = colors2, time = 1:5)
df
dim(df)
colnames(df)
rownames(df)

# the $ operator can be used to return single columns 
df$colors

# the subset function can be used to return more complicated subsets of rows:
subset(df, colors %in% c("red", "green") & time <= 2)

# A helpful function for determining if there are any missing values 
# in a row of a matrix or data frame is the complete.cases function, 
# which returns TRUE if there are no missing values:

df2 <- df

# Add missing values to the data frame
df2[1, 1] <- NA
df2[5, 2] <- NA
df2
complete.cases(df2)


############# B.7. R Functions ################

# Functions have arguments: specific slots that are used to pass
# objects into the function. 
# For example, the function for reading data stored in comma
# delimited format (CSV) into an R object has these arguments:
str(read.csv)
?read.csv

# where file is a character string that points to the CSV file 
# header indicates whether the initial row corresponds to variable names
read.csv("data.csv")
read.csv(header = FALSE, file = "data.csv")
read.csv("data.csv", na.strings = "?")

# This argument tells R which character values indicate a missing
# value in the file. Using
# Another function to read data is read.table
str(read.table)

############ B.8. The Three Faces of = ##############

# 1) Creating objects, such as 
x = 3
# 2) Testing for equivalence: 
x == 4
# 3) Specifying values to function arguments: 
read.csv(header = FALSE)

# To avoid confusion, use <- as the assignment operator.
x<-3

############# B.9. The AppliedPredictiveModeling Package ##########

# Object class     Package     predict Function syntax
# lda              MASS        predict(object) (no options needed)
# glm              stats       predict(object, type = "response")
# gbm              gbm         predict(object, type = "response", n.trees)
# mda              mda         predict(object, type = "posterior")
# rpart            rpart       predict(object, type = "prob")
# Weka_classifier  RWeka       predict(object, type = "probability")
# LogitBoost       caTools     predict(object, type = "raw", nIter)

############ B.10. The caret Package #############

# caret -- Classification And Regression Training
# used for building and evaluating predictive models

# caret provides an interface for across a wide vary of models (over 140). 
# caret also provides many options for data pre-processing and resampling-based parameter
# tuning techniques (Chaps. 3 and 4).























































