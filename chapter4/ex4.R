#4.1
#a)
#Classify music into six catagories
#12,495 music samples 191 predictors.


#In this situation, since the sample size is large it is possible to 
#set aside a testing data set and training data set.
#regardless of the artist, the underlying connection between songs, it is possible to
#catagorise the songs to different genres.
#since the data shows no large disproposion between the classes ,we can use simple random sample.
#The response categories were not balanced
#(Fig. 1.1), with the smallest segment coming from the heavy metal category
#(7%) and the largest coming from the classical category (28%).
#do stratified random sampling.
music<- read.csv("genresTrain.csv")
attach(music)
#b)
library(AppliedPredictiveModeling)
str(music)
music_train[1]
barplot(table(music[192]))

#stratified sampling
set.seed(1);trainingRows <- createDataPartition(music$GENRE,p=0.80,list=FALSE)
head(trainingRows)
train_music<-music[trainingRows,]
train_classes<-GENRE[trainingRows]
test_music <-music[-trainingRows,]
test_classes <- GENRE[-trainingRows]
str(train_music)
str(test_music)









