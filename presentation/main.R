rm(list = ls())
train<-read.csv("./train.txt",header =T, na.strings=c(""))

#visualize missing data
image(is.na(train), main = "Missing Values", xlab = "Observation", ylab = "Variable",
      xaxt = "n", yaxt = "n", bty = "n")
axis(1, seq(0, 1, length.out = nrow(train)), 1:nrow(train), col = "white")

train <- na.omit(train)
rownames(train)
trainLabel<-read.csv("./trainLabels.txt",header =T, na.strings=c(""))
trainLabel<-trainLabel[rownames(train),]

j <-0
booleanvec<-c()
for (i in as.matrix(train[1,])){
  j<-j+1;
  if(i=="YES" | i=="NO" ){
    booleanvec <-c(booleanvec,j)
    
    
  }
}

#--------convert Y/N to 1/0
BoolPred <-train[,booleanvec]
BoolPred <- BoolPred=="YES"
BoolPred <- ifelse(BoolPred,1,0)
#--------------------------

#--------hash values------

hashPred<-train[,c(4,5,35,36,62,65,66,92,95,96)]




train <- cbind(trainLabel,train)





#no missing values now.

x <-matrix(sample(LETTERS, 40, T), ncol = 4)
bag <-bag.make(x, n = 5)
bag$apply(x)


names(train)





#0----------
bag.make = function(x, n = 10){
  
  bag = sort(table(unlist(x)), dec = TRUE)
  n = min(n, length(bag))
  counts = bag[1:n]
  bag = names(bag)[1:n]
  
  b = list(items = bag, counts = counts, apply = function(targ){
    idx = apply(targ, 2, match, bag)
    m = matrix(0, nrow = nrow(targ), ncol=length(bag))
    
    sapply(1:nrow(m), function(r){
      m[r, idx[r,]] <<- 1
    })
    colnames(m) = bag
    m
  })
  class(b) = 'bag'
  b
}