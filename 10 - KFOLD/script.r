rm(list=ls())

if (!require("rgl")) install.packages("rgl")
library('rgl')
if (!require("mlbench")) install.packages("mlbench")
library('mlbench')
if (!require("caret")) install.packages("caret")
library('caret')

knn <- function (point,trainX,trainY)
{
  matrixAux <- rbind(point,trainX)
  distance <- as.matrix(dist(matrixAux, method = "euclidean"))
  distanceArray <- as.array(order(distance[1,]))
  nearest <- distanceArray[1:K+1]
  
  nPositive <- length(which(trainY[nearest-1] == 1))
  nNegative <- length(which(trainY[nearest-1] == -1))
  
  if(nPositive > nNegative)
    predict <- 1
  else if (nNegative > nPositive)
    predict <- -1
  else
    predict <- 1
  
  return(predict)
}

data(BreastCancer)
summary(BreastCancer)

n <- 3
epoch <- seq(1,n,1)
mean <- array(0, n)
sd <- array(0, n)

for(e in epoch)
{
  X <- data.matrix(BreastCancer[,2:10])
  X[is.na(X)] <- 0
  Y <- as.numeric(BreastCancer$Class)
  Y[which( Y == 2)] <- -1
  
  partition <- sort(sample(nrow(X), nrow(X)*0.7))
  
  trainX<-X[partition,]
  trainY<-Y[partition]
  
  testY <- Y[-partition]
  testX <- X[-partition,]
  
  predict <- array(0,length(testY))
  
  K <- 3
  
  for(i in 1:dim(testX)[1]){
    predict[i] <- knn(testX[i,],trainX,trainY)
  }
  
  mean[e] <- (sum(1*(predict == testY))/length(testY))
  sd[e] <- sd(predict)
}

print(paste('Mean:'))
print(mean)
print(paste('Sd:'))
print(sd)



