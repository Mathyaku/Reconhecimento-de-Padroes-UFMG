rm(list=ls())

if (!require("rgl")) install.packages("rgl")
library('rgl')
if (!require("mlbench")) install.packages("mlbench")
library('mlbench')
if (!require("caret")) install.packages("caret")
library('caret')
if (!require("mclust")) install.packages("mclust")
library('mclust')
if (!require("e1071")) install.packages("e1071")
library('e1071')
if (!require("rgl")) install.packages("rgl")
library('rgl')
if (!require("class")) install.packages("class")
library('class')
if (!require("caTools")) install.packages("caTools")
library('caTools')
if (!require("DMwR")) install.packages("DMwR")
library('DMwR')

dataFrame <- read.csv("BaseCar", skip = 1,header = FALSE)
data <- as.matrix(read.csv("BaseCar", skip = 1,header = FALSE))

X <- data.matrix(data[,2:7])
Y <- data.matrix(data[,8])
Y[which( Y == 0)] <- -1

partition <- sort(sample(nrow(X), nrow(X)*0.7))

trainX <- X[partition,]
trainY <- Y[partition]
trainY <- as.factor(trainY)

testY <- Y[-partition]
testX <- X[-partition,]

kFolds <- 3
listOfFolds <- createFolds(partition, k = kFolds)


## cross validation
gammaRange <- seq(0.1,1,0.1)
costRange <- seq(100, 1000, 100)
Acc <- array(0,length(costRange)*length(gammaRange))
Auc <- array(0,length(costRange)*length(gammaRange))
costCombination <- array(0,length(costRange)*length(gammaRange))
gammaCombination <- array(0,length(costRange)*length(gammaRange))

count <- 1
for(g in gammaRange)
{
  for(c in costRange)
  {
    #folds iterations
    KfoldAcc <- array(0,kFolds-1)
    KfoldAuc <- array(0,kFolds-1)
    for(i in 1:kFolds)
    {
      trainIndexes <- array(0,0)
      for(itrain in which(1:kFolds != i))
      {
        trainIndexes <- c(trainIndexes,listOfFolds[[itrain]])
      }
      testIndexes <- listOfFolds[[i]]
      
      trainXfolds<-trainX[trainIndexes,]
      trainYfolds<-trainY[trainIndexes]
      
      testXfolds<-trainX[testIndexes,]
      testYfolds<-trainY[testIndexes]
      
      
      svm.model <- svm(trainYfolds ~ ., data = trainXfolds, cost= c, gamma = g)
      svm.pred <- predict(svm.model, testXfolds)
      
      Pxc <- as.numeric(as.character(svm.pred))
      KfoldAcc[i] <- (sum(1*(Pxc == testYfolds))/length(testYfolds))
      KfoldAuc[i] <- colAUC(Pxc,testYfolds, plotROC=FALSE)
    }

    Acc[count] <- mean(KfoldAcc)
    Auc[count] <- mean(KfoldAuc)
    gammaCombination[count] <- g
    costCombination[count] <- c

    count <- count + 1;
  }
}

## Test Acc

iBestAcc <- which.max(Acc)

svm.model <- svm(trainY ~ ., data = trainX, cost= costCombination[iBestAcc], gamma = gammaCombination[iBestAcc])
svm.pred <- predict(svm.model, testX)

Pxc <- as.numeric(as.character(svm.pred))
accResult <- (sum(1*(Pxc == testY))/length(testY))
print(paste('Acc Result:',accResult*100))

## Test Auc

iBestAuc <- which.max(Auc)

svm.model <- svm(trainY ~ ., data = trainX, cost= costCombination[iBestAuc], gamma = gammaCombination[iBestAuc])
svm.pred <- predict(svm.model, testX)

Pxc <- as.numeric(as.character(svm.pred))
aucResult <- (sum(1*(Pxc == testY))/length(testY))

print(paste('Auc Result:',aucResult*100))


############# SMOTE

dataset <- read.csv("BaseCar", skip=1, header = FALSE)
data <- dataset[,2:8]

data$V8 <- as.factor(data$V8)

data <- SMOTE(V8 ~ ., data, perc.over = 2000, perc.under=100)

X <- data.matrix(data[,2:7])
Y <- data.matrix(data$V8)
Y[which( Y == 0)] <- -1

partition <- sort(sample(nrow(X), nrow(X)*0.7))

trainX <- X[partition,]
trainY <- Y[partition]
trainY <- as.factor(trainY)

testY <- Y[-partition]
testX <- X[-partition,]

kFolds <- 3
listOfFolds <- createFolds(partition, k = kFolds)


## cross validation
gammaRange <- seq(0.1,1,0.1)
costRange <- seq(100, 1000, 100)
Acc <- array(0,length(costRange)*length(gammaRange))
Auc <- array(0,length(costRange)*length(gammaRange))
costCombination <- array(0,length(costRange)*length(gammaRange))
gammaCombination <- array(0,length(costRange)*length(gammaRange))

count <- 1
for(g in gammaRange)
{
  for(c in costRange)
  {
    #folds iterations
    KfoldAcc <- array(0,kFolds-1)
    KfoldAuc <- array(0,kFolds-1)
    for(i in 1:kFolds)
    {
      trainIndexes <- array(0,0)
      for(itrain in which(1:kFolds != i))
      {
        trainIndexes <- c(trainIndexes,listOfFolds[[itrain]])
      }
      testIndexes <- listOfFolds[[i]]
      
      trainXfolds<-trainX[trainIndexes,]
      trainYfolds<-trainY[trainIndexes]
      
      testXfolds<-trainX[testIndexes,]
      testYfolds<-trainY[testIndexes]
      
      
      svm.model <- svm(trainYfolds ~ ., data = trainXfolds, cost= c, gamma = g)
      svm.pred <- predict(svm.model, testXfolds)
      
      Pxc <- as.numeric(as.character(svm.pred))
      KfoldAcc[i] <- (sum(1*(Pxc == testYfolds))/length(testYfolds))
      KfoldAuc[i] <- colAUC(Pxc,testYfolds, plotROC=FALSE)
    }
    
    Acc[count] <- mean(KfoldAcc)
    Auc[count] <- mean(KfoldAuc)
    gammaCombination[count] <- g
    costCombination[count] <- c
    
    count <- count + 1;
  }
}

## Test Acc

iBestAcc <- which.max(Acc)

svm.model <- svm(trainY ~ ., data = trainX, cost= costCombination[iBestAcc], gamma = gammaCombination[iBestAcc])
svm.pred <- predict(svm.model, testX)

Pxc <- as.numeric(as.character(svm.pred))
accResult <- (sum(1*(Pxc == testY))/length(testY))
print(paste('Acc Result SMOTE:',accResult*100))

## Test Auc

iBestAuc <- which.max(Auc)

svm.model <- svm(trainY ~ ., data = trainX, cost= costCombination[iBestAuc], gamma = gammaCombination[iBestAuc])
svm.pred <- predict(svm.model, testX)

Pxc <- as.numeric(as.character(svm.pred))
aucResult <- (sum(1*(Pxc == testY))/length(testY))

print(paste('Auc Result SMOTE:',aucResult*100))
  