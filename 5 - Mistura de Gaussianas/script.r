rm(list=ls())

if (!require("mlbench")) install.packages("mlbench")
library('mlbench')

if (!require("mclust")) install.packages("mclust")
library('mclust')


data(BreastCancer)
summary(BreastCancer)

X <- data.matrix(BreastCancer[,2:10])
X[is.na(X)] <- 0
Y <- as.numeric(BreastCancer$Class)

epochs <- 10
MSE <- array(0,epochs)
SD <- array(0,epochs)
PERCENT <- array(0,epochs)

for(e in 1:epochs){

partition <-  sort(sample(nrow(X), nrow(X)*0.7))
trainX<-X[partition,]
trainY<-Y[partition]

trainXc1 <- trainX[which(trainY == 1),]
trainXc2 <- trainX[which(trainY == 2),]

testY <- Y[-partition]
testX <- X[-partition,]

testXc1 <- testX[which(testY == 1),]
testXc2 <- testX[which(testY == 2),]

modelxc1<-densityMclust(trainXc1)
modelxc2<-densityMclust(trainXc2)

p1 <- dim(trainXc1)[1]/dim(trainX)[1]
p2 <- dim(trainXc2)[1]/dim(trainX)[1]
Pxc11<-  p1*as.matrix(dens(modelName=modelxc1$modelName, data = testX, parameters = modelxc1$parameters))
Pxc22<- p2*as.matrix(dens(modelName=modelxc2$modelName, data = testX, parameters = modelxc2$parameters))

Pxc <- array(0, dim(Pxc11)[1])
for(i in 1:dim(Pxc11)[1]){
  if(Pxc11[i,1] > Pxc22[i,1])
    Pxc[i] <- 1
  else 
    Pxc[i] <- 2
}

MSEepoch <- sum(1*(testY == Pxc))/length(Pxc)

PERCENT[e] <- MSEepoch 
MSE[e] <- mean((Pxc-testY)^2)
SD[e] <- sd(Pxc-testY)

}

print(mean(PERCENT))
print(mean(MSE))
print(mean(SD))

plot(100*PERCENT)
plot(MSE)
plot(SD)

