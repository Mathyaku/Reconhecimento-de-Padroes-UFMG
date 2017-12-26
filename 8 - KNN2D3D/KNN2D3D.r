rm(list=ls())

if (!require("rgl")) install.packages("rgl")
library('rgl')

knn <- function (point,trainX,trainY, K)
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

# primeira parte

N <- 100
xc1 <- matrix(rnorm(N)*1+2, 50, 2)
xc2 <- matrix(rnorm(N)*1+4, 50, 2)

X <- rbind(xc1,xc2)
Y <- array(-1, N)
Y[1:N/2] <- 1

partition <-  sort(sample(nrow(X), nrow(X)*0.7))

trainX<-X[partition,]
trainY<-Y[partition]

testY <- Y[-partition]
testX <- X[-partition,]

predict <- array(0,length(testY))

K <- 10

for(i in 1:dim(testX)[1]){
  predict[i] <- knn(testX[i,],trainX,trainY, K)
}

seqi <- seq(0,6,0.1)
seqj <- seq(0,6,0.1)
Msep <- matrix(nrow=length(seqi),ncol = length(seqj))

ci <-0
for(i in seqi)
{
  ci <- ci +1
  cj <- 0
  for(j in seqj){
    cj <- cj + 1
    x <- c(i,j)
    Msep[ci,cj] <- knn(x,trainX,trainY, K)
  }
}


# image1
plot(trainX[which(trainY == 1),],col="blue", xlim=c(0,6), ylim=c(0,6), xlab='',ylab='')
par(new=T)
plot(trainX[which(trainY == -1),],col="red", xlim=c(0,6), ylim=c(0,6), xlab='',ylab='')
# image2
plot(trainX[which(trainY == 1),],col="blue", xlim=c(0,6), ylim=c(0,6), xlab='',ylab='')
par(new=T)
plot(trainX[which(trainY == -1),],col="red", xlim=c(0,6), ylim=c(0,6), xlab='',ylab='')
par(new=T)
plot(testX,col="black", xlim=c(0,6), ylim=c(0,6), xlab='',ylab='')
# image22
plot(trainX[which(trainY == 1),],col="blue", xlim=c(0,6), ylim=c(0,6), xlab='',ylab='')
par(new=T)
plot(trainX[which(trainY == -1),],col="red", xlim=c(0,6), ylim=c(0,6), xlab='',ylab='')
par(new=T)
plot(testX,col="black", xlim=c(0,6), ylim=c(0,6), xlab='',ylab='')
contour(seqi, seqj, Msep,labels = NULL,drawlabels = FALSE, add=T)
#image3
plot(trainX[which(trainY == 1),],col="blue", xlim=c(0,6), ylim=c(0,6), xlab='',ylab='')
par(new=T)
plot(trainX[which(trainY == -1),],col="red", xlim=c(0,6), ylim=c(0,6), xlab='',ylab='')
par(new=T)
plot(testX[which(testY == 1),],col="blue", xlim=c(0,6), ylim=c(0,6), xlab='',ylab='')
par(new=T)
plot(testX[which(testY == -1),],col="red", xlim=c(0,6), ylim=c(0,6), xlab='',ylab='')
contour(seqi, seqj, Msep,labels = NULL,drawlabels = FALSE, add=T)
#image4
persp3d(seqi, seqj, Msep,col="lightblue",alpha=0.5, xlab='',ylab='',zlab='')
points3d(trainX[which(trainY == 1),1],trainX[which(trainY == 1),2],0.3, col="blue", size=6, xlab="",ylab="",zlab="")
points3d(trainX[which(trainY == -1),1],trainX[which(trainY == -1),2],0.3, col="red", size=6, xlab="",ylab="",zlab="")

percent <- 100*(sum(1*(predict == testY))/length(testY))
print(paste('Porcentagem:', percent, '%'))


