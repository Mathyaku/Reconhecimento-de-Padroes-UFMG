rm(list=ls())

if (!require("mlbench")) install.packages("mlbench")
library('mlbench')
if (!require("mclust")) install.packages("mclust")
library('mclust')
if (!require("caret")) install.packages("caret")
library('caret')
if (!require("e1071")) install.packages("e1071")
library('e1071')
if (!require("rgl")) install.packages("rgl")
library('rgl')
if (!require("simone")) install.packages("simone")
library('simone')

# 1) BreastCancer

data(BreastCancer)
summary(BreastCancer)

X <- data.matrix(BreastCancer[,2:10])
X[is.na(X)] <- 0
Y <- as.numeric(BreastCancer$Class)
Y[which( Y == 2)] <- -1

partition <- createDataPartition(BreastCancer$Class, p=0.6, list=FALSE)
XTrain <- X[partition,]
Ytrain <- Y[partition]

Xtest <- X[-partition,]
Ytest <- Y[-partition]

Ytrain <- as.factor(Ytrain)

# 1.1) BreastCancer SVM sem PCA

svm.model <- svm(Ytrain ~ ., data = XTrain, cost=1000, gamma = 0.01)
svm.pred <- predict(svm.model, Xtest)

Pxc <- as.numeric(as.character(svm.pred))

Percent <- sum(1*(Ytest == Pxc))/length(Pxc)

print(paste('Porcentagem de Acerto BreastCancer SVM sem PCA:',Percent))

# 1.2) BreastCancer SVM com PCA

meanx<-colMeans(X)
Xs<- X - t(replicate(dim(X)[1],meanx))
S<-cov(Xs)
eigS<-eigen(S)
plot(c(1:9),eigS$values,type='b',xlab='Eixo PCA',ylab='Autovalor')

projX<-Xs %*% eigS$vectors

plot(projX[,1],projX[,2],type='p',xlim=c(-20,8),ylim=c(-10,10),xlab='PCA1',ylab='PCA2')
par(new=TRUE)
plot(projX[which(Y == 1),1],projX[which(Y == 1),2],type='p',xlim=c(-20,8),ylim=c(-10,10),col='red',xlab='PCA1',ylab='PCA2')
par(new=TRUE)
plot(projX[which(Y == -1),1],projX[which(Y == -1),2],type='p',xlim=c(-20,8),ylim=c(-10,10),col='blue',xlab='PCA1',ylab='PCA2')

points3d(projX[which(Y == 1),1:3], col = "red", size = 6, xlab="x", ylab="Y", zlab = "", add=T)
points3d(projX[which(Y == -1),1:3], col = "blue", size = 6, xlab="x", ylab="Y", zlab = "", add=T)
axes3d()

svm.model <- svm(Ytrain ~ ., data = projX[partition,1:3], cost=1000, gamma = 0.01)
svm.pred <- predict(svm.model, projX[-partition,1:3])

Pxc <- as.numeric(as.character(svm.pred))

Percent <- sum(1*(Ytest == Pxc))/length(Pxc)

print(paste('Porcentagem de Acerto BreastCancer SVM com PCA:',Percent))


# 2) USArrests

rm(list=ls())

data(USArrests)
summary(USArrests)

X<-as.matrix(USArrests[,(1:4)])
meanx<-colMeans(X)
Xs<- X - t(replicate(dim(X)[1],meanx))
S<-cov(Xs)
eigS<-eigen(S)
plot(c(1,2,3,4),eigS$values,type='b',xlab='Eixo PCA',ylab='Autovalor')

projX<-Xs %*% eigS$vectors

plot(projX[,1],projX[,2],type='p',xlim=c(-200,200),ylim=c(-50,50),xlab='PCA1',ylab='PCA2')

points3d(projX[,1:3], col = "black", size = 6, xlab="x", ylab="Y", zlab = "", add=T)
axes3d()

plot(c(1:4),eigS$values,type='b',xlab='Eixo PCA',ylab='Autovalor')



# 3) Simone -> Cancer

rm(list=ls())

data(cancer)
summary(cancer)

X <- data.matrix(cancer$expr)
X[is.na(X)] <- 0
Y <- as.numeric(cancer$status)
Y[which( Y == 2)] <- -1

partition <- createDataPartition(cancer$status, p=0.6, list=FALSE)
XTrain <- X[partition,]
Ytrain <- Y[partition]

Xtest <- X[-partition,]
Ytest <- Y[-partition]

Ytrain <- as.factor(Ytrain)

# 3.1) Simone SVM sem PCA

svm.model <- svm(Ytrain ~ ., data = XTrain, cost=1000, gamma = 0.01)
svm.pred <- predict(svm.model, Xtest)

Pxc <- as.numeric(as.character(svm.pred))

Percent <- sum(1*(Ytest == Pxc))/length(Pxc)

print(paste('Porcentagem de Acerto Simone SVM sem PCA:',Percent))


# 3.2) Simone SVM com PCA


meanx<-colMeans(X)
Xs<- X - t(replicate(dim(X)[1],meanx))
S<-cov(Xs)
eigS<-eigen(S)
plot(c(1:26),eigS$values,type='b',xlab='Eixo PCA',ylab='Autovalor')


projX<-Xs %*% eigS$vectors

plot(projX[,1],projX[,2],type='p',xlim=c(-5,5),ylim=c(-2,3),xlab='PCA1',ylab='PCA2')
par(new=TRUE)
plot(projX[which(Y == 1),1],projX[which(Y == 1),2],type='p',xlim=c(-5,5),ylim=c(-2,3),col='red',xlab='PCA1',ylab='PCA2')
par(new=TRUE)
plot(projX[which(Y == -1),1],projX[which(Y == -1),2],type='p',xlim=c(-5,5),ylim=c(-2,3),col='blue',xlab='PCA1',ylab='PCA2')


points3d(projX[which(Y == 1),1:3], col = "red", size = 6, xlab="x", ylab="Y", zlab = "", add=T)
points3d(projX[which(Y == -1),1:3], col = "blue", size = 6, xlab="x", ylab="Y", zlab = "", add=T)
axes3d()

svm.model <- svm(Ytrain ~ ., data = projX[partition,1:3], cost=1000, gamma = 0.01)
svm.pred <- predict(svm.model, projX[-partition,1:3])

Pxc <- as.numeric(as.character(svm.pred))

Percent <- sum(1*(Ytest == Pxc))/length(Pxc)

print(paste('Porcentagem de Acerto Cancer SVM com PCA:',Percent))

