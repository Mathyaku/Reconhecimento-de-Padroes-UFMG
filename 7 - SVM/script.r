rm(list=ls())

if (!require("e1071")) install.packages("e1071")
library('e1071')
if (!require("MASS")) install.packages("MASS")
library('MASS')
if (!require("AtmRay")) install.packages("AtmRay")
library('AtmRay')
if (!require("varhandle")) install.packages("varhandle")
library('varhandle')
if (!require("rgl")) install.packages("rgl")
library('rgl')

sigma1 <- diag(2)
media1 <- c(2,2)
classe1 <- mvrnorm(n=30, media1,sigma1)

sigma2 <- diag(2)
media2 <- c(5,5)
classe2 <- mvrnorm(n=30, media2,sigma2)

plot(classe1[,1],classe1[,2],xlim=c(-3,8),ylim=c(-3,8), col="black")
par(new=T)
plot(classe2[,1],classe2[,2],xlim=c(-3,8),ylim=c(-3,8), col="blue")

D <- c(rep(-1,nrow(classe1)),rep(1,nrow(classe2)))
D <- as.factor(D)
X <- rbind(classe1,classe2)
X <- cbind(X,D)
X <- data.frame(X)

svm.model <- svm(D ~ ., data = X[,1:2], cost=1000, gamma = 1)
svm.pred <- predict(svm.model, X[,1:2])

alphas <- svm.model$coefs

# compute svm confusion matrix
print(table(X[,3] , pred = svm.pred))

# plotando separação
intervalo <- seq(from=-3,to=8,by=0.2)
grid <- meshgrid(intervalo,intervalo)
grid$z <- matrix(nrow= nrow(grid$x), ncol=(ncol(grid$x)))
amostra <- matrix(nrow = 1, ncol =2)
for(i in c(1:ncol(grid$x)))
{
  for(j in c(1:ncol(grid$y))){
    amostra[1,1] <- grid$x[i,j]
    amostra[1,2] <- grid$y[i,j]
    svm.pred <- predict(svm.model,amostra)
    aux <- as.numeric(svm.pred)
    if(aux == 2)
      aux <- -1
    
    grid$z[i,j] <- aux
  }
}

# separação
persp3d(intervalo,intervalo,grid$z,alpha=0.5, col="lightblue", xlab="x",ylab="y",zlab="sinc( r )")
points3d(classe1[,1],classe1[,2],0.3, col="black", size=6)
points3d(classe2[,1],classe2[,2],0.3, col="blue", size=6)



