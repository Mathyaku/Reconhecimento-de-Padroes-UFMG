rm(list=ls())

if (!require("bmp")) install.packages("bmp")
library('bmp')

if (!require("plot3D")) install.packages("plot3D")
library('plot3D')

if (!require("rgl")) install.packages("rgl")
library('rgl')

n <- 50
imageNames <- t(matrix(list.files("imagens"),nrow=10,ncol=5))

image1name<- c(imageNames[,1],imageNames[,2])
image2name<- c(imageNames[,3],imageNames[,4])
image3name<- c(imageNames[,5],imageNames[,6])
image4name<- c(imageNames[,7],imageNames[,8])
image5name<- c(imageNames[,9],imageNames[,10])

image1 <- matrix(0,n/5,46*56)
image2 <- matrix(0,n/5,46*56)
image3 <- matrix(0,n/5,46*56)
image4 <- matrix(0,n/5,46*56)
image5 <- matrix(0,n/5,46*56)

mean1 <- array(0,n/5)
mean2 <- array(0,n/5)
mean3 <- array(0,n/5)
mean4 <- array(0,n/5)
mean5 <- array(0,n/5)

sd1 <- array(0,n/5)
sd2 <- array(0,n/5)
sd3 <- array(0,n/5)
sd4 <- array(0,n/5)
sd5 <- array(0,n/5)

for(i in 1:(n/5)){
  image1[i,] <- as.vector(read.bmp(paste('imagens/',image1name[i],sep='')))
  image2[i,] <- as.vector(read.bmp(paste('imagens/',image2name[i],sep='')))
  image3[i,] <- as.vector(read.bmp(paste('imagens/',image3name[i],sep='')))
  image4[i,] <- as.vector(read.bmp(paste('imagens/',image4name[i],sep='')))
  image5[i,] <- as.vector(read.bmp(paste('imagens/',image5name[i],sep='')))
  
  mean1[i] <- mean(image1[i,])
  mean2[i] <- mean(image2[i,])
  mean3[i] <- mean(image3[i,])
  mean4[i] <- mean(image4[i,])
  mean5[i] <- mean(image5[i,])
  
  sd1[i] <- sd(image1[i,])
  sd2[i] <- sd(image2[i,])
  sd3[i] <- sd(image3[i,])
  sd4[i] <- sd(image4[i,])
  sd5[i] <- sd(image5[i,])
}

pdfnvar <- function(x,m,K,n) ((1/(sqrt((2*pi)^n*(det(K)))))*exp(-0.5*(t(x-m)%*%(solve(K))%*%(x-m))))

k1 <- cov(cbind(mean1,sd1))
k2 <- cov(cbind(mean2,sd2))
k3 <- cov(cbind(mean3,sd3))
k4 <- cov(cbind(mean4,sd4))
k5 <- cov(cbind(mean5,sd5))

nVariaveis <- 2

seqi <- seq(1,400,1)
seqj <- seq(1,400,1)
xrange <- seq(0.5,200,.5)
N <- 800
M1 <- matrix(0, N/2, N/2)
M2 <- matrix(0, N/2, N/2)
M3 <- matrix(0, N/2, N/2)
M4 <- matrix(0, N/2, N/2)
M5 <- matrix(0, N/2, N/2)
Mseparator <- matrix(0, N/2, N/2)

feature1 <- c(mean(mean1),mean(sd1))
feature2 <- c(mean(mean2),mean(sd2))
feature3 <- c(mean(mean3),mean(sd3))
feature4 <- c(mean(mean4),mean(sd4))
feature5 <- c(mean(mean5),mean(sd5))

for(i in seqi){
  for(j in seqj){
    M1[i,j] <- pdfnvar(c(xrange[i],xrange[j]),feature1,k1,nVariaveis)
    M2[i,j] <- pdfnvar(c(xrange[i],xrange[j]),feature2,k2,nVariaveis)
    M3[i,j] <- pdfnvar(c(xrange[i],xrange[j]),feature3,k3,nVariaveis)
    M4[i,j] <- pdfnvar(c(xrange[i],xrange[j]),feature4,k4,nVariaveis)
    M5[i,j] <- pdfnvar(c(xrange[i],xrange[j]),feature5,k5,nVariaveis)
    Mseparator[i,j] <- which.max(c(M1[i,j],M2[i,j],M3[i,j],M4[i,j],M5[i,j]))
  }
}

plot(mean1,sd1,col='blue',xlim=c(70,160),ylim=c(20,80),xlab='x',ylab='')
par(new=T)
plot(mean2,sd2,col='red',xlim=c(70,160),ylim=c(20,80),xlab='x',ylab='')
par(new=T)
plot(mean3,sd3,col='green',xlim=c(70,160),ylim=c(20,80),xlab='x',ylab='')
par(new=T)
plot(mean4,sd4,col='purple',xlim=c(70,160),ylim=c(20,80),xlab='x',ylab='')
par(new=T)
plot(mean5,sd5,col='black',xlim=c(70,160),ylim=c(20,80),xlab='x',ylab='')
title(main="Ilustração das características extraídas", xlab="Média", ylab="Desvio Padrão")

maxValue <- max(M1,M2,M3,M4,M5)
#ribbon3D(xrange, xrange, M1, clim=c(0,maxValue), zlim=c(0,maxValue))
#ribbon3D(xrange, xrange, M2, clim=c(0,maxValue), zlim=c(0,maxValue), add=T)
#ribbon3D(xrange, xrange, M3, clim=c(0,maxValue), zlim=c(0,maxValue), add=T)
#ribbon3D(xrange, xrange, M4, clim=c(0,maxValue), zlim=c(0,maxValue), add=T)
#ribbon3D(xrange, xrange, M5, clim=c(0,maxValue), zlim=c(0,maxValue), add=T)


persp3d(xrange,xrange,M1, zlim=c(0,maxValue), col='blue')
persp3d(xrange,xrange,M2, zlim=c(0,maxValue), col='red', add = TRUE)
persp3d(xrange,xrange,M3, zlim=c(0,maxValue), col='green', add = TRUE)
persp3d(xrange,xrange,M4, zlim=c(0,maxValue), col='purple', add = TRUE)
persp3d(xrange,xrange,M5, zlim=c(0,maxValue), col='black', add = TRUE)

ribbon3d(xrange,xrange,Mseparator, zlim=c(0,Mseparator), col='grey')

contour(xrange,xrange, Mseparator,xlim=c(70,160),ylim=c(20,80), col='black', drawlabels = F, nlevels = 5)
par(new=T)
contour(xrange,xrange, M1,xlim=c(70,160),ylim=c(20,80), col='blue', drawlabels = F, nlevels = 5)
par(new=T)
contour(xrange,xrange, M2,xlim=c(70,160),ylim=c(20,80), col='red', drawlabels = F, nlevels = 5)
par(new=T)
contour(xrange,xrange, M3,xlim=c(70,160),ylim=c(20,80), col='green', drawlabels = F, nlevels = 5)
par(new=T)
contour(xrange,xrange, M4,xlim=c(70,160),ylim=c(20,80), col='purple', drawlabels = F, nlevels = 5)
par(new=T)
contour(xrange,xrange, M5,xlim=c(70,160),ylim=c(20,80), col='black', drawlabels = F, nlevels = 5)
title(main="Ilustração das curvas de separação", xlab="Média", ylab="Desvio Padrão")
