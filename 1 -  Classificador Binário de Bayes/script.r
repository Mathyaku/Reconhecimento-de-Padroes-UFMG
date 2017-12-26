############## ploting a 3d curve

rm(list=ls())
if (!require("plot3D")) install.packages("plot3D")
library('plot3D')

seqi <- seq(-25,25,2)
seqj <- seq(-25,25,2)

M<- matrix(1,nrow=length(seqi),ncol=length(seqj))

ci <- 0
for(i in seqi)
{
  ci <- ci+1
  cj<- 0
  for(j in seqj){
    cj <- cj + 1
    M[ci,cj] <- i^3 + j^3 + 6*i*j
  }
}

persp3D(seqi,seqj,M,contour=T)


############# ploting in x axis

rm(list=ls())

N <- 30
xc1 <- rnorm(N)*0.5+2#rnorm Média 0 e desvio padrão 1
xc2 <- rnorm(N)*0.5+4
vec0 <- array(0,N)

plot(xc1,vec0,col='red',xlim=c(0,6),ylim=c(0,1),xlab='x',ylab='')
par(new=T)
plot(xc2,vec0,col='blue',xlim=c(0,6),ylim=c(0,1),xlab='x',ylab='')

########### 

fnormal1var <- function(x,m,r) ((1/(sqrt(2*pi*r*r)))*exp(-0.5*((x-m)/(r))^2))

m1 <- mean(xc1)
m2 <- mean(xc2)
s1 <- sd(xc1)
s2 <- sd(xc2)

xrange <- seq(0,6,0.1)

f1 <- fnormal1var(xrange,m1,s1)
f2 <- fnormal1var(xrange,m2,s2)

yrange2<- 1*(f2>f1)

par(new=T)
plot(xrange,f1,type='l',col='red',xlim=c(0,6),ylim=c(0,1),xlab='x',ylab='')
par(new=T)
plot(xrange,f2,type='l',col='blue',xlim=c(0,6),ylim=c(0,1),xlab='x',ylab='')
par(new=T)
plot(xrange,yrange2,type='l',col='green',xlim=c(0,6),ylim=c(0,1),xlab='x',
     ylab='P(x|C1), P(x|C2), Classe (x)')

## treinamento & testes

N <- 100
xc1 <- rnorm(N)*0.5+2 #rnorm M�dia 0 e desvio padr�o 1
xc2 <- rnorm(N)*0.5+4
vecTrain0 <- array(0,70)

sampleI <- sample(N)


xc1Train <- list()
xc1Train$x <- xc1[sampleI[1:70]]
xc1Train$y <- array(0,70)
xc2Train <- list()
xc2Train$x <- xc2[sampleI[1:70]]
xc2Train$y <- array(1,70)

xc1Test <- list()
xc1Test$x <- xc1[sampleI[71:100]]
xc1Test$y <- array(0,30)
xc2Test <- list()
xc2Test$x <- xc2[sampleI[71:100]]
xc2Test$y <- array(1,30)

plot(xc1Train$x,vecTrain0,col='red',xlim=c(0,6),ylim=c(0,1),xlab='x',ylab='')
par(new=T)
plot(xc2Train$x,vecTrain0,col='blue',xlim=c(0,6),ylim=c(0,1),xlab='x',ylab='')

# parametros do modelo
m1 <- mean(xc1Train$x)
m2 <- mean(xc2Train$x)
s1 <- sd(xc1Train$x)
s2 <- sd(xc2Train$x)

## RSNNS -> SPLITFORTRAINNINGANDTEST

# teste do modelo

xTest <- c(xc1Test$x, xc2Test$x)
f1 <- fnormal1var(xTest,m1,s1)
f2 <- fnormal1var(xTest,m2,s2)

xPred<- 1*(f2>f1)
xResp <- c(xc1Test$y, xc2Test$y)

xc1Error <- length(which(xPred[1:30] == 0))/30
xc2Error <- length(which(xPred[31:60] == 1))/30

########## matriz de confusão 

if (!require("caret")) install.packages("caret")
library('caret')

cMatrix<- confusionMatrix(xPred, xResp)
print(cMatrix)

############ Bidimensional 

rm(list=ls())
if (!require("plot3D")) install.packages("plot3D")
library('plot3D')

N <- 100
xc1 <- matrix(rnorm(N)*0.5+2, 50, 2)#rnorm Média 0 e desvio padrão 1
xc2 <- matrix(rnorm(N)*0.5+4, 50, 2)
xc0 <- array(0, 50)

fnormal1var <- function(x,m,r) ((1/(sqrt(2*pi*r*r)))*exp(-0.5*((x-m)/(r))^2))
fnormal2var <- function(x1,m1,r1,x2,m2,r2) (fnormal1var(x1,m1,r1)*fnormal1var(x2,m2,r2))

m11 <- mean(xc1[,1])
m12 <- mean(xc1[,2])
m21 <- mean(xc2[,1])
m22 <- mean(xc2[,2])
s11 <- sd(xc1[,1])
s12 <- sd(xc1[,2])
s21 <- sd(xc2[,1])
s22 <- sd(xc2[,2])

seqi <- seq(0,50,1)
seqj <- seq(1,50,1)
xrange <- seq(0,4.9,0.1)

f11 <- fnormal1var(xrange,m11,s11)
f12 <- fnormal1var(xrange,m12,s12)
f21 <- fnormal1var(xrange,m21,s21)
f22 <- fnormal1var(xrange,m22,s22)

M1 <- matrix(0, N/2, N/2)
M2 <- matrix(0, N/2, N/2)
M3 <- matrix(0, N/2, N/2)

###### x = xrange é a extensão de x, matriz K = matriz de covariancia, m = vetor de médias, n = o numero de variaveis
pdfnvar <- function(x,m,K,n) ((1/(sqrt((2*pi)^n*(det(K)))))*exp(-0.5*(t(x-m)%*%(solve(K))%*%(x-m))))

k1 <- cov(xc1)
k2 <- cov(xc2)
m1 <- c(m11,m12)
m2 <- c(m21,m22)
n <- 2

################

for(i in seqi){
  for(j in seqj){
    M1[i,j] <- pdfnvar(c(xrange[i],xrange[j]),m1,k1,n) #f11[i] * f12[j]
    M2[i,j] <- pdfnvar(c(xrange[i],xrange[j]),m2,k2,n) #f21[i] * f22[j]
    M3[i,j] <- 1*(M2[i,j] >= M1[i,j])
  }
}

# plotting a distribution

plot(xc1[,1],xc1[,2],col='blue',xlim=c(0,6),ylim=c(0,6),xlab='x',ylab='')
par(new=T)
plot(xrange,f11,type = 'l',col='blue',xlim=c(0,6),ylim=c(0,6),xlab='x',ylab='')
par(new=T)
plot(xc1[,1],xc0,col='blue',xlim=c(0,6),ylim=c(0,6),xlab='x',ylab='')
par(new=T)
plot(xc0,xc1[,2],col='blue',xlim=c(0,6),ylim=c(0,6),xlab='x',ylab='')
par(new=T)
plot(f12,xrange,type = 'l',col='blue',xlim=c(0,6),ylim=c(0,6),xlab='x',ylab='')
par(new=T)
plot(xc2[,1],xc2[,2],col='red',xlim=c(0,6),ylim=c(0,6),xlab='x',ylab='')
par(new=T)
plot(xc2[,1],xc0,col='red',xlim=c(0,6),ylim=c(0,6),xlab='x',ylab='')
par(new=T)
plot(xrange,f21,type = 'l',col='red',xlim=c(0,6),ylim=c(0,6),xlab='x',ylab='')
par(new=T)
plot(xc0,xc2[,2],col='red',xlim=c(0,6),ylim=c(0,6),xlab='x',ylab='')
par(new=T)
plot(f22,xrange,type = 'l',col='red',xlim=c(0,6),ylim=c(0,6),xlab='x',ylab='')

# plotting a 3d superficie

ribbon3D(xrange, xrange, M1,clim = c(0,2), zlim=c(0,1))
ribbon3D(xrange, xrange, M2,clim = c(0,2), zlim=c(0,1), add=T)
ribbon3D(xrange, xrange, M3,clim = c(0,2), zlim=c(0,1), add=T)
scatter3D(xc1[,1],xc1[,2],matrix(1,nrow=dim(xc1)[1]),add=T,col='blue')
scatter3D(xc2[,1],xc2[,2],matrix(1,nrow=dim(xc2)[1]),add=T,col='red')



