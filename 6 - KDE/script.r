if (!require("MASS")) install.packages("MASS")
library('MASS')

if (!require("mlbench")) install.packages("mlbench")
library('mlbench')


N = 100
mean = 2
dev = 0.5

data = as.matrix(rnorm(N, mean, dev))

pdfnvar = function(x,m,detK,ginvK,n) {
  
  ((1/(sqrt((2*pi)^n*(detK))))*exp(-0.5*(t(x-m) %*% (ginvK) %*% (x-m))))
}

fnormal1var = function(x,m,h) {
  (1/sqrt(2*pi*h*h))*exp(-0.5*((x-m)/h)^2)
}

myKde = function(x,X,H) {
  eigenvalues = eigen(H)$values
  eigenz = abs(eigenvalues)>(10^(-8))
  detK = prod(eigenvalues[eigenz])
  ginvK = ginv(H)
  
  kde = 0
  for(i in 1:nrow(X)) {
    gaussian = pdfnvar(x, X[i,],detK,ginvK,ncol(X))
    kde = kde + gaussian
  }
  
  kde = kde/nrow(X)
  return(kde)
}

h=1.06*sd(data)*nrow(data)^(-1/5)
H = diag(h^2, nrow=ncol(data))

grid = seq(0,5,0.01)
plot(data, rep(0, nrow(data)), xlim=c(0,5), ylim=c(0,2))

gaussian = sapply(grid, fnormal1var, mean, dev)
par(new=T)
plot(grid, gaussian, xlim=c(0,5), ylim=c(0,2), type='l', col='green')

kde = rep(0, length(grid))
for(i in 1:length(grid)) {
  kde[i] = myKde(grid[i], data, H)
}
par(new=T)
plot(grid, kde, xlim=c(0,5), ylim=c(0,2), type='l', col='red')



# Set-up
xlim=c(-1.1,1.1)
ylim=c(-1.1,1.1)

xrange=seq(xlim[1],xlim[2],0.05)
yrange=seq(ylim[1],ylim[2],0.05)

grid = expand.grid(x = xrange, y = yrange)

# Dataset prep
spirals = mlbench.spirals(300, sd=0.05)
class1 = spirals$x[spirals$class==1,]
class2 = spirals$x[spirals$class==2,]

# Initial dataset plot
plot(class1[,1], class1[,2], xlim=xlim, ylim=ylim, col='blue')
par(new=T)
plot(class2[,1], class2[,2], xlim=xlim, ylim=ylim, col='red')

# Parameter calc
h=0.05
H = diag(h^2, nrow=ncol(class1))

# Method apply
kde1 = apply(grid, 1, myKde, class1, H)
kde2 = apply(grid, 1, myKde, class2, H)
classify = function(probs) {
  return(which.max(probs))
} 


result = apply(cbind(kde1, kde2), 1, classify)

par(new=T)

contour(xrange, yrange, matrix(result,nrow=length(xrange), ncol=length(yrange)), xlim=xlim, ylim=ylim, nlevels=1)

plot(kde1, kde2)