rm(list=ls())

if (!require("R.matlab")) install.packages("R.matlab")
library('R.matlab')


data <- as.matrix(readMat('dataset.mat')$x)

#define o numero de grupos - Number of clusters
K <- 4
n <- dim(data)[1]

plot(data, xlim=c(2,4.5), ylim=c(2,6), col='black')

centros <- matrix(0,K,2)

pertinencia <- matrix(0,n,K)
pertinenciaStore <- matrix(0,n,K)

for(i in 1:n){
  index <- sample(1:K,1)
  pertinencia[i,index] <- 1
}


iter <- 0
changes <- TRUE;
while(changes == TRUE){
  
  iter <- iter + 1
  for(i in 1:K){
    x <- sum(data[which(pertinencia[,i] == 1),1])
    y <- sum(data[which(pertinencia[,i] == 1),2])
    tam <- length(which(pertinencia[,i] == 1))
    centros[i,1] <- x/tam
    centros[i,2] <- y/tam
  }
  
  centroidDistOfPoint <- matrix(0,n,K)
  
  for(i in 1:n){
    ponto <- data[i,]
    for (j in 1:K){
      centroid <- centros[j,]
      distance <- sum((ponto-centroid)^2)
      if(is.nan(distance))
        distance <- Inf
      centroidDistOfPoint[i,j] = distance
    }
    iMax <- which(centroidDistOfPoint[i,]==min(centroidDistOfPoint[i,]))
    for (j in 1:K){
      if(j == iMax)
        pertinencia[i,j] <- 1
      else
        pertinencia[i,j] <- 0
    }
  }
  
  if(identical(pertinenciaStore,pertinencia))
    changes <- FALSE
  else
    pertinenciaStore <- pertinencia
    
}

colors <- c("blue", "yellow", "green", "purple")
for(i in 1:K){
  iGroup <- which(pertinencia[,i] == 1)
  plot(data[iGroup,], xlim=c(2,4.5), ylim=c(2,6), col=colors[i])
  par(new=T)
}
plot(centros, xlim=c(2,4.5), ylim=c(2,6),pch=21, col="red", bg="red")

print(paste("Iterações:",iter))





