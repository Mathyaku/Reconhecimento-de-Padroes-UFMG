rm(list=ls())

if (!require("caret")) install.packages("caret")
library('caret')

pdfnvar <- function(x,m,K,n) ((1/(sqrt((2*pi)^n*(det(K)))))*exp(-0.5*(t(x-m)%*%(solve(K))%*%(x-m))))

data(iris)

K <- 30
result <- array(0,K)

for(k in 1:K)
{
  data <- iris
  n <- 4
  
  setosa <- data[data$Species == 'setosa',]
  versicolorVirginica <- data[data$Species != 'setosa',]
  
  partition <-  sort(sample(nrow(setosa), nrow(setosa)*.7))
  setosaTrain<-setosa[partition,]
  setosaTest<-setosa[-partition,]
  
  partition <-  sort(sample(nrow(versicolorVirginica), nrow(versicolorVirginica)*.7))
  versicolorVirginicaTrain<-versicolorVirginica[partition,]
  versicolorVirginicaTest<-versicolorVirginica[-partition,]
  
  setosaMean <- array(0,n)
  versicolorVirginicaMean <- array(0,n)
  
  for(i in 1:n)
  {
    setosaMean[i] <- mean(setosaTrain[,i])
    versicolorVirginicaMean[i] <- mean(versicolorVirginicaTrain[,i])
  }
  
  kSetosa <- cov(setosaTrain[,1:n])
  kversicolorVirginica <- cov(versicolorVirginicaTrain[,1:n])
  
  allTest <- rbind(setosaTest[,1:4],versicolorVirginicaTest[,1:4])
  allTrain <- rbind(setosaTrain[,1:4],versicolorVirginicaTrain[,1:4])
  
  resultModeloSetosaTest <- array(0,dim(allTest)[1])
  resultModeloversicolorVirginicaTest <- array(0,dim(allTest)[1])
  
  resultModeloSetosaTrain <- array(0,dim(allTrain)[1])
  resultModeloversicolorVirginicaTrain <- array(0,dim(allTrain)[1])
  
  predTrain <- array(0, dim(allTrain)[1])
  realTrain <- array(0, dim(allTrain)[1])
  realTrain[1:dim(setosaTrain)[1]] <- 1
  
  predTest <- array(0, dim(allTest)[1])
  realTest <- array(0, dim(allTest)[1])
  realTest[1:dim(setosaTest)[1]] <- 1
  
  for(i in 1:(dim(allTrain)[1]))
  {
    x1<-c(allTrain[i,1],allTrain[i,2],allTrain[i,3],allTrain[i,4])
    resultModeloSetosaTrain[i] <- pdfnvar(x1,setosaMean,kSetosa,n)#*(dim(versicolorVirginicaTrain)[1]/(dim(setosaTrain)[1]+dim(versicolorVirginicaTrain)[1]))
    resultModeloversicolorVirginicaTrain[i] <- pdfnvar(x1,versicolorVirginicaMean,kversicolorVirginica,n)#*(dim(setosaTrain)[1]/(dim(setosaTrain)[1]+dim(versicolorVirginicaTrain)[1]))
    predTrain[i] <- 1*(resultModeloSetosaTrain[i] > resultModeloversicolorVirginicaTrain[i])
  }
  
  for(i in 1:(dim(versicolorVirginicaTest)[1]+dim(setosaTest)[1]))
  {
      x2<-c(allTest[i,1],allTest[i,2],allTest[i,3],allTest[i,4])
      resultModeloSetosaTest[i] <- pdfnvar(x2,setosaMean,kSetosa,n)*(dim(versicolorVirginicaTrain)[1]/(dim(setosaTrain)[1]+dim(versicolorVirginicaTrain)[1]))
      resultModeloversicolorVirginicaTest[i] <- pdfnvar(x2,versicolorVirginicaMean,kversicolorVirginica,n)*(dim(setosaTrain)[1]/(dim(setosaTrain)[1]+dim(versicolorVirginicaTrain)[1]))
      predTest[i] <- 1*(resultModeloSetosaTest[i] > resultModeloversicolorVirginicaTest[i])
  }
  
  for(i in 1:dim(allTrain)[1])
  {
    if(predTrain[i] == 1)
      predTrain[i] <- 'setosa'
    if(realTrain[i] == 1)
      realTrain[i] <- 'setosa'
    if(predTrain[i] == 0)
      predTrain[i] <- 'versicolorVirginica'
    if(realTrain[i] == 0)
      realTrain[i] <- 'versicolorVirginica'
  }
  
  for(i in 1:dim(allTest)[1])
  {
    if(predTest[i] == 1)
      predTest[i] <- 'setosa'
    if(realTest[i] == 1)
      realTest[i] <- 'setosa'
    if(predTest[i] == 0)
      predTest[i] <- 'versicolorVirginica'
    if(realTest[i] == 0)
      realTest[i] <- 'versicolorVirginica'
  }
  
  table(predTrain,realTrain)
  
  table(predTest,realTest)
  
  result[k] <- (table(predTest,realTest)[1,1]+table(predTest,realTest)[2,2])
}

mean <- mean(result)
sd <- sd(result)
cMatrix <- confusionMatrix(predTest, realTest)
print(paste('Média: ',mean,sep=''))
print(paste('Desvio-padrão: ',sd,sep=''))

plot(iris)
