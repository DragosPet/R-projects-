list.files()
date <- read.csv("date_2016_r.csv",header=TRUE)
date$castig_mediu
date$VOTURI_PSD
date$CASTIG_MEDIU
summary(date)

cor(date_nec)
standardize = function(y) {
  y <- (y-mean(y))/ sd(y)
}
date_nec <- date[,-c(1,2)]
centrare = function(y) {
  y <- y-mean(y)
}

val_centrate <- apply(date_nec,2,centrare)
val_std <- apply(date_nec,2,standardize)


model1 <- lm(val_std[,1] ~ val_std[,3] + val_std[,4] + val_std[,5])
summary(model1)
#principal comp 
acp <-princomp(val_std,cor=TRUE)
summary(acp)
set.seed(123)
gr <- kmeans(date_nec,centers = 3)
gr$size
gr$centers
gr$cluster

plot(date_nec$VOTURI_PSD,date_nec$STUDII_SUPERIOARE_RECENS,type="n")
text(x=date_nec$VOTURI_PSD,y=date_nec$STUDII_SUPERIOARE_RECENS,col=gr$cluster)

require(cluster)
dendragrama <- hclust(dist(date_nec))
plot(dendragrama)

#clasificatorul naiv Bayesian, foarte bun
date_nec[,-c(2,8)]
date_nec[,8]
library(e1071)
model <- naiveBayes(date_nec[,-c(2,8)],factor(date_nec[,8]))
model
summary(model)
pred <- predict(model,date_nec[,-c(2,8)])
pred
table(pred,date_nec[,8])

#retele neuronale
library(neuralnet)
trainset <- date_nec[1:30,]
testset <- date_nec[30:42,]
testset
retea <- neuralnet::neuralnet(VOTURI_PSD ~ CASTIG_MEDIU + SOMAJ+ STUDII_SUPERIOARE_RECENS,trainset,hidden=3,linear.output =TRUE,threshold = 0.1)
plot(retea, rep="best")
test <- subset(testset, select = c("CASTIG_MEDIU","SOMAJ","STUDII_SUPERIOARE_RECENS"))
rez <- neuralnet::compute(retea,test)
rez

#arbore, abilitate predictiva de 76%
library(ISLR)
library(tree)
library(e1071)
library(MASS)
library(ggplot2)
arbore <- tree(factor(CASTIG)~.,trainset[,-1])
plot(arbore)
text(arbore)
predictie <- predict(arbore,testset,type="class")
predictie
table(predictie,testset[,8])
