#preluare baza de date
date <- read.csv("date_FIFA.csv",header = TRUE)
attach(date)
View(date)
names(date)

#statistici descriptive despre datele disponibile
summary(date)
#histograme
par(mfrow = c(2,3))
hist(date$VARSTA)
hist(date$INALTIME)
hist(date$VALOARE)
hist(date$SALARIU)
hist(date$APRECIERE)
hist(date$POTENTIAL)
par(mfrow=c(1,1))
hist(date$VITEZA)

#asimetrie si boltire
library(moments)
skewness(date$VARSTA)
skewness(date$INALTIME)
skewness(date$VALOARE)
skewness(date$SALARIU)
skewness(date$APRECIERE)
skewness(date$POTENTIAL)
skewness(date$VITEZA)

kurtosis(date$VARSTA)
kurtosis(date$INALTIME)
kurtosis(date$VALOARE)
kurtosis(date$SALARIU)
kurtosis(date$APRECIERE)
kurtosis(date$POTENTIAL)
kurtosis(date$VITEZA)

skew <- c(skewness(date$VARSTA),
          skewness(date$INALTIME),
          skewness(date$VALOARE),
          skewness(date$SALARIU),
          skewness(date$APRECIERE),
          skewness(date$POTENTIAL),
          skewness(date$VITEZA))

kurt <- c(kurtosis(date$VARSTA),
          kurtosis(date$INALTIME),
          kurtosis(date$VALOARE),
          kurtosis(date$SALARIU),
          kurtosis(date$APRECIERE),
          kurtosis(date$POTENTIAL),
          kurtosis(date$VITEZA))
matrix <- data.frame(skew,kurt,row.names = c("VARSTA","INALTIME","VALOARE","SALARIU","APRECIERE","POTENTIAL","VITEZA") )
matrix

#coeficient varianta si abatere standard
ab_std<- c(sd(date$VARSTA),
           sd(date$INALTIME),
           sd(date$VALOARE),
           sd(date$SALARIU),
           sd(date$APRECIERE),
           sd(date$POTENTIAL),
           sd(date$VITEZA))

cv_varsta<- sd(date$VARSTA)/mean(date$VARSTA)
cv_inaltime <- sd(date$INALTIME)/mean(date$INALTIME)
cv_valoare <- sd(date$VALOARE)/mean(date$VALOARE)
cv_salariu <- sd(date$SALARIU)/mean(date$SALARIU)
cv_apreciere <- sd(date$APRECIERE)/mean(date$APRECIERE)
cv_potential <- sd(date$POTENTIAL)/mean(date$POTENTIAL)
cv_viteza <- sd(date$VITEZA)/mean(date$VITEZA)

cv_varsta
cv_inaltime
cv_valoare
cv_salariu
cv_apreciere
cv_potential
cv_viteza
#boxplot 
par(mfrow = c(3,3))
boxplot(date$VARSTA,main="Boxplot varsta")
boxplot(date$INALTIME,main="Boxplot inaltime")
boxplot(date$VALOARE,main="Boxplot valoare")
boxplot(date$SALARIU, main="Boxplot salariu")
boxplot(date$APRECIERE, main="Boxplot apreciere")
boxplot(date$POTENTIAL, main="Boxplot potential")
boxplot(date$VITEZA,main="Boxplot viteza")
boxplot.stats(date$VARSTA)
boxplot.stats(date$VALOARE)
boxplot.stats(date$SALARIU)
boxplot.stats(date$APRECIERE)


#selectarea caracteristicilor numerice pentru efectuarea ACP
date_nec <- date[,c("VARSTA","INALTIME","VALOARE","SALARIU","APRECIERE","POTENTIAL","VITEZA")]
View(date_nec)

#corelatiile grafice dintre variabilele analizate
pairs(date_nec)
cor(date_nec)
write.csv(cor(date_nec),file="corelatii.csv" )
#functii pentru standardizarea si centrarea observatiilor
standardize = function(y) {
  y <- (y-mean(y))/ sd(y)
}

centrare = function(y) {
  y <- y-mean(y)
}

val_centrate <- apply(date_nec,2,centrare)
val_std <- apply(val_centrate,2,standardize)

View(val_std)
pairs(val_std)
pairs(val_centrate)
#matricea de covarianta utilzata in ACP
cov_std <- cov(val_std)

cor(val_centrate)
#acp
pca <- princomp(val_std,cor=TRUE)
summary(pca)
pca$loadings
write.csv(pca$loadings,"loadings.csv")
pca$scores
#vectori si valori proprii 
vv_pr <- eigen(cov_std)
vv_pr$values
vv_pr$vectors
#Scree plot
plot(pca)
plot(pca,type="l", main="Scree Plot")
#biplot
biplot(pca)

#matricea factor
matr_factor <- cor(val_std,pca$scores)
write.csv(matr_factor,"matr_factor_ic.csv")

#clusterizare

#kmeans
set.seed(123)
gr <- kmeans(val_std,centers = 3)
gr$size
gr$centers
gr$cluster
o <- order(gr$cluster)
dat <- data.frame(date$NUME[o],gr$cluster[o])
View(dat)
write.csv(dat,"date_clusterizate.csv")
plot(date_nec$VARSTA,date_nec$INALTIME,type="n",main="Apartenenta la clustere")
text(x=date_nec$VARSTA,y=date_nec$INALTIME,col=gr$cluster)

#matricea de confuzie
table(gr$cluster,date$NUME)
library(factoextra)
fviz_cluster(gr,data = date_nec)

#clusterizare fuzzy(fuzzy cmeans)
library(e1071)
library(cluster)
gr1 <- cmeans(date_nec,3,100,m=3,method = "cmeans")
gr1$membership

gr1
plot(date_nec$VARSTA,date_nec$INALTIME,col=gr1$cluster)
points(gr1$centers[,c(1,2,3)],col=c(1,2,3),pch=20,cex=2)


#kmedoids
date_medoids <- pam(date_nec,3)
date_medoids
o <- order(date_medoids$clustering)
dat <- data.frame(date$NUME[o],date_medoids$clustering[o])
View(dat)
plot(date_medoids$clustering)
write.csv(dat,"date_clusterizate_med.csv")

fviz_cluster(date_medoids, data=date_nec)
#matricea de confuzie
table(date_medoids$clustering,date$NUME)


date_nec#ierarhica
distante <- dist(val_std)
dendrogram <- hclust(distante)
plot(dendrogram,hang=-1)
plot(as.dendrogram(dendrogram),horiz=FALSE, main=" Dendrograma")
rect.hclust(dendrogram,k=3,border = "red")
member.c <- cutree(dendrogram,k=3)
#Ward
dend <- hclust(distante,method = "ward.D2")
plot(dend,hang=-1)
rect.hclust(dend,k=3,border="blue")
member.w <- cutree(dend,k=3)
table(member.w,member.c)


#nr optim de clustere
fviz_nbclust(val_std,kmeans,method = "wss") + geom_vline(xintercept=3,linetype=3)
fviz_nbclust(val_std,pam,method = "wss") + geom_vline(xintercept=3,linetype=3)

#silueta
fviz_nbclust(val_std,kmeans,method= "silhouette")
fviz_nbclust(val_std,pam,method= "silhouette")

#decalaj statistic
set.seed(123)
decalaj <- clusGap(val_std,FUN=kmeans,K.max =10, B=500,method)
decalaj2 <- clusGap(val_std,FUN=pam,K.max=10,B=50)
decalaj
decalaj2

#clusterizare bazata pe densitati 
library(fpc)
dsc <- dbscan(date[,-c(9,10,1)],eps = 0.45,MinPts = 3)
dsc


#SOM
library(kohonen)
train.obs <- sample(nrow(date_nec),30)
train.obs

train.set <- date_nec[train.obs,]
test.set <- date_nec[-train.obs,]

train.set <- apply(train.set,2,centrare)
train.set <- apply(train.set,2,standardize)

test.set <- apply(train.set,2,centrare)
test.set <- apply(train.set,2,standardize)


som_map1 <- som(train.set,grid = somgrid(3, 2, "hexagonal"))
som_map1$data
plot(som_map1, main = "SOM")
predictie_SOM <- predict(som_map1, newdata = as.matrix(test.set),trainX = as.matrix(train.set), 
                         trainY = classvec2classmat(date[,1][train.obs]))
predictie_SOM
predictie_SOM$unit.classif
table(predictie_SOM$unit.classif)

#inv supervizata 
#arbori decizionali
library(party)
set.seed(222)
index <- sample(2,nrow(date_nec),replace=TRUE,prob=c(0.7,0.3))
index
set_antrenare <- date_std[index==1,]
set_testare <- date_std[index==2,]
View(set_antrenare)
View(set_testare)
set_antrenare$EFICIENTA.APARARE = NULL
set_testare$EFICIENTA.APARARE = NULL

#dependenta 
dependenta <- VALOARE ~ VARSTA + SALARIU + POTENTIAL 
arbore1 <- ctree( dependenta,data=set_antrenare)
arbore1
plot(arbore1)
plot(arbore1,type="simple",main="Arbore de clasificare")
#confuzie
table(predict(arbore1),set_antrenare$EFICIENTA.ATAC)
predictie_test<- predict(arbore1, newdata=set_testare)
predictie_test
a <- table(predictie_test,set_testare$EFICIENTA.ATAC)
a
library(e1071)
classAgreement(a)

#curatare arbore 
library(MASS)
library(tree)
set.seed(123)
pruning <- cv.tree(arbore1,FUN=prune.misclass)
prune.misclass(arbore1)

#rpart
library(rpart)
arbore2 <- rpart(VALOARE~., data=set_antrenare,method="anova")
plot(arbore2,uniform=TRUE,branch=0.5,margin = 0.3)
text(arbore2,all=T,use.n=T)
plotcp(arbore2)
predictie_arb2 <- predict(arbore2,newdata = set_testare)
predictie_arb2
#curatare
arbore1 <- rpart(VALOARE~.,data = set_antrenare, method ="anova",cp=0.22)
plot(arbore1, uniform=TRUE,branch=0.5, margin=0.03,main = "Arbore curatat"
)
text(arbore1,all=T,use.n=T)
plotcp(arbore1)


#clasificatorul Naiv Bayesian
model <- naiveBayes(date_nec,factor(date[,9]))
model
predict(model, date_nec[1:5,])
table(predict(model, date_nec[]),date[,9])
date2 <- data.frame(val_std,predict(model, date_nec[]))
View(date2)

#k nearest neighbours 
library(class)
library(e1071)
set.seed(123)
set_antrenare <- val_std[index==1,]
set_testare <- val_std[index==2,]
View(set_antrenare)
eticheta_testset <- date[index==2,9]
eticheta_trainset <- date[index==1,9]
eticheta_testset
predictie <-knn(train=set_antrenare, test=set_testare,cl=eticheta_trainset,k=3)
predictie
a_knn <- table(predictie, eticheta_testset)
a_knn
classAgreement(a_knn)

#SVM 
model_svm <- svm(EFICIENTA.ATAC~., set_antrenare)
model_svm
model_svm$coefs
model_svm$decision.values
model_svm$y.scale

View(set_antrenare)
plot(model_svm,set_antrenare,as.formula(VALOARE~VARSTA))

set_testare <- date_std[index==2,]
View(set_testare)
predictie_SVM <- predict(model_svm,set_testare)
predictie_SVM
rez_pred_svm <-table(predictie_SVM,set_testare$EFICIENTA.ATAC)
rez_pred_svm
classAgreement(rez_pred_svm)
#neural 

date_std <- scale(date_nec)
date_std <- data.frame(date_std, date[,c(9,10)])
date_std
library(neuralnet)
itrain <- date_std[sample(1:54,30),]
itest <- date_std[-sample(1:54,24),]
itest <- subset(itest,select = c("VARSTA","VALOARE","SALARIU","POTENTIAL"))
View(itest)
View(itrain)
itest <- standardize((itest))
itrain$mare = c(itrain$EFICIENTA.APARARE == "mare")
itrain$mica = c(itrain$EFICIENTA.APARARE == "mica")
itrain$medie = c(itrain$EFICIENTA.APARARE == "medie")
itrain$EFICIENTA.APARARE = NULL
itrain$EFICIENTA.ATAC = NULL
View(itrain)

retea_n <- neuralnet(mare + mica + medie ~ VARSTA + VALOARE + SALARIU + POTENTIAL,itrain, hidden = 3,lifesign = "minimal")
retea_n
retea_n$weights

plot(retea_n, rep="best",main="Retea neuronala")
predictie <- compute(retea_n,covariate = itest)
predictie$neurons
rezultat <- 0
for(i in 1:24){
  rezultat[i] <- which.max(predictie$net.result[i,])
}
rezultat

for(i in 1:30) {
  if(rezultat[i]==1) {
    rezultat[i]="mare"
  }
  if(rezultat[i] ==2) {
    rezultat[i] ="mica"
  }
  if(rezultat[i] == 3) {
    rezultat[i]="medie"
  }
}
View(rezultat)
table(itrain)
comparatie <- date[sample(1:54,30),"EFICIENTA.APARARE"]
comparatie <- data.frame(date[sample(1:54,30),"EFICIENTA.APARARE"],rezultat)
View(comparatie)
table(comparatie$date.sample.1.54..30....EFICIENTA.APARARE..,comparatie$rezultat)
