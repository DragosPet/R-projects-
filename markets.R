#preluare date
date_ut <- read.csv("date_corecte.csv",header = TRUE)
attach(date_ut)
View(date_ut)


library(ggplot2)
data_format <- as.Date(date_ut$Date,"%m/%d/%Y")
Y <- date_ut[,2:9]
acpdf <- cbind.data.frame(data_format,Y)
View(acpdf)


#statistici descriptive
summary(acpdf$Pret_Adobe)
summary(acpdf$Pret_Or)
summary(acpdf$Pret_SAP)
summary(acpdf$Pret_Micro)

summary(acpdf$Volum_Adobe)
summary(acpdf$Volum_Or)
summary(acpdf$Volum_SAP)
summary(acpdf$Volum_Micro)
# forma distributiei

par(mfrow=c(2,2))
hist(acpdf$Pret_Adobe,main = "Histograma pretului actiunilor Adobe",xlab ="Pret Adobe")
hist(acpdf$Pret_Or,main = "Histograma pretului actiunilor Oracle", xlab = "Pret Oracle")
hist(acpdf$Pret_SAP,main = "Histograma pretului actiunilor SAP",xlab = "Pret SAP")
hist(acpdf$Pret_Micro,main= "Histograma pretului actiunilor Microsoft",xlab="Pret Microsoft")

hist(acpdf$Volum_Adobe,main = "Histograma volumului actiunilor Adobe",xlab ="Volum Adobe")
hist(acpdf$Volum_Or,main = "Histograma volumului actiunilor Oracle", xlab = "Volum Oracle")
hist(acpdf$Volum_SAP,main = "Histograma volumului actiunilor SAP",xlab = "Volum SAP")
hist(acpdf$Volum_Micro,main= "Histograma volumulu actiunilor Microsoft",xlab="Volum Microsoft")


#skewness si Kurtosis
require(moments)
skewness(acpdf$Pret_Adobe)
skewness(acpdf$Pret_Or)
skewness(acpdf$Pret_SAP)
skewness(acpdf$Pret_Micro)
skewness(acpdf$Volum_Adobe)
skewness(acpdf$Volum_Or)
skewness(acpdf$Volum_SAP)
skewness(acpdf$Volum_Micro)

kurtosis(acpdf$Pret_Adobe)
kurtosis(acpdf$Volum_Adobe)
kurtosis(acpdf$Pret_Or)
kurtosis(acpdf$Volum_Or)
kurtosis(acpdf$Pret_SAP)
kurtosis(acpdf$Volum_SAP)
kurtosis(acpdf$Pret_Micro)
kurtosis(acpdf$Volum_Micro)
#boxplot
boxplot(acpdf$Pret_Adobe,main="Boxplot Pret Adobe")
boxplot(acpdf$Pret_Or,main="Boxplot Pret Oracle")
boxplot(acpdf$Pret_SAP,main="Boxplot Pret SAP")
boxplot(acpdf$Pret_Micro,main="Boxplot Pret Microsoft")

boxplot(acpdf$Volum_Adobe,main="Boxplot Volum Adobe")
boxplot(acpdf$Volum_Or,main="Boxplot Volum Oracle")
boxplot(acpdf$Volum_SAP,main="Boxplot Volum SAP")
boxplot(acpdf$Volum_Micro,main="Boxplot Volum Microsoft")



#grafica folosind ggplot2
ggplot(acpdf, aes(x=data_format, value, colour=Legenda))+ 
  geom_line(aes(y=Pret_Adobe,colour="Pret_Adobe"),show.legend = TRUE)+
  geom_line(aes(y=Pret_Or,colour="Pret_Oracle"),show.legend = TRUE) + 
  geom_line(aes(y=Pret_SAP,colour="Pret_SAP"),show.legend = TRUE)+
  geom_line(aes(y=Pret_Micro,colour="Pret_Microsoft"),show.legend = TRUE)+
  xlab("Data") + ylab("Pret Actiune")+ggtitle("Evolutia Preturilor actiunilor ")+
  theme(legend.position="bottom")


ggplot(acpdf, aes(x=data_format, value, colour=Legenda))+ 
  geom_line(aes(y=Volum_Adobe,colour="Volum Adobe"),show.legend = TRUE)+
  geom_line(aes(y=Volum_Or,colour="Volum Oracle"),show.legend = TRUE) + 
  geom_line(aes(y=Volum_SAP,colour="Volum SAP"),show.legend = TRUE)+
  geom_line(aes(y=Volum_Micro,colour="Volum Microsoft"),show.legend = TRUE)+
  xlab("Data") + ylab("Nr tranzactii")+ggtitle("Evolutia Volumului tranzactionat ")+
  theme(legend.position="bottom")


#corelatii
pairs(Y) # grafice nor de puncte intre perechi de variabile
cor <- cor(Y) #matrice de corelatie
View(cor)


#Standardizare si centrare observatii
centre <- function(x) {(x - mean(x))} 
Y_c <-  apply(Y,2,centre)
pairs(Y_c) 


standardize <- function(x) {(x - mean(x))/sd(x)} 
Y_std <-  apply(Y,2,standardize)  
View(Y_std)


#matricea de covarianta 
Y_cov <- cov(Y_std)
Y_cov_c <- cov(Y_c)
View(Y_cov)


#analiza componentelor principale in mod clasic
Vect_val_pr <- eigen(Y_cov)
summary(Vect_val_pr)
Vect_val_pr$values
Vect_val_pr$vectors

#analiza componentelor principale
pca <- princomp(Y_std,scores=TRUE,cor = TRUE)
summary(pca)

sc <- pca$scores
cor(sc)

#biplot
biplot(pca)

#loadings

ld <- loadings(pca)
write.csv(ld,file = "loadingsproj.csv")
View(ld)

#screeplot
par(mfrow=c(1,1))
plot(pca)
plot(pca, type="l",main = "SCREEPLOT",col= "purple")

#matricea factor 
matr_factor <- cor(Y,sc)
write.csv(matr_factor,file="Matricea_factor.csv")
View(matr_factor)

#cp 
piata <- data.frame(sc[,1:4])

ggplot(piata,aes(x=data_format),value,color=Legenda) +
         geom_line(aes(y=piata[1],colour="PC1"),show.legend=TRUE)+
        geom_line(aes(y=piata[2],colour="PC2"),show.legend = TRUE)+
        geom_line(aes(y=piata[3],colour="PC3"),show.legend = TRUE)+
        geom_line(aes(y=piata[4],colour="Pc4"),show.legend = TRUE)+
        xlab("Data")+ggtitle("Evolutia componentelor principale")+
  theme(legend.position="bottom")

Y_c <-data.frame(Y_c)
ggplot(Y_c, aes(x=data_format, value, colour=Legenda))+ 
  geom_line(aes(y=Pret_Adobe,colour="Pret_Adobe"),show.legend = TRUE)+
  geom_line(aes(y=Pret_Or,colour="Pret_Oracle"),show.legend = TRUE) + 
  geom_line(aes(y=Pret_SAP,colour="Pret_SAP"),show.legend = TRUE)+
  geom_line(aes(y=Pret_Micro,colour="Pret_Microsoft"),show.legend = TRUE)+
  geom_line(aes(y=Volum_Adobe,colour="Volum Adobe"),show.legend = TRUE)+
  geom_line(aes(y=Volum_Or,colour="Volum Oracle"),show.legend = TRUE) + 
  geom_line(aes(y=Volum_SAP,colour="Volum SAP"),show.legend = TRUE)+
  geom_line(aes(y=Volum_Micro,colour="Volum Microsoft"),show.legend = TRUE)+
  xlab("Data") + ylab("Pret si vol Actiune")+ggtitle("Evolutia Preturilor actiunilor ")+
  theme(legend.position="bottom")

#clusterizare 


plot(acpdf$Pret_Or~acpdf$Pret_SAP)
#matricea distantelor
library(cluster)
distance <- dist(as.matrix(Y_std))
distance
View(Y_std)

#heatmap
heatmap(Y_std)

hc.c <- hclust(distance)
hc.a <- hclust(distance,method="average")
hc.w <- hclust(distance, method="ward.D2")
plot(hc.c,hang=-1)
plot(hc.w, hang=-1)
plot(hc.a,hang=-1)
member.w <- cutree(hc.w,k=2)
member.a <- cutree(hc.a,k=2)
member.c <- cutree(hc.c,k=2)
rect.hclust(hc.w,k=2,border="blue")

aggregate(Y_std, list(member.w), mean)
#acuratete
plot(silhouette(cutree(hc.a, 2), distance))
plot(silhouette(cutree(hc.w, 2), distance))

wss <- (nrow(Y_std)-1)*sum(apply(Y_std,2,var))
for (i in 2:20) wss[i] <- sum(kmeans(Y_std, centers=i)$withinss)
plot(1:20, wss, type="b", xlab="Nr. de clustere", ylab="varianta din interiorul grupelor")


table(cutree(hc.w,k=2),cutree(hc.c,k=2))
#kmeans

k.c <- kmeans(Y_std,2)
k.c
k.c$size
k.c$cluster
k.c$totss
k.c$tot.withinss
k.c$betweenss
library(factoextra)
fviz_cluster(k.c,data=Y_std)
fviz_cluster(k.c,data=date_ut[,-1])
table(k.c$cluster,cutree(hc.w,k=2))

plot(Y_std[Pret_Adobe],Y_std[Pret_Or],type="n")
text(x=Y_std[Pret_Adobe],y=Y_std[Pret_Or],col=k.c$cluster)


#analiza discriminanta
library(MASS)
new_data <-data.frame(member.w,acpdf[,-1])
View(new_data)
date.lda <- lda(member.w ~ new_data$Pret_Adobe + new_data$Volum_Adobe + new_data$Pret_Or + new_data$Volum_Or + new_data$Pret_SAP + new_data$Volum_SAP 
                + new_data$Pret_Micro + new_data$Volum_Micro, data = new_data)
date.lda

predictie <- predict(date.lda,newdata = new_data[,c(2:9)])$class
predictie
date_disponibile <- new_data[,1]
table(predictie,date_disponibile)
#leave one out
date.lda2 <- lda(member.w ~ new_data$Pret_Adobe + new_data$Volum_Adobe + new_data$Pret_Or + new_data$Volum_Or + new_data$Pret_SAP + new_data$Volum_SAP 
                + new_data$Pret_Micro + new_data$Volum_Micro, CV=TRUE)
date.lda2$class
predicte2 <- predict(date.lda2,newdata =new_data[,c(2:9)] )$class
table(date.lda2$class,new_data[,1])
