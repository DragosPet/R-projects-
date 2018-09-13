library(markovchain)
stari <- c("PSD","PNL","ALDE","USR","PMP","alte")
byRow <- TRUE
tranzitii <- matrix(data=c(0.4, 0.4, 0.05, 0.02, 0.03, 0.1,
                           0.3, 0.2, 0.2, 0.1, 0.1, 0.1, 
                           0.1, 0.2, 0.1, 0.07, 0.03, 0.5, 
                           0.5, 0.1, 0.1, 0.1, 0.15, 0.05, 
                           0.1, 0.1, 0.2, 0.3, 0.04, 0.26, 
                           0.3, 0.2, 0.1, 0.05, 0.15, 0.2 ),byrow = byRow, nrow=6,ncol = 6, dimnames = list(stari,stari))
mcPOL <- new("markovchain", states= stari, byrow= byRow,transitionMatrix= tranzitii, name="Alegeri")
show(mcPOL)
plot(mcPOL,package="diagram",box.size=0.03)
initial <- c(0.42,0.27,0.09,0.05,0.05,0.12)
prediction <- initial * mcPOL
prediction2 <- initial * (mcPOL^2)
write.csv(stari,file ="stari.csv")

tranzitii2 <- matrix(data=c(0.2, 0.6, 0.05, 0.02, 0.03, 0.1,
                           0.3, 0.2, 0.2, 0.1, 0.1, 0.1, 
                           0.1, 0.2, 0.1, 0.07, 0.03, 0.5, 
                           0.5, 0.1, 0.1, 0.1, 0.15, 0.05, 
                           0.1, 0.1, 0.2, 0.3, 0.04, 0.26, 
                           0.3, 0.2, 0.1, 0.05, 0.15, 0.2 ),byrow = byRow, nrow=6,ncol = 6, dimnames = list(stari,stari))

mcPOL2 <- new("markovchain", states= stari, byrow= byRow,transitionMatrix= tranzitii2, name="Alegeri modifcare")
prediction3 <- initial * mcPOL2
