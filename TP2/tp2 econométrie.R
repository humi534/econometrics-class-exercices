





install.packages('readxl')
library(readxl)
SandPhedge <- read_excel("C:/Users/hugop/OneDrive/Documents/économétrie/SandPhedge.xls", 
                         col_types = c("date", "numeric","numeric"))
View(SandPhedge)

#calculer les rendements
SandPhedge$rspot <- c(NA, 100*diff(log(SandPhedge$Spot))) #on met NA car pas de rendement possible à la 1ère année
SandPhedge$rfutures <- c(NA, 100*diff(log(SandPhedge$Futures))) #on voit ce NA dans le tableau

#Plot des séries
with(SandPhedge,plot(Date,spot,type='l',col='red', main='Prix au comptant et à terme des rendement du S&P500',ylab="Prix"))
with(SandPhedge,line(Date,Futures,type='l',lty=2, col='blue'))
legend("topleft", legend=c("Spot","Futures"),col=c("red","blue"),lty=1:2,cex=1)

with(SandPhedge)

#l'intercept c'est beta 0
