
library(readxl)
#on importe la base de donn?es
setwd(dir = "C:/Users/hugop/OneDrive/Documents/économétrie/TP7")
UKHP <-read_excel("UKHP.xls")
#View(UKHP)
summary(UKHP)
tsUKHP<-ts(UKHP[,c("hp","dhp")], frequency=12, start=c(1991,2), end=c(2018,3))

#l'autocorrélation, l'autocorréltion partiel
install.packages("forecast")
library("forecast") # permet de calculer les fonctions d'autocorrélation et d'autocorrélation partielle


acf(tsUKHP[,"dhp"], main="l'autocorrélation", xlab="Retards", ylab="Coefficients d'autocorrélation")
pacf(tsUKHP[,"dhp"], main="l'autocorréltion partiel", xlab="Retards", ylab="Coefficients d'autocorrélation partielle")

# On peut limiter le nombre de retards à l'aide de la commande suivante : 
acf(tsUKHP[,"dhp"],lag.max=10, main="l'autocorréltion", xlab="Retards", ylab="Coefficients d'autocorrélation")
pacf(tsUKHP[,"dhp"],lag.max=10, main="l'autocorréltion partiel", xlab="Retards", ylab="Coefficients d'autocorrélation partielle")

result_acf=acf(tsUKHP[,"dhp"])
View(result_acf)

#le test Ljung-Box = test du portemanteau
Box.test(UKHP$dhp, lag = 1, type = c("Box-Pierce", "Ljung-Box"), fitdf = 0)
#on voit qu'on peut aller jusque 4 car ils sont assez significatifs
#donne une p-value tres faible disant qu'un au moins 1 coeff de correlation est significativement different de 0
#On voit que les 4 premiers ont l'air fortement significatifs mais la suite peu ou pas 

#le modèle ARMA: 
ar11<-arima ( UKHP$dhp , order = c(1,0,1)) #ceci est un premier exemple
summary(ar11)
 
#les crit?res 
AIC(ar11)
AIC(ar11,k=log(nrow(UKHP)))

#lancer une boucle afin de pouvoir verifier tous les modeles et pas les faire un par un 
#creer de tableaux de 6 colonnes et 6 lignes, on doit estimer niveau 0 voilà pq 6, 
#et on va jusqu'à 5 pour le AR, lignes AR, colonnes MA 
aic_table = array (NA,c(6,6,2))
for (ar in 0:5) {
  for (ma in 0:5){arma = arima(UKHP$dhp,order=c(ar,0,ma))
  aic_table[ar+1,ma+1,1]=AIC(arma)
  aic_table[ar+1,ma+1,2]=AIC(arma,k=log(nrow(UKHP)))
  }
}
aic_table  #premiere table Aic, deuxieme table bic pour tous les modeles qui nous interessent 
which.min(aic_table[,,1]) #donne l'element minimum de la table, meilleur caracterisation en fonction d'AIC est un modele arma (4,2) 
which.min(aic_table[,,2]) #donne l'element minimum de la table, meilleur caracterisation en fonction du BIC est un modele arma (2,0)

ar42<-arima(UKHP$dhp,order=c(4,0,2)) 
summary(ar42)
acf(ar42$residuals,lag.max = 10)
pacf(ar42$residuals, lag.max = 10)
Box.test(ar42$residuals, lag = 2, type = c("Box-Pierce", "Ljung-Box"), fitdf = 0)
#pvalue très élevée donc on ne peut pas rejeter H0 donc les coefficients de corrélation sont egaux à zero

ar20<-arima(UKHP$dhp,order=c(2,0,0)) 
summary(ar20)  
acf(ar20$residuals,lag.max = 10)
pacf(ar20$residuals, lag.max = 10)
Box.test(ar42$residuals, lag = 2, type = c("Box-Pierce", "Ljung-Box"), fitdf = 0)



checkresiduals(ar20)



#la prediction avec le modele ARMA
ar42<-arima(UKHP$dhp[UKHP$Month <="2015-12-01"],order=c(4,0,2))
fc_ar42 = ar42$coef[7]+ar42$coef[1]*UKHP$dhp[299:325]+ar42$coef[2]*UKHP$dhp[298:324]+ar42$coef[3]*UKHP$dhp[297:323]+ar42$coef[4]*UKHP$dhp[296:322]+ar42$coef[5]*ar42$residuals[299:325]+ar42$coef[6]*ar42$residuals[298:324]

ar20<-arima(UKHP$dhp[UKHP$Month <="2015-12-01"],order=c(2,0,0))                                                                                     
fc_ar20 = ar20$coef[3]+ar20$coef[1]*UKHP$dhp[299:325]+ar20$coef[2]*UKHP$dhp[298:324]


par (lwd =2,cex.axis = 1)
plot(UKHP$Month[300:326],UKHP$dhp[300:326], type = "l",xlab = "",ylab = "")
lines(UKHP$Month[300:326],fc_ar20,col="blue")
lines(UKHP$Month[300:326],fc_ar42,col="red")
legend("topright",legend =c("Actual", "ARMA20","ARMA42",col=c("black","blue","red"),lty=1))

dynamic_fc20 = predict(ar20,n.ahead = 27)
dynamic_fc42 = predict(ar42,n.ahead = 27)

par (lwd =2,cex.axis = 1)
plot(UKHP$Month[300:326],UKHP$dhp[300:326], type = "l",xlab = "",ylab = "")
lines(UKHP$Month[300:326],dynamic_fc20$pred,col="blue")
lines(UKHP$Month[300:326],dynamic_fc42$pred,col="red")
legend("topright", legend=c("Actual ", "ARMA20", "ARMA42",col=c("black","blue","red"),lty = 1 ))
        
        
        
#le mod?le GARSH
currencies <-read_excel("currencies.xls")
View(currencies)
summary(currencies)
currencies$rjpy=c(NA,100*diff(log(currencies$JPY)))        

currencies=currencies[-1,]
currencies=currencies[-1826,]

install.packages("rugarch")
library(rugarch)

spec=ugarchspec(mean.model = list(armaOrder =c(0,0)),variance.model=list(garchOrder=c(1,1),model="sGARCH"))
ugarchfit(spec,data=currencies$rjpy)

fit=ugarchfit(spec,data=currencies$rjpy,out.sample=700)
fc_garch=ugarchforecast(fit,n.ahead =1,n.roll=699)
View(fc_garch@forecast$sigmaFor)

