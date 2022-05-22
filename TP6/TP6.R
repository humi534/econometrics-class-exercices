### TP6  


library("tseries") # permet d'utiliser un certain nombre de commandes faciliant le traitement des séries temporelles 
library("lmtest") # permet de calculer la statistique du test de Breusch-Godfrey
library("car") # permet de calculer les statistiques de DW

#on importe la base de donnees
setwd(dir = "C:/Users/hugop/OneDrive/Documents/économétrie/TP6")
inflation <-read.csv("inflation.csv", sep=',',header=TRUE)
View(inflation)
summary(inflation)

#on indique une dimension temporelle
inflation.ts<-ts(inflation[,c("DHICPsaq","DURq")], frequency=4, start=c(1980,1), end=c(2015,4))
View(inflation.ts)
data.est<-window(ts(inflation, frequency=4, start=c(1980,1), end=c(2015,4)), frequency=4, 
                                start=c(1985,1), end=c(2014,4))


#on fait la regression 
phil<-lm(DHICPsaq ~ DURq , data=data.est)
summary(phil)

#le graphe sur les résidus 
res.pc<-ts(phil$residuals, frequency=4, start=c(1985,1), end=c(2014,4))
plot(res.pc, type="p", col="blue", ylab="Résidus", xlab="Temps")
abline(h=0)

#le test de Durbin-Watson
dwt(phil)
#Autocorrelation = rho, statistic = 0.87, p-value = 0 donc on rejette l'hypoth?se H0, 
#c'est ? dire l'hypoth?se que rho = 0, puisque rho diff?rent de 0, on a bien autocorr?lation


#le test de Breusch-Godfrey 
L.res.pc<-ts(lag(res.pc,k=-1), frequency=4, start=c(1985,2), end=c(2014,4)) 
#creer une variable qui correspond aux residus du quadri precedent, 
#lag permet de dire que c'est une série temporelle avec les residus retardés d'une periode

data.est.bg<-ts.intersect(res.pc, L.res.pc,data.est[,"DURq"]) #le tout mis dans une base de donnees
colnames(data.est.bg)<-c("res.pc", "L.res.pc","DURq")
View(data.est.bg) #on a donc créé une base de données avec les résidus lagués


#coefficient de corrélation (question2)
cor(data.est.bg[,"res.pc"],data.est.bg[,"L.res.pc"]) #coefficient de correlation entre les résidus et les résidus retardés 
#d'une période assez fort donc bien la preuve qu'on a correlation entre les diff?rents residus mais 
#faire test pour etre plus rigoureux 

bg1.fit<-lm(data.est.bg[,"res.pc"] ~ data.est.bg[,"L.res.pc"] + data.est.bg[,"DURq"])
s.bg1<-summary(bg1.fit)
s.bg1
#le coefficient rho est significatif avec 3 étoiles et donc autocorrélation

n.bg1<-length(bg1.fit$residuals)
# Calcul de la statistique de test
LM.bg1<-n.bg1*s.bg1$r.squared
# Calcul de la P-value associée 
PV.LM.bg1<-1-pchisq(LM.bg1,1)
bg.test.fb<-c(LM.bg1,PV.LM.bg1)
bg.test.fb #ici on a calculé les résidus à la main" 
#stat du test est 3.123729e+0, pvalue est 5.162232e-09 (faible)

#mais on peut aussi utiliser directement une commande
bg.test<-bgtest(phil, order=1, type="Chisq", fill=NA)
bg.test 
#pvalue faible donc rejet de H0 donc autocorrélation qu'il faut corriger. 


#on utilise l'estimateur de Newey-West
install.packages("sandwich")
library(sandwich)
coeftest(phil,vcov. = NeweyWest (phil,lag = 1,adjust =T,prewhite =F))

#Mtn p-value de 0.438, BDD trop restreinte avec peu d'observations que pour l'estimation avec 
#methode de Newey-West soit vraiment efficace, il est necessaire d'avoir enormement d'informations 
#avec cette methode 


#les FGLS

FGLS.reg <- lm(data.est.bg[,"res.pc"] ~ data.est.bg[,"L.res.pc"])
summary(FGLS.reg)

Yfgls<-data.est[,"DHICPsaq"]-0.529230*data.est[,"L.DHICPsaq"]
Xfgls<-data.est[,"DURq"]-0.529230*data.est[,"L.DURq"] #ici le 0.529230 correspond à 
reg.FGLS<-lm(Yfgls~Xfgls)                             #l'estimation du coefficient faite
                                                      #lors du test BG
summary(reg.FGLS)

#Marche pas terrible non plus, p-value pas significative donc on ne peut pas rejeter H0 et
#dire qu'il n'y a pas d'autocorrelation



acf(data.est[,"DHICPsaq"], main="l'autocorrélation", xlab="Retards", ylab="Coefficients d'autocorrélation")
pacf(data.est[,"DHICPsaq"], main="l'autocorréltion partiel", xlab="Retards", ylab="Coefficients d'autocorrélation partielle")


#les modele ARDL:
# ARDL(1,1)  
ardl.fit.s1<-lm(data.est[,"DHICPsaq"]~ data.est[,"L.DHICPsaq"]+data.est[,"DURq"]+data.est[,"L.DURq"])
#DHICPsaq est le taux d'inflation
#L.DHICPsaq est le taux d'inflation retardé d'une période
#DURq est le taux de chomage
#L.DURq est le taux de chomage retardé d'une période
#C'est pourquoi on utilise 1,1 car on fait un retard sur les deux de 1
summary(ardl.fit.s1) 
#interessant ici est le Beta 1, on veut que le 0.278 soit significatif mais ici il ne l'est pas hors 
#celui d'au dessus l'est donc on est sur la bonne voie
#on vient de montrer que le taux d'inflation retardé d'une periode est une variable significative pour 
#l'explication du taux d'inflation. Nous essayons aussi de montrer que la courbe de Phillips est "correcte"
#et donc que le taux de chomage, ici DURq, soit une variable significative
res.ardl.s1<-ts(ardl.fit.s1$residuals, frequency=4, start=c(1985,1), end=c(2014,4))
bgtest(ardl.fit.s1, fill=NA) #la pvalue est > 5 pourcents donc nous avons corrigé l'autocorrélation
bgtest(ardl.fit.s1,order=2, fill=NA) #ici pvalue < 5 pourcents


#autocorrelation d'ordre 1 corrigée mais encore d'ordre 2 à corriger 


#les modele ARDL
# ARDL(2,2)
#ici on essaye d'estimer le taux d'inflation en fonction du taux d'inflation retardé d'une période, du taux
#d'inflation retardé de 2 periodes, du taux de chomage, du taux de chomage retardé d'une periode et 
#du taux de chomage retardé de 2 periodes.
ardl.fit.s4<-lm(data.est[,"DHICPsaq"]~ data.est[,"L.DHICPsaq"]+data.est[,"L2.DHICPsaq"]+
                  data.est[,"DURq"]+data.est[,"L.DURq"]+data.est[,"L2.DURq"])
summary(ardl.fit.s4)
#on remarque que la pvalue a bien diminué (celle qui nous interresse DURq)
res.ardl.s4<-ts(ardl.fit.s4$residuals, frequency=4, start=c(1985,1), end=c(2014,4))
bgtest(ardl.fit.s4, fill=NA)

# ARDL(3,3)
ardl.fit.s8<-lm(data.est[,"DHICPsaq"]~ data.est[,"L.DHICPsaq"]+data.est[,"L2.DHICPsaq"]+
                  data.est[,"L3.DHICPsaq"]+data.est[,"DURq"]+data.est[,"L.DURq"]+data.est[,"L2.DURq"]+
                  data.est[,"L3.DURq"])
summary(ardl.fit.s8)
res.ardl.s8<-ts(ardl.fit.s8$residuals, frequency=4, start=c(1985,1), end=c(2014,4))
bgtest(ardl.fit.s8, fill=NA)
bgtest(ardl.fit.s8, order=2, fill=NA)
bgtest(ardl.fit.s8, order=3, fill=NA)

# ARDL(3,0) #ici lagué que trois fois le Y et pas le X 
ardl.fit.s5<-lm(data.est[,"DHICPsaq"]~ data.est[,"L.DHICPsaq"]+data.est[,"L2.DHICPsaq"]+
                  data.est[,"L3.DHICPsaq"]+data.est[,"DURq"])
summary(ardl.fit.s5)
#enfin notre taux de chomage devient une variable significative au seuil de 5 pourcents.
#ici, p-value est de 0.0250 ce qui est encore meilleur que celui d'au-dessus 

#La meilleure maniere de corriger l'autocorrelation est de comprendre ce qui a dans le rho et 
#d'avoir un bon modele theorique qui estime ce qui est la source de l'autocorrelation 



