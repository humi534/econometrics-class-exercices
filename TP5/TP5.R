###### !! Fichier encodé au format UTF-8.
#Si le fichier ne s'ouvre pas avec les bons caractères : File > Reopen with encoding... et choisir UTF-8


### Définir le dossier de travail (remplacer ... par le dossier dans lequel vous avez sauvegardé le fichier)
setwd(dir = "C:/Users/hugop/OneDrive/Documents/économétrie/TP5")

### Importer la base de données
library(readxl)
marketing <- read_excel("marketing.xls")

### Analyse graphique
plot(marketing$youtube, marketing$sales, xlab = "YouTube", ylab = "Ventes")
abline(lm(marketing$sales ~ marketing$youtube))
#on est clairement dans un cas d'heteroscedasticite

plot(marketing$facebook, marketing$sales, xlab = "Facebook", ylab = "Ventes")
abline(lm(marketing$sales ~ marketing$facebook))
#on est clairement dans un cas d'heteroscedasticite

plot(marketing$newspaper, marketing$sales, xlab = "Presses", ylab = "Ventes")
abline(lm(marketing$sales ~ marketing$newspaper))
#ici c'est un peu plus flou

### Régression linéaire multiple
marketing.fit <- lm(sales ~ youtube + facebook + newspaper, data = marketing)

summary(marketing.fit)


###les graphiques avec les résidus
res <- residuals(marketing.fit)
yhat <- fitted(marketing.fit)

plot(marketing$sales,res, xlab="income", ylab="residuals")
abline(h=0) #pour ajouter une ligne horizontale passant par 0
#sensé avoir les memes ecarts autour de 0 mais ici on voit que ce n'est clairement pas le cas 
#Dans le graphe entre Y et les residus devraient tjrs etre autour de 0 or on voit sur la fin que 
#ca quitte la moyenne et monte ce qui montre une non linearite (simple remarque non faite TP3) 

plot(yhat,res, xlab="fitted values", ylab="residuals")
abline(h=0) #pour ajouter une ligne horizontale passant par 0
#ici on voit le V donc non linearite 
#on voit que les ecarts moyens des residus sont beaucoup plus grands sur les extremites, donc heteroscedasticite

###test de Godfeld-Quandt 
# Créer 2 sous - sets de la base ( couper l'échantillon en 2)
youtube1 <- as.matrix( marketing $ youtube [1 : ( nrow ( marketing )/2) ])
youtube2 <- as.matrix( marketing $ youtube [(( nrow ( marketing )/2) +1) : nrow ( marketing )])

facebook1 <- as.matrix( marketing $ facebook [1 : ( nrow ( marketing )/2) ])
facebook2 <- as.matrix( marketing $ facebook [(( nrow ( marketing )/2) +1) : nrow ( marketing )])

newspaper1 <- as.matrix( marketing $ newspaper [1 : ( nrow ( marketing )/2) ])
newspaper2 <- as.matrix( marketing $ newspaper [(( nrow ( marketing )/2) +1) : nrow ( marketing )])

sales1 <- as.matrix(marketing $ sales [1 :( nrow(marketing)/2)])
sales2 <- as.matrix(marketing $ sales [(( nrow( marketing )/2)+1):nrow( marketing )])

# On effectue la régression du modèle global , et des 2 modèles "réduits "
marketing.fit1<- lm(sales1 ~ youtube1 + facebook1 + newspaper1 , data = marketing )
marketing.fit2<- lm(sales2 ~ youtube2 + facebook2 + newspaper2 , data = marketing )

### on calcule maintenant la stat du test : 

df1 <- marketing.fit1$df.residual #degré de liberté du numérateur
df2 <- marketing.fit2$df.residual #degré de liberté du dénominateur


install.packages("broom")
library(broom)

#on calcule les variances 
sig1squared <- glance(marketing.fit1)$sigma^2
sig2squared <- glance(marketing.fit2)$sigma^2

###enfin on calcule la stat
numTstat <- (nrow(facebook1)-(3+1))*sig1squared
denTstat <- (nrow(facebook2)-(3+1))*sig2squared
Tstat <- numTstat/denTstat
Tstat



qt(1-0.05, 541)




pf(Tstat, df1, df2)
#la pvalue est de 0.07 donc c'est supérieur à 5% donc on ne rejete pas H0. La Pvaleur est la proba de se tromper en rejetant H0
#ici, on ne peux rejeter H0 à 5%
#voir les hyp sur feuille papier bas de page 1  

###test de White

#on crée d'abord le variable au carre
facebook_sq <- marketing$facebook^2
youtube_sq <- marketing$youtube^2
newspaper_sq <- marketing$newspaper^2
res_sq <- res^2

#on lance la régression sur les résidus au carré
regWtest=lm(res_sq~facebook_sq+youtube_sq+newspaper_sq+marketing$youtube*marketing$facebook+marketing$youtube*marketing$newspaper+marketing$facebook*marketing$newspaper, data = marketing)
summary(regWtest) #on a trois variables au carre, trois variables sans carre et trois variables d'interaction

r2=summary(regWtest)$r.squared      # Extraire le R carre de la regression auxiliaire
Wstat <- nrow(marketing)*r2         # La statistique du test de White
Wstat

qchisq(1-0.05,9)                     # La valeur critique

1-pchisq(Wstat,9)                       # la pvalue
#pvalue faible ou encore valeur critique inférieure à la stat (16.91 < 67.53) donc on rejete h0 donc on a heteroscedasticite

###facultatif: on refait Godfeld-Quandt
mark1 <- marketing[which(marketing$sales>=15),]
mark2 <- marketing[which(marketing$sales<15),]

mark1.fit <- lm(sales ~ youtube + facebook + newspaper, data = mark1)
mark2.fit <- lm(sales ~ youtube + facebook + newspaper, data = mark2)

df1_2 <- mark1.fit$df.residual #Numerator degrees of freedom
df2_2 <- mark2.fit$df.residual #Denominator df

sig1sq_2 <- glance(mark1.fit)$sigma^2
sig2sq_2 <- glance(mark2.fit)$sigma^2

###enfin on calcule la stat
numTstat_2 <- (nrow(mark1)-(3+1))*sig1sq_2
denTstat_2 <- (nrow(mark2)-(3+1))*sig2sq_2
Tstat_2 <- numTstat_2/denTstat_2
Tstat_2

pf(Tstat_2, df1_2, df2_2)


###estimation avec les standard error robust
install.packages("car")
library(car)

install.packages("lmtest")
library(lmtest)

cov1 <- hccm(marketing.fit, type="hc1") #needs package 'car'
mark_coef <- coeftest(marketing.fit, vcov.=cov1)
mark_coef

#les coeff sont toujours significatifs, on obtient la meme reponse qu'au test de base mais cette fois c'est plus robuste




