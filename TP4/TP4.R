###### !! Fichier encodé au format UTF-8.
#Si le fichier ne s'ouvre pas avec les bons caractères : File > Reopen with encoding... et choisir UTF-8


# Application 1
### Définir le dossier de travail (remplacer ... par le dossier dans lequel vous avez sauvegardé le fichier)
setwd(dir = "C:/Users/hugop/OneDrive/Documents/économétrie/TP4")

### Q1 == Importer la base de données :
cigarettes <- read.table(file="cigarettes.txt", sep="\t", header=TRUE, dec=".", row.names=1)
# Les options utilisées pour l'importation : sep pour séparateur avec valeur \t pour tabulation ; header = TRUE pour indiquer que la base comporte des noms pour les colonnes ; dec pour indiquer que le caractere de decimal est "." ; row.names pour indiquer que la 1ere colonne correspond au nom des lignes

# Nuages de points deux a deux
pairs(cigarettes)
#interessant derniere ligne, forte correlation positive en taux de goudron et la nocivite, 
#forte correlation positive entre la nictonie et la nocivite, derni?re moins evident à constater


### Q2 == Régression linéaire multiple
cigarettes.fit <- lm(CO ~ TAR + NICOTINE + WEIGHT, data = cigarettes)
summary(cigarettes.fit)
#seul le TAR est fortement significatif

# Residus
e <- cigarettes.fit$residuals 
#ou encore e <- residuals(cigarettes.fit)

# Moyenne de e
mean(e)
#on voit que la moyenne est extremement faible. R ne nous donne jamais 0 mais ici c'est vraiment un 0
#c'est tout à fait ce qui etait attendu


# Graphique des résidus avec CO
plot(x=cigarettes$CO, y=e, ylab="Résidus",xlab="CO")
abline(h=0) #pour ajouter une ligne horizontale passant par 0

# Droite de Henry => Qqplot dans le cas particulier de distribution normale
qqnorm(e)
#voir si residus suivent loi normale car sinon inference pas possible et s'attendre que les resultats de test 
#de significitavite ne soient pas interpretables et qu'on ne puissent pas juger de leur verifiabilite
# Il correspond au 2e graphique de diagnostic que R permet d'afficher après une régression : plot(cigarettes.fit, which = 2)

## Test de Jarque-Bera
# Coefficient d'asymétrie
g1 <- mean(e^3)/(mean(e^2)^1.5)
g1

# Coefficient d'applatissement
g2 <- mean(e^4)/(mean(e^2)^2)-3
g2

# Statistique de test du test de normalité de Jarque-bera
stat_test <- ((24-3-1)/6)*(g1^2+(g2^2)/4)
stat_test

# P-value du test de Jarque-Bera
pval <- pchisq(stat_test,2,lower.tail = FALSE)
pval
# pval > 5%, on ne peut pas rejeter H_0 : les résidus suivent une distribution normale à 5%


#autre facon plus simple de faire test jarque berra
library(tseries)
jarque.bera.test(cigarettes.fit$residuals)

### Q3 == résidu “studentisé”
#Points aberrants, soit logique differente des autres observations ou mauvais encodage d'un element
#si residu tres different de la student, peut-etre un point abberant, un outlayer
resid.student <- rstudent(cigarettes.fit)

# Seuil critique à partir de la loi de Student à (n-p-2) ddl ==> n = 24 obs., p = 3 explicatives, pour alpha = 0.1
alpha <- 0.1
ddl <- 24-3-2
seuil.student <- stats::qt(1-alpha/2, ddl)
seuil.student

# Détection des cigarettes en dehors des bandes délimitées par +/- le seuil
# On effectue un test logique pour vérifier si les valeurs appartiennent aux bandes
atypiques.rstudent <- (resid.student < -seuil.student | resid.student > +seuil.student)
ab.student <- cigarettes[atypiques.rstudent,]
ab.student 
#on obtient les observations atypiques, mtn voir ce qu'il se passe si on les enleve, marque de cigarette aberrante 


### Q4 == construction du graphique des résidus studentisés avec les points atypiques
plot(cigarettes$CO,resid.student)
abline(h=-seuil.student)
abline(h=+seuil.student)
abline(h=0)
text(cigarettes$CO[atypiques.rstudent],resid.student[atypiques.rstudent],rownames(cigarettes)[atypiques.rstudent])

# Calcul du levier
indicateurs <- influence.measures(cigarettes.fit)

# on s'intéresse à la matrice infmat
indicateurs$infmat

#on récupère la colonne "hat" qui correspond au levier
resid.hat <- indicateurs$infmat[,"hat"]
resid.hat

#le seuil est défini par 2x(p+1)/n ==> p = 3 expl., n = 24 obs.
seuil.hat <- 2*(3+1)/24
seuil.hat

# Les points atypiques au sens du levier
atypiques.levier <- (resid.hat > seuil.hat)
ab.hat <- cigarettes[atypiques.levier,]
ab.hat

### Q5 == Nouvelle base
# supprimer les points atypiques de la base
exclus <- (atypiques.rstudent | atypiques.levier) # identifier les éléments à exclure, l'opérateur | pour dire OU
exclus

# nouveau data frame : on garde les non-exclus ==> !exclus
cigarettes_new <- cigarettes[!exclus,]
dim(cigarettes_new)

## Nouvelle régression
cigarettes_new.fit <- lm(CO ~ ., data = cigarettes_new)
summary(cigarettes_new.fit)


#Regression apres avoir retirer les differents points aberrants, on obtient un R-carre plus eleve qu'avec
#les points aberrants avant, on a donc ameliorer notre modele. Pour estimer la qualite d'une estimation, 
#on observe le R-squared qui permet de savoir quel modele a la meilleure qualite 


# vider la mémoire
rm(list=ls())



# Application 2
### Définir le dossier de travail
setwd(dir = "C:/Users/hugop/OneDrive/Documents/économétrie/TP4")

library(readxl)
macro <- read_excel("macro.xls")

# Charger AER
library(AER)

# 2SLS de l'éq. inflation
inf_iv = ivreg(inflation ~ returns + dprod + dcredit + dmoney |
                 dcredit + dprod + rterm + dspread + dmoney, data = macro)
summary(inf_iv)

# 2SLS de l'éq. returns
ret_iv = ivreg(returns ~ inflation + dprod + dspread + rterm |
                 dcredit + dprod + rterm + dspread + dmoney, data = macro)
summary(ret_iv)

## Test de Hausman
# Faire un OLS simple de chaque éq.
inf_ols = lm(inflation ~ dprod + dspread + rterm +  dcredit + dmoney, data = macro)
ret_ols = lm(returns ~ dprod + dspread + rterm +  dcredit + dmoney, data = macro)

# Récupérer les valeurs fittées
macro$inffit = c(NA,inf_ols$fitted.values)
macro$retfit = c(NA,ret_ols$fitted.values)

# Refaire l'OLS avec les valeurs fittées
inf_hausm <- lm(inflation ~ dprod + dcredit + dmoney + returns + retfit, data = macro)
ret_hausm <- lm(returns ~ dprod + dspread + rterm + inflation + inffit, data = macro)

summary(inf_hausm)
summary(ret_hausm)

#on a bien endog?n?it? car retfit est significatif


#On voit que retfit ?tait significatif donc il a bien ?t? endog?n?it?, faut voir si gamma 1 est significatif pour endog?n?it? ou non, retfit significatif donc endog?n?it? 
#Inflation endog?n?it? 
#returns exog?n?it? 
