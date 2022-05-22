### Définir le dossier de travail (remplacer ... par le dossier dans lequel vous avez sauvegardé le fichier)
setwd(dir = "C:/Users/hugop/OneDrive/Documents/économétrie/TP2")

### Importer la base de données
library(readxl)
marketing <- read_excel("marketing.xls")
#voir les differents graphiques 
#relation positive => on a tendance à voir la droite de regression positive 
# augmentation des frais de FB, vente augmente 

### Analyse graphique
plot(marketing$sales, type = "l", main = "Ventes", xlab = "", ylab = "")
plot(marketing$youtube, type = "l", main = "Dépenses publicitaires YouTube", xlab = "", ylab = "")
plot(marketing$facebook, type = "l", main = "Dépenses publicitaires Facebook", xlab = "", ylab = "")
plot(marketing$newspaper, type = "l", main = "Dépenses publicitaires Presses", xlab = "", ylab = "")

# Nuages
plot(marketing$sales, marketing$youtube, xlab = "Ventes", ylab = "YouTube")
plot(marketing$sales, marketing$facebook, xlab = "Ventes", ylab = "Facebook")
plot(marketing$sales, marketing$newspaper, xlab = "Ventes", ylab = "Presses")

# Stat descriptives
summary(marketing)


### Régression linéaire multiple
marketing.fit <- lm(sales ~ youtube + facebook + newspaper, data = marketing)
#on met la variable ? estimer (sales), ensuite on met toutes les variables explicatives (+ entre chacunes)

summary(marketing.fit)
#les coefficients sont-ils tous significatifs ? 
#l'estimateur du beta youtube est de 0,045 donc quand youtube augmente de 1000 unites, le niveau des ventes augmente de 45 unit?s TCEPA
#pour facebook : augmente de 188 unites TCEPA
#pour le newspaper : diminue de 1 unites TCEPA
#super important de preciser TCEPA

#est ce que le coefficient de youtube est-il statistiquement significatif ? 
#Oui p-value faible (inférieur ou égal à 0,05)donc significatif 
#idem pour faecbook 
#pas significatif pour newspaper

#statistiquement significatif = on a seulement 5 pourcent de chance que ce coefficient (beta) soit égal à 0 au vu du beta estimé 
#test de student : Ho (beta i est égal à 0 pour la p-value) et H1 (différent de 0)



plot(marketing.fit)


### Obtenir l'intervalle de confiance
confint(marketing.fit, level = 0.90)
#ici c'est 1-alpha
#toutes les valeurs positives donc pour fb et youtube 
#on est pt etre sur que les valeurs seront dans cet intervalle et on est sur de la relatio décrite au dessus
#valeur negative et positive donc pt pas déterminer l'effet pour newspaper et z?ro donc effet n'existe pas



#Question 4 calcul matriciel
y<- marketing$sales
x<- as.matrix(marketing[-4]) #exclure les colonnes sales qui est a la 4e position

const <- rep(1, length(y)) #vecteur de 1
x<- cbind(const, x)
marketing.betas <- solve(t(x) %*% x) %*% t(x) %*% y #%*% = produit matriciel et solve = l'inverse et t = transposee
marketing.betas




### Test de Chow
# Créer 2 sous-sets de la base (couper l'échantillon en 2)
youtube1 <- as.matrix(marketing$youtube[1 : (nrow(marketing)/2)])
youtube2 <- as.matrix(marketing$youtube[((nrow(marketing)/2)+1) : nrow(marketing)])

facebook1 <- as.matrix(marketing$facebook[1 : (nrow(marketing)/2)])
facebook2 <- as.matrix(marketing$facebook[((nrow(marketing)/2)+1) : nrow(marketing)])

newspaper1 <- as.matrix(marketing$newspaper[1 : (nrow(marketing)/2)])
newspaper2 <- as.matrix(marketing$newspaper[((nrow(marketing)/2)+1) : nrow(marketing)])

sales1 <- as.matrix(marketing$sales[1 : (nrow(marketing)/2)])
sales2 <- as.matrix(marketing$sales[((nrow(marketing)/2)+1) : nrow(marketing)])

# On effectue la régression du modèle global, et des 2 modèles "réduits"
marketing.fit1 <- lm(sales1 ~ youtube1 + facebook1 + newspaper1, data = marketing)
marketing.fit2 <- lm(sales2 ~ youtube2 + facebook2 + newspaper2, data = marketing)

# On calcule la somme des carrés des résidus
SCR_g <- df.residual(marketing.fit) * (summary(marketing.fit)$sigma^2)
SCR_1 <- df.residual(marketing.fit1) * (summary(marketing.fit1)$sigma^2)
SCR_2 <- df.residual(marketing.fit2) * (summary(marketing.fit2)$sigma^2)

k <- length(marketing.fit$coefficients)   #nb de paramètres estimés
n1 <- nrow(marketing.fit1$model)          #nb d'obs dans l'échantillon 1
n2 <- nrow(marketing.fit2$model)          #nb d'obs dans l'échantillon 2

# La F-stat du test de Chow
chow_F <- ((SCR_g-(SCR_1+SCR_2))/k) / ((SCR_1+SCR_2)/(n1+n2-2*k))

# ON TESTE H_0 : LES COEFFICIENTS DES 2 MODELES (1 ET 2) SONT EGAUX CONTRE H_1 : LES COEFFICIENTS SONT DIFFERENTS
# DECISION : P-VALUE NON SIGNIFICATIF => NON REJET DE H_0
# La P-value du test : sous H_0, chow_F suit une distri de Fisher à k ddl pour le numérateur et (n1+n2-2*k) pour le dénominateur
chow_pval <- 1-pf(chow_F, df1 = k, df2 = (n1+n2-2*k))

# Mêmes résultats avec le Package "gap"
install.packages("gap")
library(gap)

x1 <- cbind(youtube1, facebook1, newspaper1)
x2 <- cbind(youtube2, facebook2, newspaper2)

chow.test(sales1, x1, sales2, x2)

### Retrouver les coefficients à la main
y <- marketing$sales
X <- as.matrix(marketing[-4]) # exclure la colonne sales qui est à la 4e positon
const <- rep(1, length(y)) # vecteur de 1
X <- cbind(const, X)

marketing.betas <- solve(t(X) %*% X) %*% t(X) %*% y
marketing.betas