### Définir le dossier de travail (remplacer ... par le dossier dans lequel vous avez sauvegardé le fichier)
setwd(dir = "C:/Users/hugop/OneDrive/Documents/économétrie/TP3")

### Importer la base de données :
# Le fichier à importer étant un fichier excel, commencez par charger la `bibliothèque' "readxl"
# Nous créons ensuite l'objet salaire qui contient notre base de données
library(readxl)
salaire <- read_excel("salaire_complet.xlsx")

### Exploration de la bdd
# Pour analyser les différentes catégories composant les variables catégoriques, on peut utiliser la commande "table"
table(salaire$race)
table(salaire$education)




### Regression 1 :
# Notre première régression est la régression linéaire où nous considérons nos variables en "niveau", i.e. sans aucune transformation
nivniv <- lm(wage ~ age + race + education, data = salaire)
summary(nivniv)
# on essaye d'expliquer la variable wage
# quand un travailleur est afro américain, il gagne en moyenne -7.25 que les blancs
# on voit que c'est par rapport au blanc car c'est la seule catégorie qui n'est pas présente
# dans les variables dummies, il faut toujours une catégorie qui n'est pas présente
# il est important de comprendre pq variable dummy ne reprend pas toutes les catégories

nivniv_bin <- lm(wage ~ age + Black + education, data = salaire)
summary(nivniv_bin)
#les travailleur afro-américains gagnent en moyenne 6,93 dollars de moins que les travailleurs non afro-américains
#le "en moyenne " est très important. 
#il faut préciser qu'il gagnent moins par rapport à quoi
#on voit le chiffre dans la première colonne avec la ligne Black


nivniv_bin <- lm(wage ~ age + Black +Asian+ White +Other + education, data = salaire)
summary(nivniv_bin)
#la variable other est devenu la variable categorielle ici
#donc dans les chiffres, c'est la différence moyenne de salaire par rapport à cette catégorie other


# Les résultats de la commande summary montrent que le R² ajusté est de 26%, ce qui peut laisser penser que les données ne sont pas bien représentées avec ce modèle.
# La F-stat est statistiquement significative, donc au moins l'une de nos variables explicatives est pertinente.
# Tous les coefficients, exceptés ceux sur les catégories 3 et 4 de 'race', sont significatifs à au moins 1%
# Les catégories 1 de 'race' et 'education' ne sont pas présentées : elles sont par défaut exclues de la régression pour éviter le probleme de multicolinéarité avec les autres catégories

### Variables d'interaction
# Afin d'effectuer les interactions, il suffit juste d'indiquer dans la régression le symbole d'interaction "*" entre 2 ou plusieurs variables
# Bon à savoir, cette manipulation inclut automatiquement les variables qui ont été interagies, donc pas besoin de les préciser en plus
interact <- lm(wage ~ age + race + education*jobclass, data = salaire)
summary(interact)
#le 16.03533 correspond à quand jobclass=information et etudes= advanced degree, 
#le salaire d'un travailleur qui correspond à ca aura un salaire suppérieur à 16 dollars par rapport à 
# (qqun qui n'a pas son diplome de secondaire et ne travaille pas dans l'information) pas sur du tout
# Ce chiffre 16,... représente en fait ce qu'on gagne en plus lorsque l'on combine les 2 variables.

#wage = B1 + B2 * AdvDegree + B3 * 
#les travailleurs qui ont un advanced dregree et qui travaillent dans le secteur de l'information gagneront 
#en moyenne 50,44 + 3.86 + 16.03 dollars de plus que qqun qui travaillent dans l'industrie et qui 
#n'a pas son diplome de secondaire(qui est la variable de référence ici)


#question 4



### Nuage de points
plot(salaire$age, salaire$wage, xlab = "Age", ylab = "Salaire")


### Régression 2
loglog <- lm(log(wage) ~ log(age) + race + education, data = salaire)
summary(loglog)
#quand la variable age va augmenter de 1% la varialbe wage augmente de 0,26%
#on ne s'interresse pas aux log quand on interprete
#le log est juste une astuce pour s'interresser à une relation qui n'est pas linéaire


### Régression 3
logniv <- lm(log(wage) ~ age + race + education, data = salaire)
summary(logniv)
#quand l'age augmente de 1 an, la variable salaire va augmenter de 0,00559%
#on réflechit en terme de taux de variation lorsque l'on utilise le logarithme


### Régression 4
nivlog <- lm(wage ~ log(age) + race + education, data = salaire)
summary(nivlog)
#quand la variable age augmente de 1%, la variable wage augmente de 26,621 divisé par 100, donc augmente de 0,26 dollars
#on divise par 100 car on a mis les chose en pourcentage
#

### Régression 5
# Nous créons d'abord la variable "age au carré" qui est ajoutée à la base
salaire$agecarre <- salaire$age^2
logcar <- lm(wage ~ age + agecarre + race + education, data = salaire) 
summary(logcar)
#on a estime une relation qui n'est plus lineaire mais qui permet de savoir une nouvelle relation, 
#si a et b sont de signes opposes, on aura alors un point de retournement qui pourra etre calcule
#point de retournement, moment ou la relation change de signe, a quel moment l'effet de l'age sur le salaire va 
#etre negative, pdt un moment age augmente, salaire augmente et puis age augmente salaire diminue 
#moment ou la derivee du salaire par rapport à l'age est egal à 0 
4.150127/ (2*0.041320)#l'effet de l'age sur le salaire est positif et une fois passe 50 ans le salaire devient negatif 


salaire$logwagecarre <- salaire$logwage^2
logcar2 <- lm(wage ~ age + agecarre + race + education, data = salaire) 
summary(logcar2)


### Transformation de Box-Cox
# La commande "boxcox" fait partie de la bibliothèque "MASS", à charger donc
library(MASS)
boxcox(nivniv, lambda = seq(-1, 1))

boxcox(logniv)
boxcox(logniv, lambda = seq(-1, 3))
# Si le résultat du boxcox indique une valeur pour lambda = 1, on garde les données originales
# Mais si 1 n'appartient pas à l'intervalle défini par les bandes inférieures et supérieures, une 
# transformation est nécessaire
# Proche de 0, il faudrait appliquer une transformation logarithmique
# 0,5 => prendre la racine carrée
# 2 => prendre le carré
# -1 => prendre l'inverse

boxcox(logcar2) #ici proche de 0 donc on fait une logarithmique 

