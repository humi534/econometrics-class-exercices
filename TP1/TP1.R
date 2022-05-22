### Définir le dossier de travail
setwd(dir = "C:/Users/hugop/OneDrive/Documents/économétrie/TP1")

### Importer la base de données
library(readxl)
SandPhedge <- read_excel("SandPhedge.xls",
	col_types=c("date","numeric","numeric"))

### Calculer les rendements
SandPhedge$rspot <- c(NA,100*diff(log(SandPhedge$Spot)))
SandPhedge$rfutures <- c(NA,100*diff(log(SandPhedge$Futures)))

### Plot des séries
with(SandPhedge, plot(Date, Spot,  type = "l", col = "red", main = "Prix au comptant et à terme du S&P500", ylab = "Prix"))
with(SandPhedge, lines(Date, Futures, type = "l", lty = 2, col = "blue"))
legend("topleft", legend = c("Spot", "Futures"), col = c("red", "blue"), lty = 1:2, cex = 1)

with(SandPhedge, plot(Date, rspot, type = "l", col = "red", main = "Rendement des prix au comptant et à terme du S&P500", ylab = "Rendements"))
with(SandPhedge, lines(Date, rfutures, type = "l", lty = 2, col = "blue"))
legend("bottomright", legend = c("Spot", "Futures"), col = c("red", "blue"), lty = 1:2, cex = 1)

### Histogramme des rendements
with(SandPhedge, hist(rspot, probability = TRUE, breaks = 20, main = "Histogramme des rendements du spot", xlab = NULL, ylab = "Densité"))
with(SandPhedge, lines(density(rspot, na.rm = TRUE), lwd = 2))
with(SandPhedge, hist(rfutures, probability = TRUE, breaks = 20, main = "Histogramme des rendements du future", xlab = NULL, ylab = "Densité"))
with(SandPhedge, lines(density(rfutures, na.rm = TRUE), lwd = 2))

### Nuage de point
with(SandPhedge, plot(rspot, rfutures, main = "Nuage de points des rendements", xlab = "Spot", ylab = "Future"))
with(SandPhedge, plot(Spot, Futures, main = "Nuage de points des prix", xlab = "Spot", ylab = "Future"))

### Statistiques descriptives
summary(SandPhedge[c("rspot","rfutures")])

### Calculer les moments soi-même ...
x <- na.omit(SandPhedge$rspot)
n <- length(x)
rspot_skew <- (sum((x - mean(x))^3)/n)/(sum((x - mean(x))^2)/n)^(3/2)
rspot_kurt <- n * sum((x - mean(x))^4)/(sum((x - mean(x))^2)^2)

x <- na.omit(SandPhedge$rfutures)
n <- length(x)
rfutures_skew <- (sum((x - mean(x))^3)/n)/(sum((x - mean(x))^2)/n)^(3/2)
rfutures_kurt <- n * sum((x - mean(x))^4)/(sum((x - mean(x))^2)^2)

### ... ou installer et charger le package "moments"
install.packages("moments")
library(moments)
skewness(SandPhedge$rspot, na.rm = TRUE)
kurtosis(SandPhedge$rspot, na.rm = TRUE)

### Régression linéaire rendements
lm_returns=lm(rspot ~ rfutures, data=SandPhedge)

### Résultats de la régression
summary(lm_returns)

### Analyse graphique des résultats
par(mfrow = c(2,2)) # pour afficher les 4 graphiques en 2 colonnes x 2 lignes
plot(lm_returns)

### Retrouver les coefficients de la régression
sp_cov <- cov(SandPhedge$rspot, SandPhedge$rfutures, use = "complete.obs")
sp_var <- var(SandPhedge$rfutures, na.rm = TRUE)

sp_beta1 = sp_cov/sp_var
sp_beta0 = mean(SandPhedge$rspot, na.rm = TRUE) - sp_beta1 * mean(SandPhedge$rfutures, na.rm = TRUE)

coef(lm_returns) 

### Test de significativité des coefficients
# On teste H_0 : beta = 0 (non significatif) contre H_1 : beta != 0 (significatif)
# Sous H_0, la stat de Student t-chapeau = beta-chapeau / sigma-chapeau suit une loi de Student à N-2 d.d.l.
# On rejette H_O si cette stat est > que la stat théorique

t_beta1 = coef(lm_returns)[2] / sqrt(diag(vcov(lm_returns)))[2]
t_beta0 = coef(lm_returns)[1] / sqrt(diag(vcov(lm_returns)))[1]

ddl = 246 - 2 # degré de liberté
t_theorique = stats::qt(1-0.05/2, ddl) #utiliser la fonction "qt" du package "stats" pour avoir la distribution de Student

# t_beta1 > t_theorique ==> rejet de H_O
# t_beta0 < t_theorique ==> non-rejet de H_O

