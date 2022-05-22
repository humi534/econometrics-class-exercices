
library(readxl)
setwd(dir = "C:/Users/hugop/OneDrive/Documents/économétrie/TP0")
UKHP <- read_excel("UKHP.xls",col_types = c("date", "numeric"))
View(UKHP)
summary(UKHP)
names(UKHP)
mean(UKHP$ 'Average House Price')

#renommer
names(UKHP)[2]='hp'

#le taux de croissance
UKHP$dhp = c(NA, 100*diff(UKHP$hp)/UKHP$hp[1:nrow(UKHP)-1])  #le -1 est la prcq pas de croissance pour la première année

plot(x=UKHP$Month, y=UKHP$hp, type="l",xlab="Date", ylab="Prix")


hist(UKHP$dhp)
