
install.packages('quantmod')
library(quantmod)
getSymbols(c('^GSPC','^FTSE','^HSI','^GDAXI','^FCHI','^BFX'),
            src = 'yahoo',
            from = '2005-01-01', #♣date au format yyyy-mm-dd
            to = '2015-12-31')

View(BFX)


GSPC <- na.omit(GSPC)
FTSE <- na.omit(FTSE)

hist(GSPC$GSPC.Close, probability = TRUE, main="Histogramme S&P500")
lines(density(GSPC$GSPC.Close))

closing_price <- cbind(GSPC$GSPC.Close, FTSE$FTSE.Close)
closing_price <- as.data.frame(closing_price)

#hypothèse de normalité
shapiro.test(closing_price$GSPC.Close)
# H0 l'échantillon suit une loi normale; donc si la p-value est significative, on rejette H0
#Cela implique que dans notre cas, on a un p-value très significative donc on rejette H0
#conclusion: l'indice sp500 ne suit pas une distribution normale
