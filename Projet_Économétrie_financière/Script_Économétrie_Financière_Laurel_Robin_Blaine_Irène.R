#################### Projet Économétrie Financière ######################

library(readxl)
library(fBasics)
library(dplyr) #pour %>%
library(magrittr)
library(xts)
library(PerformanceAnalytics)
library(MASS)
library(mvtnorm)
library(FinTS)
library(tseries)
library(rugarch)
options(digits = 4)

brent <- read_excel("Futures pétrole Brent - Données Historiques.xlsm", 
                    col_types = c("date", "numeric", "numeric", 
                                  "numeric", "numeric", "numeric", 
                                  "numeric"))
View(brent)

carbone_prices <- read_excel("carbone_prices.xls", 
                             col_types = c("date", "numeric"))
View(carbone_prices)

ERIXX <- read_excel("ERIXX.xls", col_types = c("date", 
                                               "numeric", "numeric", "numeric", "numeric", 
                                               "numeric", "numeric"))
View(ERIXX)

ICE <- read_excel("ICE Dutch TTF Natural Gas Futures - Données Historiques.xlsm", 
                  col_types = c("date", "numeric", "numeric", 
                                "numeric", "numeric", "numeric", 
                                "numeric"))
View(ICE)

CLIM <- read_excel("CLIM.xlsm", col_types = c("date", 
                                              "numeric", "numeric", "numeric", "numeric", 
                                              "numeric", "numeric"))
View(CLIM)

coal <- read_excel("Rotterdam Coal Futures - Données Historiques.xlsm", 
                   col_types = c("date", "numeric", "numeric", 
                                 "numeric", "numeric", "numeric", 
                                 "numeric"))
View(coal)

##################### statistiques descriptives ##################
summary(coal)
summary(CLIM)
summary(ICE)
summary(ERIXX)
summary(carbone_prices)
summary(brent)

names(brent)
names(carbone_prices)
names(ERIXX)
names(ICE)
names(CLIM)
names(coal)

##### merge et préparation à la modélisation ####

B1 <- brent %>%
  merge(carbone_prices, by = "Date") %>%
  merge(ERIXX, by = "Date") %>%
  merge(ICE, by = "Date") %>%
  merge(CLIM, by = "Date") %>%
  merge(coal, by = "Date")
View(B1)
names(B1)

#nommons les prix

brent_p  <- B1[, 2]
carbon_p <- B1[, 8]
erixx_p  <- B1[, 9]
ice_p    <- B1[, 15]
clim_p   <- B1[, 21]
coal_p   <- B1[, 27]
td <- as.Date(B1$Date)

brent_xt  <- xts(brent_p,  order.by = td)
carbon_xt <- xts(carbon_p, order.by = td)
erixx_xt  <- xts(erixx_p,  order.by = td)
ice_xt    <- xts(ice_p,    order.by = td)
clim_xt   <- xts(clim_p,   order.by = td)
coal_xt   <- xts(coal_p,   order.by = td)

all_prices <- cbind(brent_xt, carbon_xt, erixx_xt, ice_xt, clim_xt, coal_xt)
colnames(all_prices) <- c("Brent","Carbon","ERIXX","Gas","CLIM","Coal")

plot.zoo(all_prices,
         plot.type = "single",
         col = c("blue","black","orange","green","red","purple"),
         lty = 1, lwd = 1,
         main = "Evolution des prix des marchés énergie et carbone",
         xlab = "Date", ylab = "Prix")

legend("topright",
       legend = c("Brent","Carbon","ERIXX","Gas","CLIM","Coal"),
       col    = c("blue","black","orange","green","red","purple"),
       lty = 1, lwd = 2)

#pour mieux observer les conjonctures sans ERIX
prices_without_erix <- cbind(brent_xt, carbon_xt, ice_xt, clim_xt, coal_xt)
colnames(prices_without_erix) <- c("Brent","Carbon","Gas","CLIM","Coal")

plot.zoo(prices_without_erix,
         plot.type = "single",
         col = c("blue","black","green","red","purple"),
         lty = 1, lwd = 1,
         main = "Evolution des prix des marchés, sans ERIX",
         xlab = "Date", ylab = "Prix")

legend("topright",
       legend = c("Brent","Carbon","Gas","CLIM","Coal"),
       col    = c("blue","black","green","red","purple"),
       lty = 1, lwd = 2)

######### calcul des rendements ########

rbrent  <- na.omit(CalculateReturns(brent_xt,  method="log"))
rcarbon <- na.omit(CalculateReturns(carbon_xt, method="log"))
rerixx  <- na.omit(CalculateReturns(erixx_xt,  method="log"))
rice    <- na.omit(CalculateReturns(ice_xt,    method="log"))
rclim   <- na.omit(CalculateReturns(clim_xt,   method="log"))
rcoal   <- na.omit(CalculateReturns(coal_xt,   method="log"))

plot(rbrent, main="Rendements Brent")
plot(rcoal, main="Rendements Coal")
plot(rerixx, main="Rendements ERIXX")
plot(rclim, main="Rendements CLIM")
plot(rice, main="Rendements ICE")

multi <- na.omit(cbind(rbrent, rerixx, rice, rclim, rcoal, rcarbon))
multi <- multi[-1, ]
multi <- na.omit(multi)
cor(multi)

basicStats(multi)

#### quelques histogrammes ####

hist(rbrent, prob = TRUE, breaks = 100,
     main = "Histogramme rendements Brent")

lines(density(rbrent), col="blue", lwd=2)

curve(dnorm(x, mean(rbrent), sd(rbrent)),
      col="red", lwd=2, add=TRUE)

legend("topright",
       legend = c("Densité empirique","Loi normale"),
       col = c("blue","red"),
       lwd = 2)

hist(rcarbon, prob = TRUE, breaks = 100,
     main = "Histogramme rendements du Carbone")

lines(density(rcarbon), col="blue", lwd=2)

curve(dnorm(x, mean(rcarbon), sd(rcarbon)),
      col="red", lwd=2, add=TRUE)

legend("topright",
       legend = c("Densité empirique","Loi normale"),
       col = c("blue","red"),
       lwd = 2)

##################### tests de stationnarité sur les prix #####################

adf.test(brent_p)
adf.test(carbon_p)
adf.test(erixx_p)
adf.test(ice_p)
adf.test(clim_p)
adf.test(coal_p)

kpss.test(brent_p)
kpss.test(carbon_p)
kpss.test(erixx_p)
kpss.test(ice_p)
kpss.test(clim_p)
kpss.test(coal_p)

##################### tests de stationnarité sur les rendements #############

adf.test(rbrent)
adf.test(rcarbon)
adf.test(rerixx)
adf.test(rice)
adf.test(rclim)
adf.test(rcoal)

kpss.test(rbrent)
kpss.test(rcarbon)
kpss.test(rerixx)
kpss.test(rice)
kpss.test(rclim)
kpss.test(rcoal)

acf(rcoal, main="ACF coal returns")
pacf(rcoal, main="PACF coal returns")

##################### ACF / PACF des rendements #####################

acf(rbrent, main="ACF Brent returns")
pacf(rbrent, main="PACF Brent returns")

acf(rcarbon, main="ACF Carbon returns")
pacf(rcarbon, main="PACF Carbon returns")

acf(rerixx, main="ACF ERIXX returns")
pacf(rerixx, main="PACF ERIXX returns")

acf(rice, main="ACF Gas returns")
pacf(rice, main="PACF Gas returns")

acf(rclim, main="ACF CLIM returns")
pacf(rclim, main="PACF CLIM returns")

acf(rcoal, main="ACF Coal returns")
pacf(rcoal, main="PACF Coal returns")

################### modélisation brent ##############

############################# tests ARCH / normalité #############################

at = rbrent - mean(rbrent)

ArchTest(at)
Box.test(at,  lag = 15, type="Ljung-Box")
Box.test(at^2, lag = 15, type="Ljung-Box")

jarque.bera.test(rbrent)
acf(at^2)

############################# densité 

d <- density(rbrent)

hist(rbrent, prob = TRUE, breaks = 100)

lines(d, col="blue", lwd=2)

curve(dnorm(x, mean(rbrent), sd(rbrent)),
      col="orange",
      lwd=2,
      add=TRUE)

legend("topright",
       legend = c("Estimation densité","Loi normale"),
       lty=c(1,1),
       lwd=2)

abline(v=0,lwd=2)

############################# tests stationnarité #############################

adf.test(rbrent)
pp.test(rbrent)
kpss.test(rbrent)

############################# ARMA (moyenne) #############################

acf(rbrent,lag=20)
pacf(rbrent,lag=20)

m1 = arima(rbrent, order=c(1,0,1), include.mean=FALSE)
m1

Box.test(m1$residuals, lag=12, type="Ljung-Box")
ArchTest(m1$residuals,12)

############################# GARCH #############################

spec <- ugarchspec(
  variance.model = list(model = "sGARCH", garchOrder = c(1,1)),
  mean.model = list(armaOrder = c(1,1)),
  distribution.model = "norm"
)

garch.brent <- ugarchfit(spec = spec, data = rbrent)
garch.brent

AutocorTest(residuals(garch.brent, standardize = TRUE), lag = 12)
AutocorTest(residuals(garch.brent, standardize = TRUE)^2, lag = 20)
ArchTest(residuals(garch.brent, standardize = TRUE), lags = 20)

############################# EGARCH #############################

egarch.spec <- ugarchspec(
  variance.model = list(model = "eGARCH", garchOrder = c(1,1)),
  mean.model = list(armaOrder = c(1,1)),
  distribution.model = "norm"
)

egarch.brent <- ugarchfit(egarch.spec, rbrent)

egarch.brent

############################# GJR-GARCH #############################

gjr.spec <- ugarchspec(
  variance.model = list(model = "gjrGARCH", garchOrder = c(1,1)),
  mean.model = list(armaOrder = c(0,1)),
  distribution.model = "norm"
)

gjr.brent <- ugarchfit(gjr.spec, rbrent)

gjr.brent

############################# comparaison modèles #############################

model.list = list(garch.brent,
                  egarch.brent,
                  gjr.brent)

info.mat = sapply(model.list, infocriteria)

rownames(info.mat) = rownames(infocriteria(garch.brent))

info.mat

plot(gjr.brent, which = 8)

############################# distribution Student #############################

spec.std <- ugarchspec(
  variance.model = list(model = "gjrGARCH", garchOrder = c(1,1)),
  mean.model     = list(armaOrder = c(0,1)),
  distribution.model = "std"
)

gjr.brent_std <- ugarchfit(spec.std, data = rbrent)
gjr.brent_std

infocriteria(gjr.brent)
infocriteria(gjr.brent_std)

AutocorTest(residuals(gjr.brent_std, standardize=TRUE), lag=12)
AutocorTest(residuals(gjr.brent_std, standardize=TRUE)^2, lag=20)
ArchTest(residuals(gjr.brent_std, standardize=TRUE), lags=20)

plot(gjr.brent_std, which = 8)
plot(gjr.brent_std, which = 9)



################### modélisation ERIX ##############

############################# tests ARCH / normalité #############################

at_e = rerixx - mean(rerixx)

ArchTest(at_e)
Box.test(at_e,  lag = 15, type="Ljung-Box")
Box.test(at_e^2, lag = 15, type="Ljung-Box")

jarque.bera.test(rerixx)
acf(at_e^2)

############################# densité ###########################################

d_e <- density(rerixx)

hist(rerixx, prob = TRUE, breaks = 100)

lines(d_e, col="blue", lwd=2)

curve(dnorm(x, mean(rerixx), sd(rerixx)),
      col="orange",
      lwd=2,
      add=TRUE)

legend("topright",
       legend = c("Estimation densité","Loi normale"),
       lty=c(1,1),
       lwd=2)

abline(v=0,lwd=2)

############################# tests stationnarité ###############################

adf.test(rerixx)
pp.test(rerixx)
kpss.test(rerixx)

############################# ARMA (moyenne) ####################################

acf(rerixx,lag=20)
pacf(rerixx,lag=20)

m1_e = arima(rerixx, order=c(1,0,1), include.mean=FALSE)
m1_e

Box.test(m1_e$residuals, lag=12, type="Ljung-Box")
ArchTest(m1_e$residuals, 12)

############################# GARCH #############################################

spec_e <- ugarchspec(
  variance.model = list(model = "sGARCH", garchOrder = c(1,1)),
  mean.model = list(armaOrder = c(1,1)),
  distribution.model = "norm"
)

garch.erixx <- ugarchfit(spec = spec_e, data = rerixx)
garch.erixx

AutocorTest(residuals(garch.erixx, standardize = TRUE), lag = 12)
AutocorTest(residuals(garch.erixx, standardize = TRUE)^2, lag = 20)
ArchTest(residuals(garch.erixx, standardize = TRUE), lags = 20)

############################# EGARCH ###########################################

egarch.spec_e <- ugarchspec(
  variance.model = list(model = "eGARCH", garchOrder = c(1,1)),
  mean.model = list(armaOrder = c(1,1)),
  distribution.model = "norm"
)

egarch.erixx <- ugarchfit(egarch.spec_e, rerixx)
egarch.erixx

############################# GJR-GARCH ########################################

gjr.spec_e <- ugarchspec(
  variance.model = list(model = "gjrGARCH", garchOrder = c(1,1)),
  mean.model = list(armaOrder = c(0,1)),
  distribution.model = "norm"
)

gjr.erixx <- ugarchfit(gjr.spec_e, rerixx)
gjr.erixx

############################# comparaison modèles ###############################

model.list_e <- list(garch.erixx,
                     egarch.erixx,
                     gjr.erixx)

info.mat_e <- sapply(model.list_e, infocriteria)
rownames(info.mat_e) <- rownames(infocriteria(garch.erixx))
info.mat_e

plot(garch.erixx, which = 8)

############################# distribution Student #############################

spec.std_e <- ugarchspec(
  variance.model = list(model = "sGARCH", garchOrder = c(1,1)),
  mean.model     = list(armaOrder = c(1,1)),
  distribution.model = "std"
)

garch.erixx_std <- ugarchfit(spec.std_e, data = rerixx)
garch.erixx_std

infocriteria(garch.erixx)
infocriteria(garch.erixx_std)

AutocorTest(residuals(garch.erixx_std, standardize=TRUE), lag=12)
AutocorTest(residuals(garch.erixx_std, standardize=TRUE)^2, lag=20)
ArchTest(residuals(garch.erixx_std, standardize=TRUE), lags=20)

plot(garch.erixx_std, which = 8)
plot(garch.erixx_std, which = 9)



################### modélisation ICE ##############

############################# tests ARCH / normalité #############################

at_i = rice - mean(rice)

ArchTest(at_i)
Box.test(at_i,  lag = 15, type="Ljung-Box")
Box.test(at_i^2, lag = 15, type="Ljung-Box")

jarque.bera.test(rice)
acf(at_i^2)

############################# densité ###########################################

d_i <- density(rice)

hist(rice, prob = TRUE, breaks = 100)

lines(d_i, col="blue", lwd=2)

curve(dnorm(x, mean(rice), sd(rice)),
      col="orange",
      lwd=2,
      add=TRUE)

legend("topright",
       legend = c("Estimation densité","Loi normale"),
       lty=c(1,1),
       lwd=2)

abline(v=0,lwd=2)

############################# tests stationnarité ###############################

adf.test(rice)
pp.test(rice)
kpss.test(rice)

############################# ARMA (moyenne) ####################################

acf(rice,lag=20)
pacf(rice,lag=20)

m1_i = arima(rice, order=c(1,0,1), include.mean=FALSE)
m1_i

Box.test(m1_i$residuals, lag=12, type="Ljung-Box")
ArchTest(m1_i$residuals, 12)

############################# GARCH #############################################

spec_i <- ugarchspec(
  variance.model = list(model = "sGARCH", garchOrder = c(1,1)),
  mean.model = list(armaOrder = c(1,1)),
  distribution.model = "norm"
)

garch.ice <- ugarchfit(spec = spec_i, data = rice)
garch.ice

AutocorTest(residuals(garch.ice, standardize = TRUE), lag = 12)
AutocorTest(residuals(garch.ice, standardize = TRUE)^2, lag = 20)
ArchTest(residuals(garch.ice, standardize = TRUE), lags = 20)

############################# EGARCH ###########################################

egarch.spec_i <- ugarchspec(
  variance.model = list(model = "eGARCH", garchOrder = c(1,1)),
  mean.model = list(armaOrder = c(1,1)),
  distribution.model = "norm"
)

egarch.ice <- ugarchfit(egarch.spec_i, rice)
egarch.ice

############################# GJR-GARCH ########################################

gjr.spec_i <- ugarchspec(
  variance.model = list(model = "gjrGARCH", garchOrder = c(1,1)),
  mean.model = list(armaOrder = c(0,1)),
  distribution.model = "norm"
)

gjr.ice <- ugarchfit(gjr.spec_i, rice)
gjr.ice

############################# comparaison modèles ###############################

model.list_i <- list(garch.ice,
                     egarch.ice,
                     gjr.ice)

info.mat_i <- sapply(model.list_i, infocriteria)
rownames(info.mat_i) <- rownames(infocriteria(garch.ice))
info.mat_i

plot(garch.ice, which = 8)

############################# distribution Student #############################

spec.std_i <- ugarchspec(
  variance.model = list(model = "sGARCH", garchOrder = c(1,1)),
  mean.model     = list(armaOrder = c(1,1)),
  distribution.model = "std"
)

garch.ice_std <- ugarchfit(spec.std_i, data = rice)
garch.ice_std

infocriteria(garch.ice)
infocriteria(garch.ice_std)

AutocorTest(residuals(garch.ice_std, standardize=TRUE), lag=12)
AutocorTest(residuals(garch.ice_std, standardize=TRUE)^2, lag=20)
ArchTest(residuals(garch.ice_std, standardize=TRUE), lags=20)

plot(garch.ice_std, which = 8)
plot(garch.ice_std, which = 9)



################### modélisation CLIM ##############

############################# tests ARCH / normalité #############################

at_cl = rclim - mean(rclim)

ArchTest(at_cl)
Box.test(at_cl,  lag = 15, type="Ljung-Box")
Box.test(at_cl^2, lag = 15, type="Ljung-Box")

jarque.bera.test(rclim)
acf(at_cl^2)

############################# densité ###########################################

d_cl <- density(rclim)

hist(rclim, prob = TRUE, breaks = 100)

lines(d_cl, col="blue", lwd=2)

curve(dnorm(x, mean(rclim), sd(rclim)),
      col="orange",
      lwd=2,
      add=TRUE)

legend("topright",
       legend = c("Estimation densité","Loi normale"),
       lty=c(1,1),
       lwd=2)

abline(v=0,lwd=2)

############################# tests stationnarité ###############################

adf.test(rclim)
pp.test(rclim)
kpss.test(rclim)

############################# ARMA (moyenne) ####################################

acf(rclim,lag=20)
pacf(rclim,lag=20)

m1_cl = arima(rclim, order=c(1,0,1), include.mean=FALSE)
m1_cl

Box.test(m1_cl$residuals, lag=12, type="Ljung-Box")
ArchTest(m1_cl$residuals, 12)

############################# GARCH #############################################

spec_cl <- ugarchspec(
  variance.model = list(model = "sGARCH", garchOrder = c(1,1)),
  mean.model = list(armaOrder = c(1,1)),
  distribution.model = "norm"
)

garch.clim <- ugarchfit(spec = spec_cl, data = rclim)
garch.clim

AutocorTest(residuals(garch.clim, standardize = TRUE), lag = 12)
AutocorTest(residuals(garch.clim, standardize = TRUE)^2, lag = 20)
ArchTest(residuals(garch.clim, standardize = TRUE), lags = 20)

############################# EGARCH ###########################################

egarch.spec_cl <- ugarchspec(
  variance.model = list(model = "eGARCH", garchOrder = c(1,1)),
  mean.model = list(armaOrder = c(1,1)),
  distribution.model = "norm"
)

egarch.clim <- ugarchfit(egarch.spec_cl, rclim)
egarch.clim

############################# GJR-GARCH ########################################

gjr.spec_cl <- ugarchspec(
  variance.model = list(model = "gjrGARCH", garchOrder = c(1,1)),
  mean.model = list(armaOrder = c(0,1)),
  distribution.model = "norm"
)

gjr.clim <- ugarchfit(gjr.spec_cl, rclim)
gjr.clim

############################# comparaison modèles ###############################

model.list_cl <- list(garch.clim,
                      egarch.clim,
                      gjr.clim)

info.mat_cl <- sapply(model.list_cl, infocriteria)
rownames(info.mat_cl) <- rownames(infocriteria(garch.clim))
info.mat_cl

plot(garch.clim, which = 8)

############################# distribution Student #############################

spec.std_cl <- ugarchspec(
  variance.model = list(model = "sGARCH", garchOrder = c(1,1)),
  mean.model     = list(armaOrder = c(1,1)),
  distribution.model = "std"
)

garch.clim_std <- ugarchfit(spec.std_cl, data = rclim)
garch.clim_std

infocriteria(garch.clim)
infocriteria(garch.clim_std)

AutocorTest(residuals(garch.clim_std, standardize=TRUE), lag=12)
AutocorTest(residuals(garch.clim_std, standardize=TRUE)^2, lag=20)
ArchTest(residuals(garch.clim_std, standardize=TRUE), lags=20)

plot(garch.clim_std, which = 8)
plot(garch.clim_std, which = 9)



################### modélisation COAL ##############

############################# tests ARCH / normalité #############################

at_co = rcoal - mean(rcoal)

ArchTest(at_co)
Box.test(at_co,  lag = 15, type="Ljung-Box")
Box.test(at_co^2, lag = 15, type="Ljung-Box")

jarque.bera.test(rcoal)
acf(at_co^2)

############################# densité ###########################################

d_co <- density(rcoal)

hist(rcoal, prob = TRUE, breaks = 100)

lines(d_co, col="blue", lwd=2)

curve(dnorm(x, mean(rcoal), sd(rcoal)),
      col="orange",
      lwd=2,
      add=TRUE)

legend("topright",
       legend = c("Estimation densité","Loi normale"),
       lty=c(1,1),
       lwd=2)

abline(v=0,lwd=2)

############################# tests stationnarité ###############################

adf.test(rcoal)
pp.test(rcoal)
kpss.test(rcoal)

############################# ARMA (moyenne) ####################################

acf(rcoal,lag=20)
pacf(rcoal,lag=20)

m1_co = arima(rcoal, order=c(1,0,1), include.mean=FALSE)
m1_co

Box.test(m1_co$residuals, lag=12, type="Ljung-Box")
ArchTest(m1_co$residuals, 12)

############################# GARCH #############################################

spec_co <- ugarchspec(
  variance.model = list(model = "sGARCH", garchOrder = c(1,1)),
  mean.model = list(armaOrder = c(1,1)),
  distribution.model = "norm"
)

garch.coal <- ugarchfit(spec = spec_co, data = rcoal)
garch.coal

AutocorTest(residuals(garch.coal, standardize = TRUE), lag = 12)
AutocorTest(residuals(garch.coal, standardize = TRUE)^2, lag = 20)
ArchTest(residuals(garch.coal, standardize = TRUE), lags = 20)

############################# EGARCH ###########################################

egarch.spec_co <- ugarchspec(
  variance.model = list(model = "eGARCH", garchOrder = c(1,1)),
  mean.model = list(armaOrder = c(1,1)),
  distribution.model = "norm"
)

egarch.coal <- ugarchfit(egarch.spec_co, rcoal)
egarch.coal

############################# GJR-GARCH ########################################

gjr.spec_co <- ugarchspec(
  variance.model = list(model = "gjrGARCH", garchOrder = c(1,1)),
  mean.model = list(armaOrder = c(0,1)),
  distribution.model = "norm"
)

gjr.coal <- ugarchfit(gjr.spec_co, rcoal)
gjr.coal

############################# comparaison modèles ###############################

model.list_co <- list(garch.coal,
                      egarch.coal,
                      gjr.coal)

info.mat_co <- sapply(model.list_co, infocriteria)
rownames(info.mat_co) <- rownames(infocriteria(garch.coal))
info.mat_co

plot(garch.coal, which = 8)

############################# distribution Student #############################

spec.std_co <- ugarchspec(
  variance.model = list(model = "sGARCH", garchOrder = c(1,1)),
  mean.model     = list(armaOrder = c(1,1)),
  distribution.model = "std"
)

garch.coal_std <- ugarchfit(spec.std_co, data = rcoal)
garch.coal_std

infocriteria(garch.coal)
infocriteria(garch.coal_std)

AutocorTest(residuals(garch.coal_std, standardize=TRUE), lag=12)
AutocorTest(residuals(garch.coal_std, standardize=TRUE)^2, lag=20)
ArchTest(residuals(garch.coal_std, standardize=TRUE), lags=20)

plot(garch.coal_std, which = 8)
plot(garch.coal_std, which = 9)



########################## Comparons modèles en comparant leurs VaR et ES

ret_energy <- na.omit(cbind(rbrent, rerixx, rice, rclim, rcoal))
colnames(ret_energy) <- c("Brent","ERIXX","ICE","CLIM","Coal")

######################### Statistiques descriptives #########################

basicStats(ret_energy)

apply(ret_energy, 2, mean)
apply(ret_energy, 2, var)
apply(ret_energy, 2, sd)

apply(ret_energy, 2, function(x) normalTest(as.numeric(x), method="jb"))

t.test(as.numeric(rbrent))

s3 <- skewness(as.numeric(rbrent))
Tn <- length(rbrent)
t3 <- s3 / sqrt(6 / Tn)
pp <- 2 * (1 - pnorm(abs(t3)))

s4 <- kurtosis(as.numeric(rbrent))
t4 <- s4 / sqrt(24 / Tn)

normalTest(as.numeric(rbrent), method = "jb")

######################### Volatilité  #########################

apply(ret_energy, 2, sd)

######################### VaR historique #########################

apply(ret_energy, 2, quantile, probs = c(0.05, 0.01))

VaR(ret_energy, p = 0.95, method = "historical")
VaR(ret_energy, p = 0.99, method = "historical")

#########################
### ES historique manuel

ES.fun <- function(x, alpha = 0.05){
  qhat <- quantile(x, probs = alpha)
  mean(x[x <= qhat])
}

apply(ret_energy, 2, ES.fun, alpha = 0.05)
apply(ret_energy, 2, ES.fun, alpha = 0.01)

ES(ret_energy, p = 0.95, method = "historical")
ES(ret_energy, p = 0.99, method = "historical")

#########################
### Loi normale (manuel)

mu_    <- apply(ret_energy, 2, mean)
sigma_ <- apply(ret_energy, 2, sd)

q05_norm <- mu_ + sigma_ * qnorm(0.05)
q01_norm <- mu_ + sigma_ * qnorm(0.01)

ES05_norm <- -(mu_ + sigma_ * dnorm(qnorm(0.05)) / 0.05)
ES01_norm <- -(mu_ + sigma_ * dnorm(qnorm(0.01)) / 0.01)

q05_norm
q01_norm
ES05_norm
ES01_norm

#########################
### Loi normale via package

VaR(ret_energy, p = 0.95, method = "gaussian")
VaR(ret_energy, p = 0.99, method = "gaussian")

ES(ret_energy, p = 0.95, method = "gaussian")
ES(ret_energy, p = 0.99, method = "gaussian")

#########################
### Exemple calcule avec Loi t (Brent)

fit_t_brent <- fitdistr(as.numeric(rbrent) * 100, densfun = "t")

theta   <- coef(fit_t_brent)
mu_t    <- theta["m"] / 100
sigma_t <- theta["s"] / 100
df_t    <- theta["df"]

theta

q_t05 <- qt(0.05, df = df_t)
q_t01 <- qt(0.01, df = df_t)

q_brent_t05 <- mu_t + sigma_t * q_t05
q_brent_t01 <- mu_t + sigma_t * q_t01

q_brent_t05
q_brent_t01

M_t05 <- (dt(q_t05, df = df_t) / 0.05) *
  ((df_t + q_t05^2) / (df_t - 1))

es_brent_t05 <- -(mu_t + sigma_t * M_t05)
es_brent_t05

M_t01 <- (dt(q_t01, df = df_t) / 0.01) *
  ((df_t + q_t01^2) / (df_t - 1))

es_brent_t01 <- -(mu_t + sigma_t * M_t01)
es_brent_t01

#########################
### Cornish-Fisher (modified)

VaR(ret_energy, p = 0.95, method = "modified")
VaR(ret_energy, p = 0.99, method = "modified")
ES(ret_energy,  p = 0.95, method = "modified")
ES(ret_energy,  p = 0.99, method = "modified")

#########################
### Portefeuille simple Brent / ERIXX 

w <- c(0.5, 0.5)

r_port <- as.xts(0.5 * coredata(rbrent) +
                   0.5 * coredata(rerixx),
                 order.by = index(rbrent))

colnames(r_port) <- "Port"

plot(r_port, main = "Rendements du portefeuille")

mean(r_port)
sd(r_port)

VaR(r_port, p = 0.95, method = "historical")
VaR(r_port, p = 0.99, method = "historical")

ES(r_port, p = 0.95, method = "historical")
ES(r_port, p = 0.99, method = "historical")

#########################
### Covariance & volatilité portefeuille

Sigma_c <- cov(na.omit(cbind(rbrent, rerixx)))
sigma_p <- as.numeric(sqrt(t(w) %*% Sigma_c %*% w))

cont.marg.vol <- Sigma_c %*% w / sigma_p
cont.vol <- w * cont.marg.vol
cont.vol.pct <- cont.vol / sigma_p
cbind(cont.marg.vol, cont.vol, cont.vol.pct)

sigma_p
sum(cont.vol)

StdDev(na.omit(cbind(rbrent, rerixx)),
       portfolio_method = "component",
       weights = w)

#########################
### VaR / ES gaussienne portefeuille

VaR(r_port, p = 0.95, method = "gaussian")
VaR(r_port, p = 0.99, method = "gaussian")
ES(r_port,  p = 0.95, method = "gaussian")
ES(r_port,  p = 0.99, method = "gaussian")

#########################
### Distribution conjointe Brent / ERIX

plot(coredata(ret_energy[, "Brent"]),
     coredata(ret_energy[, "ERIXX"]),
     xlab = "Brent",
     ylab = "ERIX",
     main = "Distribution conjointe Brent / ERIX")

abline(h = mean(ret_energy[, "ERIXX"]),
       v = mean(ret_energy[, "Brent"]))

#########################
### Histogramme rendements Brent

chart.Histogram(rbrent,
                main = "Histogramme rendements Brent")

#########################
### Histogramme rendements ERIX

chart.Histogram(rerixx,
                main = "Histogramme rendements ERIX")

#########################
### Histogramme portefeuille Brent / ERIXX

chart.Histogram(r_port,
                main = "Histogramme rendement portefeuille (Brent / ERIXX)")




##################### backtesting ##################### 

########################## Backtesting VaR ##########################

# Definir les fenetres d'estimation et de test
n.obs <- nrow(rbrent)
FE    <- 1000
FT    <- n.obs - FE
alpha <- 0.99

### Backtesting, VaR, violations
backTestVaR <- function(x, p = 0.95) {
  normal.VaR     <- as.numeric(VaR(x, p=p, method="gaussian"))
  historical.VaR <- as.numeric(VaR(x, p=p, method="historical"))
  modified.VaR   <- as.numeric(VaR(x, p=p, method="modified"))
  ans <- c(normal.VaR, historical.VaR, modified.VaR)
  names(ans) <- c("Normal", "HS", "Modified")
  return(ans)
}

################### Backtesting BRENT ###################

VaR.results.brent <- rollapply(as.zoo(rbrent), width=FE,
                               FUN = backTestVaR, p=0.99, by.column = FALSE,
                               align = "right")
VaR.results.brent <- stats::lag(VaR.results.brent, k=-1)
chart.TimeSeries(merge(rbrent, VaR.results.brent),
                 legend.loc="topright",
                 main="Backtesting VaR - Brent")

violations.mat.brent <- matrix(0, 3, 5)
rownames(violations.mat.brent) <- c("Normal", "HS", "Modified")
colnames(violations.mat.brent) <- c("En1", "n1", "1-alpha", "Percent", "VR")
violations.mat.brent[, "En1"] <- (1-alpha)*FT
violations.mat.brent[, "1-alpha"] <- 1 - alpha

normalVaR.violations.brent <- as.zoo(rbrent[index(VaR.results.brent), ]) < VaR.results.brent[, "Normal"]
violation.dates.brent <- index(normalVaR.violations.brent[which(normalVaR.violations.brent)])

plot(as.zoo(rbrent[index(VaR.results.brent),]), col="blue", ylab="Return",
     main="Violations VaR - Brent")
abline(h=0)
lines(VaR.results.brent[, "Normal"], col="black", lwd=2)
lines(as.zoo(rbrent[violation.dates.brent,]), type="p", pch=16, col="red", lwd=2)

for(i in colnames(VaR.results.brent)) {
  VaR.violations <- as.zoo(rbrent[index(VaR.results.brent), ]) < VaR.results.brent[, i]
  violations.mat.brent[i, "n1"] <- sum(VaR.violations)
  violations.mat.brent[i, "Percent"] <- sum(VaR.violations)/FT
  violations.mat.brent[i, "VR"] <- violations.mat.brent[i, "n1"]/violations.mat.brent[i, "En1"]
}

violations.mat.brent

vaR.test.brent <- VaRTest(1-alpha,
                          actual=coredata(rbrent[index(VaR.results.brent),]),
                          VaR=coredata(VaR.results.brent[,"Normal"]))
names(vaR.test.brent)
vaR.test.brent[1:7]
vaR.test.brent[8:12]

################### Backtesting ERIXX ###################

VaR.results.erixx <- rollapply(as.zoo(rerixx), width=FE,
                               FUN = backTestVaR, p=0.99, by.column = FALSE,
                               align = "right")
VaR.results.erixx <- stats::lag(VaR.results.erixx, k=-1)
chart.TimeSeries(merge(rerixx, VaR.results.erixx),
                 legend.loc="topright",
                 main="Backtesting VaR - ERIX")

violations.mat.erixx <- matrix(0, 3, 5)
rownames(violations.mat.erixx) <- c("Normal", "HS", "Modified")
colnames(violations.mat.erixx) <- c("En1", "n1", "1-alpha", "Percent", "VR")
violations.mat.erixx[, "En1"] <- (1-alpha)*FT
violations.mat.erixx[, "1-alpha"] <- 1 - alpha

normalVaR.violations.erixx <- as.zoo(rerixx[index(VaR.results.erixx), ]) < VaR.results.erixx[, "Normal"]
violation.dates.erixx <- index(normalVaR.violations.erixx[which(normalVaR.violations.erixx)])

plot(as.zoo(rerixx[index(VaR.results.erixx),]), col="blue", ylab="Return",
     main="Violations VaR - ERIX")
abline(h=0)
lines(VaR.results.erixx[, "Normal"], col="black", lwd=2)
lines(as.zoo(rerixx[violation.dates.erixx,]), type="p", pch=16, col="red", lwd=2)

for(i in colnames(VaR.results.erixx)) {
  VaR.violations <- as.zoo(rerixx[index(VaR.results.erixx), ]) < VaR.results.erixx[, i]
  violations.mat.erixx[i, "n1"] <- sum(VaR.violations)
  violations.mat.erixx[i, "Percent"] <- sum(VaR.violations)/FT
  violations.mat.erixx[i, "VR"] <- violations.mat.erixx[i, "n1"]/violations.mat.erixx[i, "En1"]
}

violations.mat.erixx

vaR.test.erixx <- VaRTest(1-alpha,
                          actual=coredata(rerixx[index(VaR.results.erixx),]),
                          VaR=coredata(VaR.results.erixx[,"Normal"]))
names(vaR.test.erixx)
vaR.test.erixx[1:7]
vaR.test.erixx[8:12]

################### Backtesting ICE ###################

VaR.results.ice <- rollapply(as.zoo(rice), width=FE,
                             FUN = backTestVaR, p=0.99, by.column = FALSE,
                             align = "right")
VaR.results.ice <- stats::lag(VaR.results.ice, k=-1)
chart.TimeSeries(merge(rice, VaR.results.ice),
                 legend.loc="topright",
                 main="Backtesting VaR - ICE")

violations.mat.ice <- matrix(0, 3, 5)
rownames(violations.mat.ice) <- c("Normal", "HS", "Modified")
colnames(violations.mat.ice) <- c("En1", "n1", "1-alpha", "Percent", "VR")
violations.mat.ice[, "En1"] <- (1-alpha)*FT
violations.mat.ice[, "1-alpha"] <- 1 - alpha

normalVaR.violations.ice <- as.zoo(rice[index(VaR.results.ice), ]) < VaR.results.ice[, "Normal"]
violation.dates.ice <- index(normalVaR.violations.ice[which(normalVaR.violations.ice)])

plot(as.zoo(rice[index(VaR.results.ice),]), col="blue", ylab="Return",
     main="Violations VaR - ICE")
abline(h=0)
lines(VaR.results.ice[, "Normal"], col="black", lwd=2)
lines(as.zoo(rice[violation.dates.ice,]), type="p", pch=16, col="red", lwd=2)

for(i in colnames(VaR.results.ice)) {
  VaR.violations <- as.zoo(rice[index(VaR.results.ice), ]) < VaR.results.ice[, i]
  violations.mat.ice[i, "n1"] <- sum(VaR.violations)
  violations.mat.ice[i, "Percent"] <- sum(VaR.violations)/FT
  violations.mat.ice[i, "VR"] <- violations.mat.ice[i, "n1"]/violations.mat.ice[i, "En1"]
}

violations.mat.ice

vaR.test.ice <- VaRTest(1-alpha,
                        actual=coredata(rice[index(VaR.results.ice),]),
                        VaR=coredata(VaR.results.ice[,"Normal"]))
names(vaR.test.ice)
vaR.test.ice[1:7]
vaR.test.ice[8:12]

################### Backtesting CLIM ###################

VaR.results.clim <- rollapply(as.zoo(rclim), width=FE,
                              FUN = backTestVaR, p=0.99, by.column = FALSE,
                              align = "right")
VaR.results.clim <- stats::lag(VaR.results.clim, k=-1)
chart.TimeSeries(merge(rclim, VaR.results.clim),
                 legend.loc="topright",
                 main="Backtesting VaR - CLIM")

violations.mat.clim <- matrix(0, 3, 5)
rownames(violations.mat.clim) <- c("Normal", "HS", "Modified")
colnames(violations.mat.clim) <- c("En1", "n1", "1-alpha", "Percent", "VR")
violations.mat.clim[, "En1"] <- (1-alpha)*FT
violations.mat.clim[, "1-alpha"] <- 1 - alpha

normalVaR.violations.clim <- as.zoo(rclim[index(VaR.results.clim), ]) < VaR.results.clim[, "Normal"]
violation.dates.clim <- index(normalVaR.violations.clim[which(normalVaR.violations.clim)])

plot(as.zoo(rclim[index(VaR.results.clim),]), col="blue", ylab="Return",
     main="Violations VaR - CLIM")
abline(h=0)
lines(VaR.results.clim[, "Normal"], col="black", lwd=2)
lines(as.zoo(rclim[violation.dates.clim,]), type="p", pch=16, col="red", lwd=2)

for(i in colnames(VaR.results.clim)) {
  VaR.violations <- as.zoo(rclim[index(VaR.results.clim), ]) < VaR.results.clim[, i]
  violations.mat.clim[i, "n1"] <- sum(VaR.violations)
  violations.mat.clim[i, "Percent"] <- sum(VaR.violations)/FT
  violations.mat.clim[i, "VR"] <- violations.mat.clim[i, "n1"]/violations.mat.clim[i, "En1"]
}

violations.mat.clim

vaR.test.clim <- VaRTest(1-alpha,
                         actual=coredata(rclim[index(VaR.results.clim),]),
                         VaR=coredata(VaR.results.clim[,"Normal"]))
names(vaR.test.clim)
vaR.test.clim[1:7]
vaR.test.clim[8:12]

################### Backtesting COAL ###################

VaR.results.coal <- rollapply(as.zoo(rcoal), width=FE,
                              FUN = backTestVaR, p=0.99, by.column = FALSE,
                              align = "right")
VaR.results.coal <- stats::lag(VaR.results.coal, k=-1)
chart.TimeSeries(merge(rcoal, VaR.results.coal),
                 legend.loc="topright",
                 main="Backtesting VaR - Coal")

violations.mat.coal <- matrix(0, 3, 5)
rownames(violations.mat.coal) <- c("Normal", "HS", "Modified")
colnames(violations.mat.coal) <- c("En1", "n1", "1-alpha", "Percent", "VR")
violations.mat.coal[, "En1"] <- (1-alpha)*FT
violations.mat.coal[, "1-alpha"] <- 1 - alpha

normalVaR.violations.coal <- as.zoo(rcoal[index(VaR.results.coal), ]) < VaR.results.coal[, "Normal"]
violation.dates.coal <- index(normalVaR.violations.coal[which(normalVaR.violations.coal)])

plot(as.zoo(rcoal[index(VaR.results.coal),]), col="blue", ylab="Return",
     main="Violations VaR - Coal")
abline(h=0)
lines(VaR.results.coal[, "Normal"], col="black", lwd=2)
lines(as.zoo(rcoal[violation.dates.coal,]), type="p", pch=16, col="red", lwd=2)

for(i in colnames(VaR.results.coal)) {
  VaR.violations <- as.zoo(rcoal[index(VaR.results.coal), ]) < VaR.results.coal[, i]
  violations.mat.coal[i, "n1"] <- sum(VaR.violations)
  violations.mat.coal[i, "Percent"] <- sum(VaR.violations)/FT
  violations.mat.coal[i, "VR"] <- violations.mat.coal[i, "n1"]/violations.mat.coal[i, "En1"]
}

violations.mat.coal

vaR.test.coal <- VaRTest(1-alpha,
                         actual=coredata(rcoal[index(VaR.results.coal),]),
                         VaR=coredata(VaR.results.coal[,"Normal"]))
names(vaR.test.coal)
vaR.test.coal[1:7]
vaR.test.coal[8:12]

