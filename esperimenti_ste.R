
ristorante1$tipo_giorno <- as.factor(ristorazione$tipo_giorno)
ristorante1$weekday <- as.factor(ristorazione$weekday)
ristorante1$holiday <- ristorazione$holiday
ristorante1$pioggia <- ristorazione$pioggia
ristorante1$top <- ristorazione$top
ristorante1$meteo_aperto <- ristorazione$meteo_aperto
ristorante1$saldi<-ristorazione$`Black Friday e saldi Lombardia`


giornaliera <- smo3$alphahat[1:1163,"sea_dummy1"]
vendite1_pre_des <- vendite1_day_pre- giornaliera

k<-diff(vendite1_day_pre, 365)
plot(k)
par(mfrow=c(1,2))
acf(k)
pacf(k)

M4<-Arima(k, order = c(1,0,1),seasonal = list(order=c(1,0,1),period=7))
summary(M4)
acf(M4$residuals)
pacf(M4$residuals)


pacf(vendite1_pre_des)
plot(vendite1_pre_des)

library(urca)
summary(ur.kpss(vendite1_pre_des ))

ndiffs(vendite1_pre_des) #differenziazione normale
nsdiffs(vendite1_day_pre)
vendite1_pre_des_diff <- diff(vendite1_pre_des)

summary(ur.kpss(vendite1_pre_des_diff))

autoplot(vendite1_pre_des_diff)


summary(lm(vendite1_pre_des ~ ristorante1[1:1163, "saldi"] +ristorante1[1:1163, "holiday"]))

M1 <- auto.arima(vendite1_pre_des_diff, xreg = data.matrix(ristorante1[2:1163, c("holiday","saldi" )]))
summary(M1)
checkresiduals(M1)

vendite1_sett_pre_dest <- vendite1_sett_pre- stag.vendite1_sett_pre

M2<- auto.arima(vendite1_sett_pre_dest)
summary(M2)

checkresiduals(M2)



#ASDSADSADSADSA

train<- ristorante1_pre_covid[1:814, "vendite"]
test <-ristorante1_pre_covid[815:1163, "vendite"]

train <- ts(train,start=2017,frequency=365)
test <- ts(test,start=c(2019,03,26),frequency=365)

par(mfrow=c(1,1))
plot(train)

train_diff <- diff(train, 1)

plot(train_diff)


require(gridExtra)
acf<-ggAcf(train_diff, lag.max = 30)+ggtitle("Vendite1 day pre diff")
pacf<-ggPacf(train_diff, lag.max = 30)+ggtitle("Vendite1 day pre diff")
grid.arrange(acf, pacf, ncol=2)


M5 <- Arima(train_diff, order = c(1,0,1),seasonal = list(order=c(1,0,1),period=7))
summary(M5)
ggAcf(M5$residuals, lag.max = 30)+ggtitle("Vendite1 day pre diff")
ggPacf(M5$residuals, lag.max = 30)+ggtitle("Vendite1 day pre diff")

checkresiduals(M5)

autoplot(forecast(M5, h=10))

