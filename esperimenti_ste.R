
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
