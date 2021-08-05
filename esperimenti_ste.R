
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


##SETTIMANALE

train <- window(vendite1_sett_avg_pre, end=c(2019,2))
test <- window(vendite1_sett_avg_pre, start=c(2019,2))


train_dest <- seasadj(stl(train, s.window = 52))
plot(train_dest)

par(mfrow=c(1,1))
plot(train)

ndiffs(train_dest)
nsdiffs(train_dest)

train_diff <-diff(train_dest,1)

library(urca)
summary(ur.kpss(train_diff ))


nsdiffs(train_diff)

plot(train_diff)

require(gridExtra)
acf<-ggAcf(train_diff, lag.max = 52)+ggtitle("Vendite1 day pre diff")
pacf<-ggPacf(train_diff, lag.max = 52)+ggtitle("Vendite1 day pre diff")
grid.arrange(acf, pacf, ncol=2)

M9<-auto.arima(train_diff)
M9s<-Arima(train_diff, order = c(2,0,2))

checkresiduals(M9)
checkresiduals(M9s)

M9s

 M10 <-auto.arima(train, D=1)

checkresiduals(M10)

autoplot(forecast(M10, h=52))+autolayer(test)


# vendite ristorante 1 pre covid
ristorante1_pre_covid_chiuso <- ristorante1 %>%
  filter(ristorante1$data < reference_date) %>%
  select(chiuso, data)

chiuso_sett_avg_pre <- aggregate(chiuso ~ week_pre_covid, ristorante1_pre_covid_chiuso, mean)
chiuso1_sett_avg_pre <- chiuso_sett_avg_pre$chiuso
chiuso1_sett_avg_pre <- ts(chiuso1_sett_avg_pre,start=2017,frequency=52) 

train_chiuso <- window(chiuso1_sett_avg_pre, end=2019)
test_chiuso <- window(chiuso1_sett_avg_pre, start=2019)

M11 <- auto.arima(train, D=1, xreg = train_chiuso)
summary(M11)
checkresiduals(M11)

autoplot(forecast(M11, h=52, xreg = test_chiuso))+autolayer(test)





