
ristorante1$chiuso <- 0
ristorante1[c(which(is.na(ristorante1$rapprto_v_s))),"chiuso"]<-1

ristorante1$tipo_giorno <- as.factor(ristorazione$tipo_giorno)
ristorante1$weekday <- as.factor(ristorazione$weekday)
ristorante1$holiday <- ristorazione$holiday
ristorante1$pioggia <- ristorazione$pioggia
ristorante1$top <- ristorazione$top
ristorante1$meteo_aperto <- ristorazione$meteo_aperto

vendite1_pre_des <- vendite1_day_pre - stag.vendite1_day_pre

library(urca)
summary(ur.kpss(vendite1_pre_des ))

ndiffs(vendite1_pre_des) #differenziazione normale

vendite1_pre_des_diff <- diff(vendite1_pre_des)

summary(ur.kpss(vendite1_pre_des_diff))

autoplot(vendite1_pre_des_diff)


summary(lm(vendite1_pre_des ~ ristorante1[1:1163, "meteo_aperto"] +ristorante1[1:1163, "holiday"]))

M1 <- auto.arima(vendite1_pre_des_diff, xreg = data.matrix(ristorante1[2:1163, c("holiday","meteo_aperto" )]))
summary(M1)
checkresiduals(M1)

vendite1_sett_pre_dest <- vendite1_sett_pre- stag.vendite1_sett_pre

M2<- auto.arima(vendite1_sett_pre_dest)
summary(M2)

checkresiduals(M2)
