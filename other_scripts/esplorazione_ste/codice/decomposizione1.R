
#### trasformazioni, riduce la scala e rumore? ####
lambda <- BoxCox.lambda(vendite1_day)
trans_day <- BoxCox(vendite1_day,lambda)

lambda <- BoxCox.lambda(vendite1_sett_avg)
trans_sett_avg <- BoxCox(vendite1_sett_avg,lambda)

lambda <- BoxCox.lambda(vendite1_mens_avg)
trans_mens_avg <- BoxCox(vendite1_mens_avg,lambda)

print(autoplot(vendite1_day))
print(autoplot(trans_day))

print(autoplot(trans_sett_avg))
print(autoplot(vendite1_sett_avg))

print(autoplot(trans_mens_avg))
print(autoplot(vendite1_mens_avg))



#### decomposizione delle serie ####

### decomposizione giornaliera ###

vendite1_day.fit<-stl(vendite1_day,s.window="periodic")
trend.vendite1_day<-vendite1_day.fit$time.series[,2]
stag.vendite1_day<-vendite1_day.fit$time.series[,1]
res.vendite1_day<-vendite1_day.fit$time.series[,3]
print(autoplot(vendite1_day.fit))

trans_vendite1_day.fit<-stl(trans_day,s.window="periodic")
trans_trend.vendite1_day<-trans_vendite1_day.fit$time.series[,2]
trans_stag.vendite1_day<-trans_vendite1_day.fit$time.series[,1]
trans_res.vendite1_day<-trans_vendite1_day.fit$time.series[,3]
print(autoplot(trans_vendite1_day.fit))


### decomposizione settimanale ###

vendite1_sett.fit<-stl(vendite1_sett_avg,s.window="periodic")
trend.vendite1_sett<-vendite1_sett.fit$time.series[,2]
stag.vendite1_sett<-vendite1_sett.fit$time.series[,1]
res.vendite1_sett<-vendite1_sett.fit$time.series[,3]
print(autoplot(vendite1_sett.fit))

trans_vendite1_sett.fit<-stl(trans_sett_avg,s.window="periodic")
trans_trend.vendite1_sett<-trans_vendite1_sett.fit$time.series[,2]
trans_stag.vendite1_sett<-trans_vendite1_sett.fit$time.series[,1]
trans_res.vendite1_sett<-trans_vendite1_sett.fit$time.series[,3]
print(autoplot(trans_vendite1_sett.fit))


### decomposizione mensile ###

vendite1_mens.fit<-stl(vendite1_mens_avg,s.window="periodic")
trend.vendite1_mens<-vendite1_mens.fit$time.series[,2]
stag.vendite1_mens<-vendite1_mens.fit$time.series[,1]
res.vendite1_mens<-vendite1_mens.fit$time.series[,3]
print(autoplot(vendite1_mens.fit))

trans_vendite1_mens.fit<-stl(trans_mens_avg,s.window="periodic")
trans_trend.vendite1_mens<-trans_vendite1_mens.fit$time.series[,2]
trans_stag.vendite1_mens<-trans_vendite1_mens.fit$time.series[,1]
trans_res.vendite1_mens<-trans_vendite1_mens.fit$time.series[,3]
print(autoplot(trans_vendite1_mens.fit))



#### destagionalizzazione della serie ####

### giornaliera ###

vendite1_day_des <- vendite1_day - stag.vendite1_day
day_destag <-ts.intersect(vendite1_day, vendite1_day_des)

print(autoplot(day_destag) +
        xlab("Year") + ylab("euro") +
        ggtitle("Destagionalizzazione della serie"))


trans_vendite1_day_des <- trans_day - trans_stag.vendite1_day
trans_day_destag <-ts.intersect(trans_day, trans_vendite1_day_des)

print(autoplot(trans_day_destag) +
        xlab("Year") + ylab("euro") +
        ggtitle("Destagionalizzazione della serie"))

### settimanale ###

vendite1_sett_des <- vendite1_sett_avg - stag.vendite1_sett
sett_destag <-ts.intersect(vendite1_sett_avg, vendite1_sett_des)

print(autoplot(sett_destag) +
        xlab("Year") + ylab("euro") +
        ggtitle("Destagionalizzazione della serie"))


trans_vendite1_sett_des <- trans_sett_avg - trans_stag.vendite1_sett
trans_sett_destag <-ts.intersect(trans_sett_avg, trans_vendite1_sett_des)

print(autoplot(trans_sett_destag) +
        xlab("Year") + ylab("euro") +
        ggtitle("Destagionalizzazione della serie"))


### mensile ###

vendite1_mens_des <- vendite1_mens_avg - stag.vendite1_mens
mens_destag <-ts.intersect(vendite1_mens_avg, vendite1_mens_des)

print(autoplot(mens_destag) +
  xlab("Year") + ylab("euro") +
  ggtitle("Destagionalizzazione della serie"))


trans_vendite1_mens_des <- trans_mens_avg - trans_stag.vendite1_mens
trans_mens_destag <-ts.intersect(trans_mens_avg, trans_vendite1_mens_des)

print(autoplot(trans_mens_destag) +
  xlab("Year") + ylab("euro") +
  ggtitle("Destagionalizzazione della serie"))


#### controllo i residui (mi sembra correlazione, devo controllare)####

### giornalieri ###
print(Box.test(res.vendite1_day, lag=14, fitdf=0))
checkresiduals(res.vendite1_day)

print(Box.test(trans_res.vendite1_day, lag=14, fitdf=0))
checkresiduals(trans_res.vendite1_day)

### settimanali ###
print(Box.test(res.vendite1_sett, lag=52, fitdf=0))
checkresiduals(res.vendite1_sett)

print(Box.test(trans_res.vendite1_sett, lag=52, fitdf=0))
checkresiduals(trans_res.vendite1_sett)

### mensili ###
print(Box.test(res.vendite1_mens, lag=12, fitdf=0))
checkresiduals(res.vendite1_mens)

print(Box.test(trans_res.vendite1_mens, lag=12, fitdf=0))
checkresiduals(trans_res.vendite1_mens)



