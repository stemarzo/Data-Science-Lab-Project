## Ristorante2 ----
# si prende in considerazione per le successive analisi il secondo ristorante

### analisi vendite considerando tutti gli anni ----

# vendite giornaliere primo ristorante 
vendite2_day <- ts(ristorante2$vendite, start=2017, frequency=365) 

# vendite settimanali medie primo ristorante 
week <- as.Date(cut(ristorante2$data, "week"))

vendite2_sett <- aggregate(vendite ~ week, ristorante2, sum)
vendite2_sett <- vendite2_sett$vendite
vendite2_sett <- ts(vendite2_sett,start=2017,frequency=52) 

vendite2_sett_avg <- aggregate(vendite ~ week, ristorante2, mean)
vendite2_sett_avg <- vendite2_sett_avg$vendite
vendite2_sett_avg <- ts(vendite2_sett_avg,start=2017,frequency=52) 

# vendite mensili medie  primo ristorante 
month <- as.Date(cut(ristorante2$data, "month"))

vendite2_mens <- aggregate(vendite ~ month, ristorante2, sum)
vendite2_mens <- vendite2_mens$vendite
vendite2_mens <- ts(vendite2_mens,start=2017,frequency=12) 

vendite2_mens_avg <- aggregate(vendite ~ month, ristorante2, mean)
vendite2_mens_avg <- vendite2_mens_avg$vendite
vendite2_mens_avg <- ts(vendite2_mens_avg,start=2017,frequency=12) 

# plot delle diverse serie trovate sopra
x <- autoplot(vendite2_day) +
  ggtitle("Ristorante 2: vendite giornaliere") +
  xlab("anno") +
  ylab("vendite")
print(x)

autoplot(vendite2_sett_avg) +
  ggtitle("Ristorante 2: vendite medie settimanali") +
  xlab("anno") +
  ylab("vendite")

autoplot(vendite2_mens_avg) +
  ggtitle("Ristorante 2: vendite medie mensili") +
  xlab("anno") +
  ylab("vendite")



### analisi vendite considerando il periodo pre covid ----

# si procede ad analizzare ciascun ristorante nel periodo antecedente il covid-19 
reference_date <- as.Date("2020-03-09", format = "%Y-%m-%d")

# vendite Ristorante 2 pre covid
ristorante2_pre_covid_vendite <- ristorante2 %>%
  filter(ristorante2$data < reference_date) %>%
  select(vendite, data)

# vendite giornaliere primo ristorante pre covid
vendite2_day_pre <- ts(ristorante2_pre_covid_vendite$vendite,start=2017,frequency=365) 

# vendite settimanali medie primo ristorante pre covid
week_pre_covid <- as.Date(cut(ristorante2_pre_covid_vendite$data, "week"))

vendite2_sett_pre <- aggregate(vendite ~ week_pre_covid, ristorante2_pre_covid_vendite, sum)
vendite2_sett_pre <- vendite2_sett_pre$vendite
vendite2_sett_pre <- ts(vendite2_sett_pre,start=2017,frequency=52) 

vendite2_sett_avg_pre <- aggregate(vendite ~ week_pre_covid, ristorante2_pre_covid_vendite, mean)
vendite2_sett_avg_pre <- vendite2_sett_avg_pre$vendite
vendite2_sett_avg_pre <- ts(vendite2_sett_avg_pre,start=2017,frequency=52) 

# vendite mensili medie  primo ristorante pre covid
month_pre_covid <- as.Date(cut(ristorante2_pre_covid_vendite$data, "month"))

vendite2_mens_pre <- aggregate(vendite ~ month_pre_covid, ristorante2_pre_covid_vendite, sum)
vendite2_mens_pre <- vendite2_mens_pre$vendite
vendite2_mens_pre <- ts(vendite2_mens_pre,start=2017,frequency=12) 

vendite2_mens_avg_pre <- aggregate(vendite ~ month_pre_covid, ristorante2_pre_covid_vendite, mean)
vendite2_mens_avg_pre <- vendite2_mens_avg_pre$vendite
vendite2_mens_avg_pre <- ts(vendite2_mens_avg_pre,start=2017,frequency=12) 

# plot delle diverse serie pre covid trovate sopra 
autoplot(vendite2_day_pre) +
  ggtitle("Ristorante 2: vendite giornaliere pre covid") +
  xlab("anno") +
  ylab("vendite")

autoplot(vendite2_sett_avg_pre) +
  ggtitle("Ristorante 2: vendite medie settimanali pre covid") +
  xlab("anno") +
  ylab("vendite")

autoplot(vendite2_mens_avg_pre) +
  ggtitle("Ristorante 2: vendite medie mensili pre covid") +
  xlab("anno") +
  ylab("vendite")


### analisi scontrini considerando tutti gli anni ----

# eventualmente da mettere a confronto con l'andamento delle vendite (grafico sopra vendite e sotto scontrini)

# scontrini giornalieri primo ristorante 
scontrini2_day <- ts(ristorante2$scontrini,start=2017,frequency=365) 

# scontrini settimanali medie primo ristorante 
week <- as.Date(cut(ristorante2$data, "week"))

scontrini2_sett <- aggregate(scontrini ~ week, ristorante2, sum)
scontrini2_sett <- scontrini2_sett$scontrini
scontrini2_sett <- ts(scontrini2_sett,start=2017,frequency=52) 

scontrini2_sett_avg <- aggregate(scontrini ~ week, ristorante2, mean)
scontrini2_sett_avg <- scontrini2_sett_avg$scontrini
scontrini2_sett_avg <- ts(scontrini2_sett_avg,start=2017,frequency=52) 

# scontrini mensili medie  primo ristorante 
month <- as.Date(cut(ristorante2$data, "month"))

scontrini2_mens <- aggregate(scontrini ~ month, ristorante2, sum)
scontrini2_mens <- scontrini2_mens$scontrini
scontrini2_mens <- ts(scontrini2_mens,start=2017,frequency=12) 

scontrini2_mens_avg <- aggregate(scontrini ~ month, ristorante2, mean)
scontrini2_mens_avg <- scontrini2_mens_avg$scontrini
scontrini2_mens_avg <- ts(scontrini2_mens_avg,start=2017,frequency=12) 

# plot delle diverse serie trovate sopra
autoplot(scontrini2_day) +
  ggtitle("Ristorante 2: scontrini giornalieri") +
  xlab("anno") +
  ylab("scontrini")

autoplot(scontrini2_sett_avg) +
  ggtitle("Ristorante 2: scontrini medi settimanali") +
  xlab("anno") +
  ylab("scontrini")

autoplot(scontrini2_mens_avg) +
  ggtitle("Ristorante 2: scontrini medi mensili") +
  xlab("anno") +
  ylab("scontrini")

### analisi scontrini considerando il periodo pre covid ----

# si procede ad analizzare ciascun ristorante nel periodo antecedente il covid-19 
reference_date <- as.Date("2020-03-09", format = "%Y-%m-%d")

# scontrini Ristorante 2 pre covid
ristorante2_pre_covid_scontrini <- ristorante2 %>%
  filter(ristorante2$data < reference_date) %>%
  select(scontrini, data)

# scontrini giornalieri primo ristorante pre covid
scontrini2_day_pre <- ts(ristorante2_pre_covid_scontrini$scontrini,start=2017,frequency=365) 

# scontrini settimanali medi primo ristorante pre covid
week_pre_covid <- as.Date(cut(ristorante2_pre_covid_scontrini$data, "week"))

scontrini2_sett_avg_pre <- aggregate(scontrini ~ week_pre_covid, ristorante2_pre_covid_scontrini, mean)
scontrini2_sett_avg_pre <- scontrini2_sett_avg_pre$scontrini
scontrini2_sett_avg_pre <- ts(scontrini2_sett_avg_pre,start=2017,frequency=52) 

# scontrini mensili medi  primo ristorante pre covid
month_pre_covid <- as.Date(cut(ristorante2_pre_covid_scontrini$data, "month"))
scontrini2_mens_avg_pre <- aggregate(scontrini ~ month_pre_covid, ristorante2_pre_covid_scontrini, mean)
scontrini2_mens_avg_pre <- scontrini2_mens_avg_pre$scontrini
scontrini2_mens_avg_pre <- ts(scontrini2_mens_avg_pre,start=2017,frequency=12) 

# plot delle diverse serie pre covid trovate sopra 
autoplot(scontrini2_day_pre) +
  ggtitle("Ristorante 2: scontrini giornalieri pre covid") +
  xlab("anno") +
  ylab("scontrini")

autoplot(scontrini2_sett_avg_pre) +
  ggtitle("Ristorante 2: vendite medi settimanali pre covid") +
  xlab("anno") +
  ylab("scontrini")

autoplot(scontrini2_mens_avg_pre) +
  ggtitle("Ristorante 2: scontrini medi mensili pre covid") +
  xlab("anno") +
  ylab("scontrini")



### analisi stagionalità considerando tutti gli anni ----
ggseasonplot(vendite2_sett, year.labels=TRUE, year.labels.left=TRUE) +
  ylab("euro") +
  ggtitle("Seasonal plot: vendite settimanale")

ggseasonplot(vendite2_mens, year.labels=TRUE, year.labels.left=TRUE) +
  ylab("euro") +
  ggtitle("Seasonal plot: vendite mensili")

### seasonal sub series plot
ggsubseriesplot(vendite2_mens_avg) +
  ylab("$ million") +
  ggtitle("Seasonal subseries plot: vendite medie mensili")


### analisi stagionalità considerando il periodo pre covid ----
ggseasonplot(vendite2_sett_pre, year.labels=TRUE, year.labels.left=TRUE) +
  ylab("euro") +
  ggtitle("Seasonal plot: vendite settimanale pre covid")

ggseasonplot(vendite2_mens_pre, year.labels=TRUE, year.labels.left=TRUE) +
  ylab("euro") +
  ggtitle("Seasonal plot: vendite mensili pre covid")

### seasonal sub series plot
ggsubseriesplot(vendite2_mens_avg_pre) +
  ylab("$ million") +
  ggtitle("Seasonal subseries plot: vendite medie mensili pre covid")


### analisi correlazione tra vendite e scontrini ----
scontrini_sett_avg2 <- aggregate(scontrini ~ week, ristorante2, mean)
scontrini_sett_avg2 <- scontrini_sett_avg2$scontrini
scontrini_sett_avg2 <- ts(scontrini_sett_avg2,start=2017,frequency=52) 
sc_ven2_sett_avg <-ts.intersect(vendite2_sett_avg, scontrini_sett_avg2)


print(autoplot(sc_ven2_sett_avg, facets=TRUE) +
        xlab("Anni") + ylab("") +
        ggtitle("Confronto scontrini e vendite"))

print(qplot(vendite, scontrini, data=as.data.frame(ristorante2)) +
        ylab("scontrini") + xlab("vendite")+
        ggtitle("Correlazione scontrini e vendite"))



### analisi autocorrelazione considerando tutti gli anni ----
# per una serie con trend, l'autocorrelazione è alta a lag vicini, 
# e si abbassa piano piano se c'è stagionalità l'autocorrelazione presenta lag ogni tot

#gglagplot(vendite2_mens_avg)

ggAcf(vendite2_day, lag=7) +
  ggtitle("Autocorrelation vendite giornaliere")

ggAcf(vendite2_sett_avg, lag=52)+
  ggtitle("Autocorrelation vendite medie settimanali")

ggAcf(vendite2_mens_avg, lag=24)+
  ggtitle("Autocorrelation vendite medie mensili")


### analisi autocorrelazione pre covid ----

ggAcf(vendite2_day_pre, lag=7) +
  ggtitle("Autocorrelation vendite giornaliere pre covid")

ggAcf(vendite2_sett_avg_pre, lag=52)+
  ggtitle("Autocorrelation vendite medie settimanali pre covid")

ggAcf(vendite2_mens_avg_pre, lag=24)+
  ggtitle("Autocorrelation vendite medie mensili pre covid")



### decomposizione considerando tutti gli anni ----
# decomposizione giornaliera 
vendite2_day.fit<-stl(vendite2_day,s.window="periodic")
trend.vendite2_day<-vendite2_day.fit$time.series[,2]
stag.vendite2_day<-vendite2_day.fit$time.series[,1]
res.vendite2_day<-vendite2_day.fit$time.series[,3]
print(autoplot(vendite2_day.fit))

# vendite2_day %>% stl(s.window="periodic") %>% autoplot() + xlab("Time")
# vendite2_day %>% mstl() %>% autoplot() + xlab("Time")

# decomposizione settimanale
vendite2_sett.fit<-stl(vendite2_sett_avg,s.window="periodic")
trend.vendite2_sett<-vendite2_sett.fit$time.series[,2]
stag.vendite2_sett<-vendite2_sett.fit$time.series[,1]
res.vendite2_sett<-vendite2_sett.fit$time.series[,3]
print(autoplot(vendite2_sett.fit))

# decomposizione mensile 
vendite2_mens.fit<-stl(vendite2_mens_avg,s.window="periodic")
trend.vendite2_mens<-vendite2_mens.fit$time.series[,2]
stag.vendite2_mens<-vendite2_mens.fit$time.series[,1]
res.vendite2_mens<-vendite2_mens.fit$time.series[,3]
print(autoplot(vendite2_mens.fit))

components.ts = decompose(vendite2_mens_avg)
plot(components.ts)



### decomposizione pre covid ----
# decomposizione giornaliera 
vendite2_day_pre.fit<-stl(vendite2_day_pre,s.window="periodic")
trend.vendite2_day_pre<-vendite2_day_pre.fit$time.series[,2]
stag.vendite2_day_pre<-vendite2_day_pre.fit$time.series[,1]
res.vendite2_day_pre<-vendite2_day_pre.fit$time.series[,3]
print(autoplot(vendite2_day_pre.fit))

# decomposizione settimanale
vendite2_sett.fit_pre<-stl(vendite2_sett_avg_pre,s.window="periodic")
trend.vendite2_sett_pre<-vendite2_sett.fit_pre$time.series[,2]
stag.vendite2_sett_pre<-vendite2_sett.fit_pre$time.series[,1]
res.vendite2_sett_pre<-vendite2_sett.fit_pre$time.series[,3]
print(autoplot(vendite2_sett.fit_pre))

# decomposizione mensile 
vendite2_mens.fit_pre<-stl(vendite2_mens_avg_pre,s.window="periodic")
trend.vendite2_mens_pre<-vendite2_mens.fit_pre$time.series[,2]
stag.vendite2_mens_pre<-vendite2_mens.fit_pre$time.series[,1]
res.vendite2_mens_pre<-vendite2_mens.fit_pre$time.series[,3]
print(autoplot(vendite2_mens.fit_pre))

components.ts_pre = decompose(vendite2_mens_avg_pre)
plot(components.ts_pre)


### decomposizione approfondita considerando tutti gli anni----

mese<-as.factor(month(ymd(ristorante2$data)))
y=vendite2_day
step.c <- y
step.a <- y
step.c[] <- 0
step.a[] <- 0
step.c[(1167+1):length(y)] <- 1 #non so se vuoi usare la tua funzione per le date sono 12 mar  2020
step.a[(1222+1):length(y)] <- 1 #6 mag 2020
step.c
step.a

mod3<-SSModel(y ~mese+step.c+step.a+SSMtrend(degree = 2, Q=list(NA,NA))+SSMseasonal(period = 7, sea.type = "dummy", Q=NA), H=NA)
fit3<-fitSSM(mod3,inits = c(0,0,0,0))
fit3$optim.out$convergence

smo3 <- KFS(fit3$model, smoothing = c("state", "disturbance", "mean"))
smo3$alphahat


plot(step.a*smo3$alphahat[,13]+step.c*smo3$alphahat[,12]+smo3$alphahat[,14], col=2,ylim=c(0,12000))
lines(y)

auxres_ls <- rstandard(smo3, "state")
plot(auxres_ls)