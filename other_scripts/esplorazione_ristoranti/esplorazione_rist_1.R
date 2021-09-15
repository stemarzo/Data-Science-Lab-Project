## Ristorante1 ----
# si prende in considerazione per le successive analisi il primo ristorante

### analisi vendite considerando tutti gli anni ----

# vendite giornaliere primo ristorante 
vendite1_day <- ts(ristorante1$vendite, start=2017, frequency=365) 

# vendite settimanali medie primo ristorante 
week_rist1 <- as.Date(cut(ristorante1$data, "week"))

# si procede ad eliminare la data 12 aprile essendo un lunedì, altrimenti si perderebbe
# una settimana nelle successive analisi
remove_dates <- as.Date(c('2016-12-26','2021-04-12'))
all_dates <- week_rist1
week_rist1 <- all_dates[!all_dates %in% remove_dates]
# ristorante1[-c(1,1563),], si toglie solo per le analisi settimanali domenica 1 gennaio 2017, lunedì 12 aprile 2021

vendite1_sett <- aggregate(vendite ~ week_rist1, ristorante1[-c(1,1563),], sum)
vendite1_sett <- vendite1_sett$vendite
vendite1_sett <- ts(vendite1_sett,start=2017,frequency=52) 

vendite1_sett_avg <- aggregate(vendite ~ week_rist1, ristorante1[-c(1,1563),], mean)
vendite1_sett_avg <- vendite1_sett_avg$vendite
vendite1_sett_avg <- ts(vendite1_sett_avg,start=2017,frequency=52) 

# vendite mensili medie  primo ristorante 
month_rist1 <- as.Date(cut(ristorante1$data, "month"))

vendite1_mens <- aggregate(vendite ~ month_rist1, ristorante1, sum)
vendite1_mens <- vendite1_mens$vendite
vendite1_mens <- ts(vendite1_mens,start=2017,frequency=12) 

vendite1_mens_avg <- aggregate(vendite ~ month_rist1, ristorante1, mean)
vendite1_mens_avg <- vendite1_mens_avg$vendite
vendite1_mens_avg <- ts(vendite1_mens_avg,start=2017,frequency=12) 

# plot delle diverse serie trovate sopra
print(
  autoplot(vendite1_day) +
  ggtitle("Ristorante 1: vendite giornaliere") +
  xlab("anno") +
  ylab("vendite")
)

print(
  autoplot(vendite1_sett_avg) +
  ggtitle("Ristorante 1: vendite medie settimanali") +
  xlab("anno") +
  ylab("vendite")
)

print(
  autoplot(vendite1_mens_avg) +
  ggtitle("Ristorante 1: vendite medie mensili") +
  xlab("anno") +
  ylab("vendite")
)



### analisi vendite considerando il periodo pre covid ----

# si procede ad analizzare ciascun ristorante nel periodo antecedente il covid-19 
reference_date <- as.Date("2020-01-06", format = "%Y-%m-%d")

# vendite ristorante 1 pre covid
ristorante1_pre_covid <- ristorante1 %>%
  filter(ristorante1$data < reference_date) #%>%
  #select(vendite, data)

# vendite giornaliere primo ristorante pre covid
vendite1_day_pre <- ts(ristorante1_pre_covid$vendite,start=2017,frequency=365) 

# vendite settimanali medie primo ristorante pre covid
week_pre_covid_rist1 <- as.Date(cut(ristorante1_pre_covid$data, "week"))

# si procede ad eliminare il giorno 1 gennaio 2017 che risulta essere domenica
remove_dates <- as.Date(c('2016-12-26'))
all_dates <- week_pre_covid_rist1
week_pre_covid_rist1 <- all_dates[!all_dates %in% remove_dates]

vendite1_sett_pre <- aggregate(vendite ~ week_pre_covid_rist1, ristorante1_pre_covid[-1,], sum)
vendite1_sett_pre <- vendite1_sett_pre$vendite
vendite1_sett_pre <- ts(vendite1_sett_pre,start=2017,frequency=52) 

vendite1_sett_avg_pre <- aggregate(vendite ~ week_pre_covid_rist1, ristorante1_pre_covid[-1,], mean)
vendite1_sett_avg_pre <- vendite1_sett_avg_pre$vendite
vendite1_sett_avg_pre <- ts(vendite1_sett_avg_pre,start=2017,frequency=52) 

# vendite mensili medie  primo ristorante pre covid
month_pre_covid_rist1 <- as.Date(cut(ristorante1_pre_covid$data, "month"))

vendite1_mens_pre <- aggregate(vendite ~ month_pre_covid_rist1, ristorante1_pre_covid, sum)
vendite1_mens_pre <- vendite1_mens_pre$vendite
vendite1_mens_pre <- ts(vendite1_mens_pre,start=2017,frequency=12) 

vendite1_mens_avg_pre <- aggregate(vendite ~ month_pre_covid_rist1, ristorante1_pre_covid, mean)
vendite1_mens_avg_pre <- head(vendite1_mens_avg_pre, - 1) 
vendite1_mens_avg_pre <- vendite1_mens_avg_pre$vendite
vendite1_mens_avg_pre <- ts(vendite1_mens_avg_pre,start=2017,frequency=12) 

# plot delle diverse serie pre covid trovate sopra 
print(
  autoplot(vendite1_day_pre) +
  ggtitle("Ristorante 1: vendite giornaliere pre covid") +
  xlab("anno") +
  ylab("vendite")
)

print(
  autoplot(vendite1_sett_avg_pre) +
  ggtitle("Ristorante 1: vendite medie settimanali pre covid") +
  xlab("anno") +
  ylab("vendite")
)

print(
  autoplot(vendite1_mens_avg_pre) +
  ggtitle("Ristorante 1: vendite medie mensili pre covid") +
  xlab("anno") +
  ylab("vendite")
)


### analisi scontrini considerando tutti gli anni ----

# eventualmente da mettere a confronto con l'andamento delle vendite (grafico sopra vendite e sotto scontrini)

# scontrini giornalieri primo ristorante 
scontrini1_day <- ts(ristorante1$scontrini,start=2017,frequency=365) 

# scontrini settimanali medie primo ristorante 
# week_rist1 

scontrini1_sett <- aggregate(scontrini ~ week_rist1, ristorante1[-c(1,1563),], sum)
scontrini1_sett <- scontrini1_sett$scontrini
scontrini1_sett <- ts(scontrini1_sett,start=2017,frequency=52) 

scontrini1_sett_avg <- aggregate(scontrini ~ week_rist1, ristorante1[-c(1,1563),], mean)
scontrini1_sett_avg <- scontrini1_sett_avg$scontrini
scontrini1_sett_avg <- ts(scontrini1_sett_avg,start=2017,frequency=52) 

# scontrini mensili medie  primo ristorante 
month_rist1 <- as.Date(cut(ristorante1$data, "month"))

scontrini1_mens <- aggregate(scontrini ~ month_rist1, ristorante1, sum)
scontrini1_mens <- scontrini1_mens$scontrini
scontrini1_mens <- ts(scontrini1_mens,start=2017,frequency=12) 

scontrini1_mens_avg <- aggregate(scontrini ~ month_rist1, ristorante1, mean)
scontrini1_mens_avg <- scontrini1_mens_avg$scontrini
scontrini1_mens_avg <- ts(scontrini1_mens_avg,start=2017,frequency=12) 

# plot delle diverse serie trovate sopra
print(
  autoplot(scontrini1_day) +
  ggtitle("Ristorante 1: scontrini giornalieri") +
  xlab("anno") +
  ylab("scontrini")
)

print(
  autoplot(scontrini1_sett_avg) +
  ggtitle("Ristorante 1: scontrini medi settimanali") +
  xlab("anno") +
  ylab("scontrini")
)

print(
  autoplot(scontrini1_mens_avg) +
  ggtitle("Ristorante 1: scontrini medi mensili") +
  xlab("anno") +
  ylab("scontrini")
)

### analisi scontrini considerando il periodo pre covid ----

# si procede ad analizzare ciascun ristorante nel periodo antecedente il covid-19 
# reference_date <- as.Date("2020-01-06", format = "%Y-%m-%d")

# scontrini ristorante 1 pre covid
# ristorante1_pre_covid <- ristorante1 %>%
#   filter(ristorante1$data < reference_date) %>%
#   select(scontrini, data)

# scontrini giornalieri primo ristorante pre covid
scontrini1_day_pre <- ts(ristorante1_pre_covid$scontrini,start=2017,frequency=365) 

# scontrini settimanali medi primo ristorante pre covid
# week_pre_covid_rist1 

scontrini1_sett_avg_pre <- aggregate(scontrini ~ week_pre_covid_rist1, ristorante1_pre_covid[-1,], mean)
scontrini1_sett_avg_pre <- scontrini1_sett_avg_pre$scontrini
scontrini1_sett_avg_pre <- ts(scontrini1_sett_avg_pre,start=2017,frequency=52) 

# scontrini mensili medi  primo ristorante pre covid
month_pre_covid_rist1 <- as.Date(cut(ristorante1_pre_covid$data, "month"))
scontrini1_mens_avg_pre <- aggregate(scontrini ~ month_pre_covid_rist1, ristorante1_pre_covid, mean)
scontrini1_mens_avg_pre <- scontrini1_mens_avg_pre$scontrini
scontrini1_mens_avg_pre <- ts(scontrini1_mens_avg_pre,start=2017,frequency=12) 

# plot delle diverse serie pre covid trovate sopra 
print(
  autoplot(scontrini1_day_pre) +
  ggtitle("Ristorante 1: scontrini giornalieri pre covid") +
  xlab("anno") +
  ylab("scontrini")
)

print(
  autoplot(scontrini1_sett_avg_pre) +
  ggtitle("Ristorante 1: vendite medi settimanali pre covid") +
  xlab("anno") +
  ylab("scontrini")
)

print(
  autoplot(scontrini1_mens_avg_pre) +
  ggtitle("Ristorante 1: scontrini medi mensili pre covid") +
  xlab("anno") +
  ylab("scontrini")
)



### analisi stagionalità considerando tutti gli anni ----
print(
  ggseasonplot(vendite1_sett, year.labels=TRUE, year.labels.left=TRUE) +
  ylab("euro") +
  ggtitle("Seasonal plot Ristorante 1: vendite settimanale")
)

print(
  ggseasonplot(vendite1_mens, year.labels=TRUE, year.labels.left=TRUE) +
  ylab("euro") +
  ggtitle("Seasonal plot Ristorante 1: vendite mensili")
)

### seasonal sub series plot
print(
  ggsubseriesplot(vendite1_mens_avg) +
  ylab("euro") +
  ggtitle("Seasonal subseries plot Ristorante 1: vendite medie mensili")
)


### analisi stagionalità considerando il periodo pre covid ----
print(
  ggseasonplot(vendite1_sett_pre, year.labels=TRUE, year.labels.left=TRUE) +
  ylab("euro") +
  ggtitle("Seasonal plot Ristorante 1: vendite settimanale pre covid")
)

print(
  ggseasonplot(vendite1_mens_pre, year.labels=TRUE, year.labels.left=TRUE) +
  ylab("euro") +
  ggtitle("Seasonal plot Ristorante 1: vendite mensili pre covid")
)

### seasonal sub series plot
print(
  ggsubseriesplot(vendite1_mens_avg_pre) +
  ylab("euro") +
  ggtitle("Seasonal subseries plot Ristorante 1: vendite medie mensili pre covid")
)


### analisi correlazione tra vendite e scontrini ----
scontrini_sett_avg1 <- aggregate(scontrini ~ week_rist1, ristorante1[-c(1,1563),], mean)
scontrini_sett_avg1 <- scontrini_sett_avg1$scontrini
scontrini_sett_avg1 <- ts(scontrini_sett_avg1,start=2017,frequency=52) 
sc_ven1_sett_avg <-ts.intersect(vendite1_sett_avg, scontrini_sett_avg1)


print(
  autoplot(sc_ven1_sett_avg, facets=TRUE) +
        xlab("Anni") + ylab("") +
        ggtitle("Confronto scontrini e vendite Ristorante 1")
)

print(
  qplot(vendite, scontrini, data=as.data.frame(ristorante1)) +
        ylab("scontrini") + xlab("vendite")+
        ggtitle("Correlazione scontrini e vendite Ristorante 1")
)




### analisi autocorrelazione considerando tutti gli anni ----
# per una serie con trend, l'autocorrelazione è alta a lag vicini, 
# e si abbassa piano piano se c'è stagionalità l'autocorrelazione presenta lag ogni tot

#gglagplot(vendite1_mens_avg)

print(
  ggAcf(vendite1_day, lag=7) +
  ggtitle("Ristorante 1: Autocorrelation vendite giornaliere")
)

print(
  ggAcf(vendite1_sett_avg, lag=52)+
  ggtitle("Ristorante 1: Autocorrelation vendite medie settimanali")
)

print(
  ggAcf(vendite1_mens_avg, lag=24)+
  ggtitle("Ristorante 1: Autocorrelation vendite medie mensili")
)


### analisi autocorrelazione pre covid ----

print(
  ggAcf(vendite1_day_pre, lag=7) +
  ggtitle("Ristorante 1: Autocorrelation vendite giornaliere pre covid")
)

print(
  ggAcf(vendite1_sett_avg_pre, lag=52)+
  ggtitle("Ristorante 1: Autocorrelation vendite medie settimanali pre covid")
)

print(
  ggAcf(vendite1_mens_avg_pre, lag=24)+
  ggtitle("Ristorante 1: Autocorrelation vendite medie mensili pre covid")
)


### decomposizione considerando tutti gli anni ----
# decomposizione giornaliera 
multi_vendite1 <- msts(ristorante1$vendite, ts.frequency = 365, start=2017, seasonal.periods = c(7,365))
multi_vendite1_dec <- mstl(multi_vendite1, s.window = "periodic")
print(autoplot(multi_vendite1_dec) + ggtitle("Ristorante 1: Decomposizione giornaliera"))

# decomposizione settimanale
vendite1_sett.fit<-stl(vendite1_sett_avg,s.window="periodic")
trend.vendite1_sett<-vendite1_sett.fit$time.series[,2]
stag.vendite1_sett<-vendite1_sett.fit$time.series[,1]
res.vendite1_sett<-vendite1_sett.fit$time.series[,3]
print(autoplot(vendite1_sett.fit) + ggtitle("Ristorante 1: Decomposizione settimanale"))

# decomposizione mensile 
vendite1_mens.fit<-stl(vendite1_mens_avg,s.window="periodic")
trend.vendite1_mens<-vendite1_mens.fit$time.series[,2]
stag.vendite1_mens<-vendite1_mens.fit$time.series[,1]
res.vendite1_mens<-vendite1_mens.fit$time.series[,3]
print(autoplot(vendite1_mens.fit) + ggtitle("Ristorante 1: Decomposizione mensile"))

components.ts_1 = decompose(vendite1_mens_avg)
plot(components.ts_1)



### decomposizione pre covid ----
# decomposizione giornaliera 
multi_vendite1_pre <- msts(ristorante1_pre_covid$vendite, ts.frequency = 365, start=2017, seasonal.periods = c(7,365))
multi_vendite_dec1_pre <- mstl(multi_vendite1_pre, s.window = "periodic")
print(autoplot(multi_vendite_dec1_pre) + ggtitle("Ristorante 1: Decomposizione giornaliera pre covid"))

# decomposizione settimanale
vendite1_sett.fit_pre<-stl(vendite1_sett_avg_pre,s.window="periodic")
trend.vendite1_sett_pre<-vendite1_sett.fit_pre$time.series[,2]
stag.vendite1_sett_pre<-vendite1_sett.fit_pre$time.series[,1]
res.vendite1_sett_pre<-vendite1_sett.fit_pre$time.series[,3]
print(autoplot(vendite1_sett.fit_pre) + ggtitle("Ristorante 1: Decomposizione settimnanale pre covid"))

# decomposizione mensile 
vendite1_mens.fit_pre<-stl(vendite1_mens_avg_pre,s.window="periodic")
trend.vendite1_mens_pre<-vendite1_mens.fit_pre$time.series[,2]
stag.vendite1_mens_pre<-vendite1_mens.fit_pre$time.series[,1]
res.vendite1_mens_pre<-vendite1_mens.fit_pre$time.series[,3]
print(autoplot(vendite1_mens.fit_pre) + ggtitle("Ristorante 1: Decomposizione mensile pre covid"))

components.ts_pre_1 = decompose(vendite1_mens_avg_pre)
plot(components.ts_pre_1)


### decomposizione approfondita considerando tutti gli anni----

mese<-as.factor(month(ymd(ristorante1$data)))
y=vendite1_day
step.c <- y
step.a <- y
step.c[] <- 0
step.a[] <- 0
step.c[(1167+1):length(y)] <- 1 #non so se vuoi usare la tua funzione per le date sono 12 mar  2020
step.a[(1222+1):length(y)] <- 1 #6 mag 2020
# step.c
# step.a

mod3<-SSModel(y ~mese+step.c+step.a+SSMtrend(degree = 2, Q=list(NA,NA))+SSMseasonal(period = 7, sea.type = "dummy", Q=NA), H=NA)
fit3<-fitSSM(mod3,inits = c(0,0,0,0))
# fit3$optim.out$convergence

smo3 <- KFS(fit3$model, smoothing = c("state", "disturbance", "mean"))
# smo3$alphahat


plot(step.a*smo3$alphahat[,13]+step.c*smo3$alphahat[,12]+smo3$alphahat[,14], col=2,ylim=c(0,12000))
lines(y)

auxres_ls <- rstandard(smo3, "state")
plot(auxres_ls)
