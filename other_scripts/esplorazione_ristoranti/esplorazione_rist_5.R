## Ristorante5 ----
# si prende in considerazione per le successive analisi il quinto ristorante

### analisi vendite considerando tutti gli anni ----

# vendite giornaliere secondo ristorante 
vendite5_day <- ts(ristorante5$vendite, start = 2017, frequency=365) 

# vendite settimanali medie secondo ristorante 
week_rist5 <- as.Date(cut(ristorante5$data, "week"))

remove_dates <- as.Date(c('2016-12-26','2021-04-12'))
all_dates <- week_rist5
week_rist5 <- all_dates[!all_dates %in% remove_dates]

vendite5_sett <- aggregate(vendite ~ week_rist5, ristorante5[-c(1,1563),], sum)
vendite5_sett <- vendite5_sett$vendite
vendite5_sett <- ts(vendite5_sett,start=2017,frequency=52) 

vendite5_sett_avg <- aggregate(vendite ~ week_rist5, ristorante5[-c(1,1563),], mean)
vendite5_sett_avg <- vendite5_sett_avg$vendite
vendite5_sett_avg <- ts(vendite5_sett_avg,start=2017,frequency=52) 

# vendite mensili medie  secondo ristorante 
month_rist5 <- as.Date(cut(ristorante5$data, "month"))

vendite5_mens <- aggregate(vendite ~ month_rist5, ristorante5, sum)
vendite5_mens <- vendite5_mens$vendite
vendite5_mens <- ts(vendite5_mens,start=2017,frequency=12) 

vendite5_mens_avg <- aggregate(vendite ~ month_rist5, ristorante5, mean)
vendite5_mens_avg <- vendite5_mens_avg$vendite
vendite5_mens_avg <- ts(vendite5_mens_avg,start=2017,frequency=12) 

# plot delle diverse serie trovate sopra
print(
  autoplot(vendite5_day) +
    ggtitle("Ristorante 5: vendite giornaliere") +
    xlab("anno") +
    ylab("vendite")
)

print(
  autoplot(vendite5_sett_avg) +
    ggtitle("Ristorante 5: vendite medie settimanali") +
    xlab("anno") +
    ylab("vendite")
)


print(
  autoplot(vendite5_mens_avg) +
    ggtitle("Ristorante 5: vendite medie mensili") +
    xlab("anno") +
    ylab("vendite")
)


### analisi vendite considerando il periodo pre covid ----

# si procede ad analizzare ciascun ristorante nel periodo antecedente il covid-19 
reference_date <- as.Date("2020-03-09", format = "%Y-%m-%d")

# vendite Ristorante 5 pre covid
ristorante5_pre_covid_vendite <- ristorante5 %>%
  filter(ristorante5$data < reference_date) %>%
  select(vendite, data)

# vendite giornaliere secondo ristorante pre covid
vendite5_day_pre <- ts(ristorante5_pre_covid_vendite$vendite,start=2017,frequency=365) 

# vendite settimanali medie secondo ristorante pre covid
week_pre_covid_rist5 <- as.Date(cut(ristorante5_pre_covid_vendite$data, "week"))

# si procede ad eliminare il giorno 1 gennaio 2017 che risulta essere domenica
remove_dates <- as.Date(c('2016-12-26'))
all_dates <- week_pre_covid_rist5
week_pre_covid_rist5 <- all_dates[!all_dates %in% remove_dates]

vendite5_sett_pre <- aggregate(vendite ~ week_pre_covid_rist5, ristorante5_pre_covid_vendite[-1,], sum)
vendite5_sett_pre <- vendite5_sett_pre$vendite
vendite5_sett_pre <- ts(vendite5_sett_pre,start=2017,frequency=52) 

vendite5_sett_avg_pre <- aggregate(vendite ~ week_pre_covid_rist5, ristorante5_pre_covid_vendite[-1,], mean)
vendite5_sett_avg_pre <- vendite5_sett_avg_pre$vendite
vendite5_sett_avg_pre <- ts(vendite5_sett_avg_pre,start=2017,frequency=52) 

# vendite mensili medie  secondo ristorante pre covid
month_pre_covid_rist5 <- as.Date(cut(ristorante5_pre_covid_vendite$data, "month"))

vendite5_mens_pre <- aggregate(vendite ~ month_pre_covid_rist5, ristorante5_pre_covid_vendite, sum)
vendite5_mens_pre <- vendite5_mens_pre$vendite
vendite5_mens_pre <- ts(vendite5_mens_pre,start=2017,frequency=12) 

vendite5_mens_avg_pre <- aggregate(vendite ~ month_pre_covid_rist5, ristorante5_pre_covid_vendite, mean)
vendite5_mens_avg_pre <- vendite5_mens_avg_pre$vendite
vendite5_mens_avg_pre <- ts(vendite5_mens_avg_pre,start=2017,frequency=12) 

# plot delle diverse serie pre covid trovate sopra 
print(
  autoplot(vendite5_day_pre) +
    ggtitle("Ristorante 5: vendite giornaliere pre covid") +
    xlab("anno") +
    ylab("vendite")
)

print(
  autoplot(vendite5_sett_avg_pre) +
    ggtitle("Ristorante 5: vendite medie settimanali pre covid") +
    xlab("anno") +
    ylab("vendite")
)

print(
  autoplot(vendite5_mens_avg_pre) +
    ggtitle("Ristorante 5: vendite medie mensili pre covid") +
    xlab("anno") +
    ylab("vendite")
)


### analisi scontrini considerando tutti gli anni ----

# eventualmente da mettere a confronto con l'andamento delle vendite (grafico sopra vendite e sotto scontrini)

# scontrini giornalieri secondo ristorante 
scontrini5_day <- ts(ristorante5$scontrini,start=2017,frequency=365) 

# scontrini settimanali medie secondo ristorante 
# week_rist5

scontrini5_sett <- aggregate(scontrini ~ week_rist5, ristorante5[-c(1,1563),], sum)
scontrini5_sett <- scontrini5_sett$scontrini
scontrini5_sett <- ts(scontrini5_sett,start=2017,frequency=52) 

scontrini5_sett_avg <- aggregate(scontrini ~ week_rist5, ristorante5[-c(1,1563),], mean)
scontrini5_sett_avg <- scontrini5_sett_avg$scontrini
scontrini5_sett_avg <- ts(scontrini5_sett_avg,start=2017,frequency=52) 

# scontrini mensili medie  secondo ristorante 
month_rist5 <- as.Date(cut(ristorante5$data, "month"))

scontrini5_mens <- aggregate(scontrini ~ month_rist5, ristorante5, sum)
scontrini5_mens <- scontrini5_mens$scontrini
scontrini5_mens <- ts(scontrini5_mens,start=2017,frequency=12) 

scontrini5_mens_avg <- aggregate(scontrini ~ month_rist5, ristorante5, mean)
scontrini5_mens_avg <- scontrini5_mens_avg$scontrini
scontrini5_mens_avg <- ts(scontrini5_mens_avg,start=2017,frequency=12) 

# plot delle diverse serie trovate sopra
print(
  autoplot(scontrini5_day) +
    ggtitle("Ristorante 5: scontrini giornalieri") +
    xlab("anno") +
    ylab("scontrini")
)

print(
  autoplot(scontrini5_sett_avg) +
    ggtitle("Ristorante 5: scontrini medi settimanali") +
    xlab("anno") +
    ylab("scontrini")
)

print(
  autoplot(scontrini5_mens_avg) +
    ggtitle("Ristorante 5: scontrini medi mensili") +
    xlab("anno") +
    ylab("scontrini")
)

### analisi scontrini considerando il periodo pre covid ----

# si procede ad analizzare ciascun ristorante nel periodo antecedente il covid-19 
reference_date <- as.Date("2020-03-09", format = "%Y-%m-%d")

# scontrini Ristorante 5 pre covid
ristorante5_pre_covid_scontrini <- ristorante5 %>%
  filter(ristorante5$data < reference_date) %>%
  select(scontrini, data)

# scontrini giornalieri secondo ristorante pre covid
scontrini5_day_pre <- ts(ristorante5_pre_covid_scontrini$scontrini,start=2017,frequency=365) 

# scontrini settimanali medi secondo ristorante pre covid
# week_pre_covid_rist5 

scontrini5_sett_avg_pre <- aggregate(scontrini ~ week_pre_covid_rist5, ristorante5_pre_covid_scontrini[-1,], mean)
scontrini5_sett_avg_pre <- scontrini5_sett_avg_pre$scontrini
scontrini5_sett_avg_pre <- ts(scontrini5_sett_avg_pre,start=2017,frequency=52) 

# scontrini mensili medi  secondo ristorante pre covid
month_pre_covid <- as.Date(cut(ristorante5_pre_covid_scontrini$data, "month"))
scontrini5_mens_avg_pre <- aggregate(scontrini ~ month_pre_covid, ristorante5_pre_covid_scontrini, mean)
scontrini5_mens_avg_pre <- scontrini5_mens_avg_pre$scontrini
scontrini5_mens_avg_pre <- ts(scontrini5_mens_avg_pre,start=2017,frequency=12) 

# plot delle diverse serie pre covid trovate sopra 
print(
  autoplot(scontrini5_day_pre) +
    ggtitle("Ristorante 5: scontrini giornalieri pre covid") +
    xlab("anno") +
    ylab("scontrini")
)

print(
  autoplot(scontrini5_sett_avg_pre) +
    ggtitle("Ristorante 5: scontrini medi settimanali pre covid") +
    xlab("anno") +
    ylab("scontrini")
)

print(
  autoplot(scontrini5_mens_avg_pre) +
    ggtitle("Ristorante 5: scontrini medi mensili pre covid") +
    xlab("anno") +
    ylab("scontrini")
)



### analisi stagionalità considerando tutti gli anni ----
print(
  ggseasonplot(vendite5_sett, year.labels=TRUE, year.labels.left=TRUE) +
    ylab("euro") +
    ggtitle("Seasonal plot Ristorante 5: vendite settimanale")
)

print(
  ggseasonplot(vendite5_mens, year.labels=TRUE, year.labels.left=TRUE) +
    ylab("euro") +
    ggtitle("Seasonal plot Ristorante 5: vendite mensili")
)

### seasonal sub series plot
print(
  ggsubseriesplot(vendite5_mens_avg) +
    ylab("$ million") +
    ggtitle("Seasonal subseries plot Ristorante 5: vendite medie mensili")
)


### analisi stagionalità considerando il periodo pre covid ----
print(
  ggseasonplot(vendite5_sett_pre, year.labels=TRUE, year.labels.left=TRUE) +
    ylab("euro") +
    ggtitle("Seasonal plot Ristorante 5: vendite settimanale pre covid")
)

print(
  ggseasonplot(vendite5_mens_pre, year.labels=TRUE, year.labels.left=TRUE) +
    ylab("euro") +
    ggtitle("Seasonal plot Ristorante 5: vendite mensili pre covid")
)

### seasonal sub series plot
print(
  ggsubseriesplot(vendite5_mens_avg_pre) +
    ylab("$ million") +
    ggtitle("Seasonal subseries plot Ristorante 5: vendite medie mensili pre covid")
)


### analisi correlazione tra vendite e scontrini ----
scontrini_sett_avg5 <- aggregate(scontrini ~ week_rist5, ristorante5[-c(1,1563),], mean)
scontrini_sett_avg5 <- scontrini_sett_avg5$scontrini
scontrini_sett_avg5 <- ts(scontrini_sett_avg5,start=2017,frequency=52) 
sc_ven5_sett_avg <-ts.intersect(vendite5_sett_avg, scontrini_sett_avg5)


print(
  autoplot(sc_ven5_sett_avg, facets=TRUE) +
    xlab("Anni") + ylab("") +
    ggtitle("Confronto scontrini e vendite Ristorante 5")
)

print(
  qplot(vendite, scontrini, data=as.data.frame(ristorante5)) +
    ylab("scontrini") + xlab("vendite")+
    ggtitle("Correlazione scontrini e vendite Ristorante 5")
)



### analisi autocorrelazione considerando tutti gli anni ----
# per una serie con trend, l'autocorrelazione è alta a lag vicini, 
# e si abbassa piano piano se c'è stagionalità l'autocorrelazione presenta lag ogni tot

#gglagplot(vendite5_mens_avg)

plot(
  ggAcf(vendite5_day, lag=7) +
    ggtitle("Ristorante 5: Autocorrelation vendite giornaliere")
)

plot(
  ggAcf(vendite5_sett_avg, lag=52)+
    ggtitle("Ristorante 5: Autocorrelation vendite medie settimanali")
)

plot(
  ggAcf(vendite5_mens_avg, lag=24)+
    ggtitle("Ristorante 5: Autocorrelation vendite medie mensili")
)


### analisi autocorrelazione pre covid ----

plot(
  ggAcf(vendite5_day_pre, lag=7) +
    ggtitle("Ristorante 5: Autocorrelation vendite giornaliere pre covid")
)

plot(
  ggAcf(vendite5_sett_avg_pre, lag=52)+
    ggtitle("Ristorante 5: Autocorrelation vendite medie settimanali pre covid")
)

plot(
  ggAcf(vendite5_mens_avg_pre, lag=24)+
    ggtitle("Ristorante 5: Autocorrelation vendite medie mensili pre covid")
)



### decomposizione considerando tutti gli anni ----
# decomposizione giornaliera 
multi_vendite5 <- msts(ristorante1$vendite, ts.frequency = 365, start=2017, seasonal.periods = c(7,365))
multi_vendite5_dec <- mstl(multi_vendite5, s.window = "periodic")
print(autoplot(multi_vendite5_dec) + ggtitle("Ristorante 5: Decomposizione giornaliera"))

# decomposizione settimanale
vendite5_sett.fit<-stl(vendite5_sett_avg,s.window="periodic")
trend.vendite5_sett<-vendite5_sett.fit$time.series[,2]
stag.vendite5_sett<-vendite5_sett.fit$time.series[,1]
res.vendite5_sett<-vendite5_sett.fit$time.series[,3]
print(autoplot(vendite5_sett.fit) + ggtitle("Ristorante 5: Decomposizione settimanale"))

# decomposizione mensile 
vendite5_mens.fit<-stl(vendite5_mens_avg,s.window="periodic")
trend.vendite5_mens<-vendite5_mens.fit$time.series[,2]
stag.vendite5_mens<-vendite5_mens.fit$time.series[,1]
res.vendite5_mens<-vendite5_mens.fit$time.series[,3]
print(autoplot(vendite5_mens.fit) + ggtitle("Ristorante 5: Decomposizione mensile"))

components.ts_5 = decompose(vendite5_mens_avg)
plot(components.ts_5)



### decomposizione pre covid ----
# decomposizione giornaliera 
multi_vendite5_pre <- msts(ristorante1_pre_covid_vendite$vendite, ts.frequency = 365, start=2017, seasonal.periods = c(7,365))
multi_vendite_dec5_pre <- mstl(multi_vendite5_pre, s.window = "periodic")
print(autoplot(multi_vendite_dec5_pre) + ggtitle("Ristorante 5: Decomposizione giornaliera pre covid"))

# decomposizione settimanale
vendite5_sett.fit_pre<-stl(vendite5_sett_avg_pre,s.window="periodic")
trend.vendite5_sett_pre<-vendite5_sett.fit_pre$time.series[,2]
stag.vendite5_sett_pre<-vendite5_sett.fit_pre$time.series[,1]
res.vendite5_sett_pre<-vendite5_sett.fit_pre$time.series[,3]
print(autoplot(vendite5_sett.fit_pre) + ggtitle("Ristorante 5: Decomposizione settimanale pre covid"))

# decomposizione mensile 
vendite5_mens.fit_pre<-stl(vendite5_mens_avg_pre,s.window="periodic")
trend.vendite5_mens_pre<-vendite5_mens.fit_pre$time.series[,2]
stag.vendite5_mens_pre<-vendite5_mens.fit_pre$time.series[,1]
res.vendite5_mens_pre<-vendite5_mens.fit_pre$time.series[,3]
print(autoplot(vendite5_mens.fit_pre) + ggtitle("Ristorante 5: Decomposizione mensile pre covid"))

components.ts_pre_5 = decompose(vendite5_mens_avg_pre)
plot(components.ts_pre_5)


### decomposizione approfondita considerando tutti gli anni----

# mese<-as.factor(month(ymd(ristorante5$data)))
# y=vendite5_day
# step.c <- y
# step.a <- y
# step.c[] <- 0
# step.a[] <- 0
# step.c[(1167+1):length(y)] <- 1 #non so se vuoi usare la tua funzione per le date sono 12 mar  2020
# step.a[(1222+1):length(y)] <- 1 #6 mag 2020
# # step.c
# # step.a
# 
# mod3<-SSModel(y ~mese+step.c+step.a+SSMtrend(degree = 2, Q=list(NA,NA))+SSMseasonal(period = 7, sea.type = "dummy", Q=NA), H=NA)
# fit3<-fitSSM(mod3,inits = c(0,0,0,0))
# # fit3$optim.out$convergence
# 
# smo3 <- KFS(fit3$model, smoothing = c("state", "disturbance", "mean"))
# # smo3$alphahat
# 
# 
# plot(step.a*smo3$alphahat[,13]+step.c*smo3$alphahat[,12]+smo3$alphahat[,14], col=2,ylim=c(0,12000))
# lines(y)
# 
# auxres_ls <- rstandard(smo3, "state")
# plot(auxres_ls)
