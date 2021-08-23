## Ristorante4 ----
# si prende in considerazione per le successive analisi il quarto ristorante

### analisi vendite considerando tutti gli anni ----

# vendite giornaliere secondo ristorante 
vendite4_day <- ts(ristorante4$vendite, start = 2017, frequency=365) 

# vendite settimanali medie secondo ristorante 
week_rist4 <- as.Date(cut(ristorante4$data, "week"))

remove_dates <- as.Date(c('2016-12-26','2021-04-12'))
all_dates <- week_rist4
week_rist4 <- all_dates[!all_dates %in% remove_dates]

vendite4_sett <- aggregate(vendite ~ week_rist4, ristorante4[-c(1,1563),], sum)
vendite4_sett <- vendite4_sett$vendite
vendite4_sett <- ts(vendite4_sett,start=2017,frequency=52) 

vendite4_sett_avg <- aggregate(vendite ~ week_rist4, ristorante4[-c(1,1563),], mean)
vendite4_sett_avg <- vendite4_sett_avg$vendite
vendite4_sett_avg <- ts(vendite4_sett_avg,start=2017,frequency=52) 

# vendite mensili medie  secondo ristorante 
month_rist4 <- as.Date(cut(ristorante4$data, "month"))

vendite4_mens <- aggregate(vendite ~ month_rist4, ristorante4, sum)
vendite4_mens <- vendite4_mens$vendite
vendite4_mens <- ts(vendite4_mens,start=2017,frequency=12) 

vendite4_mens_avg <- aggregate(vendite ~ month_rist4, ristorante4, mean)
vendite4_mens_avg <- vendite4_mens_avg$vendite
vendite4_mens_avg <- ts(vendite4_mens_avg,start=2017,frequency=12) 

# plot delle diverse serie trovate sopra
print(
  autoplot(vendite4_day) +
    ggtitle("Ristorante 4: vendite giornaliere") +
    xlab("anno") +
    ylab("vendite")
)

print(
  autoplot(vendite4_sett_avg) +
    ggtitle("Ristorante 4: vendite medie settimanali") +
    xlab("anno") +
    ylab("vendite")
)


print(
  autoplot(vendite4_mens_avg) +
    ggtitle("Ristorante 4: vendite medie mensili") +
    xlab("anno") +
    ylab("vendite")
)


### analisi vendite considerando il periodo pre covid ----

# si procede ad analizzare ciascun ristorante nel periodo antecedente il covid-19 
reference_date <- as.Date("2020-03-09", format = "%Y-%m-%d")

# vendite Ristorante 4 pre covid
ristorante4_pre_covid_vendite <- ristorante4 %>%
  filter(ristorante4$data < reference_date) %>%
  select(vendite, data)

# vendite giornaliere secondo ristorante pre covid
vendite4_day_pre <- ts(ristorante4_pre_covid_vendite$vendite,start=2017,frequency=365) 

# vendite settimanali medie secondo ristorante pre covid
week_pre_covid_rist4 <- as.Date(cut(ristorante4_pre_covid_vendite$data, "week"))

remove_dates <- as.Date(c('2016-12-26'))
all_dates <- week_pre_covid_rist4
week_pre_covid_rist4 <- all_dates[!all_dates %in% remove_dates]

vendite4_sett_pre <- aggregate(vendite ~ week_pre_covid_rist4, ristorante4_pre_covid_vendite[-1,], sum)
vendite4_sett_pre <- vendite4_sett_pre$vendite
vendite4_sett_pre <- ts(vendite4_sett_pre,start=2017,frequency=52) 

vendite4_sett_avg_pre <- aggregate(vendite ~ week_pre_covid_rist4, ristorante4_pre_covid_vendite[-1,], mean)
vendite4_sett_avg_pre <- vendite4_sett_avg_pre$vendite
vendite4_sett_avg_pre <- ts(vendite4_sett_avg_pre,start=2017,frequency=52) 

# vendite mensili medie  secondo ristorante pre covid
month_pre_covid_rist4 <- as.Date(cut(ristorante4_pre_covid_vendite$data, "month"))

vendite4_mens_pre <- aggregate(vendite ~ month_pre_covid_rist4, ristorante4_pre_covid_vendite, sum)
vendite4_mens_pre <- vendite4_mens_pre$vendite
vendite4_mens_pre <- ts(vendite4_mens_pre,start=2017,frequency=12) 

vendite4_mens_avg_pre <- aggregate(vendite ~ month_pre_covid_rist4, ristorante4_pre_covid_vendite, mean)
vendite4_mens_avg_pre <- vendite4_mens_avg_pre$vendite
vendite4_mens_avg_pre <- ts(vendite4_mens_avg_pre,start=2017,frequency=12) 

# plot delle diverse serie pre covid trovate sopra 
print(
  autoplot(vendite4_day_pre) +
    ggtitle("Ristorante 4: vendite giornaliere pre covid") +
    xlab("anno") +
    ylab("vendite")
)

print(
  autoplot(vendite4_sett_avg_pre) +
    ggtitle("Ristorante 4: vendite medie settimanali pre covid") +
    xlab("anno") +
    ylab("vendite")
)

print(
  autoplot(vendite4_mens_avg_pre) +
    ggtitle("Ristorante 4: vendite medie mensili pre covid") +
    xlab("anno") +
    ylab("vendite")
)


### analisi scontrini considerando tutti gli anni ----

# eventualmente da mettere a confronto con l'andamento delle vendite (grafico sopra vendite e sotto scontrini)

# scontrini giornalieri secondo ristorante 
scontrini4_day <- ts(ristorante4$scontrini,start=2017,frequency=365) 

# scontrini settimanali medie secondo ristorante 
# week_rist4

scontrini4_sett <- aggregate(scontrini ~ week_rist4, ristorante4[-c(1,1563),], sum)
scontrini4_sett <- scontrini4_sett$scontrini
scontrini4_sett <- ts(scontrini4_sett,start=2017,frequency=52) 

scontrini4_sett_avg <- aggregate(scontrini ~ week_rist4, ristorante4[-c(1,1563),], mean)
scontrini4_sett_avg <- scontrini4_sett_avg$scontrini
scontrini4_sett_avg <- ts(scontrini4_sett_avg,start=2017,frequency=52) 

# scontrini mensili medie  secondo ristorante 
month_rist4 <- as.Date(cut(ristorante4$data, "month"))

scontrini4_mens <- aggregate(scontrini ~ month_rist4, ristorante4, sum)
scontrini4_mens <- scontrini4_mens$scontrini
scontrini4_mens <- ts(scontrini4_mens,start=2017,frequency=12) 

scontrini4_mens_avg <- aggregate(scontrini ~ month_rist4, ristorante4, mean)
scontrini4_mens_avg <- scontrini4_mens_avg$scontrini
scontrini4_mens_avg <- ts(scontrini4_mens_avg,start=2017,frequency=12) 

# plot delle diverse serie trovate sopra
print(
  autoplot(scontrini4_day) +
    ggtitle("Ristorante 4: scontrini giornalieri") +
    xlab("anno") +
    ylab("scontrini")
)

print(
  autoplot(scontrini4_sett_avg) +
    ggtitle("Ristorante 4: scontrini medi settimanali") +
    xlab("anno") +
    ylab("scontrini")
)

print(
  autoplot(scontrini4_mens_avg) +
    ggtitle("Ristorante 4: scontrini medi mensili") +
    xlab("anno") +
    ylab("scontrini")
)

### analisi scontrini considerando il periodo pre covid ----

# si procede ad analizzare ciascun ristorante nel periodo antecedente il covid-19 
reference_date <- as.Date("2020-03-09", format = "%Y-%m-%d")

# scontrini Ristorante 4 pre covid
ristorante4_pre_covid_scontrini <- ristorante4 %>%
  filter(ristorante4$data < reference_date) %>%
  select(scontrini, data)

# scontrini giornalieri secondo ristorante pre covid
scontrini4_day_pre <- ts(ristorante4_pre_covid_scontrini$scontrini,start=2017,frequency=365) 

# scontrini settimanali medi secondo ristorante pre covid
# week_pre_covid_rist4 

scontrini4_sett_avg_pre <- aggregate(scontrini ~ week_pre_covid_rist4, ristorante4_pre_covid_scontrini[-1,], mean)
scontrini4_sett_avg_pre <- scontrini4_sett_avg_pre$scontrini
scontrini4_sett_avg_pre <- ts(scontrini4_sett_avg_pre,start=2017,frequency=52) 

# scontrini mensili medi  secondo ristorante pre covid
month_pre_covid <- as.Date(cut(ristorante4_pre_covid_scontrini$data, "month"))
scontrini4_mens_avg_pre <- aggregate(scontrini ~ month_pre_covid, ristorante4_pre_covid_scontrini, mean)
scontrini4_mens_avg_pre <- scontrini4_mens_avg_pre$scontrini
scontrini4_mens_avg_pre <- ts(scontrini4_mens_avg_pre,start=2017,frequency=12) 

# plot delle diverse serie pre covid trovate sopra 
print(
  autoplot(scontrini4_day_pre) +
    ggtitle("Ristorante 4: scontrini giornalieri pre covid") +
    xlab("anno") +
    ylab("scontrini")
)

print(
  autoplot(scontrini4_sett_avg_pre) +
    ggtitle("Ristorante 4: scontrini medi settimanali pre covid") +
    xlab("anno") +
    ylab("scontrini")
)

print(
  autoplot(scontrini4_mens_avg_pre) +
    ggtitle("Ristorante 4: scontrini medi mensili pre covid") +
    xlab("anno") +
    ylab("scontrini")
)



### analisi stagionalità considerando tutti gli anni ----
print(
  ggseasonplot(vendite4_sett, year.labels=TRUE, year.labels.left=TRUE) +
    ylab("euro") +
    ggtitle("Seasonal plot Ristorante 4: vendite settimanale")
)

print(
  ggseasonplot(vendite4_mens, year.labels=TRUE, year.labels.left=TRUE) +
    ylab("euro") +
    ggtitle("Seasonal plot Ristorante 4: vendite mensili")
)

### seasonal sub series plot
print(
  ggsubseriesplot(vendite4_mens_avg) +
    ylab("$ million") +
    ggtitle("Seasonal subseries plot Ristorante 4: vendite medie mensili")
)


### analisi stagionalità considerando il periodo pre covid ----
print(
  ggseasonplot(vendite4_sett_pre, year.labels=TRUE, year.labels.left=TRUE) +
    ylab("euro") +
    ggtitle("Seasonal plot Ristorante 4: vendite settimanale pre covid")
)

print(
  ggseasonplot(vendite4_mens_pre, year.labels=TRUE, year.labels.left=TRUE) +
    ylab("euro") +
    ggtitle("Seasonal plot Ristorante 4: vendite mensili pre covid")
)

### seasonal sub series plot
print(
  ggsubseriesplot(vendite4_mens_avg_pre) +
    ylab("$ million") +
    ggtitle("Seasonal subseries plot Ristorante 4: vendite medie mensili pre covid")
)


### analisi correlazione tra vendite e scontrini ----
scontrini_sett_avg4 <- aggregate(scontrini ~ week_rist4, ristorante4[-c(1,1563),], mean)
scontrini_sett_avg4 <- scontrini_sett_avg4$scontrini
scontrini_sett_avg4 <- ts(scontrini_sett_avg4,start=2017,frequency=52) 
sc_ven4_sett_avg <-ts.intersect(vendite4_sett_avg, scontrini_sett_avg4)


print(
  autoplot(sc_ven4_sett_avg, facets=TRUE) +
    xlab("Anni") + ylab("") +
    ggtitle("Confronto scontrini e vendite Ristorante 4")
)

print(
  qplot(vendite, scontrini, data=as.data.frame(ristorante4)) +
    ylab("scontrini") + xlab("vendite")+
    ggtitle("Correlazione scontrini e vendite Ristorante 4")
)



### analisi autocorrelazione considerando tutti gli anni ----
# per una serie con trend, l'autocorrelazione è alta a lag vicini, 
# e si abbassa piano piano se c'è stagionalità l'autocorrelazione presenta lag ogni tot

#gglagplot(vendite4_mens_avg)

plot(
  ggAcf(vendite4_day, lag=7) +
    ggtitle("Ristorante 4: Autocorrelation vendite giornaliere")
)

plot(
  ggAcf(vendite4_sett_avg, lag=52)+
    ggtitle("Ristorante 4: Autocorrelation vendite medie settimanali")
)

plot(
  ggAcf(vendite4_mens_avg, lag=24)+
    ggtitle("Ristorante 4: Autocorrelation vendite medie mensili")
)


### analisi autocorrelazione pre covid ----

plot(
  ggAcf(vendite4_day_pre, lag=7) +
    ggtitle("Ristorante 4: Autocorrelation vendite giornaliere pre covid")
)

plot(
  ggAcf(vendite4_sett_avg_pre, lag=52)+
    ggtitle("Ristorante 4: Autocorrelation vendite medie settimanali pre covid")
)

plot(
  ggAcf(vendite4_mens_avg_pre, lag=24)+
    ggtitle("Ristorante 4: Autocorrelation vendite medie mensili pre covid")
)



### decomposizione considerando tutti gli anni ----
# decomposizione giornaliera 
multi_vendite4 <- msts(ristorante1$vendite, ts.frequency = 365, start=2017, seasonal.periods = c(7,365))
multi_vendite4_dec <- mstl(multi_vendite4, s.window = "periodic")
print(autoplot(multi_vendite4_dec) + ggtitle("Ristorante 4: Decomposizione giornaliera"))

# decomposizione settimanale
vendite4_sett.fit<-stl(vendite4_sett_avg,s.window="periodic")
trend.vendite4_sett<-vendite4_sett.fit$time.series[,2]
stag.vendite4_sett<-vendite4_sett.fit$time.series[,1]
res.vendite4_sett<-vendite4_sett.fit$time.series[,3]
print(autoplot(vendite4_sett.fit) + ggtitle("Ristorante 4: Decomposizione settimanale"))

# decomposizione mensile 
vendite4_mens.fit<-stl(vendite4_mens_avg,s.window="periodic")
trend.vendite4_mens<-vendite4_mens.fit$time.series[,2]
stag.vendite4_mens<-vendite4_mens.fit$time.series[,1]
res.vendite4_mens<-vendite4_mens.fit$time.series[,3]
print(autoplot(vendite4_mens.fit) + ggtitle("Ristorante 4: Decomposizione mensile"))

components.ts_ = decompose(vendite4_mens_avg)
plot(components.ts_)



### decomposizione pre covid ----
# decomposizione giornaliera 
multi_vendite4_pre <- msts(ristorante1_pre_covid_vendite$vendite, ts.frequency = 365, start=2017, seasonal.periods = c(7,365))
multi_vendite_dec4_pre <- mstl(multi_vendite4_pre, s.window = "periodic")
print(autoplot(multi_vendite_dec4_pre) + ggtitle("Ristorante 4: Decomposizione giornaliera pre covid"))

# decomposizione settimanale
vendite4_sett.fit_pre<-stl(vendite4_sett_avg_pre,s.window="periodic")
trend.vendite4_sett_pre<-vendite4_sett.fit_pre$time.series[,2]
stag.vendite4_sett_pre<-vendite4_sett.fit_pre$time.series[,1]
res.vendite4_sett_pre<-vendite4_sett.fit_pre$time.series[,3]
print(autoplot(vendite4_sett.fit_pre) + ggtitle("Ristorante 4: Decomposizione settimanale pre covid"))

# decomposizione mensile 
vendite4_mens.fit_pre<-stl(vendite4_mens_avg_pre,s.window="periodic")
trend.vendite4_mens_pre<-vendite4_mens.fit_pre$time.series[,2]
stag.vendite4_mens_pre<-vendite4_mens.fit_pre$time.series[,1]
res.vendite4_mens_pre<-vendite4_mens.fit_pre$time.series[,3]
print(autoplot(vendite4_mens.fit_pre) + ggtitle("Ristorante 4: Decomposizione mensile pre covid"))

components.ts_pre_4 = decompose(vendite4_mens_avg_pre)
plot(components.ts_pre_4)


### decomposizione approfondita considerando tutti gli anni----

# mese<-as.factor(month(ymd(ristorante4$data)))
# y=vendite4_day
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
