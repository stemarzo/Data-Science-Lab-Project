## Ristorante6 ----
# si prende in considerazione per le successive analisi il sesto ristorante

### analisi vendite considerando tutti gli anni ----


# si considera la serie storica a partire dai primi dati registrati
reference_date_rist6 <- as.Date("2017-09-25", format = "%Y-%m-%d")

ristorante6 <- ristorante6 %>%
  filter(ristorante6$data >= reference_date_rist6)


# vendite giornaliere secondo ristorante 
vendite6_day <- ts(ristorante6$vendite, start = decimal_date(as.Date("2017-09-21")), frequency=365) 

# vendite settimanali medie secondo ristorante 
week_rist6 <- as.Date(cut(ristorante6$data, "week"))

remove_dates <- as.Date(c('2021-04-12'))
all_dates <- week_rist6
week_rist6 <- all_dates[!all_dates %in% remove_dates]

vendite6_sett <- aggregate(vendite ~ week_rist6, ristorante6[-1296,], sum)
vendite6_sett <- vendite6_sett$vendite
vendite6_sett <- ts(vendite6_sett,start = decimal_date(as.Date("2017-09-21")),frequency=52) 

vendite6_sett_avg <- aggregate(vendite ~ week_rist6, ristorante6[-1296,], mean)
vendite6_sett_avg <- vendite6_sett_avg$vendite
vendite6_sett_avg <- ts(vendite6_sett_avg,start = decimal_date(as.Date("2017-09-21")),frequency=52) 

# vendite mensili medie  secondo ristorante 
month_rist6 <- as.Date(cut(ristorante6$data, "month"))

vendite6_mens <- aggregate(vendite ~ month_rist6, ristorante6, sum)
vendite6_mens <- vendite6_mens$vendite
vendite6_mens <- ts(vendite6_mens,start = decimal_date(as.Date("2017-09-21")),frequency=12) 

vendite6_mens_avg <- aggregate(vendite ~ month_rist6, ristorante6, mean)
vendite6_mens_avg <- vendite6_mens_avg$vendite
vendite6_mens_avg <- ts(vendite6_mens_avg,start = decimal_date(as.Date("2017-09-21")),frequency=12) 

# plot delle diverse serie trovate sopra
print(
  autoplot(vendite6_day) +
    ggtitle("Ristorante 6: vendite giornaliere") +
    xlab("anno") +
    ylab("vendite")
)

print(
  autoplot(vendite6_sett_avg) +
    ggtitle("Ristorante 6: vendite medie settimanali") +
    xlab("anno") +
    ylab("vendite")
)


print(
  autoplot(vendite6_mens_avg) +
    ggtitle("Ristorante 6: vendite medie mensili") +
    xlab("anno") +
    ylab("vendite")
)


### analisi vendite considerando il periodo pre covid ----

# si procede ad analizzare ciascun ristorante nel periodo antecedente il covid-19 
reference_date <- as.Date("2020-01-06", format = "%Y-%m-%d")

# vendite Ristorante 6 pre covid
ristorante6_pre_covid_vendite <- ristorante6 %>%
  filter(ristorante6$data < reference_date) %>%
  select(vendite, data)

# vendite giornaliere secondo ristorante pre covid
vendite6_day_pre <- ts(ristorante6_pre_covid_vendite$vendite,start = decimal_date(as.Date("2017-09-21")),frequency=365) 

# vendite settimanali medie secondo ristorante pre covid
week_pre_covid_rist6 <- as.Date(cut(ristorante6_pre_covid_vendite$data, "week"))

vendite6_sett_pre <- aggregate(vendite ~ week_pre_covid_rist6, ristorante6_pre_covid_vendite, sum)
vendite6_sett_pre <- vendite6_sett_pre$vendite
vendite6_sett_pre <- ts(vendite6_sett_pre,start = decimal_date(as.Date("2017-09-21")),frequency=52) 

vendite6_sett_avg_pre <- aggregate(vendite ~ week_pre_covid_rist6, ristorante6_pre_covid_vendite, mean)
vendite6_sett_avg_pre <- vendite6_sett_avg_pre$vendite
vendite6_sett_avg_pre <- ts(vendite6_sett_avg_pre,start = decimal_date(as.Date("2017-09-21")),frequency=52) 

# vendite mensili medie  secondo ristorante pre covid
month_pre_covid_rist6 <- as.Date(cut(ristorante6_pre_covid_vendite$data, "month"))

vendite6_mens_pre <- aggregate(vendite ~ month_pre_covid_rist6, ristorante6_pre_covid_vendite, sum)
vendite6_mens_pre <- vendite6_mens_pre$vendite
vendite6_mens_pre <- ts(vendite6_mens_pre,start = decimal_date(as.Date("2017-09-21")),frequency=12) 

vendite6_mens_avg_pre <- aggregate(vendite ~ month_pre_covid_rist6, ristorante6_pre_covid_vendite, mean)
vendite6_mens_avg_pre <- vendite6_mens_avg_pre$vendite
vendite6_mens_avg_pre <- ts(vendite6_mens_avg_pre,start = decimal_date(as.Date("2017-09-21")),frequency=12) 

# plot delle diverse serie pre covid trovate sopra 
print(
  autoplot(vendite6_day_pre) +
    ggtitle("Ristorante 6: vendite giornaliere pre covid") +
    xlab("anno") +
    ylab("vendite")
)

print(
  autoplot(vendite6_sett_avg_pre) +
    ggtitle("Ristorante 6: vendite medie settimanali pre covid") +
    xlab("anno") +
    ylab("vendite")
)

print(
  autoplot(vendite6_mens_avg_pre) +
    ggtitle("Ristorante 6: vendite medie mensili pre covid") +
    xlab("anno") +
    ylab("vendite")
)


### analisi scontrini considerando tutti gli anni ----

# eventualmente da mettere a confronto con l'andamento delle vendite (grafico sopra vendite e sotto scontrini)

# scontrini giornalieri secondo ristorante 
scontrini6_day <- ts(ristorante6$scontrini,start = decimal_date(as.Date("2017-09-21")),frequency=365) 

# scontrini settimanali medie secondo ristorante 
# week_rist6 

scontrini6_sett <- aggregate(scontrini ~ week_rist6, ristorante6[-1296,], sum)
scontrini6_sett <- scontrini6_sett$scontrini
scontrini6_sett <- ts(scontrini6_sett,start = decimal_date(as.Date("2017-09-21")),frequency=52) 

scontrini6_sett_avg <- aggregate(scontrini ~ week_rist6, ristorante6[-1296,], mean)
scontrini6_sett_avg <- scontrini6_sett_avg$scontrini
scontrini6_sett_avg <- ts(scontrini6_sett_avg,start = decimal_date(as.Date("2017-09-21")),frequency=52) 

# scontrini mensili medie  secondo ristorante 
month_rist6 <- as.Date(cut(ristorante6$data, "month"))

scontrini6_mens <- aggregate(scontrini ~ month_rist6, ristorante6, sum)
scontrini6_mens <- scontrini6_mens$scontrini
scontrini6_mens <- ts(scontrini6_mens,start = decimal_date(as.Date("2017-09-21")),frequency=12) 

scontrini6_mens_avg <- aggregate(scontrini ~ month_rist6, ristorante6, mean)
scontrini6_mens_avg <- scontrini6_mens_avg$scontrini
scontrini6_mens_avg <- ts(scontrini6_mens_avg,start = decimal_date(as.Date("2017-09-21")),frequency=12) 

# plot delle diverse serie trovate sopra
print(
  autoplot(scontrini6_day) +
    ggtitle("Ristorante 6: scontrini giornalieri") +
    xlab("anno") +
    ylab("scontrini")
)

print(
  autoplot(scontrini6_sett_avg) +
    ggtitle("Ristorante 6: scontrini medi settimanali") +
    xlab("anno") +
    ylab("scontrini")
)

print(
  autoplot(scontrini6_mens_avg) +
    ggtitle("Ristorante 6: scontrini medi mensili") +
    xlab("anno") +
    ylab("scontrini")
)

### analisi scontrini considerando il periodo pre covid ----

# si procede ad analizzare ciascun ristorante nel periodo antecedente il covid-19 
reference_date <- as.Date("2020-01-06", format = "%Y-%m-%d")

# scontrini Ristorante 6 pre covid
ristorante6_pre_covid_scontrini <- ristorante6 %>%
  filter(ristorante6$data < reference_date) %>%
  select(scontrini, data)

# scontrini giornalieri secondo ristorante pre covid
scontrini6_day_pre <- ts(ristorante6_pre_covid_scontrini$scontrini,start = decimal_date(as.Date("2017-09-21")),frequency=365) 

# scontrini settimanali medi secondo ristorante pre covid
# week_pre_covid_rist6 

scontrini6_sett_avg_pre <- aggregate(scontrini ~ week_pre_covid_rist6, ristorante6_pre_covid_scontrini, mean)
scontrini6_sett_avg_pre <- scontrini6_sett_avg_pre$scontrini
scontrini6_sett_avg_pre <- ts(scontrini6_sett_avg_pre,start = decimal_date(as.Date("2017-09-21")),frequency=52) 

# scontrini mensili medi  secondo ristorante pre covid
month_pre_covid_rist6 <- as.Date(cut(ristorante6_pre_covid_scontrini$data, "month"))
scontrini6_mens_avg_pre <- aggregate(scontrini ~ month_pre_covid_rist6, ristorante6_pre_covid_scontrini, mean)
scontrini6_mens_avg_pre <- scontrini6_mens_avg_pre$scontrini
scontrini6_mens_avg_pre <- ts(scontrini6_mens_avg_pre,start = decimal_date(as.Date("2017-09-21")),frequency=12) 

# plot delle diverse serie pre covid trovate sopra 
print(
  autoplot(scontrini6_day_pre) +
    ggtitle("Ristorante 6: scontrini giornalieri pre covid") +
    xlab("anno") +
    ylab("scontrini")
)

print(
  autoplot(scontrini6_sett_avg_pre) +
    ggtitle("Ristorante 6: scontrini medi settimanali pre covid") +
    xlab("anno") +
    ylab("scontrini")
)

print(
  autoplot(scontrini6_mens_avg_pre) +
    ggtitle("Ristorante 6: scontrini medi mensili pre covid") +
    xlab("anno") +
    ylab("scontrini")
)



### analisi stagionalità considerando tutti gli anni ----
print(
  ggseasonplot(vendite6_sett, year.labels=TRUE, year.labels.left=TRUE) +
    ylab("euro") +
    ggtitle("Seasonal plot Ristorante 6: vendite settimanale")
)

print(
  ggseasonplot(vendite6_mens, year.labels=TRUE, year.labels.left=TRUE) +
    ylab("euro") +
    ggtitle("Seasonal plot Ristorante 6: vendite mensili")
)

### seasonal sub series plot
print(
  ggsubseriesplot(vendite6_mens_avg) +
    ylab("$ million") +
    ggtitle("Seasonal subseries plot Ristorante 6: vendite medie mensili")
)


### analisi stagionalità considerando il periodo pre covid ----
print(
  ggseasonplot(vendite6_sett_pre, year.labels=TRUE, year.labels.left=TRUE) +
    ylab("euro") +
    ggtitle("Seasonal plot Ristorante 6: vendite settimanale pre covid")
)

print(
  ggseasonplot(vendite6_mens_pre, year.labels=TRUE, year.labels.left=TRUE) +
    ylab("euro") +
    ggtitle("Seasonal plot Ristorante 6: vendite mensili pre covid")
)

### seasonal sub series plot
print(
  ggsubseriesplot(vendite6_mens_avg_pre) +
    ylab("$ million") +
    ggtitle("Seasonal subseries plot Ristorante 6: vendite medie mensili pre covid")
)


### analisi correlazione tra vendite e scontrini ----
scontrini_sett_avg6 <- aggregate(scontrini ~ week_rist6, ristorante6[-1296,], mean)
scontrini_sett_avg6 <- scontrini_sett_avg6$scontrini
scontrini_sett_avg6 <- ts(scontrini_sett_avg6,start = decimal_date(as.Date("2017-09-21")),frequency=52) 
sc_ven6_sett_avg <-ts.intersect(vendite6_sett_avg, scontrini_sett_avg6)


print(
  autoplot(sc_ven6_sett_avg, facets=TRUE) +
    xlab("Anni") + ylab("") +
    ggtitle("Confronto scontrini e vendite Ristorante 6")
)

print(
  qplot(vendite, scontrini, data=as.data.frame(ristorante6)) +
    ylab("scontrini") + xlab("vendite")+
    ggtitle("Correlazione scontrini e vendite Ristorante 6")
)



### analisi autocorrelazione considerando tutti gli anni ----
# per una serie con trend, l'autocorrelazione è alta a lag vicini, 
# e si abbassa piano piano se c'è stagionalità l'autocorrelazione presenta lag ogni tot

#gglagplot(vendite6_mens_avg)

plot(
  ggAcf(vendite6_day, lag=7) +
    ggtitle("Ristorante 6: Autocorrelation vendite giornaliere")
)

plot(
  ggAcf(vendite6_sett_avg, lag=52)+
    ggtitle("Ristorante 6: Autocorrelation vendite medie settimanali")
)

plot(
  ggAcf(vendite6_mens_avg, lag=24)+
    ggtitle("Ristorante 6: Autocorrelation vendite medie mensili")
)


### analisi autocorrelazione pre covid ----

plot(
  ggAcf(vendite6_day_pre, lag=7) +
    ggtitle("Ristorante 6: Autocorrelation vendite giornaliere pre covid")
)

plot(
  ggAcf(vendite6_sett_avg_pre, lag=52)+
    ggtitle("Ristorante 6: Autocorrelation vendite medie settimanali pre covid")
)

plot(
  ggAcf(vendite6_mens_avg_pre, lag=24)+
    ggtitle("Ristorante 6: Autocorrelation vendite medie mensili pre covid")
)



### decomposizione considerando tutti gli anni ----
# decomposizione giornaliera 
multi_vendite6 <- msts(ristorante1$vendite, ts.frequency = 365, start=2017, seasonal.periods = c(7,365))
multi_vendite6_dec <- mstl(multi_vendite6, s.window = "periodic")
print(autoplot(multi_vendite6_dec) + ggtitle("Ristorante 6: Decomposizione giornaliera"))

# decomposizione settimanale
vendite6_sett.fit<-stl(vendite6_sett_avg,s.window="periodic")
trend.vendite6_sett<-vendite6_sett.fit$time.series[,2]
stag.vendite6_sett<-vendite6_sett.fit$time.series[,1]
res.vendite6_sett<-vendite6_sett.fit$time.series[,3]
print(autoplot(vendite6_sett.fit) + ggtitle("Ristorante 6: Decomposizione settimanale"))

# decomposizione mensile 
vendite6_mens.fit<-stl(vendite6_mens_avg,s.window="periodic")
trend.vendite6_mens<-vendite6_mens.fit$time.series[,2]
stag.vendite6_mens<-vendite6_mens.fit$time.series[,1]
res.vendite6_mens<-vendite6_mens.fit$time.series[,3]
print(autoplot(vendite6_mens.fit) + ggtitle("Ristorante 6: Decomposizione mensile"))

components.ts_6 = decompose(vendite6_mens_avg)
plot(components.ts_6)



### decomposizione pre covid ----
# decomposizione giornaliera 
multi_vendite6_pre <- msts(ristorante1_pre_covid_vendite$vendite, ts.frequency = 365, start=2017, seasonal.periods = c(7,365))
multi_vendite_dec6_pre <- mstl(multi_vendite6_pre, s.window = "periodic")
print(autoplot(multi_vendite_dec6_pre) + ggtitle("Ristorante 6: Decomposizione giornaliera pre covid"))

# decomposizione settimanale
vendite6_sett.fit_pre<-stl(vendite6_sett_avg_pre,s.window="periodic")
trend.vendite6_sett_pre<-vendite6_sett.fit_pre$time.series[,2]
stag.vendite6_sett_pre<-vendite6_sett.fit_pre$time.series[,1]
res.vendite6_sett_pre<-vendite6_sett.fit_pre$time.series[,3]
print(autoplot(vendite6_sett.fit_pre) + ggtitle("Ristorante 6: Decomposizione settimanale pre covid"))

# decomposizione mensile 
vendite6_mens.fit_pre<-stl(vendite6_mens_avg_pre,s.window="periodic")
trend.vendite6_mens_pre<-vendite6_mens.fit_pre$time.series[,2]
stag.vendite6_mens_pre<-vendite6_mens.fit_pre$time.series[,1]
res.vendite6_mens_pre<-vendite6_mens.fit_pre$time.series[,3]
print(autoplot(vendite6_mens.fit_pre) + ggtitle("Ristorante 6: Decomposizione mensile pre covid"))

components.ts_pre_6 = decompose(vendite6_mens_avg_pre)
plot(components.ts_pre_6)


### decomposizione approfondita considerando tutti gli anni----

# mese<-as.factor(month(ymd(ristorante6$data)))
# y=vendite6_day
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
