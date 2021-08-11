## Ristorante3 ----
# si prende in considerazione per le successive analisi il terzo ristorante

### analisi vendite considerando tutti gli anni ----


# si considera la serie storica a partire dai primi dati registrati
reference_date_rist3 <- as.Date("2019-10-16", format = "%Y-%m-%d")

ristorante3 <- ristorante3 %>%
  filter(ristorante3$data >= reference_date_rist3)


# vendite giornaliere secondo ristorante 
vendite3_day <- ts(ristorante3$vendite, start = decimal_date(as.Date("2019-10-16")), frequency=365) 

# vendite settimanali medie secondo ristorante 
week <- as.Date(cut(ristorante3$data, "week"))

vendite3_sett <- aggregate(vendite ~ week, ristorante3, sum)
vendite3_sett <- vendite3_sett$vendite
vendite3_sett <- ts(vendite3_sett,start = decimal_date(as.Date("2019-10-16")),frequency=52) 

vendite3_sett_avg <- aggregate(vendite ~ week, ristorante3, mean)
vendite3_sett_avg <- vendite3_sett_avg$vendite
vendite3_sett_avg <- ts(vendite3_sett_avg,start = decimal_date(as.Date("2019-10-16")),frequency=52) 

# vendite mensili medie  secondo ristorante 
month <- as.Date(cut(ristorante3$data, "month"))

vendite3_mens <- aggregate(vendite ~ month, ristorante3, sum)
vendite3_mens <- vendite3_mens$vendite
vendite3_mens <- ts(vendite3_mens,start = decimal_date(as.Date("2019-10-16")),frequency=12) 

vendite3_mens_avg <- aggregate(vendite ~ month, ristorante3, mean)
vendite3_mens_avg <- vendite3_mens_avg$vendite
vendite3_mens_avg <- ts(vendite3_mens_avg,start = decimal_date(as.Date("2019-10-16")),frequency=12) 

# plot delle diverse serie trovate sopra
print(
  autoplot(vendite3_day) +
    ggtitle("Ristorante 3: vendite giornaliere") +
    xlab("anno") +
    ylab("vendite")
)

print(
  autoplot(vendite3_sett_avg) +
    ggtitle("Ristorante 3: vendite medie settimanali") +
    xlab("anno") +
    ylab("vendite")
)


print(
  autoplot(vendite3_mens_avg) +
    ggtitle("Ristorante 3: vendite medie mensili") +
    xlab("anno") +
    ylab("vendite")
)


### analisi vendite considerando il periodo pre covid ----

# si procede ad analizzare ciascun ristorante nel periodo antecedente il covid-19 
reference_date <- as.Date("2020-03-09", format = "%Y-%m-%d")

# vendite Ristorante 3 pre covid
ristorante3_pre_covid_vendite <- ristorante3 %>%
  filter(ristorante3$data < reference_date) %>%
  select(vendite, data)

# vendite giornaliere secondo ristorante pre covid
vendite3_day_pre <- ts(ristorante3_pre_covid_vendite$vendite,start = decimal_date(as.Date("2019-10-16")),frequency=365) 

# vendite settimanali medie secondo ristorante pre covid
week_pre_covid <- as.Date(cut(ristorante3_pre_covid_vendite$data, "week"))

vendite3_sett_pre <- aggregate(vendite ~ week_pre_covid, ristorante3_pre_covid_vendite, sum)
vendite3_sett_pre <- vendite3_sett_pre$vendite
vendite3_sett_pre <- ts(vendite3_sett_pre,start = decimal_date(as.Date("2019-10-16")),frequency=52) 

vendite3_sett_avg_pre <- aggregate(vendite ~ week_pre_covid, ristorante3_pre_covid_vendite, mean)
vendite3_sett_avg_pre <- vendite3_sett_avg_pre$vendite
vendite3_sett_avg_pre <- ts(vendite3_sett_avg_pre,start = decimal_date(as.Date("2019-10-16")),frequency=52) 

# vendite mensili medie  secondo ristorante pre covid
month_pre_covid <- as.Date(cut(ristorante3_pre_covid_vendite$data, "month"))

vendite3_mens_pre <- aggregate(vendite ~ month_pre_covid, ristorante3_pre_covid_vendite, sum)
vendite3_mens_pre <- vendite3_mens_pre$vendite
vendite3_mens_pre <- ts(vendite3_mens_pre,start = decimal_date(as.Date("2019-10-16")),frequency=12) 

vendite3_mens_avg_pre <- aggregate(vendite ~ month_pre_covid, ristorante3_pre_covid_vendite, mean)
vendite3_mens_avg_pre <- vendite3_mens_avg_pre$vendite
vendite3_mens_avg_pre <- ts(vendite3_mens_avg_pre,start = decimal_date(as.Date("2019-10-16")),frequency=12) 

# plot delle diverse serie pre covid trovate sopra 
print(
  autoplot(vendite3_day_pre) +
    ggtitle("Ristorante 3: vendite giornaliere pre covid") +
    xlab("anno") +
    ylab("vendite")
)

print(
  autoplot(vendite3_sett_avg_pre) +
    ggtitle("Ristorante 3: vendite medie settimanali pre covid") +
    xlab("anno") +
    ylab("vendite")
)

print(
  autoplot(vendite3_mens_avg_pre) +
    ggtitle("Ristorante 3: vendite medie mensili pre covid") +
    xlab("anno") +
    ylab("vendite")
)


### analisi scontrini considerando tutti gli anni ----

# eventualmente da mettere a confronto con l'andamento delle vendite (grafico sopra vendite e sotto scontrini)

# scontrini giornalieri secondo ristorante 
scontrini3_day <- ts(ristorante3$scontrini,start = decimal_date(as.Date("2019-10-16")),frequency=365) 

# scontrini settimanali medie secondo ristorante 
week <- as.Date(cut(ristorante3$data, "week"))

scontrini3_sett <- aggregate(scontrini ~ week, ristorante3, sum)
scontrini3_sett <- scontrini3_sett$scontrini
scontrini3_sett <- ts(scontrini3_sett,start = decimal_date(as.Date("2019-10-16")),frequency=52) 

scontrini3_sett_avg <- aggregate(scontrini ~ week, ristorante3, mean)
scontrini3_sett_avg <- scontrini3_sett_avg$scontrini
scontrini3_sett_avg <- ts(scontrini3_sett_avg,start = decimal_date(as.Date("2019-10-16")),frequency=52) 

# scontrini mensili medie  secondo ristorante 
month <- as.Date(cut(ristorante3$data, "month"))

scontrini3_mens <- aggregate(scontrini ~ month, ristorante3, sum)
scontrini3_mens <- scontrini3_mens$scontrini
scontrini3_mens <- ts(scontrini3_mens,start = decimal_date(as.Date("2019-10-16")),frequency=12) 

scontrini3_mens_avg <- aggregate(scontrini ~ month, ristorante3, mean)
scontrini3_mens_avg <- scontrini3_mens_avg$scontrini
scontrini3_mens_avg <- ts(scontrini3_mens_avg,start = decimal_date(as.Date("2019-10-16")),frequency=12) 

# plot delle diverse serie trovate sopra
print(
  autoplot(scontrini3_day) +
    ggtitle("Ristorante 3: scontrini giornalieri") +
    xlab("anno") +
    ylab("scontrini")
)

print(
  autoplot(scontrini3_sett_avg) +
    ggtitle("Ristorante 3: scontrini medi settimanali") +
    xlab("anno") +
    ylab("scontrini")
)

print(
  autoplot(scontrini3_mens_avg) +
    ggtitle("Ristorante 3: scontrini medi mensili") +
    xlab("anno") +
    ylab("scontrini")
)

### analisi scontrini considerando il periodo pre covid ----

# si procede ad analizzare ciascun ristorante nel periodo antecedente il covid-19 
reference_date <- as.Date("2020-03-09", format = "%Y-%m-%d")

# scontrini Ristorante 3 pre covid
ristorante3_pre_covid_scontrini <- ristorante3 %>%
  filter(ristorante3$data < reference_date) %>%
  select(scontrini, data)

# scontrini giornalieri secondo ristorante pre covid
scontrini3_day_pre <- ts(ristorante3_pre_covid_scontrini$scontrini,start = decimal_date(as.Date("2019-10-16")),frequency=365) 

# scontrini settimanali medi secondo ristorante pre covid
week_pre_covid <- as.Date(cut(ristorante3_pre_covid_scontrini$data, "week"))

scontrini3_sett_avg_pre <- aggregate(scontrini ~ week_pre_covid, ristorante3_pre_covid_scontrini, mean)
scontrini3_sett_avg_pre <- scontrini3_sett_avg_pre$scontrini
scontrini3_sett_avg_pre <- ts(scontrini3_sett_avg_pre,start = decimal_date(as.Date("2019-10-16")),frequency=52) 

# scontrini mensili medi  secondo ristorante pre covid
month_pre_covid <- as.Date(cut(ristorante3_pre_covid_scontrini$data, "month"))
scontrini3_mens_avg_pre <- aggregate(scontrini ~ month_pre_covid, ristorante3_pre_covid_scontrini, mean)
scontrini3_mens_avg_pre <- scontrini3_mens_avg_pre$scontrini
scontrini3_mens_avg_pre <- ts(scontrini3_mens_avg_pre,start = decimal_date(as.Date("2019-10-16")),frequency=12) 

# plot delle diverse serie pre covid trovate sopra 
print(
  autoplot(scontrini3_day_pre) +
    ggtitle("Ristorante 3: scontrini giornalieri pre covid") +
    xlab("anno") +
    ylab("scontrini")
)

print(
  autoplot(scontrini3_sett_avg_pre) +
    ggtitle("Ristorante 3: scontrini medi settimanali pre covid") +
    xlab("anno") +
    ylab("scontrini")
)

print(
  autoplot(scontrini3_mens_avg_pre) +
    ggtitle("Ristorante 3: scontrini medi mensili pre covid") +
    xlab("anno") +
    ylab("scontrini")
)



### analisi stagionalità considerando tutti gli anni ----
print(
  ggseasonplot(vendite3_sett, year.labels=TRUE, year.labels.left=TRUE) +
    ylab("euro") +
    ggtitle("Seasonal plot Ristorante 3: vendite settimanale")
)

print(
  ggseasonplot(vendite3_mens, year.labels=TRUE, year.labels.left=TRUE) +
    ylab("euro") +
    ggtitle("Seasonal plot Ristorante 3: vendite mensili")
)

### seasonal sub series plot
print(
  ggsubseriesplot(vendite3_mens_avg) +
    ylab("$ million") +
    ggtitle("Seasonal subseries plot Ristorante 3: vendite medie mensili")
)


### analisi stagionalità considerando il periodo pre covid ----
print(
  ggseasonplot(vendite3_sett_pre, year.labels=TRUE, year.labels.left=TRUE) +
    ylab("euro") +
    ggtitle("Seasonal plot Ristorante 3: vendite settimanale pre covid")
)

print(
  ggseasonplot(vendite3_mens_pre, year.labels=TRUE, year.labels.left=TRUE) +
    ylab("euro") +
    ggtitle("Seasonal plot Ristorante 3: vendite mensili pre covid")
)

### seasonal sub series plot
print(
  ggsubseriesplot(vendite3_mens_avg_pre) +
    ylab("$ million") +
    ggtitle("Seasonal subseries plot Ristorante 3: vendite medie mensili pre covid")
)


### analisi correlazione tra vendite e scontrini ----
scontrini_sett_avg3 <- aggregate(scontrini ~ week, ristorante3, mean)
scontrini_sett_avg3 <- scontrini_sett_avg3$scontrini
scontrini_sett_avg3 <- ts(scontrini_sett_avg3,start = decimal_date(as.Date("2019-10-16")),frequency=52) 
sc_ven3_sett_avg <-ts.intersect(vendite3_sett_avg, scontrini_sett_avg3)


print(
  autoplot(sc_ven3_sett_avg, facets=TRUE) +
    xlab("Anni") + ylab("") +
    ggtitle("Confronto scontrini e vendite Ristorante 3")
)

print(
  qplot(vendite, scontrini, data=as.data.frame(ristorante3)) +
    ylab("scontrini") + xlab("vendite")+
    ggtitle("Correlazione scontrini e vendite Ristorante 3")
)



### analisi autocorrelazione considerando tutti gli anni ----
# per una serie con trend, l'autocorrelazione è alta a lag vicini, 
# e si abbassa piano piano se c'è stagionalità l'autocorrelazione presenta lag ogni tot

#gglagplot(vendite3_mens_avg)

plot(
  ggAcf(vendite3_day, lag=7) +
    ggtitle("Ristorante 3: Autocorrelation vendite giornaliere")
)

plot(
  ggAcf(vendite3_sett_avg, lag=52)+
    ggtitle("Ristorante 3: Autocorrelation vendite medie settimanali")
)

plot(
  ggAcf(vendite3_mens_avg, lag=24)+
    ggtitle("Ristorante 3: Autocorrelation vendite medie mensili")
)


### analisi autocorrelazione pre covid ----

plot(
  ggAcf(vendite3_day_pre, lag=7) +
    ggtitle("Ristorante 3: Autocorrelation vendite giornaliere pre covid")
)

plot(
  ggAcf(vendite3_sett_avg_pre, lag=52)+
    ggtitle("Ristorante 3: Autocorrelation vendite medie settimanali pre covid")
)

plot(
  ggAcf(vendite3_mens_avg_pre, lag=24)+
    ggtitle("Ristorante 3: Autocorrelation vendite medie mensili pre covid")
)



### decomposizione considerando tutti gli anni ----
# decomposizione giornaliera 
vendite3_day.fit<-stl(vendite3_day,s.window="periodic")
trend.vendite3_day<-vendite3_day.fit$time.series[,2]
stag.vendite3_day<-vendite3_day.fit$time.series[,1]
res.vendite3_day<-vendite3_day.fit$time.series[,3]
print(autoplot(vendite3_day.fit) + ggtitle("Ristorante 3: Decomposizione giornaliera"))

# vendite3_day %>% stl(s.window="periodic") %>% autoplot() + xlab("Time")
# vendite3_day %>% mstl() %>% autoplot() + xlab("Time")

# decomposizione settimanale
vendite3_sett.fit<-stl(vendite3_sett_avg,s.window="periodic")
trend.vendite3_sett<-vendite3_sett.fit$time.series[,2]
stag.vendite3_sett<-vendite3_sett.fit$time.series[,1]
res.vendite3_sett<-vendite3_sett.fit$time.series[,3]
print(autoplot(vendite3_sett.fit) + ggtitle("Ristorante 3: Decomposizione settimanale"))

# decomposizione mensile 
vendite3_mens.fit<-stl(vendite3_mens_avg,s.window="periodic")
trend.vendite3_mens<-vendite3_mens.fit$time.series[,2]
stag.vendite3_mens<-vendite3_mens.fit$time.series[,1]
res.vendite3_mens<-vendite3_mens.fit$time.series[,3]
print(autoplot(vendite3_mens.fit) + ggtitle("Ristorante 3: Decomposizione mensile"))

components.ts = decompose(vendite3_mens_avg)
plot(components.ts)



### decomposizione pre covid ----
# decomposizione giornaliera 
vendite3_day_pre.fit<-stl(vendite3_day_pre,s.window="periodic")
trend.vendite3_day_pre<-vendite3_day_pre.fit$time.series[,2]
stag.vendite3_day_pre<-vendite3_day_pre.fit$time.series[,1]
res.vendite3_day_pre<-vendite3_day_pre.fit$time.series[,3]
print(autoplot(vendite3_day_pre.fit) + ggtitle("Ristorante 3: Decomposizione giornaliera pre covid"))

# decomposizione settimanale
vendite3_sett.fit_pre<-stl(vendite3_sett_avg_pre,s.window="periodic")
trend.vendite3_sett_pre<-vendite3_sett.fit_pre$time.series[,2]
stag.vendite3_sett_pre<-vendite3_sett.fit_pre$time.series[,1]
res.vendite3_sett_pre<-vendite3_sett.fit_pre$time.series[,3]
print(autoplot(vendite3_sett.fit_pre) + ggtitle("Ristorante 3: Decomposizione settimanale pre covid"))

# decomposizione mensile 
vendite3_mens.fit_pre<-stl(vendite3_mens_avg_pre,s.window="periodic")
trend.vendite3_mens_pre<-vendite3_mens.fit_pre$time.series[,2]
stag.vendite3_mens_pre<-vendite3_mens.fit_pre$time.series[,1]
res.vendite3_mens_pre<-vendite3_mens.fit_pre$time.series[,3]
print(autoplot(vendite3_mens.fit_pre) + ggtitle("Ristorante 3: Decomposizione mensile pre covid"))

components.ts_pre = decompose(vendite3_mens_avg_pre)
plot(components.ts_pre)


### decomposizione approfondita considerando tutti gli anni----

# mese<-as.factor(month(ymd(ristorante3$data)))
# y=vendite3_day
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