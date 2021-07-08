
#### STUDIO TERZO RISTORANTE #### 

#### creazione delle serie e dei plot del primo ristorante sulle vendite ####

#### vendite giornaliere primo ristorante ####
vendite3_day <- ts(ristorante3$vendite,start=2017,frequency=365) 

#### vendite settimanali primo ristorante ####
week <- as.Date(cut(ristorante3$data, "week"))
vendite3_sett <- aggregate(vendite ~ week, ristorante3, sum)
vendite3_sett <- vendite3_sett$vendite
vendite3_sett <- ts(vendite3_sett,start=2017,frequency=52) 

#### vendite settimanali medie primo ristorante ####
vendite3_sett_avg <- aggregate(vendite ~ week, ristorante3, mean)
vendite3_sett_avg <- vendite3_sett_avg$vendite
vendite3_sett_avg <- ts(vendite3_sett_avg,start=2017,frequency=52) 

#### vendite mensili primo ristorante ####
month <- as.Date(cut(ristorante3$data, "month"))
vendite3_mens <- aggregate(vendite ~ month, ristorante3, sum)
vendite3_mens <- vendite3_mens$vendite
vendite3_mens <- ts(vendite3_mens,start=2017,frequency=32) 

#### vendite mensili medie  primo ristorante ####
vendite3_mens_avg <- aggregate(vendite ~ month, ristorante3, mean)
vendite3_mens_avg <- vendite3_mens_avg$vendite
vendite3_mens_avg <- ts(vendite3_mens_avg,start=2017,frequency=32) 

#### plot delle diverse serie trovate sopra ####
p31 <- autoplot(vendite3_day) +
  ggtitle("Ristorante 3: vendite giornaliere") +
  xlab("anno") +
  ylab("vendite")

p32<-autoplot(vendite3_sett) +
  ggtitle("Ristorante 3: vendite settimanali") +
  xlab("anno") +
  ylab("vendite")


p33<-autoplot(vendite3_sett_avg) +
  ggtitle("Ristorante 3: vendite medie settimanali") +
  xlab("anno") +
  ylab("vendite")

p34<-autoplot(vendite3_mens) +
  ggtitle("Ristorante 3: vendite mensili") +
  xlab("anno") +
  ylab("vendite")

p35<-autoplot(vendite3_mens_avg) +
  ggtitle("Ristorante 3: vendite medie mensili") +
  xlab("anno") +
  ylab("vendite")

p31+p32+p33+p34+p35

#### grafici stagionalità ####

ps32 <- ggseasonplot(vendite3_sett, year.labels=TRUE, year.labels.left=TRUE) +
  ylab("euro") +
  ggtitle("Seasonal plot: vendite settimanale")

ps33<- ggseasonplot(vendite3_sett_avg, year.labels=TRUE, year.labels.left=TRUE) +
  ylab("euro") +
  ggtitle("Seasonal plot: vendite medie settimanale")

ps34 <- ggseasonplot(vendite3_mens, year.labels=TRUE, year.labels.left=TRUE) +
  ylab("euro") +
  ggtitle("Seasonal plot: vendite mensili")

ps35 <- ggseasonplot(vendite3_mens_avg, year.labels=TRUE, year.labels.left=TRUE) +
  ylab("euro") +
  ggtitle("Seasonal plot: vendite medie mensili")

ps32+ps33+ps34+ps35


#### time plot e stagionalità ####
p32+ps32
p33+ps33
p34+ps34
p35+ps35


#### seasonal sub series plot ####

# ggsubseriesplot(vendite3_mens) +
#   ylab("$ million") +
#   ggtitle("Seasonal subseries plot: vendite mensili")
# 
# ggsubseriesplot(vendite3_mens_avg) +
#   ylab("$ million") +
#   ggtitle("Seasonal subseries plot: vendite medie mensili")




#### correlazione tra vendite e scontrini ####
scontrini_sett_avg <- aggregate(scontrini ~ week, ristorante3, mean)
scontrini_sett_avg <- scontrini_sett_avg$scontrini
scontrini_sett_avg <- ts(scontrini_sett_avg,start=2017,frequency=52) 
sc_ven3_sett_avg <-ts.intersect(vendite3_sett_avg, scontrini_sett_avg)


autoplot(sc_ven3_sett_avg, facets=TRUE) +
  xlab("Anni") + ylab("") +
  ggtitle("Confronto scontrini e vendite")

qplot(vendite, scontrini, data=as.data.frame(ristorante3)) +
  ylab("scontrini") + xlab("vendite")+
  ggtitle("Correlazione scontrini e vendite")


#### autocorrelazione ####
#### per una serie con trend, l'auto correlazione è alta a lag vicini, 
#### e si abbassa piano piano se c'è stagionalità l'autocorrelazione presenta lag ogni tot

#gglagplot(vendite3_mens_avg)

pa31 <- ggAcf(vendite3_day, lag=34) +
  ggtitle("Autocorrelation vendite giornaliere")
pa32 <- ggAcf(vendite3_sett, lag=52)+
  ggtitle("Autocorrelation vendite settimanali")
pa33 <- ggAcf(vendite3_sett_avg, lag=52)+
  ggtitle("Autocorrelation vendite medie settimanali")
pa34 <- ggAcf(vendite3_mens, lag=34)+
  ggtitle("Autocorrelation vendite mensili")
pa35 <- ggAcf(vendite3_mens_avg, lag=34)+
  ggtitle("Autocorrelation vendite medie mensili")

pa31 +pa32 +pa33 +pa34 +pa35 

#### time plot e stagionalità e acf ####
p31+pa31
p32+ps32+pa32
p33+ps33+pa33
p34+ps34+pa34
p35+ps35+pa35