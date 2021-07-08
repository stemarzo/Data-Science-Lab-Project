#### ESPLORAZIONE #### 

#### STUDIO PRIMO RISTORANTE #### 

#### creazione delle serie e dei plot del primo ristorante sulle vendite ####

#### vendite giornaliere primo ristorante ####
vendite1_day <- ts(ristorante1$vendite,start=2017,frequency=365) 

#### vendite settimanali primo ristorante ####
week <- as.Date(cut(ristorante1$data, "week"))
vendite1_sett <- aggregate(vendite ~ week, ristorante1, sum)
vendite1_sett <- vendite1_sett$vendite
vendite1_sett <- ts(vendite1_sett,start=2017,frequency=52) 

#### vendite settimanali medie primo ristorante ####
vendite1_sett_avg <- aggregate(vendite ~ week, ristorante1, mean)
vendite1_sett_avg <- vendite1_sett_avg$vendite
vendite1_sett_avg <- ts(vendite1_sett_avg,start=2017,frequency=52) 

#### vendite mensili primo ristorante ####
month <- as.Date(cut(ristorante1$data, "month"))
vendite1_mens <- aggregate(vendite ~ month, ristorante1, sum)
vendite1_mens <- vendite1_mens$vendite
vendite1_mens <- ts(vendite1_mens,start=2017,frequency=12) 

#### vendite mensili medie  primo ristorante ####
vendite1_mens_avg <- aggregate(vendite ~ month, ristorante1, mean)
vendite1_mens_avg <- vendite1_mens_avg$vendite
vendite1_mens_avg <- ts(vendite1_mens_avg,start=2017,frequency=12) 

#### plot delle diverse serie trovate sopra ####
p11 <- autoplot(vendite1_day) +
  ggtitle("Ristorante 1: vendite giornaliere") +
  xlab("anno") +
  ylab("vendite")

p12<-autoplot(vendite1_sett) +
  ggtitle("Ristorante 1: vendite settimanali") +
  xlab("anno") +
  ylab("vendite")


p13<-autoplot(vendite1_sett_avg) +
  ggtitle("Ristorante 1: vendite medie settimanali") +
  xlab("anno") +
  ylab("vendite")

p14<-autoplot(vendite1_mens) +
  ggtitle("Ristorante 1: vendite mensili") +
  xlab("anno") +
  ylab("vendite")

p15<-autoplot(vendite1_mens_avg) +
  ggtitle("Ristorante 1: vendite medie mensili") +
  xlab("anno") +
  ylab("vendite")

print(p11+p12+p13+p14+p15)

#### grafici stagionalità ####

ps12 <- ggseasonplot(vendite1_sett, year.labels=TRUE, year.labels.left=TRUE) +
  ylab("euro") +
  ggtitle("Seasonal plot: vendite settimanale")

ps13<- ggseasonplot(vendite1_sett_avg, year.labels=TRUE, year.labels.left=TRUE) +
  ylab("euro") +
  ggtitle("Seasonal plot: vendite medie settimanale")

ps14 <- ggseasonplot(vendite1_mens, year.labels=TRUE, year.labels.left=TRUE) +
  ylab("euro") +
  ggtitle("Seasonal plot: vendite mensili")

ps15 <- ggseasonplot(vendite1_mens_avg, year.labels=TRUE, year.labels.left=TRUE) +
  ylab("euro") +
  ggtitle("Seasonal plot: vendite medie mensili")

print(ps12+ps13+ps14+ps15)


#### time plot e stagionalità ####
print(p12+ps12)
print(p13+ps13)
print(p14+ps14)
print(p15+ps15)


#### seasonal sub series plot ####

# ggsubseriesplot(vendite1_mens) +
#   ylab("$ million") +
#   ggtitle("Seasonal subseries plot: vendite mensili")
# 
# ggsubseriesplot(vendite1_mens_avg) +
#   ylab("$ million") +
#   ggtitle("Seasonal subseries plot: vendite medie mensili")




#### correlazione tra vendite e scontrini ####
scontrini_sett_avg <- aggregate(scontrini ~ week, ristorante1, mean)
scontrini_sett_avg <- scontrini_sett_avg$scontrini
scontrini_sett_avg <- ts(scontrini_sett_avg,start=2017,frequency=52) 
sc_ven1_sett_avg <-ts.intersect(vendite1_sett_avg, scontrini_sett_avg)


print(autoplot(sc_ven1_sett_avg, facets=TRUE) +
  xlab("Anni") + ylab("") +
  ggtitle("Confronto scontrini e vendite"))

print(qplot(vendite, scontrini, data=as.data.frame(ristorante1)) +
  ylab("scontrini") + xlab("vendite")+
  ggtitle("Correlazione scontrini e vendite"))


#### autocorrelazione ####
#### per una serie con trend, l'auto correlazione è alta a lag vicini, 
#### e si abbassa piano piano se c'è stagionalità l'autocorrelazione presenta lag ogni tot

#gglagplot(vendite1_mens_avg)

pa11 <- ggAcf(vendite1_day, lag=7) +
  ggtitle("Autocorrelation vendite giornaliere")

pa12 <- ggAcf(vendite1_sett , lag=52)+
  ggtitle("Autocorrelation vendite settimanali")
pa13 <- ggAcf(vendite1_sett_avg, lag=52)+
  ggtitle("Autocorrelation vendite medie settimanali")
pa14 <- ggAcf(vendite1_mens, lag=24)+
  ggtitle("Autocorrelation vendite mensili")
pa15 <- ggAcf(vendite1_mens_avg, lag=24)+
  ggtitle("Autocorrelation vendite medie mensili")

print(pa11 +pa12 +pa13 +pa14 +pa15)

#### time plot e stagionalità e acf ####
print(p11+pa11)
print(p12+ps12+pa12)
print(p13+ps13+pa13)
print(p14+ps14+pa14)
print(p15+ps15+pa15)




