
#### STUDIO SECONDO RISTORANTE #### 

#### creazione delle serie e dei plot del primo ristorante sulle vendite ####

#### vendite giornaliere primo ristorante ####
vendite2_day <- ts(ristorante2$vendite,start=2017,frequency=365) 

#### vendite settimanali primo ristorante ####
week <- as.Date(cut(ristorante2$data, "week"))
vendite2_sett <- aggregate(vendite ~ week, ristorante2, sum)
vendite2_sett <- vendite2_sett$vendite
vendite2_sett <- ts(vendite2_sett,start=2017,frequency=52) 

#### vendite settimanali medie primo ristorante ####
vendite2_sett_avg <- aggregate(vendite ~ week, ristorante2, mean)
vendite2_sett_avg <- vendite2_sett_avg$vendite
vendite2_sett_avg <- ts(vendite2_sett_avg,start=2017,frequency=52) 

#### vendite mensili primo ristorante ####
month <- as.Date(cut(ristorante2$data, "month"))
vendite2_mens <- aggregate(vendite ~ month, ristorante2, sum)
vendite2_mens <- vendite2_mens$vendite
vendite2_mens <- ts(vendite2_mens,start=2017,frequency=22) 

#### vendite mensili medie  primo ristorante ####
vendite2_mens_avg <- aggregate(vendite ~ month, ristorante2, mean)
vendite2_mens_avg <- vendite2_mens_avg$vendite
vendite2_mens_avg <- ts(vendite2_mens_avg,start=2017,frequency=22) 

#### plot delle diverse serie trovate sopra ####
p21 <- autoplot(vendite2_day) +
  ggtitle("Ristorante 2: vendite giornaliere") +
  xlab("anno") +
  ylab("vendite")

p22<-autoplot(vendite2_sett) +
  ggtitle("Ristorante 2: vendite settimanali") +
  xlab("anno") +
  ylab("vendite")


p23<-autoplot(vendite2_sett_avg) +
  ggtitle("Ristorante 2: vendite medie settimanali") +
  xlab("anno") +
  ylab("vendite")

p24<-autoplot(vendite2_mens) +
  ggtitle("Ristorante 2: vendite mensili") +
  xlab("anno") +
  ylab("vendite")

p25<-autoplot(vendite2_mens_avg) +
  ggtitle("Ristorante 2: vendite medie mensili") +
  xlab("anno") +
  ylab("vendite")

p21+p22+p23+p24+p25

#### grafici stagionalità ####

ps22 <- ggseasonplot(vendite2_sett, year.labels=TRUE, year.labels.left=TRUE) +
  ylab("euro") +
  ggtitle("Seasonal plot: vendite settimanale")

ps23<- ggseasonplot(vendite2_sett_avg, year.labels=TRUE, year.labels.left=TRUE) +
  ylab("euro") +
  ggtitle("Seasonal plot: vendite medie settimanale")

ps24 <- ggseasonplot(vendite2_mens, year.labels=TRUE, year.labels.left=TRUE) +
  ylab("euro") +
  ggtitle("Seasonal plot: vendite mensili")

ps25 <- ggseasonplot(vendite2_mens_avg, year.labels=TRUE, year.labels.left=TRUE) +
  ylab("euro") +
  ggtitle("Seasonal plot: vendite medie mensili")

ps22+ps23+ps24+ps25


#### time plot e stagionalità ####
p22+ps22
p23+ps23
p24+ps24
p25+ps25


#### seasonal sub series plot ####

# ggsubseriesplot(vendite2_mens) +
#   ylab("$ million") +
#   ggtitle("Seasonal subseries plot: vendite mensili")
# 
# ggsubseriesplot(vendite2_mens_avg) +
#   ylab("$ million") +
#   ggtitle("Seasonal subseries plot: vendite medie mensili")




#### correlazione tra vendite e scontrini ####
scontrini_sett_avg <- aggregate(scontrini ~ week, ristorante2, mean)
scontrini_sett_avg <- scontrini_sett_avg$scontrini
scontrini_sett_avg <- ts(scontrini_sett_avg,start=2017,frequency=52) 
sc_ven1_sett_avg <-ts.intersect(vendite2_sett_avg, scontrini_sett_avg)


autoplot(sc_ven1_sett_avg, facets=TRUE) +
  xlab("Anni") + ylab("") +
  ggtitle("Confronto scontrini e vendite")

qplot(vendite, scontrini, data=as.data.frame(ristorante2)) +
  ylab("scontrini") + xlab("vendite")+
  ggtitle("Correlazione scontrini e vendite")


#### autocorrelazione ####
#### per una serie con trend, l'auto correlazione è alta a lag vicini, 
#### e si abbassa piano piano se c'è stagionalità l'autocorrelazione presenta lag ogni tot

#gglagplot(vendite2_mens_avg)

pa21 <- ggAcf(vendite2_day, lag=24) +
  ggtitle("Autocorrelation vendite giornaliere")
pa22 <- ggAcf(vendite2_sett, lag=52)+
  ggtitle("Autocorrelation vendite settimanali")
pa23 <- ggAcf(vendite2_sett_avg, lag=52)+
  ggtitle("Autocorrelation vendite medie settimanali")
pa24 <- ggAcf(vendite2_mens, lag=24)+
  ggtitle("Autocorrelation vendite mensili")
pa25 <- ggAcf(vendite2_mens_avg, lag=24)+
  ggtitle("Autocorrelation vendite medie mensili")

pa21 +pa22 +pa23 +pa24 +pa25 

#### time plot e stagionalità e acf ####
p21+pa21
p22+ps22+pa22
p23+ps23+pa23
p24+ps24+pa24
p25+ps25+pa25

