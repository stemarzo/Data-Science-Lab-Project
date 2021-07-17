# setting librerie
library(readxl)
library(readr)
library(forecast)
library(dplyr)
library(magrittr)
library(ggplot2)
library(forcats)
library(lubridate)
library(RQuantLib)
library(devtools)
library(dplyr)
library(patchwork)

ristorazione_original <- read_excel("C:/Users/Stefano/Documents/progetto_dslab/dati/Ristorazione.xls")

#### SISTEMAZIONE DATASET & AGGIUNTA NUOVE FEATURES ####

# sistemazione dataset
ristorazione <- ristorazione_original # lavoro su una copia del dataset originale
ristorazione<-ristorazione[-c(1),-c(2,3,4,5,7,10)]  # header e colonne prive di dati
colnames(ristorazione)[1]<- "data"
colnames(ristorazione)[2]<- "data_anno_prec"
colnames(ristorazione)[3]<- "vendite1"
colnames(ristorazione)[4]<- "scontrini1"
colnames(ristorazione)[5]<- "vendite2"
colnames(ristorazione)[6]<- "scontrini2"
colnames(ristorazione)[7]<- "vendite3"
colnames(ristorazione)[8]<- "scontrini3"
colnames(ristorazione)[9]<- "vendite4"
colnames(ristorazione)[10]<- "scontrini4"
colnames(ristorazione)[11]<- "vendite5"
colnames(ristorazione)[12]<- "scontrini5"
colnames(ristorazione)[13]<- "vendite6"
colnames(ristorazione)[14]<- "scontrini6"

# coversione da char a numeric delle colonne vendite e scontrini
ristorazione[3:14] <- lapply(ristorazione[3:14], as.numeric)

# sistemazione date
ristorazione$data <- substr(ristorazione$data, 3, 100)
ristorazione$data <- parse_date(ristorazione$data, "%d %b %Y", locale = locale("it"))
ristorazione$data_anno_prec <- parse_date(ristorazione$data_anno_prec, "%d %b %Y", locale = locale("it"))
#ristorazione["giorno_settimana"] <- format(as.Date(ristorazione$data), "%a")  # ricavo il giorno_settimana della settimana



#### CREAZIONE DF PER CIASCUN RISTORANTE 

# creo un dataframe per ciascun ristorante
col_date <- subset(ristorazione, select = c(1, 2))
col_nomi <- c("data", "data_anno_prec", "vendite", "scontrini")

# col_date <- subset(ristorazione, select = c(1, 2, 3, 4, 5)) # data, data_anno_prec, giorno_settimana, uguali per ogni ristorante
# col_nomi <- c("data", "data_anno_prec", "giorno_settimana", "mese", "stagione", "vendite", "scontrini")


#### CREAZIONE RISTORANTE ####

#ristorante1
ristorante1 <- data.frame(col_date, ristorazione$vendite1, ristorazione$scontrini1)
colnames(ristorante1) <- col_nomi
ristorante1$vendite[is.na(ristorante1$vendite)] <- 0  # sostiuisco i valori NA con 0
ristorante1$rapprto_v_s <- ristorante1$vendite/ristorante1$scontrini

#ristorante2
ristorante2 <- data.frame(col_date, ristorazione$vendite2, ristorazione$scontrini2)
colnames(ristorante2) <- col_nomi
ristorante2$vendite[is.na(ristorante2$vendite)] <- 0 
ristorante2$rapprto_v_s <- ristorante2$vendite/ristorante2$scontrini

#ristorante3
ristorante3 <- data.frame(col_date, ristorazione$vendite3, ristorazione$scontrini3)
colnames(ristorante3) <- col_nomi
ristorante3$vendite[is.na(ristorante3$vendite)] <- 0 
ristorante3$rapprto_v_s <- ristorante3$vendite/ristorante3$scontrini

#ristorante4
ristorante4 <- data.frame(col_date,ristorazione$vendite4, ristorazione$scontrini4)
colnames(ristorante4) <- col_nomi
ristorante4$vendite[is.na(ristorante4$vendite)] <- 0  
ristorante4$rapprto_v_s <- ristorante4$vendite/ristorante4$scontrini

#ristorante5
ristorante5 <- data.frame(col_date, ristorazione$vendite5, ristorazione$scontrini5)
colnames(ristorante5) <- col_nomi
ristorante5$vendite[is.na(ristorante5$vendite)] <- 0 
ristorante5$rapprto_v_s <- ristorante5$vendite/ristorante5$scontrini

#ristorante6
ristorante6 <- data.frame(col_date, ristorazione$vendite6, ristorazione$scontrini6)
colnames(ristorante6) <- col_nomi
ristorante6$vendite[is.na(ristorante6$vendite)] <- 0  
ristorante6$rapprto_v_s <- ristorante6$vendite/ristorante6$scontrini

#### ristoranti pre covid 

#ristorante1
ristorante1_pre <- ristorante1[1:1095,]

#ristorante2
ristorante2_pre <- ristorante2[1:1095,]

#ristorante3
ristorante3_pre <- ristorante3[1:1095,]

#ristorante4
ristorante4_pre <- ristorante4[1:1095,]

#ristorante5
ristorante5_pre <- ristorante5[1:1095,]

#ristorante6
ristorante6_pre <- ristorante6[1:1095,]


# rimuovo le variabili che mi sono servite nella fase sopra
rm(list = c('col_date','col_nomi'))




#### ESPLORAZIONE 1 #### 

### creazione delle serie e dei plot del primo ristorante sulle vendite 

### vendite giornaliere primo ristorante 
vendite1_day <- ts(ristorante1$vendite,start=2017,frequency=365) 

### vendite settimanali primo ristorante 
week <- as.Date(cut(ristorante1$data, "week"))
vendite1_sett <- aggregate(vendite ~ week, ristorante1, sum)
vendite1_sett <- vendite1_sett$vendite
vendite1_sett <- ts(vendite1_sett,start=2017,frequency=52) 

### vendite settimanali medie primo ristorante 
vendite1_sett_avg <- aggregate(vendite ~ week, ristorante1, mean)
vendite1_sett_avg <- vendite1_sett_avg$vendite
vendite1_sett_avg <- ts(vendite1_sett_avg,start=2017,frequency=52) 

### vendite mensili primo ristorante 
month <- as.Date(cut(ristorante1$data, "month"))
vendite1_mens <- aggregate(vendite ~ month, ristorante1, sum)
vendite1_mens <- vendite1_mens$vendite
vendite1_mens <- ts(vendite1_mens,start=2017,frequency=12) 

### vendite mensili medie  primo ristorante 
vendite1_mens_avg <- aggregate(vendite ~ month, ristorante1, mean)
vendite1_mens_avg <- vendite1_mens_avg$vendite
vendite1_mens_avg <- ts(vendite1_mens_avg,start=2017,frequency=12) 

### plot delle diverse serie trovate sopra 
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

### grafici stagionalità

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


### time plot e stagionalità
print(p12+ps12)
print(p13+ps13)
print(p14+ps14)
print(p15+ps15)


### seasonal sub series plot

# ggsubseriesplot(vendite1_mens) +
#   ylab("$ million") +
#   ggtitle("Seasonal subseries plot: vendite mensili")
# 
# ggsubseriesplot(vendite1_mens_avg) +
#   ylab("$ million") +
#   ggtitle("Seasonal subseries plot: vendite medie mensili")




### correlazione tra vendite e scontrini 
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


### autocorrelazione 
### per una serie con trend, l'auto correlazione è alta a lag vicini, 
### e si abbassa piano piano se c'è stagionalità l'autocorrelazione presenta lag ogni tot

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

### time plot e stagionalità e acf 
print(p11+pa11)
print(p12+ps12+pa12)
print(p13+ps13+pa13)
print(p14+ps14+pa14)
print(p15+ps15+pa15)






#### DECOMPOSIZIONE DELLA PRIMA SERIE ####
library(KFAS)

# studio la serie giornaliera, creo praticamente un modello (tipo quando facevamo la reg lin) formato però da trend, stagionalità 
# e H sarebbero i residui

# creo una variabile mese per catturare anche la stagionalità mensile, che poi andrò ad aggiungere al modello come regressore
# per catturare la stag mensile all'interno di questa variabile
mese<-as.factor(month(ymd(ristorante1$data)))
y=vendite1_day

# creo il modello con i regressori che ho detto prima 
mod3<-SSModel(y ~ mese+SSMtrend(degree = 2, Q=list(NA,NA))+SSMseasonal(period = 7, sea.type = "dummy", Q=NA), H=NA)

# faccio fittare il modello creato ai miei dati, quindi scelgo i "parametri", cioè tutti quelli che nel modello sono NA
# che permettono di "ricreare meglio" i miei dati, penso sia una roba iterativa che parte da 0 0 0 0 e sceglie i migliori
fit3<-fitSSM(mod3,inits = c(0,0,0,0))
fit3$optim.out$convergence # se è zero il modello converge

# faccio smooting, cioè liscio un pelo i cambiamenti del mio modello, non so come dirlo meglio a parole (smussa?)
smo3 <- KFS(fit3$model, smoothing = c("state", "disturbance", "mean"))
smo3$alphahat #alphahat non so bene cosa voglia dire, ma qui ci sono tutti i valori dei parametri del modello


## ci sono 11 variabili mese, il valore è quello della stagionalità mensile, ovvero gennaio ha valore 0, mese 2 = febbraio
## ed è -150.8482 da gennaio, mentre marzo è -1171.085 da gennaio etc etc....
## level è il trend, slope fa sempre parte del trend e se sommi level e slope trovi il level successivo
## sea dummy catturano la stagionalità settimanale, infatti sono 6 una in meno dei giorni della settimana.
## qui però bisogna vederne solo una perchè le altre sono la prima shiftata di uno, questa si somma al trend per avere
## il valore, sesi prendono i primi 7 valori della colonna "sea_dummy1" si notano che quelli col meno sono i giorni in 
## settimana dove si vende meno, poi ci sono tre giorni positivi che è il weekend


# Time Series:
#   Start = c(2017, 1) 
# End = c(2021, 103) 
# Frequency = 365 
#             mese2     mese3     mese4     mese5    mese6     mese7     mese8    mese9    mese10    mese11   mese12
# 2017.000 -150.8482 -1171.085 -979.0524 -810.0454 -635.698 -309.6313 -380.2435 130.9629 -552.7485 -724.7769 475.5783
# 2017.003 -150.8482 -1171.085 -979.0524 -810.0454 -635.698 -309.6313 -380.2435 130.9629 -552.7485 -724.7769 475.5783
# 2017.005 -150.8482 -1171.085 -979.0524 -810.0454 -635.698 -309.6313 -380.2435 130.9629 -552.7485 -724.7769 475.5783
# 2017.008 -150.8482 -1171.085 -979.0524 -810.0454 -635.698 -309.6313 -380.2435 130.9629 -552.7485 -724.7769 475.5783
# 2017.011 -150.8482 -1171.085 -979.0524 -810.0454 -635.698 -309.6313 -380.2435 130.9629 -552.7485 -724.7769 475.5783
# 2017.014 -150.8482 -1171.085 -979.0524 -810.0454 -635.698 -309.6313 -380.2435 130.9629 -552.7485 -724.7769 475.5783
# 
#           level        slope    sea_dummy1    sea_dummy2    sea_dummy3    sea_dummy4    sea_dummy5    sea_dummy6
# 2017.000 6532.040  -7.89220939  1536.2010523  4330.4983948    10.7874764 -1418.1567900 -1008.2934784 -1715.4931959
# 2017.003 6524.148  -7.89317431 -1735.5434591  1536.2010523  4330.4983948    10.7874764 -1418.1567900 -1008.2934784
# 2017.005 6516.251  -7.89029495 -1715.4931959 -1735.5434591  1536.2010523  4330.4983948    10.7874764 -1418.1567900
# 2017.008 6508.352  -7.87989614 -1008.2934784 -1715.4931959 -1735.5434591  1536.2010523  4330.4983948    10.7874764
# 2017.011 6500.460  -7.85956861 -1418.1567900 -1008.2934784 -1715.4931959 -1735.5434591  1536.2010523  4330.4983948
# 2017.014 6492.581  -7.82195140    10.7874764 -1418.1567900 -1008.2934784 -1715.4931959 -1735.5434591  1536.2010523
# 
# 
# [ reached getOption("max.print") -- omitted 1511 rows ]



## questo è il nostro trend in rosso
plot(smo3$alphahat[,12], col=2,ylim=c(0,12000))
lines(y)

## questi sono i residui nelle varie componenti che abbiamo riscontrato nel modello, essendo distribuiti come normali, 
## ci dobbiamo preoccupare se gi errori sono sopra o sotto 3 e -3, nel caso del periodo covid si poteva vedere anche nel 
## grafico prima che il trend si abbassava, ma di poco, perchè tende a smussare tutto. vediamo infatti nei
## residui del level (il trend) che i valori vanno fino a -10 e +10, male male, ma no prob fra :-)
auxres_ls <- rstandard(smo3, "state")
plot(auxres_ls)

## inseriamo nel modello principale un altro regressore, che è la data di inizio del covid, creiamo una time series booleana,
## tutta zero fino all'inizio delle chiusure, in questo caso, ho messo come inizio degli 1 il valore con errore più grande del
## grafico precedente, ma sarebbe meglio mettere la data della chiusra, dopo cerco di recuperarla
ndx<-which.min(auxres_ls[,1])
step <- y
step[] <- 0
step[(ndx+1):length(y)] <- 1
step ## questa è la serie storica booleana


## ricreo il modello con anche la componente step, e rifaccio tutto come prima...
mod3<-SSModel(y ~mese+step+SSMtrend(degree = 2, Q=list(NA,NA))+SSMseasonal(period = 7, sea.type = "dummy", Q=NA), H=NA)
fit3<-fitSSM(mod3,inits = c(0,0,0,0))
fit3$optim.out$convergence

smo3 <- KFS(fit3$model, smoothing = c("state", "disturbance", "mean"))
smo3$alphahat


#             step    level        slope   sea_dummy1   sea_dummy2   sea_dummy3   sea_dummy4   sea_dummy5   sea_dummy6
# 2017.000 -5156.508 6609.027  -7.06863082  1493.200336  4519.666128    61.940610 -1419.635993 -1062.635741 -1804.473662
# 2017.003 -5156.508 6601.959  -7.06909024 -1788.061678  1493.200336  4519.666128    61.940610 -1419.635993 -1062.635741
# 2017.005 -5156.508 6594.887  -7.06790757 -1804.473662 -1788.061678  1493.200336  4519.666128    61.940610 -1419.635993
# 2017.008 -5156.508 6587.814  -7.06344815 -1062.635741 -1804.473662 -1788.061678  1493.200336  4519.666128    61.940610
# 2017.011 -5156.508 6580.744  -7.05467124 -1419.635993 -1062.635741 -1804.473662 -1788.061678  1493.200336  4519.666128
# 2017.014 -5156.508 6573.679  -7.03839902    61.940610 -1419.635993 -1062.635741 -1804.473662 -1788.061678  1493.200336
# 2017.016 -5156.508 6566.625  -7.01184471  4509.300062    61.940610 -1419.635993 -1062.635741 -1804.473662 -1788.061678


## in qesto caso le variabili sono le stesse del caso precedente, ma abbiamo in più step, che sarebbe da queando sono state 
## chiuse le attività, questa ci dice di quando si è abbassato il trend, il trend a causa delle chiusure ha avuto un 
## abbassamento di 5156.508 (questo potrebbe essere carino da dire)


plot(step*smo3$alphahat[,12]+smo3$alphahat[,13], col=2,ylim=c(0,12000))
lines(y)

auxres_ls <- rstandard(smo3, "state")
plot(auxres_ls) #adesso l'errore peggiore è con le riaperture, allora devo catturare anche quello con un'altro step.1

ndx.1<-which.max(auxres_ls[,1])
step.1 <- y
step.1[] <- 0
step.1[(ndx.1+1):length(y)] <- 1
step.1

mod3<-SSModel(y ~mese+step+step.1+SSMtrend(degree = 2, Q=list(NA,NA))+SSMseasonal(period = 7, sea.type = "dummy", Q=NA), H=NA)
fit3<-fitSSM(mod3,inits = c(0,0,0,0))
fit3$optim.out$convergence

smo3 <- KFS(fit3$model, smoothing = c("state", "disturbance", "mean"))
smo3$alphahat


plot(step.1*smo3$alphahat[,13]+step*smo3$alphahat[,12]+smo3$alphahat[,14], col=2,ylim=c(0,12000))
lines(y)

auxres_ls <- rstandard(smo3, "state")
plot(auxres_ls)

## ci sarebbero altri errori sulla stagionalità e sul level, ma essendoci così tanti dati, i peggiori gli abbiamo 
## tolti

## un'altra roba che si può dire è che la varianza dopo le riaperture è minore rispetto a prima, 
## mentre il trend è partito dallo stesso livello dopo le riaperture.




#### DIFFERENCING E ARIMA ####

### differenzio la serie giornaliera
### funzioni che mi dicono di quanto differenziare

ndiffs(vendite1_day) #differenziazione normale
nsdiffs(vendite1_day) #differenziazione stagionale

#anche questo per capire se differenziare o meno
library(urca)
vendite1_day %>% ur.kpss() %>% summary()

vendite1_day_diff <- diff(vendite1_day)

cbind("vendite giornaliere" = vendite1_day,
      "vendite diff" = vendite1_day_diff) %>%
  autoplot(facets=TRUE) +
  xlab("Year") + ylab("") +
  ggtitle("Differencing")

ggAcf(diff(vendite1_day), lag=20)
ggPacf(diff(vendite1_day), lag=20)

Box.test(vendite1_day_diff, lag=312, type = "Ljung-Box") # sono correlati � un problema

# arima e previsione, usando come training tutto quello che ho (per ora ho fatto cos�, ma non credo sia giusto)
fit <- auto.arima(vendite1_day_diff, seasonal=FALSE) #in teoria non ci dovrebbe essere bisogno qui della stag, ma mi sa di si
fit
fit %>% forecast(h=30) %>% autoplot(include=100)

checkresiduals(fit) # errori sono correlati, vuol dire che c'� dell'info nei residui e non va bene
Box.test(residuals(fit), lag=312) #312= tutti/5




