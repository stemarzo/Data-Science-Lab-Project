# SETTING PROGETTO --------------------------------------------------------

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
library("dplyr") 

# setting working directory
# working_dir = "/Users/lorenzolorgna/Desktop/Progetto ds lab/progetto_dslab/dati"
# setwd(working_dir)

# lettura dataset
ristorazione_original <- read_excel("Ristorazione.xls")




# SISTEMAZIONE DATASET & AGGIUNTA NUOVE FEATURES --------------------------

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

# aggiunta colonna stagione
getSeason <- function(DATES) {  # funzione per convertire da data a stagione
  WS <- as.Date("2012-12-15", format = "%Y-%m-%d") # Winter Solstice
  SE <- as.Date("2012-03-15",  format = "%Y-%m-%d") # Spring Equinox
  SS <- as.Date("2012-06-15",  format = "%Y-%m-%d") # Summer Solstice
  FE <- as.Date("2012-09-15",  format = "%Y-%m-%d") # Fall Equinox
  
  # Convert dates from any year to 2012 dates
  d <- as.Date(strftime(DATES, format="2012-%m-%d"))
  
  ifelse (d >= WS | d < SE, "Inverno",
          ifelse (d >= SE & d < SS, "Primavera",
                  ifelse (d >= SS & d < FE, "Estate", "Autunno")))
}
ristorazione$stagione <- getSeason(ristorazione$data)

# aggiunta colonna mese
ristorazione$mese <- format(ristorazione$data,"%B")

# colonna giorni festivi e feriali
ristorazione <- ristorazione %>%
  mutate(weekday = wday(data, week_start = getOption("lubridate.week.start", 1))) %>%
  mutate(holiday = isHoliday("Italy", data)) %>%  # aggiungiere eventuali feste
  mutate(tipo_giorno = case_when(
    (weekday %in% c(6,7)) ~ "weekend"
    , (weekday < 7) ~ "weekday"
    , TRUE ~ "other"
  )
  )

# conversione true/false -> 1/0
ristorazione$holiday <- as.integer(ristorazione$holiday)


# colore zona in base alla data
colori_zone <- read_csv("colori_zone.csv")
# colori_zone <- read_csv("C:/Users/Stefano/Documents/progetto_dslab/codice_progetto/dati/dataset.csv")

# selezione regioni
colori_lombardia <- colori_zone %>% filter(
  denominazione_regione == "Lombardia")
names(colori_lombardia)[3] <- "colore_lombardia"


colori_emilia_romagna <- colori_zone %>% filter(
  denominazione_regione == "Emilia-Romagna")
names(colori_emilia_romagna)[3] <- "colore_emilia_romagna"


colori_lombardia_emilia_romagna <- merge(x = colori_lombardia, y = colori_emilia_romagna, by = "data", all.x = TRUE)
colori_lombardia_emilia_romagna <- colori_lombardia_emilia_romagna[,-c(2,4)]


# faccio il join con ristorazione su data
ristorazione<-merge(x=ristorazione,y=colori_lombardia_emilia_romagna,by="data",all.x=TRUE)

# colonna asporto (per Lombardia, bisogna decidere quale regione)
# zona rossa: solo asporto e consegne a domicilio senza limiti
# zona arancione: solo asporto e consegne a domicilio senza limiti
# zona gialla: si può mangiare in presenza e consegne a domicilio senza limiti
ristorazione <- ristorazione %>%
  mutate(solo_asporto = case_when(
    (colore_lombardia == "rosso") ~ TRUE
    ,(colore_lombardia == "arancione") ~ TRUE
    , (colore_lombardia == "giallo") ~ FALSE
    , TRUE ~ FALSE
  ) 
  )      

# conversione true/false -> 1/0
ristorazione$solo_asporto <- as.integer(ristorazione$solo_asporto)



# inserimento delle colonne che riguardano gli eventi sportivi
eventi_sportivi <- read_delim("eventi_sportivi.csv", ";", escape_double = FALSE, trim_ws = TRUE)
eventi_sportivi <- eventi_sportivi[, -c(1)]
ristorazione<-merge(x=ristorazione,y=eventi_sportivi,by="data",all.x=TRUE)

# inserimento colonne riguardanti il meteo
meteo <- read_delim("meteo.csv", ";", escape_double = FALSE, trim_ws = TRUE)
meteo <- meteo[, -c(1)]
meteo$data<-as.Date(meteo$data, format = "%d/%m/%Y")
ristorazione<-merge(x=ristorazione,y=meteo,by="data",all.x=TRUE)

# aggiunta colonna somministrazioni vaccini
vaccini <- read_csv("somministrazioni_vaccini.csv")

vaccini <- vaccini %>%
  filter(area=="LOM" | area=="EMR") %>%
  filter(data_somministrazione <= as.Date("2021-04-12", format = "%Y-%m-%d")) %>%
  select(data_somministrazione, area, totale)

vaccini_lombardia <- vaccini %>%
  filter(area=="LOM") %>%
  select(data_somministrazione, area, totale)

# per capire se ci sono dei giorni in cui non ci sono stati vaccini
d <- vaccini_lombardia$data_somministrazione
d <- as.Date(d)
date_range <- seq(min(d), max(d), by = 1) 
date_range[!date_range %in% d]  # non vi sono missing values

vaccini_emilia_romagna <- vaccini %>%
  filter(area=="EMR") %>%
  select(data_somministrazione, area, totale)

# per capire se ci sono dei giorni in cui non ci sono stati vaccini
d <- vaccini_emilia_romagna$data_somministrazione
d <- as.Date(d)
date_range <- seq(min(d), max(d), by = 1) 
date_range[!date_range %in% d] 
# nei giorni "2020-12-28" "2020-12-29" "2020-12-30" "2021-01-01" non sono state
# resgistrate vaccinazioni

# aggiungo date mancanti
data_somministrazione <- seq(as.Date("2020-12-28", format = "%Y-%m-%d"), as.Date("2020-12-30", format = "%Y-%m-%d"), by = 1)
area = rep("EMR", times = 3)
totale = rep(0, times = 3)
missing_dates_28_30 <- data.frame(data_somministrazione, area, totale)

# aggiungo date mancanti
data_somministrazione <- as.Date("2021-01-01", format = "%Y-%m-%d")
area = "EMR"
totale = 0
missing_dates_01_01 <- data.frame(data_somministrazione, area, totale)

# aggiorno i dati per EMR
vaccini_emilia_romagna = rbind(vaccini_emilia_romagna[1,],  # ora non ci sono più date mancanti
                               missing_dates_28_30,
                               vaccini_emilia_romagna[2,],
                               missing_dates_01_01, 
                               vaccini_emilia_romagna[-(1:2),])
# sistemazione colonne
names(vaccini_lombardia)[3] <- "tot_vaccini_lombardia"
names(vaccini_lombardia)[1] <- "data"
vaccini_lombardia$area <- NULL

names(vaccini_emilia_romagna)[3] <- "tot_vaccini_emilia_romagna"
names(vaccini_emilia_romagna)[1] <- "data"
vaccini_emilia_romagna$area <- NULL


# integro con il file ristorazione (ci saranno righe duplicate perchè bisogna ancora scegliere tra Lombardia e Emilia Romagna cosa tenere in colori regioni)
ristorazione <- merge(x = ristorazione, y = vaccini_lombardia, by = "data", all.x = TRUE)
ristorazione <- merge(x = ristorazione, y = vaccini_emilia_romagna, by = "data", all.x = TRUE)


# check NA values
sum(is.na(ristorazione$data))  # 0 NA 

sum(is.na(ristorazione$data_anno_prec))  # 68 NA
which(is.na(ristorazione$data_anno_prec))

sum(is.na(ristorazione$giorno_settimana))  # 0 NA 

sum(is.na(ristorazione$vendite1))  # 74 NA
which(is.na(ristorazione$vendite1))

# e cosi via per le altre variabili: bisogna capire cosa si vuole fare.
# potrebbero esserci dei NA che corrispondono alle 0 vendite in periodo covid,
# ma anche degli NA che sono magari dovuti alla registrazione non corretta del 
# valore

first_date = ristorazione[421,"data_anno_prec"]+1
last_date = ristorazione[427,"data_anno_prec"]-1
dates <- seq(first_date, last_date, by = "1 day")
ristorazione[c(422:426),"data_anno_prec"] <- dates

ristorazione[516, "data_anno_prec"] <-  as.Date("2017-06-01", format = "%Y-%m-%d")

first_date = ristorazione[1166,"data_anno_prec"]+1
last_date = ristorazione[1223,"data_anno_prec"]-1
dates <- seq(first_date, last_date, by = "1 day")
ristorazione[c(1167:1222),"data_anno_prec"] <- dates


first_date = ristorazione[1454,"data_anno_prec"]+1
last_date = ristorazione[1457,"data_anno_prec"]-1
dates <- seq(first_date, last_date, by = "1 day")
ristorazione[c(1455:1456),"data_anno_prec"] <- dates

ristorazione[1462 , "data_anno_prec"] <-  as.Date("2020-01-03", format = "%Y-%m-%d")

ristorazione[1467 , "data_anno_prec"] <-  as.Date("2020-01-08", format = "%Y-%m-%d")


first_date = ristorazione[1554,"data_anno_prec"]+1
last_date = ristorazione[1557,"data_anno_prec"]-1
dates <- seq(first_date, last_date, by = "1 day") 
ristorazione[c(1555:1556),"data_anno_prec"] <- dates


# aggiunta manuale vacanze scolastiche/ festività straniere/ blackfriday ecc.
other_dates <- read_excel("vacanze_scolastiche_saldi.xls")
other_dates <- other_dates[-1,21:24]
names(other_dates) <- c("Vacanze scolastiche Lombardia", "Black Friday e saldi Lombardia",
                        "Vacanze scolastiche Emilia-Romagna", "Black Friday e saldi  Emilia-Romagna")

ristorazione <- cbind(ristorazione, other_dates)


# CREAZIONE DF PER CIASCUN RISTORANTE -------------------------------------

# creo un dataframe per ciascun ristorante
col_date <- subset(ristorazione, select = c(1, 2))
col_nomi <- c("data", "data_anno_prec", "vendite", "scontrini")

# col_date <- subset(ristorazione, select = c(1, 2, 3, 4, 5)) # data, data_anno_prec, giorno_settimana, uguali per ogni ristorante
# col_nomi <- c("data", "data_anno_prec", "giorno_settimana", "mese", "stagione", "vendite", "scontrini")

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

# rimuovo le variabili che mi sono servite nella fase sopra
rm(list = c('col_date','col_nomi'))




# ESPLORAZIONE DATASET ------------------------------------------------------------


# Time series consist of four components:
# (1) Seasonal variations: that repeat over a specific period such as a day, week, month, season, etc.,
# (2) Trend: Trend is defined as long term increase or decrease in the data. It can be linear or non-linear
# (3) Cyclical variations: A cyclic pattern exists when data exhibit rises and falls that are not of fixed period.
# (4) Random variations: that do not fall under any of the above three classifications.



## andamento giornaliero delle vendite negli anni considerati, per ciascun ristorante 
par(mfrow=c(3,2))

# ristorante 1
plot(ristorante1$data, ristorante1$vendite, xlab = "data", ylab = "vendite", type="l", main = "Ristorante 1")
abline(h=mean(as.integer(ristorante1$vendite)))

plot(ristorante2$data, ristorante2$vendite, xlab = "data", ylab = "vendite", type="l", main = "Ristorante 2")
abline(h=mean(as.integer(ristorante2$vendite)))

plot(ristorante3$data, ristorante3$vendite, xlab = "data", ylab = "vendite", type="l", main = "Ristorante 3")
abline(h=mean(as.integer(ristorante3$vendite)))

plot(ristorante4$data, ristorante4$vendite, xlab = "data", ylab = "vendite", type="l", main = "Ristorante 4")
abline(h=mean(as.integer(ristorante4$vendite)))

plot(ristorante5$data, ristorante5$vendite, xlab = "data", ylab = "vendite", type="l", main = "Ristorante 5")
abline(h=mean(as.integer(ristorante5$vendite)))

plot(ristorante6$data, ristorante6$vendite, xlab = "data", ylab = "vendite", type="l", main = "Ristorante 6")
abline(h=mean(as.integer(ristorante6$vendite)))


# si analizza singolarmente ciascun ristorante più nel dettaglio

# ristorante 1
ggplot(ristorante1, aes(data, vendite)) +
  geom_line() +
  geom_vline(xintercept=as.numeric(ristorante1$data[yday(ristorante1$data)==1]), size=1.2, color= "red") +
  
  scale_x_date(date_labels=paste(c(rep(" ",11), "%b"), collapse=""), 
               date_breaks="month", expand=c(0,0)) +
  facet_grid(~ year(data), space="free_x", scales="free_x", switch="x") +
  theme_bw() +
  ggtitle("Ristorante 1")  +
  theme(strip.placement = "outside",
        strip.background = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.border = element_rect(colour="grey70"),
        panel.spacing=unit(0,"cm"))

# ristorante 2
ggplot(ristorante2, aes(data, vendite)) +
  geom_line() +
  geom_vline(xintercept=as.numeric(ristorante2$data[yday(ristorante2$data)==1]), size=1.2, color= "red") +
  
  scale_x_date(date_labels=paste(c(rep(" ",11), "%b"), collapse=""), 
               date_breaks="month", expand=c(0,0)) +
  facet_grid(~ year(data), space="free_x", scales="free_x", switch="x") +
  theme_bw() +
  ggtitle("Ristorante 2")  +
  theme(strip.placement = "outside",
        strip.background = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.border = element_rect(colour="grey70"),
        panel.spacing=unit(0,"cm"))


# ristorante 3
ggplot(ristorante3, aes(data, vendite)) +
  geom_line() +
  geom_vline(xintercept=as.numeric(ristorante3$data[yday(ristorante3$data)==1]), size=1.2, color= "red") +
  
  scale_x_date(date_labels=paste(c(rep(" ",11), "%b"), collapse=""), 
               date_breaks="month", expand=c(0,0)) +
  facet_grid(~ year(data), space="free_x", scales="free_x", switch="x") +
  theme_bw() +
  ggtitle("Ristorante 3")  +
  theme(strip.placement = "outside",
        strip.background = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.border = element_rect(colour="grey70"),
        panel.spacing=unit(0,"cm"))

# ristorante 4
plot_ <- ggplot(ristorante4, aes(data, vendite)) +
  geom_line() +
  geom_vline(xintercept=as.numeric(ristorante4$data[yday(ristorante4$data)==1]), size=1.2, color= "red") +
  
  scale_x_date(date_labels=paste(c(rep(" ",11), "%b"), collapse=""), 
               date_breaks="month", expand=c(0,0)) +
  facet_grid(~ year(data), space="free_x", scales="free_x", switch="x") +
  theme_bw() +
  ggtitle("Ristorante 4")  +
  theme(strip.placement = "outside",
        strip.background = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.border = element_rect(colour="grey70"),
        panel.spacing=unit(0,"cm"))
print(ris)

# ristorante 5
ggplot(ristorante5, aes(data, vendite)) +
  geom_line() +
  geom_vline(xintercept=as.numeric(ristorante5$data[yday(ristorante5$data)==1]), size=1.2, color= "red") +
  
  scale_x_date(date_labels=paste(c(rep(" ",11), "%b"), collapse=""), 
               date_breaks="month", expand=c(0,0)) +
  facet_grid(~ year(data), space="free_x", scales="free_x", switch="x") +
  theme_bw() +
  ggtitle("Ristorante 5")  +
  theme(strip.placement = "outside",
        strip.background = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.border = element_rect(colour="grey70"),
        panel.spacing=unit(0,"cm"))

# ristorante 6
ggplot(ristorante6, aes(data, vendite)) +
  geom_line() +
  geom_vline(xintercept=as.numeric(ristorante6$data[yday(ristorante6$data)==1]), size=1.2, color= "red") +
  
  scale_x_date(date_labels=paste(c(rep(" ",11), "%b"), collapse=""), 
               date_breaks="month", expand=c(0,0)) +
  facet_grid(~ year(data), space="free_x", scales="free_x", switch="x") +
  theme_bw() +
  ggtitle("Ristorante 6")  +
  theme(strip.placement = "outside",
        strip.background = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.border = element_rect(colour="grey70"),
        panel.spacing=unit(0,"cm"))

## decomposizione serie per ciascun ristorante (considerando le vendite)
par(mfrow=c(1,1))

vendite1 <- ts(ristorante1[, 3],start=2017,frequency=365) 
plot(vendite1)
vendite1.fit<-stl(vendite1,s.window="periodic")
attributes(vendite1.fit)
trend.vendite1<-vendite1.fit$time.series[,2]
stag.vendite1<-vendite1.fit$time.series[,1]
res.vendite1<-vendite1.fit$time.series[,3]
plot(vendite1.fit,main="Decomposizione con la funzione 'stl' per il ristorante 1")


vendite2 <- ts(ristorante2[, 3],start=2017,frequency=365) 
plot(vendite2)
vendite2.fit<-stl(vendite2,s.window="periodic")
attributes(vendite2.fit)
trend.vendite2<-vendite2.fit$time.series[,2]
stag.vendite2<-vendite2.fit$time.series[,1]
res.vendite2<-vendite2.fit$time.series[,3]
plot(vendite2.fit,main="Decomposizione con la funzione 'stl' per il ristorante 2")


vendite3 <- ts(ristorante3[, 3],start=2017,frequency=365) 
plot(vendite3)
vendite3.fit<-stl(vendite3,s.window="periodic")
attributes(vendite3.fit)
trend.vendite3<-vendite3.fit$time.series[,2]
stag.vendite3<-vendite3.fit$time.series[,1]
res.vendite3<-vendite3.fit$time.series[,3]
plot(vendite3.fit,main="Decomposizione con la funzione 'stl' per il ristorante 3")


vendite4 <- ts(ristorante4[, 3],start=2017,frequency=365) 
plot(vendite4)
vendite4.fit<-stl(vendite4,s.window="periodic")
attributes(vendite4.fit)
trend.vendite4<-vendite4.fit$time.series[,2]
stag.vendite4<-vendite4.fit$time.series[,1]
res.vendite4<-vendite4.fit$time.series[,3]
plot(vendite4.fit,main="Decomposizione con la funzione 'stl' per il ristorante 4")


vendite5 <- ts(ristorante5[, 3],start=2017,frequency=365) 
plot(vendite5)
vendite5.fit<-stl(vendite5,s.window="periodic")
attributes(vendite5.fit)
trend.vendite5<-vendite5.fit$time.series[,2]
stag.vendite5<-vendite5.fit$time.series[,1]
res.vendite5<-vendite5.fit$time.series[,3]
plot(vendite5.fit,main="Decomposizione con la funzione 'stl' per il ristorante 5")


vendite6 <- ts(ristorante6[, 3],start=2017,frequency=365) 
plot(vendite6)
vendite6.fit<-stl(vendite6,s.window="periodic")
attributes(vendite6.fit)
trend.vendite6<-vendite6.fit$time.series[,2]
stag.vendite6<-vendite6.fit$time.series[,1]
res.vendite6<-vendite6.fit$time.series[,3]
plot(vendite6.fit,main="Decomposizione con la funzione 'stl' per il ristorante 6")


## si procede ad analizzare ciascun ristorante nel periodo antecedente il covid-19 
reference_date <- as.Date("2020-03-09", format = "%Y-%m-%d")

# vendite ristorante 1 pre covid
vendite1_pre <- ristorante1 %>%
  filter(ristorante1$data < reference_date) %>%
  select(vendite, data)

# VENDITE GIORNALIERE
vendite1_day <- ts(vendite1_pre$vendite,start=2017,frequency=365)

autoplot(vendite1_day) +
  ggtitle("Ristorante 1: vendite giornaliere pre-covid") +
  xlab("anno") +
  ylab("vendite")

# VENDITE SETTIMANALI
week <- as.Date(cut(vendite1_pre$data, "week"))
vendite1_sett <- aggregate(vendite ~ week, vendite1_pre, sum)
vendite1_sett <- vendite1_sett$vendite
vendite1_sett <- ts(vendite1_sett,start=2017,frequency=52) 

autoplot(vendite1_sett) +
  ggtitle("Ristorante 1: vendite settimanali pre-covid") +
  xlab("anno") +
  ylab("vendite")

# VENDITE SETTIMANALI AVG 
week <- as.Date(cut(ristorante1_pre$data, "week"))
vendite1_sett_avg <- aggregate(vendite ~ week, ristorante1_pre, mean)
vendite1_sett_avg <- vendite1_sett_avg$vendite
vendite1_sett_avg <- ts(vendite1_sett_avg,start=2017,frequency=52) 

autoplot(vendite1_sett_avg) +
  ggtitle("Ristorante 1: vendite medie settimanali pre-covid") +
  xlab("anno") +
  ylab("vendite")

# VENDITE MENSILI
month <- as.Date(cut(ristorante1_pre$data, "month"))
vendite1_mens <- aggregate(vendite ~ month, ristorante1_pre, sum)
vendite1_mens <- vendite1_mens$vendite
vendite1_mens <- ts(vendite1_mens,start=2017,frequency=12) 

autoplot(vendite1_mens) +
  ggtitle("Ristorante 1: vendite mensili pre-covid") +
  xlab("anno") +
  ylab("vendite")


# VENDITE MENSILI AVG
month <- as.Date(cut(ristorante1_pre$data, "month"))
vendite1_mens_avg <- aggregate(vendite ~ month, ristorante1_pre, mean)
vendite1_mens_avg <- vendite1_mens_avg$vendite
vendite1_mens_avg <- ts(vendite1_mens_avg,start=2017,frequency=12) 

autoplot(vendite1_mens_avg) +
  ggtitle("Ristorante 1: vendite medie mensili pre-covid") +
  xlab("anno") +
  ylab("vendite")



# ripetere con gli altri ristoranti

# aggiungere altri grafici:
# - GRAFICI STAGIONALITA
# - ALTRI ASPETTI SULLA STAGIONALITA'
# - CORRELAZIONE TRA SCONTRINI E VENDITE
# - AUTOCORRELAZIONE
# - FORECAST



# ULTERIORI ANALISI DA FARE PER CIASCUN RISTORANTE

# autocorrleation plots
par(mfrow = c(1,2))
acf(as.ts(ristorante1$vendite), main = "Sales")
pacf(as.ts(ristorante1$vendite), main = "Sales")

# we apply auto.arima function, that searches the best ARIMA model
arima1 <- auto.arima(as.ts(ristorante1$vendite))
arima1



# da escludere
# #decomposizione covid
# rist1 <- ristorante1
# rist_pre_covid<-rist1[1:1164, 4]
# rist_pre_covid[is.na(rist_pre_covid)] <- 0
# rist_pre_covid<-ts(rist_pre_covid,start=2017,frequency=365) 
# plot(rist_pre_covid)
# 
# 
# stl.fit_pre<-stl(rist_pre_covid,s.window="periodic")
# attributes(stl.fit_pre)
# trend.stl<-stl.fit_pre$time.series[,2]
# stag.stl<-stl.fit_pre$time.series[,1]
# res.stl<-stl.fit_pre$time.series[,3]
# plot(stl.fit_pre,main="Decomposizione con la funzione 'stl'")
# 
# rist_post_covid<-rist1[1164:1563, 4]
# summary(rist_post_covid)
# rist_post_covid[is.na(rist_post_covid)] <- 0
# rist_post_covid<-ts(rist_post_covid, start = 2020, frequency = 365) 
# plot(rist_post_covid)
# rist_post_covid
# 
# 
# stl.fit_post<-stl(rist_post_covid,s.window="periodic")
# attributes(stl.fit_pre)
# trend.stl<-stl.fit_pre$time.series[,2]
# stag.stl<-stl.fit_pre$time.series[,1]
# res.stl<-stl.fit_pre$time.series[,3]
# plot(stl.fit_post,main="Decomposizione con la funzione 'stl'")


# aggregare ristorante 1 per mese
# vendite_agg1<-ristorante1
# vendite_agg1$mo <- strftime(vendite_agg1$data, "%m")
# vendite_agg1$yr <- strftime(vendite_agg1$data, "%Y")
# vendite_agg1<-ts(vendite_agg1$vendite,start=2017,frequency=12) 
# vendite_agg1 <- aggregate(vendite ~ mo + yr, vendite_agg1, FUN = mean)
# 
# monthplot(ristm)
# plot(vendite_agg1)
# 
# vendite_agg1.fit<-stl(vendite_agg1,s.window="periodic")
# attributes(vendite_agg1.fit)
# trend.vendite_agg1<-vendite_agg1.fit$time.series[,2]
# stag.vendite_agg1<-vendite_agg1.fit$time.series[,1]
# res.vendite_agg1<-vendite_agg1.fit$time.series[,3]
# plot(vendite_agg1.fit,main="Decomposizione con la funzione 'stl'")
# 
# 
# acf(res.vendite_agg1,type="correlation",plot=TRUE,main="Correlogramma della serie dei residui")
# pacf(res.vendite_agg1,plot=TRUE,main="Grafico delle correlazioni parziali")




# PREVISIONE FATTURATO NO COVID -------------------------------------------

# bisogna consierare un periodo pre covid e fare le previsioni sui mesi del covid per vedere come sarebbero andate le vendite del ristorante

# Lo scopo di ARIMA è di trovare il modello che meglio rappresenti i valori di una serie storica.
# 
# Un modello ARIMA può essere espresso come ARIMA (p, d, q), dove, come abbiamo già visto, p è l’ordine del modello autoregressivo, d indica il grado di differenziazione e q indica l’ordine della media mobile..
# 
# Operativamente, possiamo definire cinque passaggi per adattare le serie storiche a un modello ARIMA:
#   
# Visualizzare le serie temporali con un grafico.
# Differenziare le serie storiche non stazionarie per ottenere serie temporali stazionarie.
# Tracciare grafici ACF e PACF per trovare i valori ottimali di p e q, oppure ricavarli usando la funzione auto.arima.
# Costruire il modello ARIMA.
# Fare la previsione.

# quindi per rispondere a questa domanda creiamo un modello arima che ci permette poi
# di fare delle previsioni


# CONFRONTO ESTATE COVID & ESTATE NO COVID --------------------------------

# seleziono un periodo temporale che corrisponde all'estate
# ricavo due serie storiche, per 2019 e 2020

inizio_estate_2019 <- as.Date("2019-06-21", format = "%Y-%m-%d")
fine_estate_2019 <- as.Date("2019-09-22", format = "%Y-%m-%d")

inizio_estate_2020 <- as.Date("2020-06-21", format = "%Y-%m-%d")
fine_estate_2020 <- as.Date("2020-09-22", format = "%Y-%m-%d")


ristorante1_estate_2019 <- subset(ristorante1, data >= inizio_estate_2019 & data <= fine_estate_2019)
ristorante1_estate_2019 <- ristorante1_estate_2019[,-2]

ristorante1_estate_2020 <- subset(ristorante1, data >= inizio_estate_2020 & data <= fine_estate_2020)
 ristorante1_estate_2020 <- ristorante1_estate_2020[,-2]

ristoranti_unione_2019_2020 <- rbind(ristorante1_estate_2019, ristorante1_estate_2020)

ristoranti_unione_2019_2020$Year <- format(ristoranti_unione_2019_2020$data, "%Y")
ristoranti_unione_2019_2020$Month <- format(ristoranti_unione_2019_2020$data, "%b")
ristoranti_unione_2019_2020$Day <- format(ristoranti_unione_2019_2020$data, "%d")

ristoranti_unione_2019_2020$MonthDay <- format(ristoranti_unione_2019_2020$data, "%d-%b")
ristoranti_unione_2019_2020$MonthDay2 <- ristoranti_unione_2019_2020$MonthDay

ristoranti_unione_2019_2020$MonthDay2[as.numeric(row.names(ristoranti_unione_2019_2020))%%3!=0] <- ""
labels <- ristoranti_unione_2019_2020$MonthDay2

p <- ggplot(data=ristoranti_unione_2019_2020, mapping=aes(x=MonthDay, y=vendite, shape=Year, color=Year)) + geom_point() +geom_line(aes(group = 1))
p <- p + facet_grid(facets = Year ~ ., margins = FALSE) + theme_bw()
p + scale_y_continuous() + scale_x_discrete(labels=labels) + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, size = 8))



# si procede con un'analisi più approfondita e più tencica del grafico per rispondere 
# alla domanda di ricerca: ovvero nel 2020 c'era pochissime restrizioni, quasi 
# zero però si vuole dimostrare che non è stata un'estate normale come lo può 
# essere quella del 2019

# CLUSTERING RISTORANTI ---------------------------------------------------


