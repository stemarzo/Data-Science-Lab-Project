# SETTING PROGETTO --------------------------------------------------------

# setting librerie
# Package names
packages <- c("readxl",  "readr", "forecast", "dplyr", "magrittr", "ggplot2",
              "forcats", "lubridate", "RQuantLib", "devtools", "patchwork", "KFAS",
              "caret", "tseries", "urca", "TSstudio", "gridExtra") 

# Install packages not yet installed
installed_packages <- packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(packages[!installed_packages])
}

# Packages loading
invisible(lapply(packages, library, character.only = TRUE))


# setting working directory
#working_dir = "C:/Users/Stefano/Documents/progetto_dslab/dati"
#working_dir = "C:/Users/Lorenzo/Desktop/Progetto ds lab/progetto_dslab/dati"
working_dir = "~/Desktop/progetti uni github/progetto_dslab/dati"
setwd(working_dir)

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
# conversione in numeric
ristorazione$stagione[ristorazione$stagione == "Primavera"] <- 1
ristorazione$stagione[ristorazione$stagione == "Estate"] <- 2
ristorazione$stagione[ristorazione$stagione == "Autunno"] <- 3
ristorazione$stagione[ristorazione$stagione == "Inverno"] <- 4

# aggiunta colonna mese
# ristorazione$mese <- format(ristorazione$data,"%B")
ristorazione$mese <- month(ristorazione$data)


# colonna giorni festivi e feriali
ristorazione <- ristorazione %>%
  mutate(weekday = wday(data, week_start = getOption("lubridate.week.start", 1))) %>%
  mutate(tipo_giorno = case_when(
    (weekday %in% c(6,7)) ~ "weekend"
    , (weekday < 7) ~ "weekday"
    , TRUE ~ "other"
  )
  )

# colonna holiday
# lista vacanze italiane
new_holiday = getHolidayList(calendar = "Italy", from=as.Date("2017-01-01"), to= as.Date("2021-04-12"), includeWeekends=FALSE)
new_holiday <- append(new_holiday, c(as.Date("2017-04-16"), as.Date("2018-04-01"),
                                     as.Date("2019-04-21"), as.Date("2021-04-04"), as.Date("2020-04-12")))
for (i in 1:nrow(ristorazione)) {
  ristorazione[i,"holiday"]<-ristorazione[i,"data"]%in% new_holiday
}
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


# integro con il file ristorazione
ristorazione <- merge(x = ristorazione, y = vaccini_lombardia, by = "data", all.x = TRUE)
ristorazione <- merge(x = ristorazione, y = vaccini_emilia_romagna, by = "data", all.x = TRUE)


# check NA values
sum(is.na(ristorazione$data))  # 0 NA 

sum(is.na(ristorazione$data_anno_prec))  # 68 NA, viene 
which(is.na(ristorazione$data_anno_prec))

# per ogni ristorante viene verificata la presenza di valori NA o meno
# i seguenti NA potrebbero essere dovuti a giorni senza vendite oppure ad errori di imputazione

sum(is.na(ristorazione$vendite1))  # 68 NA
which(is.na(ristorazione$vendite1))
subset(ristorazione[,c(1,3)], is.na(ristorazione$vendite1))
# 26 febbraio 2018 - 2 marzo 2018: ondata di gelo e neve
# 31 maggio 2018: ?
# 12 marzo 2020 - 6 maggio 2020: covid
# 25-26 dicembre 2020, 1,6 gennaio 2021: feste
# 4-5 aprile 2021: pasqua


sum(is.na(ristorazione$vendite2))  # 55 NA
which(is.na(ristorazione$vendite2))
subset(ristorazione[,c(1,5)], is.na(ristorazione$vendite2))
# 12 marzo 2020 - 3 maggio 2020: covid
# 25 dicembre 2020: festa
# 4 aprile 2021: pasqua


sum(is.na(ristorazione$vendite3))  # 1096 NA
which(is.na(ristorazione$vendite3))
subset(ristorazione[,c(1,7)], is.na(ristorazione$vendite3))
# fino al 8 novembre 2019: ristorante non attivo, se non per alcuni giorni (test)
# 12 marzo 2020 - 6 maggio 2020: covid
# 25-26 dicembre 2020, 1 gennaio 2021: feste
# 4-5 aprile 2021: pasqua

sum(is.na(ristorazione$vendite4))  # 108 NA
which(is.na(ristorazione$vendite4))
subset(ristorazione[,c(1,9)], is.na(ristorazione$vendite4))
# 1 gennaio 2014: feste
# 16-17 aprile 2017: pasqua
# 15 agosto 2017: ferragosto
# 25-26 dicembre 2017, 1 gennaio 2018: feste
# 1-2 aprile 2018: pasqua
# 15 maggio 2018: ?
# 15 agosto 2018: ferragosto
# 25-26 dicembre 2018, 1 gennaio 2019: feste
# 21-22 aprile 2019: pasqua
# 15 agosto 2019: ferragosto
# 25-26 dicembre 2019, 1 gennaio 2020: feste
# 12 marzo 2020 - 2 giungo 2020: covid
# 15-16 agosto 2020: ferragosto
# 25-26 dicembre 2020: feste
# 4-5 aprile 2021: pasqua


sum(is.na(ristorazione$vendite5))  # 62 NA
which(is.na(ristorazione$vendite5))
subset(ristorazione[,c(1,11)], is.na(ristorazione$vendite5))
# 9 aprile - 13 aprile 2018: ?
# 16 maggio 2018: ?
# 5 giugno 2018: ?
# 12 marzo 2020 - 3 maggio 2020: covid
# 25 dicembre 2020: festa
# 4 aprile 2021: pasqua


sum(is.na(ristorazione$vendite6))  # 319 NA
which(is.na(ristorazione$vendite6))
subset(ristorazione[,c(1,13)], is.na(ristorazione$vendite6))
# 1 gennaio - 20 settembre 2017: ristorante non attivo
# 14 maggio 2018: ?
# 12 marzo 2020 - 3 maggio 2020: covid
# 25 dicembre 2020: festa
# 4 aprile 2021: pasqua

# i seguenti valori NA verranno trattati nelle fasi di creazioni di df per ciascun ristorante
# il numero di scontrini NA per ciascun ristorante corrisponde con il numero di vendite NA
# effettuare check di consistenza: agli scontrini con vendite pari a 0 devono corrispondere 0 scontrini e cosi via 


# sistemazione NA data_anno_prec
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

# aggiunta colonna neve
meteo <- read_delim("meteo2.csv", ";", escape_double = FALSE, trim_ws = TRUE)
meteo <- meteo[, -c(1)]
meteo$data<-as.Date(meteo$data, format = "%d/%m/%Y")
ristorazione<-merge(x=ristorazione,y=meteo,by="data",all.x=TRUE)




# CREAZIONE DF PER CIASCUN RISTORANTE -------------------------------------

# creo un dataframe per ciascun ristorante
col_date <- subset(ristorazione, select = c(1, 2))
col_nomi <- c("data", "data_anno_prec", "vendite", "scontrini")

# col_date <- subset(ristorazione, select = c(1, 2, 3, 4, 5)) # data, data_anno_prec, giorno_settimana, uguali per ogni ristorante
# col_nomi <- c("data", "data_anno_prec", "giorno_settimana", "mese", "stagione", "vendite", "scontrini")

#ristorante1
ristorante1 <- data.frame(col_date, ristorazione$vendite1, ristorazione$scontrini1)
colnames(ristorante1) <- col_nomi
ristorante1$vendite[is.na(ristorante1$vendite)] <- 0  # sostiuisco i valori NA con 0 (per alcuni casi si potrebbe sostituire con valori quali la media)
ristorante1$scontrini[is.na(ristorante1$scontrini)] <- 0  
ristorante1$rapprto_v_s <- ristorante1$vendite/ristorante1$scontrini
ristorante1$rapprto_v_s[is.na(ristorante1$rapprto_v_s)] <- 0
ristorante1$chiuso <- 0
ristorante1$chiuso <- ifelse(ristorante1$rapprto_v_s == 0, 1, 0)


#ristorante2
ristorante2 <- data.frame(col_date, ristorazione$vendite2, ristorazione$scontrini2)
colnames(ristorante2) <- col_nomi
ristorante2$vendite[is.na(ristorante2$vendite)] <- 0 
ristorante2$scontrini[is.na(ristorante2$scontrini)] <- 0  
ristorante2$rapprto_v_s <- ristorante2$vendite/ristorante2$scontrini
ristorante2$rapprto_v_s[is.na(ristorante2$rapprto_v_s)] <- 0
ristorante2$chiuso <- 0
ristorante2$chiuso <- ifelse(ristorante2$rapprto_v_s == 0, 1, 0)

#ristorante3
ristorante3 <- data.frame(col_date, ristorazione$vendite3, ristorazione$scontrini3)
colnames(ristorante3) <- col_nomi
ristorante3$vendite[is.na(ristorante3$vendite)] <- 0 
ristorante3$scontrini[is.na(ristorante3$scontrini)] <- 0  
ristorante3$rapprto_v_s <- ristorante3$vendite/ristorante3$scontrini
ristorante3$rapprto_v_s[is.na(ristorante3$rapprto_v_s)] <- 0
ristorante3$chiuso <- 0
ristorante3$chiuso <- ifelse(ristorante3$rapprto_v_s == 0, 1, 0)

#ristorante4
ristorante4 <- data.frame(col_date,ristorazione$vendite4, ristorazione$scontrini4)
colnames(ristorante4) <- col_nomi
ristorante4$vendite[is.na(ristorante4$vendite)] <- 0  
ristorante4$scontrini[is.na(ristorante4$scontrini)] <- 0  
ristorante4$rapprto_v_s <- ristorante4$vendite/ristorante4$scontrini
ristorante4$rapprto_v_s[is.na(ristorante4$rapprto_v_s)] <- 0
ristorante4$chiuso <- 0
ristorante4$chiuso <- ifelse(ristorante4$rapprto_v_s == 0, 1, 0)

#ristorante5
ristorante5 <- data.frame(col_date, ristorazione$vendite5, ristorazione$scontrini5)
colnames(ristorante5) <- col_nomi
ristorante5$vendite[is.na(ristorante5$vendite)] <- 0 
ristorante5$scontrini[is.na(ristorante5$scontrini)] <- 0  
ristorante5$rapprto_v_s <- ristorante5$vendite/ristorante5$scontrini
ristorante5$rapprto_v_s[is.na(ristorante5$rapprto_v_s)] <- 0
ristorante5$chiuso <- 0
ristorante5$chiuso <- ifelse(ristorante5$rapprto_v_s == 0, 1, 0)

#ristorante6
ristorante6 <- data.frame(col_date, ristorazione$vendite6, ristorazione$scontrini6)
colnames(ristorante6) <- col_nomi
ristorante6$vendite[is.na(ristorante6$vendite)] <- 0  
ristorante6$scontrini[is.na(ristorante6$scontrini)] <- 0  
ristorante6$rapprto_v_s <- ristorante6$vendite/ristorante6$scontrini
ristorante6$rapprto_v_s[is.na(ristorante6$rapprto_v_s)] <- 0
ristorante6$chiuso <- 0
ristorante6$chiuso <- ifelse(ristorante6$rapprto_v_s == 0, 1, 0)

# rimuovo le variabili che mi sono servite nella fase sopra
rm(list = c('col_date','col_nomi'))




# ESPLORAZIONE DATASET ------------------------------------------------------------

# un aspetto da osservare potrebbe essere: i ristoranti aperti da più tempo hanno reagito meglio o peggio rispetto al covid , rispetto a ristoranti magari aperti da poco ? 
# nei plot sarebbe bello inserire le date degli eventi principali: chiusura ristoranti, inizio vaccinazione ecc (tipo https://statisticsglobe.com/draw-time-series-plot-with-events-using-ggplot2-in-r)

## analisi vendite giornaliere considerando tutti gli anni e tutti i ristoranti ----
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
ggplot(ristorante4, aes(data, vendite)) +
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




## esplorazioni dettagliate dei ristoranti ----

# esplorazione ristorante 1
source("~/Desktop/progetti uni github/progetto_dslab/other_scripts/esplorazione_ristoranti/esplorazione_rist_1.R")

# esplorazione ristorante 2
source("~/Desktop/progetti uni github/progetto_dslab/other_scripts/esplorazione_ristoranti/esplorazione_rist_2.R")

# esplorazione ristorante 3
source("~/Desktop/progetti uni github/progetto_dslab/other_scripts/esplorazione_ristoranti/esplorazione_rist_3.R")

# esplorazione ristorante 4
source("~/Desktop/progetti uni github/progetto_dslab/other_scripts/esplorazione_ristoranti/esplorazione_rist_4.R")

# esplorazione ristorante 5
source("~/Desktop/progetti uni github/progetto_dslab/other_scripts/esplorazione_ristoranti/esplorazione_rist_5.R")

# esplorazione ristorante 6
source("~/Desktop/progetti uni github/progetto_dslab/other_scripts/esplorazione_ristoranti/esplorazione_rist_6.R")



# PREVISIONE FATTURATO NO COVID LISCIAMENTO ESPONENZIALE PRIMO RISTORANTE  -------------------------------------------

# # work in progress
# vendite1_sett_avg_pre_split <- ts_split(vendite1_sett_avg_pre_dest_diff)
# 
# # divisione in train e test
# train <- vendite1_sett_avg_pre_split$train
# test <- vendite1_sett_avg_pre_split$test
# autoplot(train)
# autoplot(test)
# 
# autoplot(vendite1_sett_avg_pre_dest_diff) +
#   autolayer(train, series="Training") +
#   autolayer(test, series="Test")
# 
# M0 <- HoltWinters(train)
# previsioni <- forecast(M0, h=50)
# accuracy(previsioni, test)
# 
# par(mfrow=c(1,1))
# plot(previsioni)
# lines(test, col="red")
# legend("topleft",lty=1,bty = "n",col=c("red","blue"),c("testData","HoltWintersPred"))



# PREVISIONE FATTURATO NO COVID ARIMA PRIMO RISTORANTE -------------------------------------------

### Arima manuale----
# per rendere stazionaria la serie storica, bisogna eliminare stagionalità e trend

# rimozione stagionalità
vendite1_sett_avg_pre_dest <- seasadj(stl(vendite1_sett_avg_pre, s.window = 52))
autoplot(vendite1_sett_avg_pre_dest) + autolayer(vendite1_sett_avg_pre)

# verifica se necessario differenziare
ndiffs(vendite1_sett_avg_pre_dest)
nsdiffs(vendite1_sett_avg_pre_dest)

# rimozione trend con differenziazione
vendite1_sett_avg_pre_dest_diff <- diff(vendite1_sett_avg_pre_dest)
autoplot(vendite1_sett_avg_pre_dest_diff)

# verifica stazionarietà

adf.test(vendite1_sett_avg_pre_dest_diff, alternative = "stationary")  
# The null-hypothesis for an ADF test is that the data are non-stationary. 
# So p-value greater than 0.05 indicates non-stationarity, and  p-values less than 
# 0.05 suggest stationarity.
# risulta essere stazionaria

kpss.test(vendite1_sett_avg_pre_dest_diff)
# In this case, the null-hypothesis is that the data are stationary. In this case, 
# p-value less than 0.05 indicates non-stationary series and p-value greater than 
# 0.05 indicates stationary series.
# risulta essere stazionaria

summary(ur.kpss(vendite1_sett_avg_pre_dest_diff ))
ndiffs(vendite1_sett_avg_pre_dest_diff)
nsdiffs(vendite1_sett_avg_pre_dest_diff)

# una volta ottenuta la serie storica stazionaria si procede con la creazione
# di train e test
vendite1_sett_avg_pre_split <- ts_split(vendite1_sett_avg_pre_dest_diff)

# divisione in train e test
train <- vendite1_sett_avg_pre_split$train
test <- vendite1_sett_avg_pre_split$test
autoplot(train)
autoplot(test)

autoplot(vendite1_sett_avg_pre_dest_diff) +
  autolayer(train, series="Training") +
  autolayer(test, series="Test")

# # scelta di parametri p e q con acf e pacf
# acf<-ggAcf(train, lag.max = 52)+ggtitle("Vendite1 day pre diff")  # q = 2
# pacf<-ggPacf(train, lag.max = 52)+ggtitle("Vendite1 day pre diff")  # p = 2
# grid.arrange(acf, pacf, ncol=2)
# 
# # creazione modello arima
# M1<-Arima(train, order = c(2,0,2))
# summary(M1) 
# checkresiduals(M1)
# 
# # autoplot(forecast(M1, h=50))+autolayer(test)

# si analizzano acf e pacf del train, per la scelta dei parametri p e q
acf <- ggAcf(train, lag.max = 52) + ggtitle("Vendite1 day pre diff")
pacf <- ggPacf(train, lag.max = 52) + ggtitle("Vendite1 day pre diff")
grid.arrange(acf, pacf, ncol=2)

# si inzia provando con un ar 2, osservando che nel pacf due lag sono 
# significativamente correlati
M2 <- Arima(train, order = c(2,0,0))

# si analizzano pacf e acf dei residui 
acf <- ggAcf(M2$residuals, lag.max = 52) + ggtitle("Vendite1 day pre diff")
pacf <- ggPacf(M2$residuals, lag.max = 52) + ggtitle("Vendite1 day pre diff")
grid.arrange(acf, pacf, ncol=2)

# si osserva che il lag 52 è molto correlato nell'acf
M2 <- Arima(train, order = c(2,0,0),seasonal =  list(order=c(0,0,1),period=52)) # in alternativa seasonal: (1,0,0)
summary(M2)
acf <- ggAcf(M2$residuals, lag.max = 52) + ggtitle("Vendite1 day pre diff")
pacf <- ggPacf(M2$residuals, lag.max = 52) + ggtitle("Vendite1 day pre diff")
grid.arrange(acf, pacf, ncol=2)
checkresiduals(M2)  # tutte le autocorrelazioni si trovano all'interno della banda, questo significa che i residui si comportano come un white noise
autoplot(forecast(M2, h=50)) + autolayer(test)

# si potrebbe procedere selezionando altri modelli variando i parametri p e q,
# optando per il modello il cui valore di AIC è minore


### Auto Arima----

vendite1_sett_avg_pre_split_auto <- ts_split(vendite1_sett_avg_pre)

# divisione in train e test
train_auto <- vendite1_sett_avg_pre_split_auto$train
test_auto <- vendite1_sett_avg_pre_split_auto$test

autoplot(vendite1_sett_avg_pre) +
  autolayer(train_auto, series="Training") +
  autolayer(train_auto, series="Test")

# auto.arima per selezione modello migliore
arima_diag(train_auto)
M3 <- auto.arima(train_auto, seasonal = T)

# per valutare la qualità del modello si possono inizialmente plottare i grafici
# ACF e PACF dei residui del modello
tsdisplay(residuals(M3), lag.max=15, main='Seasonal Model Residuals')

# AIC = 1052.04, si ottiene un valore migliore rispetto al modello precedente

accuracy(M3)
# MAPE = 6.050572, < 10, highly accurate forecasting (https://www.researchgate.net/publication/257812432_Using_the_R-MAPE_index_as_a_resistant_measure_of_forecast_accuracy)
# MASE = 0.4430427, < 1, buon risultato

summary(M3)

# si vuole verificare che non ci sia correlazione tra gli errori
checkresiduals(M3)
check_res(M3)
M3$residuals

# A Ljung-Box test can also indicate the presence of these correlations. 
# As long as we score a p-value > 0.05, there is a 95% chance the residuals are independent
acf(M3$residuals, lag.max=20, na.action=na.pass)
Box.test(M3$residuals, lag=20, type="Ljung-Box")  # p-value > 0.05 -> independent residuals
hist(M3$residuals)


# previsioni con test set
M3 %>%
  forecast(h=50) %>%  # h Number of periods for forecasting
  autoplot() + autolayer(test_auto)

forecast <- M3 %>%
  forecast(h=30)

# alternativa per vedere previsioni con test set
par(mfrow=c(1,1))
plot(forecast)
lines(test_auto, col="red")
legend("topleft",lty=1,bty = "n",col=c("red","blue"),c("testData","ARIMAPred"))

# valutazione qualità previsioni
accuracy(forecast, test)

M3 %>%
  forecast(h=106) %>%  # h Number of periods for forecasting
  autoplot() + autolayer(vendite1_sett_avg)


### Auto Arima con regressore----
ristorante1$neve <- as.numeric(ristorazione$neve)
ristorante1_pre_covid_neve <- ristorante1 %>%
  filter(ristorante1$data < reference_date) %>%
  select(neve, data)

neve_sett_avg_pre <- aggregate(neve ~ week_pre_covid, ristorante1_pre_covid_neve, sum)
neve1_sett_avg_pre <- neve_sett_avg_pre$neve
neve1_sett_avg_pre <- ts(neve1_sett_avg_pre,start=2017,frequency=52) 

neve_split <- ts_split(neve1_sett_avg_pre)
train_neve <- neve_split$train
test_neve <- neve_split$test

MR1 <- auto.arima(train_auto, D=1, xreg = train_neve)
summary(MR1)

pvalue = 2*pt(-825.0664/115.4211,115)
pvalue

MR1 %>%
  forecast(h=30, xreg = test_neve) %>%  # h Number of periods for forecasting
  autoplot() + autolayer(test_auto)




# PREVISIONE FATTURATO NO COVID RANDOM FOREST PRIMO RISTORANTE -------------------------------------------

# CONFRONTO ESTATE COVID 2020 & ESTATE NO COVID 2019 --------------------------------

# seleziono un periodo temporale che corrisponde all'estate
# ricavo due serie storiche, per 2019 e 2020

inizio_estate_2019 <- as.Date("2019-06-21", format = "%Y-%m-%d")
fine_estate_2019 <- as.Date("2019-09-22", format = "%Y-%m-%d")

inizio_estate_2020 <- as.Date("2020-06-21", format = "%Y-%m-%d")
fine_estate_2020 <- as.Date("2020-09-22", format = "%Y-%m-%d")


# confronto estati ristorante 1
source("~/Desktop/progetti uni github/progetto_dslab/other_scripts/confronti_estati_ristoranti/confronto_estati_rist_1.R")

# confronto estati ristorante 2
source("~/Desktop/progetti uni github/progetto_dslab/other_scripts/confronti_estati_ristoranti/confronto_estati_rist_2.R")

# confronto estati ristorante 3
source("~/Desktop/progetti uni github/progetto_dslab/other_scripts/confronti_estati_ristoranti/confronto_estati_rist_3.R")

# confronto estati ristorante 4
source("~/Desktop/progetti uni github/progetto_dslab/other_scripts/confronti_estati_ristoranti/confronto_estati_rist_4.R")

# confronto estati ristorante 5
source("~/Desktop/progetti uni github/progetto_dslab/other_scripts/confronti_estati_ristoranti/confronto_estati_rist_5.R")

# confronto estati ristorante 6
source("~/Desktop/progetti uni github/progetto_dslab/other_scripts/confronti_estati_ristoranti/confronto_estati_rist_6.R")
