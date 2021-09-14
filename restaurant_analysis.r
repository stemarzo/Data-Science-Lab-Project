# SETTING PROGETTO --------------------------------------------------------

set.seed(100)

# setting librerie
# Package names
packages <- c("readxl",  "readr", "forecast", "dplyr", "magrittr", "ggplot2",
              "forcats", "lubridate", "RQuantLib", "devtools", "patchwork", "KFAS",
              "caret", "tseries", "urca", "TSstudio", "gridExtra", "randomForest",
              "prophet", "xts", "corrplot", "rstan") 

# Install packages not yet installed
installed_packages <- packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(packages[!installed_packages])
}

# Packages loading
invisible(lapply(packages, library, character.only = TRUE))


# setting working directory
# working_dir = "C:/Users/Stefano/Documents/progetto_dslab/dati"
#working_dir = "C:/Users/Lorenzo/Desktop/Progetto ds lab/progetto_dslab/dati"
working_dir = "~/Desktop/progetti uni github/progetto_dslab/dati"
setwd(working_dir)

# functions 
mape <- function(actual,pred){
  mape <- mean(abs((actual - pred)/actual))*100
  return (mape)
}

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
ristorazione$stagione <- as.factor(ristorazione$stagione)

# aggiunta colonna mese
# ristorazione$mese <- format(ristorazione$data,"%B")
ristorazione$mese <- month(ristorazione$data)
ristorazione$mese <- as.factor(ristorazione$mese)


# colonna giorni festivi e feriali
ristorazione <- ristorazione %>%
  mutate(weekday = wday(data, week_start = getOption("lubridate.week.start", 1))) %>%
  mutate(tipo_giorno = case_when(
    (weekday %in% c(6,7)) ~ "weekend"
    , (weekday < 7) ~ "weekday"
    , TRUE ~ "other"
  )
  )
ristorazione$weekday <- as.factor(ristorazione$weekday)
ristorazione["tipo_giorno"][ristorazione["tipo_giorno"] == "weekend"] <- "1"
ristorazione["tipo_giorno"][ristorazione["tipo_giorno"] == "weekday"] <- "0"
colnames(ristorazione)[which(names(ristorazione) == "tipo_giorno")] <- "is_weekend"
ristorazione$is_weekend <- as.factor(ristorazione$is_weekend)


# colonna holiday
# lista vacanze italiane
# new_holiday = getHolidayList(calendar = "Italy", from=as.Date("2017-01-01"), to= as.Date("2021-04-12"), includeWeekends=FALSE)
# new_holiday <- append(new_holiday, c(as.Date("2017-04-16"), as.Date("2018-04-01"),
#                                      as.Date("2019-04-21"), as.Date("2021-04-04"), as.Date("2020-04-12")))
# for (i in 1:nrow(ristorazione)) {
#   ristorazione[i,"holiday"]<-ristorazione[i,"data"]%in% new_holiday
# }
# # conversione true/false -> 1/0
# ristorazione$holiday <- as.integer(ristorazione$holiday)

# risulta più comodo settare manulmente le i giorni di festività
holidays_2017 <- as.Date(c("2017-01-01", "2017-01-06", "2017-02-14", "2017-04-16", 
                           "2017-04-17", "2017-04-25", "2017-05-01", "2017-06-02", 
                           "2017-08-15", "2017-11-01", "2017-12-08", "2017-12-24", 
                           "2017-12-25", "2017-12-26", "2017-12-31"))

holidays_2018 <- as.Date(c("2018-01-01", "2018-01-06", "2018-02-14", "2018-04-01", 
                           "2018-04-02", "2018-04-25", "2018-05-01", "2018-06-02", 
                           "2018-08-15", "2018-11-01", "2018-12-08", "2018-12-24", 
                           "2018-12-25", "2018-12-26", "2018-12-31"))

holidays_2019 <- as.Date(c("2019-01-01", "2019-01-06", "2019-02-14", "2019-04-21", 
                           "2019-04-22", "2019-04-25", "2019-05-01", "2019-06-02", 
                           "2019-08-15", "2019-11-01", "2019-12-08", "2019-12-24", 
                           "2019-12-25", "2019-12-26", "2019-12-31"))

holidays_2020 <- as.Date(c("2020-01-01", "2020-01-06", "2020-02-14", "2020-04-12", 
                           "2020-04-13", "2020-04-25", "2020-05-01", "2020-06-02", 
                           "2020-08-15", "2020-11-01", "2020-12-08", "2020-12-24", 
                           "2020-12-25", "2020-12-26", "2020-12-31"))

holidays_2021 <- as.Date(c("2021-01-01", "2021-01-06", "2021-02-14", "2021-04-04", 
                           "2021-04-05", "2021-04-25", "2021-05-01", "2021-06-02", 
                           "2021-08-15", "2021-11-01", "2021-12-08", "2021-12-24", 
                           "2021-12-25", "2021-12-26", "2021-12-31"))

holidays_2017_to_2021 <- c(holidays_2017, holidays_2018, holidays_2019, holidays_2020, holidays_2021)

ristorazione$is_holiday <- 0
ristorazione$is_holiday[which(ristorazione$data %in% holidays_2017_to_2021)] <- 1
ristorazione$is_holiday <- as.factor(ristorazione$is_holiday)



# colore zona in base alla data
colori_zone <- read_csv("colori_zone.csv")  # https://github.com/imcatta/restrizioni_regionali_covid/blob/main/dataset.csv
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
ristorazione$colore_lombardia[is.na(ristorazione$colore_lombardia)] <- "bianco"
ristorazione$colore_emilia_romagna[is.na(ristorazione$colore_emilia_romagna)] <- "bianco"

# sistemazione manuale colori zone (per emilia romagna, colonna che verrà effettivamente utilizzata)
ristorazione[1:1164,"colore_emilia_romagna"]<-"bianco"
ristorazione[1165:1234,"colore_emilia_romagna"]<-"rosso"
ristorazione[1235:1258,"colore_emilia_romagna"]<-"arancione"
ristorazione[1259:1405,"colore_emilia_romagna"]<-"giallo"

ristorazione$colore_lombardia <- as.factor(ristorazione$colore_lombardia)
ristorazione$colore_emilia_romagna <- as.factor(ristorazione$colore_emilia_romagna)

# aggiungo colonna zona rossa lombardia
ristorazione$rossa_lombardia <- ifelse(ristorazione$colore_lombardia == "rosso", 1, 0)
ristorazione$rossa_lombardia <- as.factor(ristorazione$rossa_lombardia)

# aggiungo colonna zona rossa emilia romagna
ristorazione$rossa_emilia_romagna <- ifelse(ristorazione$colore_emilia_romagna == "rosso", 1, 0)
ristorazione$rossa_emilia_romagna <- as.factor(ristorazione$rossa_emilia_romagna)

# colonna asporto (per Lombardia, bisogna decidere quale regione)
# zona rossa: solo asporto e consegne a domicilio senza limiti
# zona arancione: solo asporto e consegne a domicilio senza limiti
# zona gialla: si può mangiare in presenza e consegne a domicilio senza limiti
ristorazione <- ristorazione %>%
  mutate(solo_asporto_lombardia = case_when(
    (colore_lombardia == "rosso") ~ TRUE
    ,(colore_lombardia == "arancione") ~ TRUE
    , (colore_lombardia == "giallo") ~ FALSE
    , TRUE ~ FALSE
  ) 
  )    


# conversione true/false -> 1/0
ristorazione$solo_asporto_lombardia <- as.integer(ristorazione$solo_asporto_lombardia)
ristorazione$solo_asporto_lombardia <- as.factor(ristorazione$solo_asporto_lombardia )


ristorazione <- ristorazione %>%
  mutate(solo_asporto_emilia_romagna = case_when(
    (colore_lombardia == "rosso") ~ TRUE
    ,(colore_lombardia == "arancione") ~ TRUE
    , (colore_lombardia == "giallo") ~ FALSE
    , TRUE ~ FALSE
  ) 
  ) 
# conversione true/false -> 1/0
ristorazione$solo_asporto_emilia_romagna <- as.integer(ristorazione$solo_asporto_emilia_romagna)
ristorazione$solo_asporto_emilia_romagna <- as.factor(ristorazione$solo_asporto_emilia_romagna)

# inserimento delle colonne che riguardano gli eventi sportivi
# eventi_sportivi <- read_delim("eventi_sportivi.csv", ";", escape_double = FALSE, trim_ws = TRUE)
# eventi_sportivi <- eventi_sportivi[, -c(1)]
# cols <- colnames(eventi_sportivi)
# eventi_sportivi[cols] <- lapply(eventi_sportivi[cols], factor) 
# ristorazione<-merge(x=ristorazione,y=eventi_sportivi,by="data",all.x=TRUE)

# inserimento colonne riguardanti il meteo
meteo <- read_delim("meteo.csv", ";", escape_double = FALSE, trim_ws = TRUE)
meteo <- meteo[, -c(1)]
meteo$data<-as.Date(meteo$data, format = "%d/%m/%Y")
ristorazione<-merge(x=ristorazione,y=meteo[,-3],by="data",all.x=TRUE)
ristorazione$pioggia <- as.factor(ristorazione$pioggia)

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
ristorazione$tot_vaccini_lombardia[is.na(ristorazione$tot_vaccini_lombardia)] <- 0
ristorazione$tot_vaccini_emilia_romagna[is.na(ristorazione$tot_vaccini_emilia_romagna)] <- 0


# check NA values
sum(is.na(ristorazione$data))  # 0 NA 

sum(is.na(ristorazione$data_anno_prec))  # 68 NA, viene 
# which(is.na(ristorazione$data_anno_prec))

# per ogni ristorante viene verificata la presenza di valori NA o meno
# i seguenti NA potrebbero essere dovuti a giorni senza vendite oppure ad errori di imputazione

sum(is.na(ristorazione$vendite1))  # 68 NA
# which(is.na(ristorazione$vendite1))
# subset(ristorazione[,c(1,3)], is.na(ristorazione$vendite1))
# 26 febbraio 2018 - 2 marzo 2018: ondata di gelo e neve
# 31 maggio 2018: ?
# 12 marzo 2020 - 6 maggio 2020: covid
# 25-26 dicembre 2020, 1,6 gennaio 2021: feste
# 4-5 aprile 2021: pasqua


sum(is.na(ristorazione$vendite2))  # 55 NA
# which(is.na(ristorazione$vendite2))
# subset(ristorazione[,c(1,5)], is.na(ristorazione$vendite2))
# 12 marzo 2020 - 3 maggio 2020: covid
# 25 dicembre 2020: festa
# 4 aprile 2021: pasqua


sum(is.na(ristorazione$vendite3))  # 1096 NA
# which(is.na(ristorazione$vendite3))
# subset(ristorazione[,c(1,7)], is.na(ristorazione$vendite3))
# fino al 8 novembre 2019: ristorante non attivo, se non per alcuni giorni (test)
# 12 marzo 2020 - 6 maggio 2020: covid
# 25-26 dicembre 2020, 1 gennaio 2021: feste
# 4-5 aprile 2021: pasqua

sum(is.na(ristorazione$vendite4))  # 108 NA
# which(is.na(ristorazione$vendite4))
# subset(ristorazione[,c(1,9)], is.na(ristorazione$vendite4))
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
# which(is.na(ristorazione$vendite5))
# subset(ristorazione[,c(1,11)], is.na(ristorazione$vendite5))
# 9 aprile - 13 aprile 2018: ?
# 16 maggio 2018: ?
# 5 giugno 2018: ?
# 12 marzo 2020 - 3 maggio 2020: covid
# 25 dicembre 2020: festa
# 4 aprile 2021: pasqua


sum(is.na(ristorazione$vendite6))  # 319 NA
# which(is.na(ristorazione$vendite6))
# subset(ristorazione[,c(1,13)], is.na(ristorazione$vendite6))
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
names(other_dates) <- c("vacanze_scolastiche_lombardia", "black_friday_saldi_lombardia",
                        "vacanze_scolastiche_emilia_romagna", "black_friday_saldi_emilia_romagna")
ristorazione <- cbind(ristorazione, other_dates)

ristorazione$vacanze_scolastiche_lombardia <- as.factor(ristorazione$vacanze_scolastiche_lombardia)
ristorazione$vacanze_scolastiche_emilia_romagna <- as.factor(ristorazione$vacanze_scolastiche_emilia_romagna)

ristorazione$black_friday_saldi_lombardia <- as.factor(ristorazione$black_friday_saldi_lombardia)
ristorazione$black_friday_saldi_emilia_romagna <- as.factor(ristorazione$black_friday_saldi_emilia_romagna)

# correzione manuale colonna saldi e black friday per Emilia Romagna
ristorazione[, "black_friday_saldi_emilia_romagna"] <- 0
ristorazione[5:64, "black_friday_saldi_emilia_romagna"] <- 1
ristorazione[182:242, "black_friday_saldi_emilia_romagna"] <- 1
ristorazione[328, "black_friday_saldi_emilia_romagna"] <- 1
ristorazione[370:428, "black_friday_saldi_emilia_romagna"] <- 1
ristorazione[549:609, "black_friday_saldi_emilia_romagna"] <- 1
ristorazione[692, "black_friday_saldi_emilia_romagna"] <- 1
ristorazione[735:794, "black_friday_saldi_emilia_romagna"] <- 1
ristorazione[917:972, "black_friday_saldi_emilia_romagna"] <- 1
ristorazione[1063, "black_friday_saldi_emilia_romagna"] <- 1
ristorazione[1100:1160, "black_friday_saldi_emilia_romagna"] <- 1
ristorazione[1309:1369, "black_friday_saldi_emilia_romagna"] <- 1
ristorazione[1427, "black_friday_saldi_emilia_romagna"] <- 1
ristorazione[1491:1550, "black_friday_saldi_emilia_romagna"] <- 1
ristorazione$black_friday_saldi_emilia_romagna <- as.factor(ristorazione$black_friday_saldi_emilia_romagna)


# aggiunta colonna neve
# meteo <- read_delim("meteo2.csv", ";", escape_double = FALSE, trim_ws = TRUE)
# meteo <- meteo[, -c(1)]
# meteo$data<-as.Date(meteo$data, format = "%d/%m/%Y")
# ristorazione<-merge(x=ristorazione,y=meteo,by="data",all.x=TRUE)

# aggiunta colonna covid
ristorazione$covid <- 0
ristorazione[ristorazione$data > "2020-03-09",]$covid <- 1
ristorazione$covid  <- as.factor(ristorazione$covid )


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
ristorante1$chiuso <- as.factor(ristorante1$chiuso)

ristorante1 <- merge(x = ristorante1, y = ristorazione[ , -c(2:14)], by = "data", all.x=TRUE)

# ristorante1$covid <- ristorazione$covid
# ristorante1$rossa <- ristorazione$rossa_emilia_romagna


#ristorante2
ristorante2 <- data.frame(col_date, ristorazione$vendite2, ristorazione$scontrini2)
colnames(ristorante2) <- col_nomi
ristorante2$vendite[is.na(ristorante2$vendite)] <- 0 
ristorante2$scontrini[is.na(ristorante2$scontrini)] <- 0  
ristorante2$rapprto_v_s <- ristorante2$vendite/ristorante2$scontrini
ristorante2$rapprto_v_s[is.na(ristorante2$rapprto_v_s)] <- 0
ristorante2$chiuso <- 0
ristorante2$chiuso <- ifelse(ristorante2$rapprto_v_s == 0, 1, 0)
ristorante2$chiuso <- as.factor(ristorante2$chiuso)

ristorante2 <- merge(x = ristorante2, y = ristorazione[ , -c(2:14)], by = "data", all.x=TRUE)

# ristorante2$covid <- ristorazione$covid
# ristorante2$rossa <- ristorazione$rossa_emilia_romagna


#ristorante3
ristorante3 <- data.frame(col_date, ristorazione$vendite3, ristorazione$scontrini3)
colnames(ristorante3) <- col_nomi
ristorante3$vendite[is.na(ristorante3$vendite)] <- 0 
ristorante3$scontrini[is.na(ristorante3$scontrini)] <- 0  
ristorante3$rapprto_v_s <- ristorante3$vendite/ristorante3$scontrini
ristorante3$rapprto_v_s[is.na(ristorante3$rapprto_v_s)] <- 0
ristorante3$chiuso <- 0
ristorante3$chiuso <- ifelse(ristorante3$rapprto_v_s == 0, 1, 0)
ristorante3$chiuso <- as.factor(ristorante3$chiuso)

ristorante3 <- merge(x = ristorante3, y = ristorazione[ , -c(2:14)], by = "data", all.x=TRUE)


# ristorante3$covid <- ristorazione$covid
# ristorante3$rossa <- ristorazione$rossa_emilia_romagna


#ristorante4
ristorante4 <- data.frame(col_date,ristorazione$vendite4, ristorazione$scontrini4)
colnames(ristorante4) <- col_nomi
ristorante4$vendite[is.na(ristorante4$vendite)] <- 0  
ristorante4$scontrini[is.na(ristorante4$scontrini)] <- 0  
ristorante4$rapprto_v_s <- ristorante4$vendite/ristorante4$scontrini
ristorante4$rapprto_v_s[is.na(ristorante4$rapprto_v_s)] <- 0
ristorante4$chiuso <- 0
ristorante4$chiuso <- ifelse(ristorante4$rapprto_v_s == 0, 1, 0)
ristorante4$chiuso <- as.factor(ristorante4$chiuso)

ristorante4 <- merge(x = ristorante4, y = ristorazione[ , -c(2:14)], by = "data", all.x=TRUE)

# ristorante4$covid <- ristorazione$covid
# ristorante4$rossa <- ristorazione$rossa_emilia_romagna

#ristorante5
ristorante5 <- data.frame(col_date, ristorazione$vendite5, ristorazione$scontrini5)
colnames(ristorante5) <- col_nomi
ristorante5$vendite[is.na(ristorante5$vendite)] <- 0 
ristorante5$scontrini[is.na(ristorante5$scontrini)] <- 0  
ristorante5$rapprto_v_s <- ristorante5$vendite/ristorante5$scontrini
ristorante5$rapprto_v_s[is.na(ristorante5$rapprto_v_s)] <- 0
ristorante5$chiuso <- 0
ristorante5$chiuso <- ifelse(ristorante5$rapprto_v_s == 0, 1, 0)
ristorante5$chiuso <- as.factor(ristorante5$chiuso)

ristorante5 <- merge(x = ristorante5, y = ristorazione[ , -c(2:14)], by = "data", all.x=TRUE)

# ristorante5$covid <- ristorazione$covid
# ristorante5$rossa <- ristorazione$rossa_emilia_romagna


#ristorante6
ristorante6 <- data.frame(col_date, ristorazione$vendite6, ristorazione$scontrini6)
colnames(ristorante6) <- col_nomi
ristorante6$vendite[is.na(ristorante6$vendite)] <- 0  
ristorante6$scontrini[is.na(ristorante6$scontrini)] <- 0  
ristorante6$rapprto_v_s <- ristorante6$vendite/ristorante6$scontrini
ristorante6$rapprto_v_s[is.na(ristorante6$rapprto_v_s)] <- 0
ristorante6$chiuso <- 0
ristorante6$chiuso <- ifelse(ristorante6$rapprto_v_s == 0, 1, 0)
ristorante6$chiuso <- as.factor(ristorante6$chiuso)

ristorante6 <- merge(x = ristorante6, y = ristorazione[ , -c(2:14)], by = "data", all.x=TRUE)

# ristorante6$covid <- ristorazione$covid
# ristorante6$rossa <- ristorazione$rossa_emilia_romagna

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
# source("C:/Users/Stefano/Documents/progetto_dslab/other_scripts/esplorazione_ristoranti/esplorazione_rist_1.R")

# esplorazione ristorante 2
source("~/Desktop/progetti uni github/progetto_dslab/other_scripts/esplorazione_ristoranti/esplorazione_rist_2.R")
# source("C://Users/Stefano/Documents/progetto_dslab/other_scripts/esplorazione_ristoranti/esplorazione_rist_2.R")

# esplorazione ristorante 3
source("~/Desktop/progetti uni github/progetto_dslab/other_scripts/esplorazione_ristoranti/esplorazione_rist_3.R")
# source("C://Users/Stefano/Documents/progetto_dslab/other_scripts/esplorazione_ristoranti/esplorazione_rist_3.R")

# esplorazione ristorante 4
source("~/Desktop/progetti uni github/progetto_dslab/other_scripts/esplorazione_ristoranti/esplorazione_rist_4.R")
# source("C://Users/Stefano/Documents/progetto_dslab/other_scripts/esplorazione_ristoranti/esplorazione_rist_4.R")

# esplorazione ristorante 5
source("~/Desktop/progetti uni github/progetto_dslab/other_scripts/esplorazione_ristoranti/esplorazione_rist_5.R")
# source("C://Users/Stefano/Documents/progetto_dslab/other_scripts/esplorazione_ristoranti/esplorazione_rist_5.R")

# esplorazione ristorante 6
source("~/Desktop/progetti uni github/progetto_dslab/other_scripts/esplorazione_ristoranti/esplorazione_rist_6.R")
# source("C:/Users/Stefano/Documents/progetto_dslab/other_scripts/esplorazione_ristoranti/esplorazione_rist_6.R")


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




# ANALISI TREND SCONTRINI --------------------------------

# ristorante 1
trend_scontrini_df_rist1 <- data.frame(ristorante1$rapprto_v_s ,ristorante1$scontrini)
names(trend_scontrini_df_rist1) <- c("prezzo medio scontrino", "scontrini registrati")
trend_scontrini_ts_rist1 <- ts(trend_scontrini_df_rist1, start = 2017,frequency = 365)
autoplot(trend_scontrini_ts_rist1, facets = 1, main = "Ristorante 1: analisi trend scontrini")

# ristorante 2
trend_scontrini_df_rist2 <- data.frame(ristorante2$rapprto_v_s ,ristorante2$scontrini)
names(trend_scontrini_df_rist2) <- c("prezzo medio scontrino", "scontrini registrati")
trend_scontrini_ts_rist2 <- ts(trend_scontrini_df_rist2, start = 2017,frequency = 365)
autoplot(trend_scontrini_ts_rist2, facets = 1, main = "Ristorante 2: analisi trend scontrini")

# ristorante 3
trend_scontrini_df_rist3 <- data.frame(ristorante3$rapprto_v_s ,ristorante3$scontrini)
names(trend_scontrini_df_rist3) <- c("prezzo medio scontrino", "scontrini registrati")
trend_scontrini_ts_rist3 <- ts(trend_scontrini_df_rist3, start = decimal_date(as.Date("2019-11-04")),frequency = 365)
autoplot(trend_scontrini_ts_rist3, facets = 1, main = "Ristorante 3: analisi trend scontrini")

# ristorante 4
trend_scontrini_df_rist4 <- data.frame(ristorante4$rapprto_v_s ,ristorante4$scontrini)
names(trend_scontrini_df_rist4) <- c("prezzo medio scontrino", "scontrini registrati")
trend_scontrini_ts_rist4 <- ts(trend_scontrini_df_rist4, start = 2017,frequency = 365)
autoplot(trend_scontrini_ts_rist4, facets = 1, main = "Ristorante 4: analisi trend scontrini")

# ristorante 5
trend_scontrini_df_rist5 <- data.frame(ristorante5$rapprto_v_s ,ristorante5$scontrini)
names(trend_scontrini_df_rist5) <- c("prezzo medio scontrino", "scontrini registrati")
trend_scontrini_ts_rist5 <- ts(trend_scontrini_df_rist5, start = 2017,frequency = 365)
autoplot(trend_scontrini_ts_rist5, facets = 1, main = "Ristorante 5: analisi trend scontrini")

# ristorante 6
trend_scontrini_df_rist6 <- data.frame(ristorante6$rapprto_v_s ,ristorante6$scontrini)
names(trend_scontrini_df_rist6) <- c("prezzo medio scontrino", "scontrini registrati")
trend_scontrini_ts_rist6 <- ts(trend_scontrini_df_rist6, start = decimal_date(as.Date("2017-09-21")),frequency = 365)
autoplot(trend_scontrini_ts_rist6, facets = 1, main = "Ristorante 6: analisi trend scontrini")




# PREVISIONE FATTURATO PERIODO COVID PRIMO RISTORANTE -------------------------------------------
# NOTA BENE:
# per i modelli a  seguire è stata scelta coem data di inizio covid (colonna covid)
# il 9 marzo, data in cui l'Italia entra in lockdown. Il 5 gennaio invece viene 
# scelto come data di split tra train e test set in modo tale che il train non subisca
# influenze del covid

### Arima manuale----
# le vendite settimanali pre covid vengono divise in train e test per cercare di modellare
# i dati a disposizione e cercare di valutarne la qualità del modello ottenuto.
# Il seguente modello non viene utilizzato per fare previsioni

# per rendere stazionaria la serie storica, bisogna eliminare stagionalità e trend

# rimozione stagionalità
vendite1_sett_avg_pre_dest_arima <- seasadj(stl(vendite1_sett_avg_pre, s.window = 52))
autoplot(vendite1_sett_avg_pre_dest_arima) + autolayer(vendite1_sett_avg_pre)

# verifica se necessario differenziare
ndiffs(vendite1_sett_avg_pre_dest_arima)
nsdiffs(vendite1_sett_avg_pre_dest_arima)

# rimozione trend con differenziazione
vendite1_sett_avg_pre_dest_diff_arima <- diff(vendite1_sett_avg_pre_dest_arima)
autoplot(vendite1_sett_avg_pre_dest_diff_arima)

# verifica stazionarietà
adf.test(vendite1_sett_avg_pre_dest_diff_arima, alternative = "stationary")  
# The null-hypothesis for an ADF test is that the data are non-stationary. 
# So p-value greater than 0.05 indicates non-stationarity, and  p-values less than 
# 0.05 suggest stationarity.
# risulta essere stazionaria

kpss.test(vendite1_sett_avg_pre_dest_diff_arima)
# In this case, the null-hypothesis is that the data are stationary. In this case, 
# p-value less than 0.05 indicates non-stationary series and p-value greater than 
# 0.05 indicates stationary series.
# risulta essere stazionaria

summary(ur.kpss(vendite1_sett_avg_pre_dest_diff_arima ))
ndiffs(vendite1_sett_avg_pre_dest_diff_arima)
nsdiffs(vendite1_sett_avg_pre_dest_diff_arima)

# una volta ottenuta la serie storica stazionaria si procede con la creazione
# di train e test
vendite1_sett_avg_pre_split_arima <- ts_split(vendite1_sett_avg_pre_dest_diff_arima)

# divisione in train e test
train_arima <- vendite1_sett_avg_pre_split_arima$train
test_arima <- vendite1_sett_avg_pre_split_arima$test
autoplot(train_arima)
autoplot(test_arima)

autoplot(vendite1_sett_avg_pre_dest_diff_arima) +
  autolayer(train_arima, series="train_arima") +
  autolayer(test_arima, series="test_arima")

# si analizzano acf e pacf del train, per la scelta dei parametri p e q
acf <- ggAcf(train_arima, lag.max = 52) + ggtitle("Vendite1 day pre diff")
pacf <- ggPacf(train_arima, lag.max = 52) + ggtitle("Vendite1 day pre diff")
grid.arrange(acf, pacf, ncol=2)

# si inzia provando con un ar 2, osservando che nel pacf due lag sono 
# significativamente correlati
M1 <- Arima(train_arima, order = c(2,0,0))

# si analizzano pacf e acf dei residui 
acf <- ggAcf(M1$residuals, lag.max = 52) + ggtitle("Vendite1 day pre diff")
pacf <- ggPacf(M1$residuals, lag.max = 52) + ggtitle("Vendite1 day pre diff")
grid.arrange(acf, pacf, ncol=2)

# si osserva che il lag 52 è molto correlato nell'acf, si corregge il modello
M1 <- Arima(train_arima, order = c(2,0,0),seasonal =  list(order=c(0,0,1),period=52)) # in alternativa seasonal: (1,0,0)
summary(M1)

# si analizzano nuovamente pacf e acf dei residui 
acf <- ggAcf(M1$residuals, lag.max = 52) + ggtitle("Vendite1 day pre diff")
pacf <- ggPacf(M1$residuals, lag.max = 52) + ggtitle("Vendite1 day pre diff")
grid.arrange(acf, pacf, ncol=2)
checkresiduals(M1)  # tutte le autocorrelazioni si trovano all'interno della banda, questo significa che i residui si comportano come un white noise
autoplot(forecast(M1, h=50)) + autolayer(test_arima)

# si potrebbe procedere selezionando altri modelli variando i parametri p e q,
# optando per il modello il cui valore di AIC è minore


### Auto Arima senza regressori ----
# le vendite settimanali pre covid vengono divise in train e test per cercare di modellare
# i dati a disposizione e cercare di valutarne la qualità del modello ottenuto.
# Il seguente modello viene utilizzato per fare previsioni su valori futuri, in 
# particolar modo per prevedere come le vendite sarebbero andate durante il periodo
# covid, durante il quale per alcune settimane le vendite effettive invece sono 
# state pari a zero

# non è necessario rendere la serie storica stazionaria, ciò viene fatto in modo
# automatico da auto arima

# divisione in train e test
vendite1_sett_avg_pre_split_auto_arima <- ts_split(vendite1_sett_avg_pre)
train_auto_arima <- vendite1_sett_avg_pre_split_auto_arima$train
test_auto_arima <- vendite1_sett_avg_pre_split_auto_arima$test

autoplot(vendite1_sett_avg_pre) +
  autolayer(train_auto_arima, series="Training") +
  autolayer(train_auto_arima, series="Test")

# auto.arima per selezione modello migliore
arima_diag(train_auto_arima)
M2 <- auto.arima(train_auto_arima, seasonal = T)
# i dati vengono addestrati sul train e poi viene valutato il modello sul test

# per valutare la qualità del modello si possono inizialmente plottare i grafici
# ACF e PACF dei residui del modello
tsdisplay(residuals(M2), lag.max=15, main='Seasonal Model Residuals')

# AIC = 1052.04, si ottiene un valore migliore rispetto al modello precedente

accuracy(M2)
# MAPE = 6.050572, < 10, highly accurate forecasting (https://www.researchgate.net/publication/257812432_Using_the_R-MAPE_index_as_a_resistant_measure_of_forecast_accuracy)
# MASE = 0.4430427, < 1, buon risultato

summary(M2)

# si vuole verificare che non ci sia correlazione tra gli errori
checkresiduals(M2)
check_res(M2)
M2$residuals

# A Ljung-Box test can also indicate the presence of these correlations. 
# As long as we score a p-value > 0.05, there is a 95% chance the residuals are independent
acf(M2$residuals, lag.max=20, na.action=na.pass)
Box.test(M2$residuals, lag=20, type="Ljung-Box")  # p-value > 0.05 -> independent residuals
hist(M2$residuals)

#  considerando test set
M2 %>%
  forecast(h=50) %>%  # h Number of periods for forecasting
  autoplot() + autolayer(test_auto_arima)

# alternativa per verifica addatamento dati con test set
forecast_covid_auto_arima <- M2 %>%
  forecast(h=30)

par(mfrow=c(1,1))
plot(forecast_covid_auto_arima)
lines(test_auto_arima, col="red")
legend("topleft",lty=1,bty = "n",col=c("red","blue"),c("testData","ARIMAPred"))

# valutazione qualità previsioni
accuracy(forecast_covid_auto_arima, test_auto_arima)

# si procede ora utilizzando il modello ottenuto per fare previsioni su dati nuovi,
# per capire come sarebbero andate le vendite se non ci fosse stato il covid

M2 %>%
  forecast(h=106) %>%  # h Number of periods for forecasting
  autoplot() + autolayer(vendite1_sett_avg)
# CHECK IT ULTIMI GRAFICI (controllare da dove parte previsione)


### Random forest----
# https://www.pluralsight.com/guides/machine-learning-for-time-series-data-in-r
# si opera su dati giornalieri
# si addestra il modello sui dati pre covid (ristorante1_pre_covid$vendite) e 
# si prevedono le vendite nel periodo covid

# divisione in train e test
index_rf <- sample(1:nrow(ristorante1_pre_covid),
                   size = 0.7*nrow(ristorante1_pre_covid))
train_rf <- ristorante1_pre_covid[index_rf,]
test_rf <- ristorante1_pre_covid[-index_rf,] 

# implementazione modelli
M3 <- randomForest(vendite ~ is_holiday + is_weekend + pioggia + covid + stagione 
                   + weekday + solo_asporto_emilia_romagna + rossa_emilia_romagna
                   + tot_vaccini_emilia_romagna + mese, data = train_rf)
varImpPlot(M3)
print(M3)

# si selezionano le variabili più rilevanti
M4 <- randomForest(vendite ~ weekday + is_weekend + mese + is_holiday + stagione,
                   data = train_rf)
varImpPlot(M4)
print(M4)

predictions_rf <- predict(M4, newdata = train_rf)
mape(train_rf$vendite, predictions_rf)

predictions_rf <- predict(M4, newdata = test_rf)
mape(test_rf$vendite, predictions_rf)

RMSE.rf <- sqrt(mean((predictions_rf-test_rf$vendite)^2))
RMSE.rf

MAE.rf <- mean(abs(predictions_rf-test_rf$vendite))
MAE.rf

# predizioni su valori nuovi (sul periodo covid dove nei dati reali si hanno 0)

# selezione periodo covid (su cui verranno fatte le previsioni)
reference_date_rf <- as.Date("2020-01-06", format = "%Y-%m-%d")
ristorante1_periodo_covid <- ristorante1 %>%
  filter(ristorante1$data >= reference_date_rf)

# si seleziona la lunghezza del periodo da prevedere
# viene selezionato un periodo che include i giorni in cui si registrano 0 
# vendite/scontrini
ristorante1_periodo_covid <- ristorante1_periodo_covid[1:133,]

# si utilizza il modello appena creato per fare previsioni
vendite_forecast_rf <- predict(M4, ristorante1_periodo_covid)
vendite_forecast_rf <- as.data.frame(vendite_forecast_rf)

# si uniscono le due serie storiche

# serie storica previsioni durante periodo covid
interval_covid <- seq(as.Date("2020-01-06"), as.Date("2020-05-17"), by = "day")
interval_covid_df <- data.frame(date = interval_covid, 
                                val=vendite_forecast_rf)
interval_covid_df$date <- as.Date(interval_covid_df$date)  
interval_covid_ts <- xts(interval_covid_df$val, interval_covid_df$date)

plot(interval_covid_df$date, interval_covid_df$vendite_forecast_rf, xlab = "data", ylab = "vendite", type="l", main = "Ristorante 1")

# serie storica dati reali fino a prima covid (ristorante1_pre_covid$vendite)
interval_pre_covid <- seq(as.Date("2017-01-01"), as.Date("2020-01-05"), by = "day")
interval_pre_covid_df <- data.frame(date = interval_pre_covid, 
                                    val=ristorante1_pre_covid$vendite)

interval_pre_covid_df$date<-as.Date(interval_pre_covid_df$date)  
interval_covid_ts_pre <- xts(interval_pre_covid_df$val, interval_pre_covid_df$date)

# si uniscono le due serie storiche
names(interval_covid_df)[1] <- "data"
names(interval_covid_df)[2] <- "vendite"

names(interval_pre_covid_df)[1] <- "data"
names(interval_pre_covid_df)[2] <- "vendite"

interval_complete <- rbind(interval_covid_df, interval_pre_covid_df)
interval_complete <- interval_complete[order(interval_complete$data), ]
row.names(interval_complete) <- NULL

# serie storica con previsioni
plot(interval_complete$data, interval_complete$vendite, xlab = "data", ylab = "vendite", type="l", main = "Ristorante 1 previsioni")

# serie storica originale
ristorazione_temp <- ristorazione[1:1228,]
ristorazione_temp$vendite1[is.na(ristorazione_temp$vendite1)] <- 0 
plot(ristorazione_temp$data, ristorazione_temp$vendite1, xlab = "data", ylab = "vendite", type="l", main = "Ristorante 1 dati reali")


### Prophet----
# si opera su dati giornalieri
# si addestra il modello sui dati pre covid e si prevedono le vendite nel periodo covid
prophet_vendite <- ristorante1_pre_covid %>% 
  select(data, vendite)
colnames(prophet_vendite) <- c("ds", "y")

# si crea il modello
M5 <- prophet(prophet_vendite)

# vengono fatte le previsioni
future_prophet <- make_future_dataframe(M5, periods=365)
vendite_forecast_prophet <- predict(M5, future_prophet)
tail(vendite_forecast_prophet[c('ds', 'yhat', 'yhat_lower', 'yhat_upper')])
# yhat containing the vendite_forecast_prophet. It has additional columns for uncertainty 
# intervals and seasonal components

plot(M5, vendite_forecast_prophet)
prophet_plot_components(M5, vendite_forecast_prophet)
# prophet_vendite.cv <- cross_validation(M5, initial=180, period=60, horizon=120, units='days')
# plot_cross_validation_metric(prophet_vendite.cv, metric='mape')
dyplot.prophet(M5, vendite_forecast_prophet)


# PREVISIONE FATTURATO POST APRILE 2021 PRIMO RISTORANTE -------------------------------------------

### HoltWinters----
# si addestra il modello su tutti i dati a disposizione (settimanali) per poi fare previsioni
# per il periodo post aprile, periodo per cui non si hanno a disposizione valori 
# relativi a vendite e scontrini

# metodo lisciamento esponenziale
M6 <- HoltWinters(vendite1_sett_avg)  # il modello permette di catturare trend e stagionalità
plot(M6)

# parametri
M6$alpha
M6$beta
M6$gamma

# analisi residui
acf(residuals(M6), lag = 52)

# previsione
prev <- forecast(M6, h=10)
autoplot(prev)

# in alternativa
prev <- predict(M6, 10, prediction.interval=TRUE)
plot(M6, prev)

### Auto Arima con regressori----
# il modello viene addestrato su tutti i dati a disposizione (settimanali, vendite1_sett_avg) 
# per poi utilizzarlo per effettuare previsioni su date per cui non si hanno a 
# disposizione i dati reali di vendite e scontrini, dunque oltre aprile 2021. 
# In questo caso non vi è una suddivisione in train e test (approccio statistico)

# si considera la regione emilia-romagna

# NOTA BENE
# Splitting in Train and Test sets or not depends from the purpose of your analysis. 
# You can follow a statistical approach or a machine learning approach.
# 
# In the classical, statistical approach, you fit a model on the whole batch of data. 
# Your goal here is to check the sign of the variables' parameters, and whether 
# they are significant or not. Scientifically speaking, each of those parameters 
# represents the test of an hypothesis. <- è questo caso
# 
# In the machine learning approach, you want a model that is good at predicting 
# data it has never seen before. You don't care whether a given variable has a 
# positive or negative association with you dependend variable, you don't 
# care whether your parameters are 95% significant or not, you just care 
# that the model predicts the output as precisely as possible.

# si procede considerando i dati su base settimanale

# setting regressori

# dati covid su base settimanale (somma, si contano i giorni della settimana in cui c'è il covid)
ristorante1$covid <- as.numeric(as.character(ristorante1$covid))
week_covid_sum <- aggregate(covid ~ week_rist1, ristorante1[-c(1,1563),], sum)  
week_covid_sum <- week_covid_sum$covid
ristorante1$covid <- as.factor(ristorante1$covid)

# dati chiuso su base settimanale (somma, si contano i giorni della settimana in cui il ristorante è chiuso, ovvero non ci sono state vendite)
ristorante1$chiuso <- as.numeric(as.character(ristorante1$chiuso))
week_chiuso_sum <- aggregate(chiuso ~ week_rist1, ristorante1[-c(1,1563),], sum) 
week_chiuso_sum <- week_chiuso_sum$chiuso
ristorante1$chiuso <- as.factor(ristorante1$chiuso)

# dati rossa su base settimanale (somma, si contano i giorni della settimana in cui c'è zona rossa)
ristorante1$rossa_emilia_romagna <- as.numeric(as.character(ristorante1$rossa_emilia_romagna))
week_rossa_sum <- aggregate(rossa_emilia_romagna ~ week_rist1, ristorante1[-c(1,1563),], sum)
week_rossa_sum <- week_rossa_sum$rossa_emilia_romagna
ristorante1$rossa_emilia_romagna <- as.factor(ristorante1$rossa_emilia_romagna)

regressori_week <- data.frame(week_covid_sum, week_chiuso_sum, week_rossa_sum)

# trasformazione colonne precedenti in valori binari
regressori_week <- regressori_week %>%
  mutate(week_covid_bin = ifelse(week_covid_sum>0, 1, 0))  # se almeno un giorno durante la settimana ha registrato il covid allora tale settimana viene etichettata come settimana covid
#regressori_week$week_covid_bin <- as.factor(regressori_week$week_covid_bin)

regressori_week <- regressori_week %>%
  mutate(week_chiuso_bin = ifelse(week_chiuso_sum>4, 1, 0))  # se un ristorante durante la settimana rimane chiuso per più di 4 giorni allora tale settimana viene etichettata come settimana chiusa
#regressori_week$week_chiuso_bin <- as.factor(regressori_week$week_chiuso_bin)

regressori_week <- regressori_week %>%
  mutate(week_rossa_bin = ifelse(week_rossa_sum>4, 1, 0))  # se un ristorante durante la settimana è in zona rossa per più di 4 giorni allora tale settimana viene etichettata come settimana rossa
#regressori_week$week_rossa_bin <- as.factor(regressori_week$week_rossa_bin)

# verifica collinearità variabili
numeric.var <- sapply(regressori_week, is.numeric)
corr.matrix <- cor(regressori_week[,numeric.var])
corrplot(corr.matrix, main="\n\nCorrelation Plot for Numerical Variables", method="number")

# regressori: "week_covid_bin", "week_rossa_bin"

M7 <- auto.arima(vendite1_sett_avg, seasonal = TRUE, 
                 xreg = data.matrix(regressori_week[, c("week_covid_bin", "week_rossa_bin")]))
summary(M7)  # AIC: 3548.42; RMSE: 667.4276   
checkresiduals(M7)
tsdisplay(residuals(M7), lag.max=52, main='Seasonal Model Residuals')

valori <- M7$coef["week_covid_bin"]/sqrt(diag(M7$var.coef))
pvalue = 2*pt(valori["week_covid_bin"] ,220)
pvalue

valori <- M7$coef["week_rossa_bin"]/sqrt(diag(M7$var.coef))
pvalue = 2*pt(valori["week_rossa_bin"] ,220)
pvalue

# verifica adattamento modello
autoplot(M7$fitted) + autolayer(vendite1_sett_avg)

# si procede ora utilizzando il modello ottenuto per fare previsioni su dati nuovi,
# in particolare si cerca di prevedere le vendite dopo aprile 2021, date per cui 
# non si hanno a disposizone informazioni relative a vendite. In particolare si cerca 
# di prevedere per il periodo 12 aprile 2021 - 12 agosto 2021, date per cui si 
# possono ricavare i valori dei regressori ma non si possono avere i valori di 
# vendite, valori che dunque vengono previsti utilizzando il modello precedente
# e i regressori ottenuti per le nuove date

# N.b.: il 12 agosto è un giovedì, quindi non viene presa in considerazione la 
# settimana intera ma ciò non influenza il valore dei regressori (la settimana 
# è comunque etichettata covid, non ci sono possibili chiusure e non c'è zona rossa)

# per procedere bisogna prima avere i valori dei regressori per le date per cui
# verranno eseguite le previsioni

# colori aggiornati fino al 12 agosto 2021
colori_zone_aggiornato <- read_csv("colori_zone_aggiornato.csv")  # https://github.com/imcatta/restrizioni_regionali_covid/blob/main/dataset.csv

colori_emilia_romagna_new <- colori_zone_aggiornato %>% 
  filter(denominazione_regione == "Emilia-Romagna")
names(colori_emilia_romagna_new)[3] <- "colore_emilia_romagna"

reference_date_colori <- as.Date("2021-04-11", format = "%Y-%m-%d")  

colori_emilia_romagna_new <- colori_emilia_romagna_new  %>% 
  filter(data > reference_date_colori)


# creazione df (dal 12 aprile 2021 al 12 agosto 2021) 
regressori_forecast_day <- data.frame(colori_emilia_romagna_new)
regressori_forecast_day <- regressori_forecast_day[,-2]

# colonna zona rossa
regressori_forecast_day$rossa <- ifelse(regressori_forecast_day$colore_emilia_romagna == "rosso", 1, 0)  # no zone rosse


# covid aggiornato fino al 12 agosto
regressori_forecast_day$covid <- 1  # il covid è presente


# chiuso aggiornato fino al 12 agosto
regressori_forecast_day$chiuso <- 0  # non ci sono date in cui i ristoranti avrebbero potuto chiudere


# divisione in settimane
week_new_rist1 <- as.Date(cut(regressori_forecast_day$data, "week"))

week_rossa_new <- aggregate(rossa ~ week_new_rist1, regressori_forecast_day, sum)  # per settimana
week_chiuso_new <- aggregate(chiuso ~ week_new_rist1, regressori_forecast_day, sum)  # per settimana
covid_chiuso_new <- aggregate(covid ~ week_new_rist1, regressori_forecast_day, sum)  # per settimana

regressori_forecast_week <- data.frame(covid_chiuso_new$covid, week_chiuso_new$chiuso, week_rossa_new$rossa)
colnames(regressori_forecast_week) <- c("week_covid_sum", "week_chiuso_sum", "week_rossa_sum")

# trasformazione colonne precedenti in valori binari
regressori_forecast_week <- regressori_forecast_week %>%
  mutate(week_covid_bin = ifelse(week_covid_sum>0, 1, 0))
#regressori_forecast_week$week_covid_bin <- as.factor(regressori_forecast_week$week_covid_bin)

regressori_forecast_week <- regressori_forecast_week %>%
  mutate(week_rossa_bin = ifelse(week_rossa_sum>4, 1, 0))
#regressori_forecast_week$week_rossa_bin <- as.factor(regressori_forecast_week$week_rossa_bin)

regressori_forecast_week <- regressori_forecast_week %>%
  mutate(week_chiuso_bin = ifelse(week_chiuso_sum>4, 1, 0))
#regressori_forecast_week$week_chiuso_bin <- as.factor(regressori_forecast_week$week_chiuso_bin)


# previsione vendite settimanali su dati nuovi
forecast_2021 <- M7 %>%
  forecast(h=18,  xreg =data.matrix(regressori_forecast_week[, c("week_covid_bin", "week_rossa_bin")])) 

autoplot(forecast_2021)


### Random forest----
# si addestra il modello su tutti i dati a disposizione (giornalieri) per poi fare previsioni
# per il periodo post aprile, periodo per cui non si hanno a disposizione valori 
# relativi a vendite e scontrini

# implementazione modelli
M8 <- randomForest(vendite ~ is_holiday + is_weekend +pioggia+  covid + stagione 
                  + weekday + solo_asporto_emilia_romagna + rossa_emilia_romagna
                  + tot_vaccini_emilia_romagna + mese, data = ristorante1)
varImpPlot(M8)
print(M8)

# si selezionano le variabili più rilevanti
M9 <- randomForest(vendite ~ weekday + is_weekend + covid + rossa_emilia_romagna + mese,
                                    data = ristorante1)
varImpPlot(M9)
print(M9)

# si seleziona il periodo su cui verranno fatte le previsioni
interval_post_aprile = seq(from = as.Date("2021-04-13"), 
                           to = as.Date("2021-08-12"), by = 'day')
ristorante1_post_aprile <- data.frame(interval_post_aprile)
colnames(ristorante1_post_aprile) <- "data"

# setting regressori su date nuove
ristorante1_post_aprile$mese <- month(ristorante1_post_aprile$data)

# colonna giorni festivi e feriali
ristorante1_post_aprile <- ristorante1_post_aprile %>%
  mutate(weekday = wday(data, week_start = getOption("lubridate.week.start", 1))) %>%
  mutate(tipo_giorno = case_when(
    (weekday %in% c(6,7)) ~ "weekend"
    , (weekday < 7) ~ "weekday"
    , TRUE ~ "other"
  )
  )
ristorante1_post_aprile$weekday <- as.factor(ristorante1_post_aprile$weekday)
ristorante1_post_aprile["tipo_giorno"][ristorante1_post_aprile["tipo_giorno"] == "weekend"] <- "1"
ristorante1_post_aprile["tipo_giorno"][ristorante1_post_aprile["tipo_giorno"] == "weekday"] <- "0"
colnames(ristorante1_post_aprile)[which(names(ristorante1_post_aprile) == "tipo_giorno")] <- "is_weekend"
ristorante1_post_aprile$is_weekend <- as.factor(ristorante1_post_aprile$is_weekend)

# covid aggiornato fino al 12 agosto
ristorante1_post_aprile$covid <- 1  # il covid è presente


# colori aggiornati fino al 12 agosto 2021
colori_zone_aggiornato <- read_csv("colori_zone_aggiornato.csv")

colori_emilia_romagna_post_aprile <- colori_zone_aggiornato %>% 
  filter(denominazione_regione == "Emilia-Romagna")
names(colori_emilia_romagna_post_aprile)[3] <- "colore_emilia_romagna"

reference_date_colori <- as.Date("2021-04-12", format = "%Y-%m-%d")  

colori_emilia_romagna_post_aprile <- colori_emilia_romagna_post_aprile  %>% 
  filter(data > reference_date_colori)


# creazione df (dal 12 aprile 2021 al 12 agosto 2021) 
ristorante1_post_aprile$colore_emilia_romagna <- colori_emilia_romagna_post_aprile$colore_emilia_romagna  # deve essere l'analogo di regressori_forecast_day


# colonna zona rossa
ristorante1_post_aprile$rossa_emilia_romagna <- ifelse(ristorante1_post_aprile$colore_emilia_romagna == "rosso", 1, 0)  # no zone rosse
ristorante1_post_aprile$rossa_emilia_romagna<- as.factor(ristorante1_post_aprile$rossa_emilia_romagna)
ristorante1_post_aprile$covid<- as.factor(ristorante1_post_aprile$covid)
ristorante1_post_aprile$colore_emilia_romagna <- as.factor(ristorante1_post_aprile$colore_emilia_romagna)
ristorante1_post_aprile$mese<- as.factor(ristorante1_post_aprile$mese)

ristorante1_post_aprile$vendite <- 0

# sistemazione colonne
ristorante1_post_aprile <- data.frame(ristorante1_post_aprile$data,
                                      ristorante1_post_aprile$vendite,
                                      ristorante1_post_aprile$weekday,
                                      ristorante1_post_aprile$is_weekend,
                                      ristorante1_post_aprile$covid, 
                                      ristorante1_post_aprile$rossa_emilia_romagna,
                                      ristorante1_post_aprile$mese)
names(ristorante1_post_aprile)<- c("data", 
                                   "vendite",
                                   "weekday",
                                   "is_weekend",
                                   "covid",
                                   "rossa_emilia_romagna",
                                   "mese")

ristorante1_pre_post_aprile <-rbind(ristorante1[,c("data", "vendite", "weekday",
                                             "is_weekend", "covid", "rossa_emilia_romagna",
                                             "mese")],
                              ristorante1_post_aprile)


# si utilizza il modello appena creato per fare previsioni sul futuro
vendite_forecast_rf_new <- predict(M9, ristorante1_pre_post_aprile[1564:1685,])  # dal 13 aprile in poi compreso
vendite_forecast_rf_new <- as.data.frame(vendite_forecast_rf_new)

# si uniscono le tue serie storiche

# serie storica previsioni (dal 13 aprile fino al 12 agosto)
# interval_post_aprile
interval_post_aprile_df <- data.frame(date = interval_post_aprile, 
                       val=vendite_forecast_rf_new)
interval_post_aprile_df$date<-as.Date(interval_post_aprile_df$date)  
interval_post_aprile_ts <- xts(interval_post_aprile_df$val, interval_post_aprile_df$date)

plot(interval_post_aprile_df$date, interval_post_aprile_df$vendite_forecast, xlab = "data", 
     ylab = "vendite", type="l", main = "Ristorante 1")

# serie storica dati reali fino al 12 aprile 2021
reference_date_pre_aprile <- as.Date("2021-04-12", format = "%Y-%m-%d")
vendite_pre_aprile <- ristorante1 %>%
  filter(data <= reference_date_pre_aprile) %>%
  select(data, vendite)

interval_pre_aprile <- seq(as.Date("2017-01-01"), as.Date("2021-04-12"), by = "day")
interval_pre_aprile_df <- data.frame(date = interval_pre_aprile, 
                           val=vendite_pre_aprile$vendite)

interval_pre_aprile_df$date<-as.Date(interval_pre_aprile_df$date)  
interval_pre_aprile_ts <- xts(interval_pre_aprile_df$val, interval_pre_aprile_df$date)

# si uniscono le due serie storiche
names(interval_post_aprile_df)[1] <- "data"
names(interval_post_aprile_df)[2] <- "vendite"

names(interval_pre_aprile_df)[1] <- "data"
names(interval_pre_aprile_df)[2] <- "vendite"

interval_complete_new <- rbind(interval_post_aprile_df, interval_pre_aprile_df)
interval_complete_new <- interval_complete_new[order(interval_complete_new$data), ]
row.names(interval_complete_new) <- NULL

# serie storica con previsioni
plot(interval_complete_new$data, interval_complete_new$vendite, xlab = "data", 
     ylab = "vendite", type="l", main = "Ristorante 1 previsioni")
ristorazione_temp <- ristorazione

# serie storica originale
ristorazione_temp$vendite1[is.na(ristorazione_temp$vendite1)] <- 0 
plot(ristorazione_temp$data, ristorazione_temp$vendite1, xlab = "data", 
     ylab = "vendite", type="l", main = "Ristorante 1 dati reali")

# verifica performance modello
RMSE.rf <- sqrt(mean((M9$predicted - ristorante1$vendite)^2))
RMSE.rf


### Prophet con regressori ----
# si addestra il modello su tutti i dati a disposizione (giornalieri) per poi 
# fare previsioni per il periodo post aprile, periodo per cui non si hanno a 
# disposizione valori relativi a vendite e scontrini

covid <- function(ds) {
  dates <- as.Date(ds)
  reference_date_pre <- as.Date("2020-03-09", format = "%Y-%m-%d")
  as.numeric(dates > reference_date_pre)
}

# periodo pre aprile
vendite_pre_aprile_prophet <- data.frame(ristorante1$data, 
                                         ristorante1$rossa_emilia_romagna)
names(vendite_pre_aprile_prophet) <- c("data", "rossa")

# periodo post aprile
data = seq(from = as.Date("2021-04-13"), to = as.Date("2021-07-11"), by = 'day')
vendite_post_aprile_prophet <- data.frame(data)
vendite_post_aprile_prophet$rossa <- 0

# unione periodo pre aprile e periodo post aprile
vendite_pre_post_aprile_prophet <- rbind(vendite_pre_aprile_prophet, 
                                         vendite_post_aprile_prophet)

# si considerano le vendite fino ad aprile 2021
prophet_vendite_totali <- ristorante1 %>% 
  select(data, vendite)
colnames(prophet_vendite_totali) <- c("ds", "y")
prophet_vendite_totali$covid <- covid(prophet_vendite_totali$ds)
prophet_vendite_totali$rossa <- vendite_pre_aprile_prophet$rossa

# si crea il modello
M10 <- prophet(daily.seasonality=FALSE)
M10 <- add_country_holidays(M10, country_name = 'IT')
M10 <- add_regressor(M10, 'covid')
M10 <- add_regressor(M10, 'rossa')
M10 <- fit.prophet(M10, prophet_vendite_totali)

# vengono fatte le previsioni
future_prophet_regr <- make_future_dataframe(M10, periods=90)

future_prophet_regr$covid <- covid(future_prophet_regr$ds)
future_prophet_regr$rossa <- vendite_pre_post_aprile_prophet$rossa

vendite_forecast_prophet_regr <- predict(M10, future_prophet_regr)

plot(M10, vendite_forecast_prophet_regr)
prophet_plot_components(M10, vendite_forecast_prophet_regr)
# prophet_vendite_totali.cv <- cross_validation(M10, initial=180, period=60, horizon=120, units='days')
# plot_cross_validation_metric(prophet_vendite_totali.cv, metric = 'rmse')
dyplot.prophet(M10, vendite_forecast_prophet_regr)

sqrt(mean((prophet_vendite_totali$y- vendite_forecast_prophet_regr[1:1563,"yhat"])^2))
