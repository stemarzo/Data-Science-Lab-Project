# settaggio librerie
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

# lettura dataset
#ristorazione_original <- read_excel("C:/Users/Stefano/Documents/progetto_dslab/codice_progetto/dati/Ristorazione.xls")
ristorazione_original <- read_excel("/Users/lorenzolorgna/Desktop/ds lab/Ristorazione.xls")

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

# colore zona in base alla data
colori_zone <- read_csv("/Users/lorenzolorgna/Desktop/dataset.csv")

# selezione regioni
colori_zone <- colori_zone %>% filter(
  denominazione_regione == "Lombardia" | denominazione_regione == "Emilia-Romagna"  #  ci sono date in cui Lombardia e Emilaia-Romagna hanno colori diversi (quindi ci sono date duplicate)
)
# faccio il join con ristorazione su data
ristorazione<-merge(x=ristorazione,y=colori_zone,by="data",all.x=TRUE)

# colonna asporto 
# zona rossa: solo asporto e consegne a domicilio senza limiti
# zona arancione: solo asporto e consegne a domicilio senza limiti
# zona gialla: si può mangiare in presenza e consegne a domicilio senza limiti
ristorazione <- ristorazione %>%
  mutate(solo_asporto = case_when(
    (colore == "rosso") ~ TRUE
    ,(colore == "arancione") ~ TRUE
    , (colore == "giallo") ~ FALSE
    , TRUE ~ FALSE
  ) 
  )        

ristorazione <- ristorazione[c(1,2,15,16,17,18,19,20,21,22,3,4,5,6,7,8,9,10,11,12,13,14)]  # cambio ordine colonne

#check NA values
sum(is.na(ristorazione$data))  # 0 NA 
sum(is.na(ristorazione$data_anno_prec))  # 68 NA - DA SISTEMARE!
sum(is.na(ristorazione$giorno_settimana))  # 0 NA 
# nelle vendite e scontrini sostituire gli NA CON 0 ?

# creo un dataframe per ciascun ristorante
col_date <- subset(ristorazione, select = c(1, 2, 3, 4, 5)) # data, data_anno_prec, giorno_settimana, uguali per ogni ristorante
col_nomi <- c("data", "data_anno_prec", "giorno_settimana", "mese", "stagione", "vendite", "scontrini")

#ristorante1
ristorante1 <- data.frame(col_date, ristorazione$vendite1, ristorazione$scontrini1)
colnames(ristorante1) <- col_nomi
ristorante1$rapprto_v_s <- ristorante1$vendite/ristorante1$scontrini

#ristorante2
ristorante2 <- data.frame(col_date, ristorazione$vendite2, ristorazione$scontrini2)
colnames(ristorante2) <- col_nomi
ristorante2$rapprto_v_s <- ristorante2$vendite/ristorante2$scontrini

#ristorante3
ristorante3 <- data.frame(col_date, ristorazione$vendite3, ristorazione$scontrini3)
colnames(ristorante3) <- col_nomi
ristorante3$rapprto_v_s <- ristorante3$vendite/ristorante3$scontrini

#ristorante4
ristorante4 <- data.frame(col_date,ristorazione$vendite4, ristorazione$scontrini4)
colnames(ristorante4) <- col_nomi
ristorante4$rapprto_v_s <- ristorante4$vendite/ristorante4$scontrini

#ristorante5
ristorante5 <- data.frame(col_date, ristorazione$vendite5, ristorazione$scontrini5)
colnames(ristorante5) <- col_nomi
ristorante5$rapprto_v_s <- ristorante5$vendite/ristorante5$scontrini

#ristorante6
ristorante6 <- data.frame(col_date, ristorazione$vendite6, ristorazione$scontrini6)
colnames(ristorante6) <- col_nomi
ristorante6$rapprto_v_s <- ristorante6$vendite/ristorante6$scontrini

# rimuovo le variabili che mi sono servite nella fase sopra
rm(list = c('col_date','col_nomi'))

# prova esplorazione, primo ristorante, andamento vendite nei diversi anni, considerando il dato giornaliero
plot(ristorante1$data, ristorante1$vendite, xlab = "data", ylab = "vendite", type="l")
abline(h=mean(as.integer(ristorante1$vendite)))
# è possibile notare la presenza di valori NA

# decomposizione serie primo ristorante
vendite1 <- ristorante1[, 4]
vendite1[is.na(vendite1)] <- 0  # sostiuisco i valori NA con 0
vendite1 <- ts(vendite1,start=2017,frequency=365) 
plot(vendite1)

vendite1.fit<-stl(vendite1,s.window="periodic")
attributes(vendite1.fit)
trend.vendite1<-vendite1.fit$time.series[,2]
stag.vendite1<-vendite1.fit$time.series[,1]
res.vendite1<-vendite1.fit$time.series[,3]
plot(vendite1.fit,main="Decomposizione con la funzione 'stl'")


# decomposizione serie secondo ristorante
vendite2<-ristorante2[, 4]
vendite2[is.na(vendite2)] <- 0
vendite2<-ts(vendite2,start=2017,frequency=365) 
plot(vendite2)

vendite2.fit<-stl(vendite2,s.window="periodic")
attributes(vendite2.fit)
trend.vendite2<-vendite2.fit$time.series[,2]
stag.vendite2<-vendite2.fit$time.series[,1]
res.vendite2<-vendite2.fit$time.series[,3]
plot(vendite2.fit,main="Decomposizione con la funzione 'stl'")


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
vendite_agg1<-ristorante1
vendite_agg1$mo <- strftime(vendite_agg1$data, "%m")
vendite_agg1$yr <- strftime(vendite_agg1$data, "%Y")
vendite_agg1<-ts(vendite_agg1$vendite,start=2017,frequency=12) 
vendite_agg1 <- aggregate(vendite ~ mo + yr, vendite_agg1, FUN = mean)

monthplot(ristm)
plot(vendite_agg1)

vendite_agg1.fit<-stl(vendite_agg1,s.window="periodic")
attributes(vendite_agg1.fit)
trend.vendite_agg1<-vendite_agg1.fit$time.series[,2]
stag.vendite_agg1<-vendite_agg1.fit$time.series[,1]
res.vendite_agg1<-vendite_agg1.fit$time.series[,3]
plot(vendite_agg1.fit,main="Decomposizione con la funzione 'stl'")


acf(res.vendite_agg1,type="correlation",plot=TRUE,main="Correlogramma della serie dei residui")
pacf(res.vendite_agg1,plot=TRUE,main="Grafico delle correlazioni parziali")
