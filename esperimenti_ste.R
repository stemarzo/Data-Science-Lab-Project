#### CESTINO ---------------------------------------------------------------------------

##INSERIMENTO ristorazione
new_holiday = getHolidayList(calendar = "Italy", from=as.Date("2017-01-01"), to= as.Date("2021-04-12"), includeWeekends=FALSE)
new_holiday <- append(new_holiday, c(as.Date("2017-04-16"), as.Date("2018-04-01"),
                                     as.Date("2019-04-21"), as.Date("2021-04-04"), as.Date("2020-04-12")))

for (i in 1:nrow(ristorazione)) {
  ristorazione[i,"holiday"]<-ristorazione[i,"data"]%in% new_holiday
}

View(ristorazione)
ristorante1$tipo_giorno <- as.factor(ristorazione$tipo_giorno)
ristorante1$weekday <- as.factor(ristorazione$weekday)
ristorante1$holiday <- ristorazione$holiday
ristorante1$pioggia <- ristorazione$pioggia
ristorante1$top <- ristorazione$top
ristorante1$meteo_aperto <- ristorazione$meteo_aperto
ristorante1$saldi<-ristorazione$`Black Friday e saldi Lombardia`


giornaliera <- smo3$alphahat[1:1163,"sea_dummy1"]
vendite1_pre_des <- vendite1_day_pre- giornaliera

k<-diff(vendite1_day_pre, 365)
plot(k)
par(mfrow=c(1,2))
acf(k)
pacf(k)

M4<-Arima(k, order = c(1,0,1),seasonal = list(order=c(1,0,1),period=7))
summary(M4)
acf(M4$residuals)
pacf(M4$residuals)


pacf(vendite1_pre_des)
plot(vendite1_pre_des)

library(urca)
summary(ur.kpss(vendite1_pre_des ))

ndiffs(vendite1_pre_des) #differenziazione normale
nsdiffs(vendite1_day_pre)
vendite1_pre_des_diff <- diff(vendite1_pre_des)

summary(ur.kpss(vendite1_pre_des_diff))

autoplot(vendite1_pre_des_diff)


summary(lm(vendite1_pre_des ~ ristorante1[1:1163, "saldi"] +ristorante1[1:1163, "holiday"]))

M1 <- auto.arima(vendite1_pre_des_diff, xreg = data.matrix(ristorante1[2:1163, c("holiday","saldi" )]))
summary(M1)
checkresiduals(M1)

vendite1_sett_pre_dest <- vendite1_sett_pre- stag.vendite1_sett_pre

M2<- auto.arima(vendite1_sett_pre_dest)
summary(M2)

checkresiduals(M2)



#ASDSADSADSADSA

train<- ristorante1_pre_covid[1:814, "vendite"]
test <-ristorante1_pre_covid[815:1163, "vendite"]

train <- ts(train,start=2017,frequency=365)
test <- ts(test,start=c(2019,03,26),frequency=365)

par(mfrow=c(1,1))
plot(train)

train_diff <- diff(train, 1)

plot(train_diff)


require(gridExtra)
acf<-ggAcf(train_diff, lag.max = 30)+ggtitle("Vendite1 day pre diff")
pacf<-ggPacf(train_diff, lag.max = 30)+ggtitle("Vendite1 day pre diff")
grid.arrange(acf, pacf, ncol=2)


M5 <- Arima(train_diff, order = c(1,0,1),seasonal = list(order=c(1,0,1),period=7))
summary(M5)
ggAcf(M5$residuals, lag.max = 30)+ggtitle("Vendite1 day pre diff")
ggPacf(M5$residuals, lag.max = 30)+ggtitle("Vendite1 day pre diff")

checkresiduals(M5)

autoplot(forecast(M5, h=10))


##SETTIMANALE

train <- window(vendite1_sett_avg_pre, end=c(2019,2))
test <- window(vendite1_sett_avg_pre, start=c(2019,2))


train_dest <- seasadj(stl(train, s.window = 52))
plot(train_dest)

par(mfrow=c(1,1))
plot(train)

ndiffs(train_dest)
nsdiffs(train_dest)

train_diff <-diff(train_dest,1)

library(urca)
summary(ur.kpss(train_diff ))


nsdiffs(train_diff)

plot(train_diff)

require(gridExtra)
acf<-ggAcf(train_diff, lag.max = 52)+ggtitle("Vendite1 day pre diff")
pacf<-ggPacf(train_diff, lag.max = 52)+ggtitle("Vendite1 day pre diff")
grid.arrange(acf, pacf, ncol=2)

M9<-auto.arima(train_diff)
M9s<-Arima(train_diff, order = c(2,0,2))

checkresiduals(M9)
checkresiduals(M9s)

M9s

 M10 <-auto.arima(train, D=1)

checkresiduals(M10)

autoplot(forecast(M10, h=52))+autolayer(test)


# vendite ristorante 1 pre covid
ristorante1_pre_covid_chiuso <- ristorante1 %>%
  filter(ristorante1$data < reference_date) %>%
  select(chiuso, data)

chiuso_sett_avg_pre <- aggregate(chiuso ~ week_pre_covid, ristorante1_pre_covid_chiuso, mean)
chiuso1_sett_avg_pre <- chiuso_sett_avg_pre$chiuso
chiuso1_sett_avg_pre <- ts(chiuso1_sett_avg_pre,start=2017,frequency=52) 

train_chiuso <- window(chiuso1_sett_avg_pre, end=2019)
test_chiuso <- window(chiuso1_sett_avg_pre, start=2019)

M11 <- auto.arima(train, D=1, xreg = train_chiuso)
summary(M11)
checkresiduals(M11)

autoplot(forecast(M11, h=52, xreg = test_chiuso))+autolayer(test)


## DEFINITIVO 

library(tseries)
library(urca)
library(TSstudio)
require(gridExtra)

# ARIMA MANUALE
# rendiamo stazionaria la ts, dobbiamo togliere stagionalità e trend

#rimuovo stagionalità

vendite1_sett_avg_pre_dest <- seasadj(stl(vendite1_sett_avg_pre, s.window = 52))
autoplot(vendite1_sett_avg_pre_dest)+autolayer(vendite1_sett_avg_pre)

#controllo se devo differenziare

ndiffs(vendite1_sett_avg_pre_dest)
nsdiffs(vendite1_sett_avg_pre_dest)

#rimuovo trend con differenziazione

vendite1_sett_avg_pre_dest_diff <- diff(vendite1_sett_avg_pre_dest)
autoplot(vendite1_sett_avg_pre_dest_diff)

#controllo stazionarietà
adf.test(vendite1_sett_avg_pre_dest_diff, alternative = "stationary")  
summary(ur.kpss(vendite1_sett_avg_pre_dest_diff ))
ndiffs(vendite1_sett_avg_pre_dest_diff)
nsdiffs(vendite1_sett_avg_pre_dest_diff)


#CREAZIONE TRAINIG SET E TEST SET

vendite1_sett_avg_pre_split <- ts_split(vendite1_sett_avg_pre_dest_diff)

# divisione in train e test
train <- vendite1_sett_avg_pre_split$train
test <- vendite1_sett_avg_pre_split$test
autoplot(train)
autoplot(test)

autoplot(vendite1_sett_avg_pre_dest_diff) +
  autolayer(train, series="Training") +
  autolayer(test, series="Test")

# scelta di parametri p e q con acf e pacf
acf<-ggAcf(train, lag.max = 52)+ggtitle("Vendite1 day pre diff")
pacf<-ggPacf(train, lag.max = 52)+ggtitle("Vendite1 day pre diff")
grid.arrange(acf, pacf, ncol=2)

#applico ARMA
M1<-Arima(train, order = c(2,0,2))
summary(M1)#guardare meglio accuracy
checkresiduals(M1)

#autoplot(forecast(M1, h=50))+autolayer(test)

#USO AUTO ARIMA
vendite1_sett_avg_pre_split_auto <- ts_split(vendite1_sett_avg_pre)

# divisione in train e test
train_auto <- vendite1_sett_avg_pre_split_auto$train
test_auto <- vendite1_sett_avg_pre_split_auto$test

autoplot(vendite1_sett_avg_pre) +
  autolayer(train_auto, series="Training") +
  autolayer(train_auto, series="Test")

# auto.arima per selezione modello migliore
auto.model = auto.arima(train_auto, seasonal = T)
summary(auto.model)


# previsioni con test set
auto.model %>%
  forecast(h=50) %>%  # h Number of periods for fore
  autoplot() + autolayer(test_auto)

forecast <- auto.model %>%
  forecast(h=50)

# alternativa per vedere previsioni con test set
par(mfrow=c(1,1))
plot(forecast)
lines(test_auto, col="red")
legend("topleft",lty=1,bty = "n",col=c("red","blue"),c("testData","ARIMAPred"))

###NUOVO MANUALE 

#guardo acf e pacf del train

acf<-ggAcf(train, lag.max = 52)+ggtitle("Vendite1 day pre diff")
pacf<-ggPacf(train, lag.max = 52)+ggtitle("Vendite1 day pre diff")
grid.arrange(acf, pacf, ncol=2)

#provo ad iniziare con un ar 2, vedendo che nel pacf due lag sono significativamente
#correlati

M4 <- Arima(train, order = c(2,0,0))

#guardo pacf e acf dei residui, in modo tale da capire dove  è e poi
#catturare l'informazione che abbiamo lasciato 
acf<-ggAcf(M4$residuals, lag.max = 52)+ggtitle("Vendite1 day pre diff")
pacf<-ggPacf(M4$residuals, lag.max = 52)+ggtitle("Vendite1 day pre diff")
grid.arrange(acf, pacf, ncol=2)

#vedo che il lag 52 è molto correlato nell'acf
M4<-Arima(train, order = c(2,0,0),seasonal =  list(order=c(0,0,1),period=52))# il mio amico in seasonal aveva messo (1,0,0)
#ma dal grafico mi sebra più sbarellato il 52 lag nell'acf quindi bho, alla fine penso che non esistea uno perfetto, basta
#che si catturi l'info.
summary(M4)
acf<-ggAcf(M4$residuals, lag.max = 52)+ggtitle("Vendite1 day pre diff")
pacf<-ggPacf(M4$residuals, lag.max = 52)+ggtitle("Vendite1 day pre diff")
grid.arrange(acf, pacf, ncol=2)
checkresiduals(M4)
autoplot(forecast(M4, h=50))+autolayer(test)

M5<-Arima(train, order = c(2,0,0),seasonal =  list(order=c(1,0,0),period=52))
summary(M3)#guardare meglio accuracy
checkresiduals(M5)
acf<-ggAcf(M3$residuals, lag.max = 52)+ggtitle("Vendite1 day pre diff")
pacf<-ggPacf(M3$residuals, lag.max = 52)+ggtitle("Vendite1 day pre diff")
grid.arrange(acf, pacf, ncol=2)
summary(M5)





PROVA REGRESSORE------------------------------------------

ristorante1$top <- ristorazione$top

ristorante1_pre_covid_top <- ristorante1 %>%
  filter(ristorante1$data < reference_date) %>%
  select(top, data)

top_sett_sum_pre <- aggregate(top ~ week_pre_covid, ristorante1_pre_covid_top, sum)
top1_sett_sum_pre <- top_sett_sum_pre$top
top1_sett_sum_pre <- ts(top1_sett_sum_pre,start=2017,frequency=52) 

top_split <- ts_split(top1_sett_sum_pre)
train_top <- as.factor(top_split$train)
test_top <- top_split$test


MR1 <- auto.arima(train_auto, seasonal = T, xreg = as.numeric(train_top))
summary(MR1)

pvalue = 2*pt(-350.8245/280.9505,115)


summary(lm(train_auto ~ train_top))

ristorante1$pioggia <- ristorazione$pioggia

ristorante1_pre_covid_pioggia <- ristorante1 %>%
  filter(ristorante1$data < reference_date) %>%
  select(pioggia, data)

pioggia_sett_avg_pre <- aggregate(pioggia ~ week_pre_covid, ristorante1_pre_covid_pioggia, sum)
pioggia1_sett_avg_pre <- pioggia_sett_avg_pre$pioggia
pioggia1_sett_avg_pre <- ts(pioggia1_sett_avg_pre,start=2017,frequency=52) 

pioggia_split <- ts_split(pioggia1_sett_avg_pre)
train_pioggia <- pioggia_split$train
test_pioggia <- pioggia_split$test


regr <- data.frame(train_top, train_pioggia)

MR2 <- auto.arima(train_auto, seasonal = T, xreg = data.matrix(regr[,c("train_top", "train_pioggia")]))
summary(MR2)


MR3 <- auto.arima(train_auto, seasonal = T, xreg = train_pioggia)
summary(MR3)

ristorante1$vacanze_scuola <- as.numeric(ristorazione$`Vacanze scolastiche Lombardia`)

ristorante1_pre_covid_vacanze_scuola <- ristorante1 %>%
  filter(ristorante1$data < reference_date) %>%
  select(vacanze_scuola, data)

vacanze_scuola_sett_avg_pre <- aggregate(vacanze_scuola ~ week_pre_covid, ristorante1_pre_covid_vacanze_scuola, sum)
vacanze_scuola1_sett_avg_pre <- vacanze_scuola_sett_avg_pre$vacanze_scuola
vacanze_scuola1_sett_avg_pre <- ts(vacanze_scuola1_sett_avg_pre,start=2017,frequency=52) 

vacanze_scuola_split <- ts_split(vacanze_scuola1_sett_avg_pre)
train_vacanze_scuola <- vacanze_scuola_split$train
test_vacanze_scuola <- vacanze_scuola_split$test

regr <- data.frame(train_chiuso, train_vacanze_scuola)



ristorante1_pre_covid_chiuso <- ristorante1 %>%
  filter(ristorante1$data < reference_date) %>%
  select(chiuso, data)

chiuso_sett_avg_pre <- aggregate(chiuso ~ week_pre_covid, ristorante1_pre_covid_chiuso, sum)
chiuso1_sett_avg_pre <- chiuso_sett_avg_pre$chiuso
chiuso1_sett_avg_pre <- ts(chiuso1_sett_avg_pre,start=2017,frequency=52) 

chiuso_split <- ts_split(chiuso1_sett_avg_pre)
train_chiuso <- chiuso_split$train
test_chiuso <- chiuso_split$test



MR4 <- auto.arima(train_auto, D=1, xreg = train_chiuso)
summary(MR4)

pvalue = 2*pt(-740.1464/60.7140,115)

MR4 %>%
  forecast(h=50, xreg= test_neve) %>%  # h Number of periods for forecasting
  autoplot() + autolayer(test_auto)


forecast <- MR4 %>%
  forecast(h=10, xreg= test_chiuso) 

# alternativa per vedere previsioni con test set
par(mfrow=c(1,1))
plot(forecast)
lines(test_auto, col="red")
legend("topleft",lty=1,bty = "n",col=c("red","blue"),c("testData","ARIMAPred"))

# valutazione qualità previsioni
accuracy(forecast, test_auto[1:10])



ristorante1$fuori <- ristorazione$meteo_aperto

ristorante1_pre_covid_fuori <- ristorante1 %>%
  filter(ristorante1$data < reference_date) %>%
  select(fuori, data)

fuori_sett_avg_pre <- aggregate(fuori ~ week_pre_covid, ristorante1_pre_covid_fuori, sum)
fuori1_sett_avg_pre <- fuori_sett_avg_pre$fuori
fuori1_sett_avg_pre <- ts(fuori1_sett_avg_pre,start=2017,frequency=52) 

fuori_split <- ts_split(fuori1_sett_avg_pre)
train_fuori <- fuori_split$train
test_fuori <- fuori_split$test


MR5 <- auto.arima(train_auto, D=1, xreg = train_fuori)
summary(MR5)


meteo <- read_delim("meteo2.csv", ";", escape_double = FALSE, trim_ws = TRUE)
meteo <- meteo[, -c(1)]
meteo$data<-as.Date(meteo$data, format = "%d/%m/%Y")
ristorazione<-merge(x=ristorazione,y=meteo,by="data",all.x=TRUE)


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

MR6 <- auto.arima(train_auto, D=1, xreg = train_neve)
summary(MR6)

pvalue = 2*pt(-825.0664/115.4211,115)
pvalue


MR6 %>%
  forecast(h=10, xreg=test_neve) %>%  # h Number of periods for forecasting
  autoplot() + autolayer(vendite1_sett_avg)


forecast <- MR6 %>%
  forecast(h=10, xreg=test_neve) 




accuracy(forecast, test_auto[1:10])

# alternativa per vedere previsioni con test set
par(mfrow=c(1,1))
plot(forecast)
lines(vendite1_sett_avg, col="red")
legend("topleft",lty=1,bty = "n",col=c("red","blue"),c("testData","ARIMAPred"))

# valutazione qualità previsioni
accuracy(forecast, test_auto[1:10])



MR7 <- auto.arima(train_auto, D=1, xreg = train_vendite)
summary(MR7)

pvalue = 2*pt(-825.0664/115.4211,115)
pvalue





vacanze_pre <- ristorazione %>%
  filter(ristorazione$data < reference_date) %>%
  select(holiday, data)

View(ristorazione)
vacanze_pre_agg <- aggregate(holiday ~ week_pre_covid, vacanze_pre, sum)
vacanze_pre_agg <- vacanze_pre_agg$holiday
vacanze_pre_agg <- ts(vacanze_pre_agg,start=2017,frequency=52) 

vacanze_split <- ts_split(vacanze_pre_agg)
train_vacanze <- vacanze_split$train
test_vacanze <- vacanze_split$test


df= data.frame(train_vacanze, train_neve)

MR7 <- auto.arima(train_auto, seasonal = TRUE, xreg =data.matrix(df[, c("train_vacanze","train_neve" )]))
summary(MR7)

pvalue = 2*pt(-185.3499/199.1393, 114)
pvalue

pvalue = 2*pt( -824.4265/114.2885, 114)
pvalue

df= data.frame(test_vacanze, test_neve)

forecast <- MR7 %>%
  forecast(h=10,  xreg =data.matrix(df[, c("test_vacanze","test_neve" )])) 




accuracy(forecast, test_auto[1:10])



ristorazione[, "Black Friday e saldi  Emilia-Romagna"] <- 0
ristorazione[5:64, "Black Friday e saldi  Emilia-Romagna"] <- 1
ristorazione[182:242, "Black Friday e saldi  Emilia-Romagna"] <- 1
ristorazione[328, "Black Friday e saldi  Emilia-Romagna"] <- 1
ristorazione[370:428, "Black Friday e saldi  Emilia-Romagna"] <- 1
ristorazione[549:609, "Black Friday e saldi  Emilia-Romagna"] <- 1
ristorazione[692, "Black Friday e saldi  Emilia-Romagna"] <- 1
ristorazione[735:794, "Black Friday e saldi  Emilia-Romagna"] <- 1
ristorazione[917:972, "Black Friday e saldi  Emilia-Romagna"] <- 1
ristorazione[1063, "Black Friday e saldi  Emilia-Romagna"] <- 1
ristorazione[1100:1160, "Black Friday e saldi  Emilia-Romagna"] <- 1
ristorazione[1309:1369, "Black Friday e saldi  Emilia-Romagna"] <- 1
ristorazione[1427, "Black Friday e saldi  Emilia-Romagna"] <- 1
ristorazione[1491:1550, "Black Friday e saldi  Emilia-Romagna"] <- 1








ristorante1$saldi <- ristorazione$`Black Friday e saldi  Emilia-Romagna`

r1_saldi_pre <- ristorante1 %>%
  filter(ristorante1$data < reference_date) %>%
  select(saldi, data)

r1_saldi_pre_avg <- aggregate(saldi ~ week_pre_covid, r1_saldi_pre, mean)
r1_saldi_pre_avg <- r1_saldi_pre_avg$saldi
r1_saldi_pre_avg <- ts(r1_saldi_pre_avg,start=2017,frequency=52) 

saldi_split <- ts_split(r1_saldi_pre_avg)
train_saldi <- saldi_split$train
test_saldi <- saldi_split$test


#regr <- data.frame(train_top, train_pioggia)

MR2 <- auto.arima(train_auto, seasonal = T, xreg = train_saldi)
summary(MR2)

valori <- MR2$coef["xreg"]/sqrt(diag(MR2$var.coef))


pvalue = 2*pt(valori["xreg"] ,115)
pvalue


forecast <- MR2 %>%
  forecast(h=20, xreg=test_saldi) 

accuracy(forecast, test_auto[1:20])

forecast <- M3 %>%
  forecast(h=20) 

accuracy(forecast, test_auto[1:20])
checkresiduals(MR2)


summary(M3)





summary(lm(train_auto ~ train_top))




ristorazione[1:1164,"colore_emilia_romagna"]<-"bianco"
ristorazione[1165:1234,"colore_emilia_romagna"]<-"rosso"
ristorazione[1235:1258,"colore_emilia_romagna"]<-"arancionene"
ristorazione[1259:1405,"colore_emilia_romagna"]<-"giallo"


 LAVORO FATTO GIOVEDI----------------------------------------------------------

ristorazione$rossa <- ifelse(ristorazione$colore_emilia_romagna == "rosso", 1, 0)
ristorazione$bianca <- ifelse(ristorazione$colore_emilia_romagna == "bianco", 1, 0)
ristorazione$gialla <- ifelse(ristorazione$colore_emilia_romagna == "giallo", 1, 0)
ristorazione$arancione <- ifelse(ristorazione$colore_emilia_romagna == "arancionene", 1, 0)

ristorante1$rossa <- ristorazione$rossa
ristorante1$bianca <- ristorazione$bianca
ristorante1$gialla <- ristorazione$gialla
ristorante1$arancione <- ristorazione$arancione



giorni_rossa <- aggregate(rossa ~ week, ristorante1, sum)
giorni_rossa <- giorni_rossa$rossa
giorni_rossa <- ts(giorni_rossa,start=2017,frequency=52) 


giorni_arancione <- aggregate(arancione ~ week, ristorante1, sum)
giorni_arancione <- giorni_arancione$arancione
giorni_arancione <- ts(giorni_arancione,start=2017,frequency=52) 

giorni_gialla <- aggregate(gialla ~ week, ristorante1, sum)
giorni_gialla <- giorni_gialla$gialla
giorni_gialla <- ts(giorni_gialla,start=2017,frequency=52) 

giorni_bianca <- aggregate(bianca ~ week, ristorante1, sum)
giorni_bianca <- giorni_bianca$bianca
giorni_bianca <- ts(giorni_bianca,start=2017,frequency=52) 

df <- data.frame(giorni_arancione, giorni_bianca, giorni_gialla, giorni_rossa)


MT1 <- auto.arima(vendite1_sett_avg, seasonal = TRUE, xreg = data.matrix(df[, c("giorni_rossa", "giorni_arancione" )]))
summary(MT1)
checkresiduals(MT1)
tsdisplay(residuals(MT1), lag.max=52, main='Seasonal Model Residuals')

valori <- MT1$coef["giorni_arancione"]/sqrt(diag(MT1$var.coef))


pvalue = 2*pt(valori["giorni_arancione"] ,221)
pvalue

summary(lm(vendite1_sett_avg~ giorni_arancione+ giorni_rossa))



autoplot(MT1$fitted) + autolayer(vendite1_sett_avg)

 
for (i in 1:nrow(df)) {
  df[i,"colore"] <- which.max(c(df[i,"giorni_arancione"],df[i,"giorni_bianca"],df[i,"giorni_gialla"],df[i,"giorni_rossa"]))
}

MT2 <- auto.arima(vendite1_sett_avg, seasonal = TRUE, xreg = data.matrix(df[, "colore"]))
summary(MT2)
checkresiduals(MT2)
tsdisplay(residuals(MT2), lag.max=52, main='Seasonal Model Residuals')

df$rosso <- ifelse(df$colore == 4, 1, 0)
df$arancione <- ifelse(df$colore == 1, 1, 0)
df$bianco <- ifelse(df$colore == 2, 1, 0)
df$giallo <- ifelse(df$colore == 3, 1, 0)

MT2 <- auto.arima(vendite1_sett_avg, seasonal = TRUE, xreg = data.matrix(df[, c("rosso", "giallo", "bianco")]))
summary(MT2)
checkresiduals(MT2)
tsdisplay(residuals(MT2), lag.max=52, main='Seasonal Model Residuals')

autoplot(MT2$fitted) + autolayer(vendite1_sett_avg)


autoplot(M3$fitted) + autolayer(train_auto)


AUTO ARIMA NEVE----------------------------------------------------------

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
ristorante1$rossa <- as.numeric(as.character(ristorante1$rossa))
week_rossa_sum <- aggregate(rossa ~ week_rist1, ristorante1[-c(1,1563),], sum)
week_rossa_sum <- week_rossa_sum$rossa
ristorante1$rossa <- as.factor(ristorante1$rossa)


regressori_week <- data.frame(week_covid_sum, week_chiuso_sum, week_rossa_sum)

# trasformazione colonne precedenti in valori binari
regressori_week <- regressori_week %>%
  mutate(week_covid_bin = ifelse(week_covid_sum>0, 1, 0))  # se almeno un giorno durante la settimana ha registrato il covid allora tale settimana viene etichettata come settimana covid
regressori_week$week_covid_bin <- as.factor(regressori_week$week_covid_bin)

regressori_week <- regressori_week %>%
  mutate(week_chiuso_bin = ifelse(week_chiuso_sum>4, 1, 0))  # se un ristorante durante la settimana rimane chiuso per più di 4 giorni allora tale settimana viene etichettata come settimana chiusa
regressori_week$week_chiuso_bin <- as.factor(regressori_week$week_chiuso_bin)

regressori_week <- regressori_week %>%
  mutate(week_rossa_bin = ifelse(week_rossa_sum>4, 1, 0))  # se un ristorante durante la settimana è in zona rossa per più di 4 giorni allora tale settimana viene etichettata come settimana rossa
regressori_week$week_rossa_bin <- as.factor(regressori_week$week_rossa_bin)

# verifica collinearità variabili
library(corrplot)

numeric.var <- sapply(regressori_week, is.numeric)
corr.matrix <- cor(regressori_week[,numeric.var])
corrplot(corr.matrix, main="\n\nCorrelation Plot for Numerical Variables", method="number")

# regressori: "covid_bin", "rossa_sum", "chiuso_sum" (check se serve as.factor())

M4 <- auto.arima(vendite1_sett_avg, seasonal = TRUE, 
                 xreg = data.matrix(regressori_week[, c("week_covid_bin", "week_rossa_sum", "week_chiuso_sum")]))
summary(M4)  # AIC: 3453.91   
checkresiduals(M4)
tsdisplay(residuals(M4), lag.max=52, main='Seasonal Model Residuals')

# verifica p-value
valori <- M4$coef["week_chiuso_sum"]/sqrt(diag(M4$var.coef))
pvalue = 2*pt(valori["week_chiuso_sum"] ,221)
pvalue

valori <- M4$coef["week_covid_bin"]/sqrt(diag(M4$var.coef))
pvalue = 2*pt(valori["week_covid_bin"] ,221)
pvalue

valori <- M4$coef["week_rossa_sum"]/sqrt(diag(M4$var.coef))
pvalue = 2*pt(valori["week_rossa_sum"] ,221)
pvalue

# verifica adattamento modello
autoplot(M4$fitted) + autolayer(vendite1_sett_avg)
# autoplot(M34fitted) + autolayer(train_auto)

# si procede ora utilizzando il modello ottenuto per fare previsioni su dati nuovi,
# in particolare si cerca di prevedere le vendite dopo aprile 2021, date per cui 
# non si hanno a disposizone informazioni relative a vendite. In particolare si cerca 
# di prevedere per il periodo 12 aprile 2021 - 12 agosto 2021, date per cui si 
# possono ricavare i valori dei regressori ma non si possono avere i valori di 
# vendite, valori che dunque vengono previsti utilzizando il modello precedente
# e i regressori ottenuti per le nuove date

# N.b.: il 12 agosto è un giovedì, quindi non viene presa in considerazione la 
# settimana intera ma ciò è non influenza il valore dei regressori (la settimana 
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
regressori_forecast_day <- data.frame(colori_emilia_romagna_new)  # deve essere l'analogo di regressori_forecast_day
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
regressori_forecast_week$week_covid_bin <- as.factor(regressori_forecast_week$week_covid_bin)

regressori_forecast_week <- regressori_forecast_week %>%
  mutate(week_rossa_bin = ifelse(week_rossa_sum>4, 1, 0))
regressori_forecast_week$week_rossa_bin <- as.factor(regressori_forecast_week$week_rossa_bin)

regressori_forecast_week <- regressori_forecast_week %>%
  mutate(week_chiuso_bin = ifelse(week_chiuso_sum>4, 1, 0))
regressori_forecast_week$week_chiuso_bin <- as.factor(regressori_forecast_week$week_chiuso_bin)


# previsione vendite settimanali su dati nuovi
forecast_2021 <- M4 %>%
  forecast(h=18, xreg =data.matrix(regressori_forecast_week[, c("week_covid_bin", "week_rossa_sum", "week_chiuso_sum")])) 

autoplot(forecast_2021)




### RANDOM FOREST --------------------------

# https://www.pluralsight.com/guides/machine-learning-for-time-series-data-in-
mape <- function(actual,pred){
  mape <- mean(abs((actual - pred)/actual))*100
  return (mape)
}
set.seed(100)
library(randomForest)

vendite_giornaliere_prev_post_aprile <- ristorazione

# sistemazione NA
sum(is.na(vendite_giornaliere_prev_post_aprile))

sum(is.na(vendite_giornaliere_prev_post_aprile$vendite1))
vendite_giornaliere_prev_post_aprile$vendite1[is.na(vendite_giornaliere_prev_post_aprile$vendite1)] <- 0 


# implementazione modelli
rf1_prev_post_aprile = randomForest(vendite1 ~ is_holiday + is_weekend +pioggia+  covid + stagione 
                   + weekday + solo_asporto_emilia_romagna + rossa_emilia_romagna
                   + tot_vaccini_emilia_romagna + mese, data = vendite_giornaliere_prev_post_aprile)
varImpPlot(rf1_prev_post_aprile)
print(rf1_prev_post_aprile)

# si selezionano le variabili più rilevanti
rf2_prev_post_aprile = randomForest(vendite1 ~ weekday + is_weekend + covid + rossa_emilia_romagna + mese,
                   data = vendite_giornaliere_prev_post_aprile)
varImpPlot(rf2_prev_post_aprile)
print(rf2_prev_post_aprile)

vendite_giornaliere_prev_post_aprile <- data.frame(vendite_giornaliere_prev_post_aprile$data,vendite_giornaliere_prev_post_aprile$vendite1, vendite_giornaliere_prev_post_aprile$weekday,
                                  vendite_giornaliere_prev_post_aprile$is_weekend,vendite_giornaliere_prev_post_aprile$covid, vendite_giornaliere_prev_post_aprile$rossa_emilia_romagna,
                                  vendite_giornaliere_prev_post_aprile$mese)
names(vendite_giornaliere_prev_post_aprile)<- c("data", "vendite1","weekday","is_weekend","covid","rossa_emilia_romagna","mese")

data = seq(from = as.Date("2021-04-13"), to = as.Date("2021-08-12"), by = 'day')

vendite_giornaliere_prev_post_aprile_forecast <- data.frame(data)
# aggiunta colonna mese
# ristorazione$mese <- format(ristorazione$data,"%B")
vendite_giornaliere_prev_post_aprile_forecast$mese <- month(vendite_giornaliere_prev_post_aprile_forecast$data)
ristorazione$mese <- as.factor(ristorazione$mese)


# colonna giorni festivi e feriali
vendite_giornaliere_prev_post_aprile_forecast <- vendite_giornaliere_prev_post_aprile_forecast %>%
  mutate(weekday = wday(data, week_start = getOption("lubridate.week.start", 1))) %>%
  mutate(tipo_giorno = case_when(
    (weekday %in% c(6,7)) ~ "weekend"
    , (weekday < 7) ~ "weekday"
    , TRUE ~ "other"
  )
  )
vendite_giornaliere_prev_post_aprile_forecast$weekday <- as.factor(vendite_giornaliere_prev_post_aprile_forecast$weekday)
vendite_giornaliere_prev_post_aprile_forecast["tipo_giorno"][vendite_giornaliere_prev_post_aprile_forecast["tipo_giorno"] == "weekend"] <- "1"
vendite_giornaliere_prev_post_aprile_forecast["tipo_giorno"][vendite_giornaliere_prev_post_aprile_forecast["tipo_giorno"] == "weekday"] <- "0"
colnames(vendite_giornaliere_prev_post_aprile_forecast)[which(names(vendite_giornaliere_prev_post_aprile_forecast) == "tipo_giorno")] <- "is_weekend"
vendite_giornaliere_prev_post_aprile_forecast$is_weekend <- as.factor(vendite_giornaliere_prev_post_aprile_forecast$is_weekend)

# covid aggiornato fino al 12 agosto
vendite_giornaliere_prev_post_aprile_forecast$covid <- 1  # il covid è presente


# colori aggiornati fino al 12 agosto 2021

colori_zone_aggiornato <- read_csv("colori_zone_aggiornato.csv")  # https://github.com/imcatta/restrizioni_regionali_covid/blob/main/dataset.csv

colori_emilia_romagna_new <- colori_zone_aggiornato %>% 
  filter(denominazione_regione == "Emilia-Romagna")
names(colori_emilia_romagna_new)[3] <- "colore_emilia_romagna"

reference_date_colori <- as.Date("2021-04-12", format = "%Y-%m-%d")  

colori_emilia_romagna_new <- colori_emilia_romagna_new  %>% 
  filter(data > reference_date_colori)


# creazione df (dal 12 aprile 2021 al 12 agosto 2021) 
vendite_giornaliere_prev_post_aprile_forecast$colore_emilia_romagna <- colori_emilia_romagna_new$colore_emilia_romagna  # deve essere l'analogo di regressori_forecast_day


# colonna zona rossa
vendite_giornaliere_prev_post_aprile_forecast$rossa_emilia_romagna <- ifelse(vendite_giornaliere_prev_post_aprile_forecast$colore_emilia_romagna == "rosso", 1, 0)  # no zone rosse
vendite_giornaliere_prev_post_aprile_forecast$rossa_emilia_romagna<- as.factor(vendite_giornaliere_prev_post_aprile_forecast$rossa_emilia_romagna)
vendite_giornaliere_prev_post_aprile_forecast$covid<- as.factor(vendite_giornaliere_prev_post_aprile_forecast$covid)
vendite_giornaliere_prev_post_aprile_forecast$colore_emilia_romagna <- as.factor(vendite_giornaliere_prev_post_aprile_forecast$colore_emilia_romagna)
vendite_giornaliere_prev_post_aprile_forecast$mese<- as.factor(vendite_giornaliere_prev_post_aprile_forecast$mese)

vendite_giornaliere_prev_post_aprile_forecast$vendite1 <- 0

vendite_giornaliere_prev_post_aprile_forecast <- data.frame(vendite_giornaliere_prev_post_aprile_forecast$data,vendite_giornaliere_prev_post_aprile_forecast$vendite1, vendite_giornaliere_prev_post_aprile_forecast$weekday,
                                  vendite_giornaliere_prev_post_aprile_forecast$is_weekend,vendite_giornaliere_prev_post_aprile_forecast$covid, vendite_giornaliere_prev_post_aprile_forecast$rossa_emilia_romagna,
                                  vendite_giornaliere_prev_post_aprile_forecast$mese)
names(vendite_giornaliere_prev_post_aprile_forecast)<- c("data", "vendite1","weekday","is_weekend","covid","rossa_emilia_romagna","mese")

totale <-rbind(vendite_giornaliere_prev_post_aprile, vendite_giornaliere_prev_post_aprile_forecast)












# si utilizza il modello appena creato per fare previsioni sulle date in cui
# si registrano 0 vendite/scontrini
vendite_forecast <- predict(rf2_prev_post_aprile, totale[1563:1685,])
vendite_forecast <- as.data.frame(vendite_forecast)

# si uniscono le tue serie storiche
library("xts")

# serie storica previsioni durante periodo covid
interval <- seq(as.Date("2021-04-12"), as.Date("2021-08-12"), by = "day")
gfg_date <- data.frame(date = interval, 
                       val=vendite_forecast)
gfg_date$date<-as.Date(gfg_date$date)  
gfg_ts <- xts(gfg_date$val, gfg_date$date)

plot(gfg_date$date, gfg_date$vendite_forecast, xlab = "data", ylab = "vendite", type="l", main = "Ristorante 1")

# serie storica dati reali fino a prima covid 
reference_date_pre <- as.Date("2021-04-11", format = "%Y-%m-%d")
vendite_pre <- ristorazione %>%
  filter(ristorazione$data <= reference_date_pre) %>%
  select(data, vendite1)

interval_pre <- seq(as.Date("2017-01-01"), as.Date("2021-04-11"), by = "day")
gfg_date_pre <- data.frame(date = interval_pre, 
                           val=vendite_pre$vendite1)

gfg_date_pre$date<-as.Date(gfg_date_pre$date)  
gfg_ts_pre <- xts(gfg_date_pre$val, gfg_date_pre$date)

# si uniscono le due serie storiche
names(gfg_date)[1] <- "data"
names(gfg_date)[2] <- "vendite"

names(gfg_date_pre)[1] <- "data"
names(gfg_date_pre)[2] <- "vendite"

merge_df <- rbind(gfg_date, gfg_date_pre)
merge_df <- merge_df[order(merge_df$data), ]
row.names(merge_df) <- NULL
merge_df$vendite[is.na(merge_df$vendite)] <- 0 
# serie storica con previsioni
plot(merge_df$data, merge_df$vendite, xlab = "data", ylab = "vendite", type="l", main = "Ristorante 1 previsioni")
ristorazione_temp <- ristorazione

# serie storica originale
ristorazione_temp$vendite1[is.na(ristorazione_temp$vendite1)] <- 0 
plot(ristorazione_temp$data, ristorazione_temp$vendite1, xlab = "data", ylab = "vendite", type="l", main = "Ristorante 1 dati reali")


# sia per random forest sia per arima accorciare periodo pre covid altrimenti si 
# prendono già giorni in cui le vendite stavano già andando a picco




### Auto Arima con regressori----
#  il modello viene addestrato su tutti i dati a disposizione (vendite1_sett_avg) 
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


# dati rossa su base settimanale (somma, si contano i giorni della settimana in cui c'è zona rossa)
ristorante1$rossa_emilia_romagna <- as.numeric(as.character(ristorante1$rossa_emilia_romagna))
week_rossa_sum <- aggregate(rossa_emilia_romagna ~ week_rist1, ristorante1[-c(1,1563),], sum)
week_rossa_sum <- week_rossa_sum$rossa_emilia_romagna

regressori_week <- data.frame(week_covid_sum, week_rossa_sum)

# trasformazione colonne precedenti in valori binari
regressori_week <- regressori_week %>%
  mutate(week_covid_bin = ifelse(week_covid_sum>0, 1, 0))  # se almeno un giorno durante la settimana ha registrato il covid allora tale settimana viene etichettata come settimana covid
regressori_week$week_covid_bin <- as.factor(regressori_week$week_covid_bin)

regressori_week <- regressori_week %>%
  mutate(week_rossa_bin = ifelse(week_rossa_sum>4, 1, 0))  # se un ristorante durante la settimana è in zona rossa per più di 4 giorni allora tale settimana viene etichettata come settimana rossa
regressori_week$week_rossa_bin <- as.factor(regressori_week$week_rossa_bin)

# verifica collinearità variabili
library(corrplot)

numeric.var <- sapply(regressori_week, is.numeric)
corr.matrix <- cor(regressori_week[,numeric.var])
corrplot(corr.matrix, main="\n\nCorrelation Plot for Numerical Variables", method="number")

# regressori: "covid_bin", "rossa_sum", "chiuso_sum" (check se serve as.factor())

M4 <- auto.arima(vendite1_sett_avg, seasonal = TRUE, 
                 xreg = data.matrix(regressori_week[, c("week_covid_bin", "week_rossa_bin")]))
summary(M4)  # AIC: 3453.91   
checkresiduals(M4)
tsdisplay(residuals(M4), lag.max=52, main='Seasonal Model Residuals')


valori <- M4$coef["week_covid_bin"]/sqrt(diag(M4$var.coef))
pvalue = 2*pt(valori["week_covid_bin"] ,221)
pvalue

valori <- M4$coef["week_rossa_bin"]/sqrt(diag(M4$var.coef))
pvalue = 2*pt(valori["week_rossa_bin"] ,221)
pvalue

# verifica adattamento modello
autoplot(M4$fitted) + autolayer(vendite1_sett_avg)
# autoplot(M34fitted) + autolayer(train_auto)

# si procede ora utilizzando il modello ottenuto per fare previsioni su dati nuovi,
# in particolare si cerca di prevedere le vendite dopo aprile 2021, date per cui 
# non si hanno a disposizone informazioni relative a vendite. In particolare si cerca 
# di prevedere per il periodo 12 aprile 2021 - 12 agosto 2021, date per cui si 
# possono ricavare i valori dei regressori ma non si possono avere i valori di 
# vendite, valori che dunque vengono previsti utilzizando il modello precedente
# e i regressori ottenuti per le nuove date

# N.b.: il 12 agosto è un giovedì, quindi non viene presa in considerazione la 
# settimana intera ma ciò è non influenza il valore dei regressori (la settimana 
# è comunque etichettata covid, non ci sono possibili chiusure e non c'è zona rossa)

# per procedere bisogna prima avere i valori dei regressori per le date per cui
# verranno eseguite le previsioni

# colori aggiornati fino al 12 agosto 2021
colori_zone_aggiornato <- read_csv("dati/colori_zone_aggiornato.csv")
colori_zone_aggiornato <- read_csv("colori_zone_aggiornato.csv")  # https://github.com/imcatta/restrizioni_regionali_covid/blob/main/dataset.csv

colori_emilia_romagna_new <- colori_zone_aggiornato %>% 
  filter(denominazione_regione == "Emilia-Romagna")
names(colori_emilia_romagna_new)[3] <- "colore_emilia_romagna"

reference_date_colori <- as.Date("2021-04-11", format = "%Y-%m-%d")  

colori_emilia_romagna_new <- colori_emilia_romagna_new  %>% 
  filter(data > reference_date_colori)


# creazione df (dal 12 aprile 2021 al 12 agosto 2021) 
regressori_forecast_day <- data.frame(colori_emilia_romagna_new)  # deve essere l'analogo di regressori_forecast_day
regressori_forecast_day <- regressori_forecast_day[,-2]

# colonna zona rossa
regressori_forecast_day$rossa <- ifelse(regressori_forecast_day$colore_emilia_romagna == "rosso", 1, 0)  # no zone rosse


# covid aggiornato fino al 12 agosto
regressori_forecast_day$covid <- 1  # il covid è presente

# divisione in settimane
week_new_rist1 <- as.Date(cut(regressori_forecast_day$data, "week"))

week_rossa_new <- aggregate(rossa ~ week_new_rist1, regressori_forecast_day, sum)  # per settimana
week_covid_new <- aggregate(covid ~ week_new_rist1, regressori_forecast_day, sum)  # per settimana

regressori_forecast_week <- data.frame(week_covid_new$covid, week_rossa_new$rossa)
colnames(regressori_forecast_week) <- c("week_covid_sum", "week_rossa_sum")

# trasformazione colonne precedenti in valori binari
regressori_forecast_week <- regressori_forecast_week %>%
  mutate(week_covid_bin = ifelse(week_covid_sum>0, 1, 0))
regressori_forecast_week$week_covid_bin <- as.factor(regressori_forecast_week$week_covid_bin)

regressori_forecast_week <- regressori_forecast_week %>%
  mutate(week_rossa_bin = ifelse(week_rossa_sum>4, 1, 0))
regressori_forecast_week$week_rossa_bin <- as.factor(regressori_forecast_week$week_rossa_bin)


# previsione vendite settimanali su dati nuovi
forecast_2021 <- M4 %>%
  forecast(h=18,  xreg =data.matrix(regressori_forecast_week[, c("week_covid_bin", "week_rossa_bin")])) 

autoplot(forecast_2021)

# in commit precedenti non vi erano salti in questi grafico, però potrebbe essere
# ritenuto giusto anche questo




### PROPHET ----------------------------
library(rstan)
library(prophet)

df <- data.frame(ristorante1$data, ristorante1$vendite)
tail(df)

names(df) <- c('ds', 'y') 

m <- prophet(df, daily.seasonality=FALSE)

future <- make_future_dataframe(m, periods=90)

forecast <- predict(m, future)
tail(forecast)

plot(m, forecast)
dyplot.prophet(m, forecast)
prophet_plot_components(m, forecast)

df.cv <- cross_validation(m, initial=180, period=60, horizon=120, units='days')
tail(df.cv)

plot_cross_validation_metric(df.cv, metric = 'rmse')

###covid con prophet e regressore-----
covid <- function(ds) {
  dates <- as.Date(ds)
  reference_date_pre <- as.Date("2020-01-05", format = "%Y-%m-%d")
  as.numeric(dates > reference_date_pre)
}
df$covid <- covid(df$ds)

m <- prophet(daily.seasonality=FALSE)
m <- add_regressor(m, 'covid')
m <- fit.prophet(m, df)

future$covid <- covid(future$ds)

forecast <- predict(m, future)
prophet_plot_components(m, forecast)
plot(m, forecast)
dyplot.prophet(m, forecast)
df.cv <- cross_validation(m, initial=180, period=60, horizon=120, units='days')
tail(df.cv)

plot_cross_validation_metric(df.cv, metric = 'rmse')



regressori_week$week_covid_bin <- as.numeric(regressori_week$week_covid_bin)
regressori_week$week_chiuso_bin <- as.numeric(regressori_week$week_chiuso_bin )
regressori_week$week_rossa_bin <- as.numeric(regressori_week$week_rossa_bin)


regressori_forecast_week$week_covid_bin <- as.numeric(regressori_forecast_week$week_covid_bin)
regressori_forecast_week$week_chiuso_bin <- as.numeric(regressori_forecast_week$week_chiuso_bin )
regressori_forecast_week$week_rossa_bin <- as.numeric(regressori_forecast_week$week_rossa_bin)



M4 <- auto.arima(vendite1_sett_avg, seasonal = TRUE, 
                 xreg = regressori_week$week_chiuso_bin )
summary(M4)  # AIC: 3453.91 

forecast_2021 <- M4 %>%
  forecast(h=18,  xreg =regressori_forecast_week$) 

regressori_forecast_week[10:18,"week_chiuso_bin"] <-0


autoplot(fc.c1)
fc.c1 <- forecast(M4, xreg = regressori_forecast_week$week_chiuso_bin)
