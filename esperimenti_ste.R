#### CESTINO ---------------------------------------------------------------------------
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





## PROVA REGRESSORE------------------------------------------

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
  forecast(h=50, xreg= test_neve) %>%  # h Number of periods for forecasting
  autoplot() + autolayer(test_auto)


forecast <- MR6 %>%
  forecast(h=10, xreg= test_neve) 

# alternativa per vedere previsioni con test set
par(mfrow=c(1,1))
plot(forecast)
lines(test_auto, col="red")
legend("topleft",lty=1,bty = "n",col=c("red","blue"),c("testData","ARIMAPred"))

# valutazione qualità previsioni
accuracy(forecast, test_auto[1:10])

