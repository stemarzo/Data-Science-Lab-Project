# WORKING IN PROGRESS #

# esplorazione ristoranti ----

# evidenziare che maggio 2020 (post covid) registra più vendite rispetto a maggio 2019



# confronto estati ----

# si procede con un'analisi più approfondita e più tencica del grafico per rispondere 
# alla domanda di ricerca: ovvero nel 2020 c'era pochissime restrizioni, quasi 
# zero però si vuole dimostrare che non è stata un'estate normale come lo può 
# essere quella del 2019

# esempio analisi:

# Just came across this. Your first answer us plotting g the two sets the same 
# scale (timewise) to see the differences visually. You have done this and can easily 
# see there are some glaring differences. The next step is to use simple correlation 
# analysis...and see how well are they related using the correlation coefficient (r). 
# If the r is small your conclusion would be that they are weakly related and so no 
# desirable comparisons and a larger value if r would suggest good comparisons s between 
# the two series. The third step where there is good correlation is to test the statistical 
# significance of the r. Here you can use the Shapiro Welch test which would assume the two 
# series are normally distributed (null hypothesis ) or not (alternative hypothesis). 
# There are other tests you can do but let me hope my answer helps.

# To compare two time series simply estimate the COMMON appropriate arima model 
# for each time series separately AND then estimate it globally ( putting the second 
# series behind the first ) . Make sure that your software recognizes the beginning 
# of the scond series and doesn't forecast it from the latter values of the first series. 
# Perform an F test ala G. Chow to test the hypothesis of a common set of parameters. AUTOBOX , 
# a program that I am involved with allows this test to be performed. SPSS may not.



# prophet con periodo accorciato ----

library(prophet)

# l'ultima settimana considerata è quella che va dal 30/12/19 al 05/01/20
reference_date <- as.Date("2020-01-05", format = "%Y-%m-%d")

prophet_vendite <- ristorazione %>% 
  filter(data < reference_date) %>%
  select(data, vendite1)

colnames(prophet_vendite) <- c("ds", "y")

m <- prophet(prophet_vendite)

future <- make_future_dataframe(m, periods=365)

forecast <- predict(m, future)
tail(forecast[c('ds', 'yhat', 'yhat_lower', 'yhat_upper')])
# yhat containing the forecast. It has additional columns for uncertainty intervals and seasonal components

plot(m, forecast)

prophet_plot_components(m, forecast)

df.cv <- cross_validation(m, initial=180, period=60, horizon=120, units='days')

plot_cross_validation_metric(df.cv, metric='mape')

dyplot.prophet(m, forecast)



# random forest ----
# periodo accorciato
# il train si ferma al 5 gennaio
# https://www.pluralsight.com/guides/machine-learning-for-time-series-data-in-r
mape <- function(actual,pred){
  mape <- mean(abs((actual - pred)/actual))*100
  return (mape)
}
set.seed(100)
library(randomForest)

# selezione periodo pre covid per poi fare previsione sul periodo covid
# bisogna considerar un periodo più corto altrimenti si è troppo vicini al covid,
# e i dati potrebbero risentirne
reference_date <- as.Date("2020-01-06", format = "%Y-%m-%d")
# vendite gionaliere pre covid
vendite_giornaliere <- ristorazione %>%
  filter(ristorazione$data < reference_date)

# sistemazione NA
sum(is.na(vendite_giornaliere))

sum(is.na(vendite_giornaliere$vendite1))
vendite_giornaliere$vendite1[is.na(vendite_giornaliere$vendite1)] <- 0 

# divisione in train e tes
index <- sample(1:nrow(vendite_giornaliere),size = 0.7*nrow(vendite_giornaliere))
train <- vendite_giornaliere[index,]
test <- vendite_giornaliere[-index,] 
dim(train)
dim(test)

# implementazione modelli
rf1 = randomForest(vendite1 ~ is_holiday + is_weekend + pioggia + covid + stagione 
                   + weekday + solo_asporto_emilia_romagna + rossa_emilia_romagna
                   + tot_vaccini_emilia_romagna + mese, data = train)
varImpPlot(rf1)
print(rf1)

# si selezionano le variabili più rilevanti
rf2 = randomForest(vendite1 ~ weekday + is_weekend + mese + is_holiday + stagione,
                   data = train)
varImpPlot(rf2)
print(rf2)

predictions = predict(rf2, newdata = train)
mape(train$vendite1, predictions)

predictions = predict(rf2, newdata = test)
mape(test$vendite1, predictions)

RMSE.forest <- sqrt(mean((predictions-test$vendite1)^2))
RMSE.forest

MAE.forest <- mean(abs(predictions-test$vendite1))
MAE.forest

# predizioni su valori nuovi (sul periodo covid dove nei dati reali si hanno 0)

# selezione periodo post covid
reference_date <- as.Date("2020-01-06", format = "%Y-%m-%d")
# vendite giornaliere periodo covid
vendite_giornaliere_forecast <- ristorazione %>%
  filter(ristorazione$data >= reference_date)

# si selezioanno le date in cui si registrano 0 vendite/scontrini
vendite_giornaliere_forecast <- vendite_giornaliere_forecast[1:133,]

# si utilizza il modello appena creato per fare previsioni sulle date in cui
# si registrano 0 vendite/scontrini
vendite_forecast <- predict(rf2, vendite_giornaliere_forecast)
vendite_forecast <- as.data.frame(vendite_forecast)

# si uniscono le tue serie storiche
library("xts")

# serie storica previsioni durante periodo covid
interval <- seq(as.Date("2020-01-06"), as.Date("2020-05-17"), by = "day")
gfg_date <- data.frame(date = interval, 
                       val=vendite_forecast)
gfg_date$date<-as.Date(gfg_date$date)  
gfg_ts <- xts(gfg_date$val, gfg_date$date)

plot(gfg_date$date, gfg_date$vendite_forecast, xlab = "data", ylab = "vendite", type="l", main = "Ristorante 1")

# serie storica dati reali fino a prima covid 
reference_date_pre <- as.Date("2020-01-05", format = "%Y-%m-%d")
vendite_pre <- ristorazione %>%
  filter(ristorazione$data <= reference_date_pre) %>%
  select(data, vendite1)

interval_pre <- seq(as.Date("2017-01-01"), as.Date("2020-01-05"), by = "day")
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

# serie storica con previsioni
plot(merge_df$data, merge_df$vendite, xlab = "data", ylab = "vendite", type="l", main = "Ristorante 1 previsioni")
ristorazione_temp <- ristorazione[1:1228,]

# serie storica originale
ristorazione_temp$vendite1[is.na(ristorazione_temp$vendite1)] <- 0 
plot(ristorazione_temp$data, ristorazione_temp$vendite1, xlab = "data", ylab = "vendite", type="l", main = "Ristorante 1 dati reali")




# arima con regressori diversi ----

# si può valutare anche modello SARIMA e VAR (per gestire + serie storiche)

# bisogna provare con altre variabili: tipo is weekend, is holiday, 
# noi utilizziamo ad ora solo variabili che sono legate al covid, non è troppo 
# rigida questa scelta? I colori della zone sono abbastanza limitati, partendo da 
# novembre essi agiscono su una piccola porzione di dati

# ricordarsi come nel random forest modificare periodi pre e post covid, il periodo
# pre dovrebbe fermarmi un pochino prima del picco





























# arima con periodo accorciato ----
# nel main l'arima veniva usato su tutti i dati, qui si usa per fare previsioni per il periodo "post" covid,
# quindi le vendite "pre" covid verranno divise in train e test

# modifico temporaneamente il periodo considerato (presente in esplorazione rist 1)
# si procede ad analizzare ciascun ristorante nel periodo antecedente il covid-19 
reference_date <- as.Date("2020-01-06", format = "%Y-%m-%d")

# vendite ristorante 1 pre covid
ristorante1_pre_covid_vendite <- ristorante1 %>%
  filter(ristorante1$data < reference_date) %>%
  select(vendite, data)

# vendite giornaliere primo ristorante pre covid
vendite1_day_pre <- ts(ristorante1_pre_covid_vendite$vendite,start=2017,frequency=365) 

# vendite settimanali medie primo ristorante pre covid
week_pre_covid_rist1 <- as.Date(cut(ristorante1_pre_covid_vendite$data, "week"))

# si procede ad eliminare il giorno 1 gennaio 2017 che risulta essere domenica
remove_dates <- as.Date(c('2016-12-26'))
all_dates <- week_pre_covid_rist1
week_pre_covid_rist1 <- all_dates[!all_dates %in% remove_dates]

vendite1_sett_pre <- aggregate(vendite ~ week_pre_covid_rist1, ristorante1_pre_covid_vendite[-1,], sum)
vendite1_sett_pre <- vendite1_sett_pre$vendite
vendite1_sett_pre <- ts(vendite1_sett_pre,start=2017,frequency=52) 

vendite1_sett_avg_pre <- aggregate(vendite ~ week_pre_covid_rist1, ristorante1_pre_covid_vendite[-1,], mean)
vendite1_sett_avg_pre <- vendite1_sett_avg_pre$vendite
vendite1_sett_avg_pre <- ts(vendite1_sett_avg_pre,start=2017,frequency=52) 


# procedo con modello (in rest analysis)






















