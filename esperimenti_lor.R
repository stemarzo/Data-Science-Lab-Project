# WORKING IN PROGRESS #

# considero la serie storica relativa al primo ristorante

# tentativo 1 ----
vendite1_day_pre
arima1 <- auto.arima(vendite1_day_pre)
arima1

train_index <- 800
n_total <- nrow(ristorante1_pre_covid)
rist1_vendite_train1 <- ristorante1_pre_covid[1:(train_index),]
rist1_vendite_test <- ristorante1_pre_covid[(train_index+1):n_total,]
predicted <- numeric(n_total-train_index)


for (i in 1:(n_total-train_index)) {
  shampoo_sales_train <- ristorante1_pre_covid[1:(train_index-1+i),]
  arima_model <- auto.arima(vendite1_day_pre)
  pred <- forecast(arima_model, 1)
  predicted[i] <- pred$mean
}


df_pred <- tibble(obs = c(rist1_vendite_train1$vendite, rist1_vendite_test$vendite), 
                  predicted = c(rist1_vendite_train1$vendite, predicted), 
                  time = ristorante1_pre_covid$data) 



ggplot(gather(df_pred, obs_pred, value, -time) %>% 
         mutate(obs_pred = factor(obs_pred, levels = c("predicted", "obs"))), 
       aes(x = time, y = value, col = obs_pred, linetype = obs_pred)) +
  geom_line() +
  xlab("") + ylab("") +
  scale_color_manual(values=c("black", "hotpink")) +
  scale_linetype_manual(values=c(2, 1)) +
  scale_x_date(date_labels = "%y %b", date_breaks = "2 month") +
  theme_bw() + theme(legend.title = element_blank(),
                     axis.text.x  = element_text(angle=45, vjust=0.5))

# tentativo 2 ----
tsData <- vendite1_day_pre

components.ts = decompose(tsData)
plot(components.ts)

library("fUnitRoots")
par(mfrow=c(1,1))

urkpssTest(tsData, type = c("tau"), lags = c("short"),use.lag = NULL, doplot = TRUE)
tsstationary = diff(tsData, differences=1)
plot(tsstationary)



acf(tsData,lag.max=34) 


timeseriesseasonallyadjusted <- tsData- components.ts$seasonal
tsstationary <- diff(timeseriesseasonallyadjusted, differences=1)

acf(tsstationary, lag.max=34)
pacf(tsstationary, lag.max=34)


fitARIMA <- auto.arima(tsData, trace=FALSE) 


predict(fitARIMA,n.ahead = 5)
library("fUnitRoots")
library(lmtest)
library(forecast)
library(FitAR)
futurVal <- forecast.Arima(fitARIMA,h=10, level=c(99.5))
plot.forecast(futurVal)

# tentativo 3 ----

library('ggplot2')
library('forecast')
library('tseries')

ggplot(ristorante1, aes(data, vendite)) + geom_line() + scale_x_date('month')  + ylab("Daily Sales") +
  xlab("")

vendite1_day_pre

