# TBATS ----

#### periodo covid ----
# https://robjhyndman.com/hyndsight/seasonal-periods/
vendite1_day_pre_split_tbats <- ts_split(vendite1_day_pre)
train_tbats <- vendite1_day_pre_split_tbats$train
test_tbats <- vendite1_day_pre_split_tbats$test

tbats_data <- msts(train_tbats, seasonal.periods=c(7,365.25))
M6 <- tbats(tbats_data)

#  considerando test set
vendite_forecast_tbats <- forecast(M6, h=length(test_tbats))
autoplot(vendite_forecast_tbats, vendite1_day_pre.colour = 'black')

M6 %>%
  forecast(h=length(test_tbats)) %>%  
  autoplot() + autolayer(test_tbats)

# performance
rmse_tbats <- sqrt(M6$variance)  # 1056.667
mape_tbats <- mape(vendite_forecast_tbats$mean, test_tbats)  # 17.01783

# previsioni periodo covid
M6 %>%
  forecast(h=463) %>%  # fino a metà maggio circa
  autoplot() + autolayer(train_tbats)

# cofronto con valori reali
autoplot(ts(vendite1_day[1:1233], start=2017,frequency=365))



#### periodo post aprile 2021 ----

tbats_data <- msts(vendite1_day, seasonal.periods=c(7,365.25))
M7 <- tbats(tbats_data)

# previsioni post aprile 2021
vendite_forecast_tbats <- forecast(M7, h=100)
autoplot(vendite_forecast_tbats, tbats_data = 'black')





# sarima ----
# https://medium.com/analytics-vidhya/time-series-forecasting-sarima-vs-auto-arima-models-f95e76d71d8f

# 836.9348 RMSE PREC

vendite1_sett_avg_pre_split_auto_arima <- ts_split(vendite1_sett_avg_pre)
train_auto_arima <- vendite1_sett_avg_pre_split_auto_arima$train
test_auto_arima <- vendite1_sett_avg_pre_split_auto_arima$test

arima_optimal = auto.arima(train_auto_arima)
# ARIMA(0,1,1)(0,1,0)[52] 


library(astsa)
sarima_forecast = sarima.for(train_auto_arima, n.ahead=length(test_auto_arima),
                             p=0,d=1,q=1,P=0,D=1,Q=0,S=52)
library(MLmetrics)
MAPE(sarima_forecast$pred, test_auto_arima) * 100  # 8.145823

accuracy(sarima_forecast$pred, test_auto_arima)  # 797.8461


