# TBATS ----

# tentativo 1
vendite1_day_pre_split_tbats <- ts_split(vendite1_day_pre)
train_tbats <- vendite1_day_pre_split_tbats$train
test_tbats <- vendite1_day_pre_split_tbats$test

tbats_model = tbats(train_tbats)
tbats_forecast = forecast(tbats_model, h=length(test_tbats))
plot(tbats_model, tbats_forecast)

# tentativo 2
# https://robjhyndman.com/hyndsight/seasonal-periods/
vendite1_day_pre_split_tbats <- ts_split(vendite1_day_pre)
train_tbats <- vendite1_day_pre_split_tbats$train
test_tbats <- vendite1_day_pre_split_tbats$test

tbats_data <- msts(train_tbats, seasonal.periods=c(7,365.25))
tbats_data.fit <- tbats(tbats_data)
tbats_forecast <- forecast(tbats_data.fit, h=length(test_tbats))

# performance
rmse_tbats <- sqrt(tbats_data.fit$variance)  # 1056.667
mape_tbats <- mape(tbats_forecast$mean, test_tbats)  # 17.01783

# grafico
autoplot(tbats_forecast, vendite1_day_pre.colour = 'black') +
  scale_color_manual(labels = c("Actual", "Forecasted"),
                     values=c("black", "blue")) +
  scale_size_manual(labels = c("Actual", "Forecasted"),
                    values = 5) +
  autolayer(tbats_forecast, series = 'Forecasts') +
  autolayer(tbats_data, series='Data') +
  guides(colour = guide_legend(title= 'Data series'))


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


