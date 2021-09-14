# TBATS

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

# grafico
autoplot(tbats_forecast, vendite1_day_pre.colour = 'black') +
  scale_color_manual(labels = c("Actual", "Forecasted"),
                     values=c("black", "blue")) +
  scale_size_manual(labels = c("Actual", "Forecasted"),
                    values = 5) +
  autolayer(tbats_forecast, series = 'Forecasts') +
  autolayer(tbats_data, series='Data') +
  guides(colour = guide_legend(title= 'Data series'))

