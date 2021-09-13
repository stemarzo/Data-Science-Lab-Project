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


















#proviamo il modello tbats a doppia stagionalit? sui dati giornalieri


vendite1_day_pre <- vendite1_day_pre[-1]
vendite1_day_pre <- vendite1_day_pre[-1562]

y <- msts(vendite1_day_pre, end=c(2021,95), seasonal.periods=c(7,365.25))
train <- window(y, end = c(2018, 365))
test <- window(y, start = c(2019, 1), end = c(2019,365))
te <- msts(test, start = c(2019,1), end=c(2019,365), seasonal.periods=c(7,365.25))

tr <- msts(train, end=c(2018,365), seasonal.periods=c(7,365.25))
fit <- tbats(tr)
fc <- forecast(fit, h=length(te))

#prova a visualizzare solo 2018 a 2020, oppure stampa grafico delle dispense

autoplot(fc, vendite1_day_pre.colour = 'black') +
  scale_color_manual(labels = c("Actual", "Forecasted"),
                     values=c("black", "blue")) +
  scale_size_manual(labels = c("Actual", "Forecasted"),
                    values = 5) +
  autolayer(fc, series = 'Forecasts') +
  autolayer(te, series='Data') +
  guides(colour = guide_legend(title= 'Data series')) +
  xlim(2018.75, 2019.25) +
  ggtitle('Previsione serie storica aggregato giornaliero') +
  labs(x='Anni', y='Vendite') +
  theme(legend.position = c(0.1, 0.88), legend.text=element_text(size=rel(3)), legend.title=element_text(size=rel(3.5)),
        axis.title = element_text(size = rel(3.5)), plot.title = element_text(size = rel(3)),
        axis.text.x = element_text(size = rel(3)), axis.text.y = element_text(size = rel(3)))





























