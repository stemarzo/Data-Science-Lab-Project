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


# tentativo 4 ----
# lisciamento esponenziale 
# https://webs.rete.toscana.it/lso/c/document_library/get_file?uuid=dcfe244a-3038-4e00-a2c6-fff1aa19be72&groupId=38526

# tentativo 4 ----

library(tseries)

vendite1_day_pre
vendite1_day_pre_diff <- diff(vendite1_day_pre)

# visualize data
autoplot(vendite1_day_pre)
autoplot(vendite1_day_pre_diff)


# check stationarity
adf.test(vendite1_day_pre, k=1)
# con lag = 1 i dati sono stazionari

# altro test
pp.test(vendite1_day_pre)

# altro test
kpss.test(vendite1_day_pre)

library(TSstudio)
arima_diag(vendite1_day_pre)




check_res(M4)


# altri link da vedere ----
# https://rpubs.com/riazakhan94/arima_with_example


# materiale per valutazioni performance modelli ----

# esempio Random Forest:

library(ROCR)
# eventuali altre librerie

# Fitting The Model
# install.packages(randomForest)
# memory.limit(100000)
rf.model <- randomForest(CHURN ~  TOT_PURCHASE + NUM_OF_PURCHASES + RECENCY + REGION + LAST_COD_FID + TYP_CLI_ACCOUNT + FIRST_ID_NEG,
                         data = train , ntree = 100)
print(rf.model)

plot(rf.model)  # stima dell’errore di test chiamato Out Of Bag (OOB) error
# fornisce supporto nella selezione del numero di alberi
# oltre 40 e 50 alberi non è possibile diminuire ulteriormente l'error


varImpPlot(rf.model, sort=T, n.var = 4, main = 'Features Importance')
# come si può notare la variabile più importante risulta essere RECENCY


# Making Predictions
rf.pred <- predict(rf.model, test, type = "class")  
rf.prob <- predict(rf.model, test, type="prob")
qplot(x=rf.prob[, "1"], geom="histogram")

# Evaluating The Model
rf.result <- confusionMatrix(rf.pred, test$CHURN)

table(Predicted = rf.pred, Actual = test$CHURN)
#               Actual
# Predicted     0     1
#           0  4596  2389
#           1  7223 20093

rf.accuracy <- Accuracy(rf.pred,test$CHURN) # 0.7197749
rf.precision <- precision(rf.pred, test$CHURN,relevant = '1') # 0.7355762
rf.recall <- recall(rf.pred, test$CHURN,relevant = '1') # 0.8937372
rf.F1 <- F1_Score(rf.pred, test$CHURN,positive = '1') # 0.8069802


# ROC
rf.pr <- prediction(rf.prob[,2], test$CHURN)
rf.prf <- performance(rf.pr, measure = "tpr", x.measure = "fpr")
plot(rf.prf, main = "ROC RANDOM FOREST")
abline(a = 0,b = 1,lwd = 2,lty = 3,col = "black")


# AUC 
rf.auc <- performance(rf.pr, measure = "auc")
rf.auc <- rf.auc@y.values[[1]]
rf.auc  # 0.7386719



