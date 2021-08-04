# WORKING IN PROGRESS #


# lisciamento esponenziale ----

# https://webs.rete.toscana.it/lso/c/document_library/get_file?uuid=dcfe244a-3038-4e00-a2c6-fff1aa19be72&groupId=38526



# arima ----


vendite1_day_pre

vendite1_day_pre_split <- ts_split(vendite1_day_pre)

train <- vendite1_day_pre_split$train
autoplot(train)
test <- vendite1_day_pre_split$test
autoplot(test)


arima1 <- auto.arima(train)

predict(arima1,n.ahead = 50)








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



