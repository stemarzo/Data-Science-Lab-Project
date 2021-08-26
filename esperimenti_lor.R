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






# arima con regressori----

# si può valutare anche modello SARIMA e VAR (per gestire + serie storiche)

# bisogna provare con altre variabili: tipo is weekend, is holiday, 
# noi utilizziamo ad ora solo variabili che sono legate al covid, non è troppo 
# rigida questa scelta? I colori della zone sono abbastanza limitati, partendo da 
# novembre essi agiscono su una piccola porzione di dati



# random forest con regressori----
# https://www.pluralsight.com/guides/machine-learning-for-time-series-data-in-r
# provo a utilizzare come varibaile scontrini1
mape <- function(actual,pred){
  mape <- mean(abs((actual - pred)/actual))*100
  return (mape)
}
set.seed(100)
library(randomForest)

reference_date <- as.Date("2020-03-09", format = "%Y-%m-%d")

vendite_giornaliere <- ristorazione %>%
  filter(ristorazione$data < reference_date)

numberOfRows <- nrow(vendite_giornaliere)
bound <- as.integer(numberOfRows *0.8)
train <- vendite_giornaliere[1:bound ,]
test <-  vendite_giornaliere[(bound+1):numberOfRows ,]

dim(train)
dim(test)

sum(is.na(train$scontrini1))  
train$scontrini1[is.na(train$scontrini1)] <- 0  
sum(is.na(train$pioggia))  
sum(is.na(train$is_weekend))  
sum(is.na(train$is_holiday)) 
sum(is.na(train$stagione))  

rf = randomForest(scontrini1 ~ is_holiday + is_weekend + pioggia + covid + stagione + weekday + solo_asporto_emilia_romagna, data = train)
rf = randomForest(scontrini1 ~ is_weekend + weekday, data = train)

varImpPlot(rf)
print(rf)

predictions = predict(rf, newdata = train)
mape(train$scontrini1, predictions)

predictions = predict(rf, newdata = test)
mape(test$vendite1, predictions)

rf.result <- confusionMatrix(predictions, test$scontrini1)



