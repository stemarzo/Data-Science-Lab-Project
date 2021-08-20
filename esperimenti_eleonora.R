#PREVISIONE FATTURATO CON COVID RANDOM FOREST 


View(ristorazione)
str(ristorazione)
ristorante1_pre_covid <- ristorazione[, colnames(ristorazione)[c(1,2,3,4,15,16,17,18,19,23,24,25,26,27,28,29,30,31,32,35,36,37,38)]]
View(ristorante1_pre_covid)


suppressPackageStartupMessages(require(tidyverse))
suppressPackageStartupMessages(require(tsibble))
suppressPackageStartupMessages(require(randomForest))
suppressPackageStartupMessages(require(forecast))

# implicit missings
sapply(ristorante1_pre_covid, function(x)(sum(is.na(x)))) 

newdata<- ristorante1_pre_covid[1:1166, ]  
ristorante1_pre_covid <- newdata
View(ristorante1_pre_covid)
sapply(ristorante1_pre_covid, function(x)(sum(is.na(x)))) 
ristorante1_pre_covid <- na.omit(ristorante1_pre_covid)



ristorante1_pre_covid$sky = factor(ristorante1_pre_covid$sky, levels = c(0, 1))
ristorante1_pre_covid$holiday = factor(ristorante1_pre_covid$holiday, levels = c(0, 1))
ristorante1_pre_covid$mediaset = factor(ristorante1_pre_covid$mediaset, levels = c(0, 1))
ristorante1_pre_covid$dazn = factor(ristorante1_pre_covid$dazn, levels = c(0, 1))
ristorante1_pre_covid$top = factor(ristorante1_pre_covid$top, levels = c(0, 1))
ristorante1_pre_covid$milan = factor(ristorante1_pre_covid$milan, levels = c(0, 1))
ristorante1_pre_covid$inter = factor(ristorante1_pre_covid$inter, levels = c(0, 1))
ristorante1_pre_covid$juventus = factor(ristorante1_pre_covid$juventus, levels = c(0, 1))
ristorante1_pre_covid$emilia = factor(ristorante1_pre_covid$emilia, levels = c(0, 1))
ristorante1_pre_covid$pioggia = factor(ristorante1_pre_covid$pioggia, levels = c(0, 1))
ristorante1_pre_covid$meteo_aperto = factor(ristorante1_pre_covid$meteo_aperto, levels = c(0, 1))
ristorante1_pre_covid$`Vacanze scolastiche Lombardia` = factor(ristorante1_pre_covid$`Vacanze scolastiche Lombardia`, levels = c(0, 1))
ristorante1_pre_covid$`Vacanze scolastiche Emilia-Romagna` = factor(ristorante1_pre_covid$`Vacanze scolastiche Emilia-Romagna`, levels = c(0, 1))
ristorante1_pre_covid$`Black Friday e saldi Lombardia` = factor(ristorante1_pre_covid$`Black Friday e saldi Lombardia`, levels = c(0, 1))
ristorante1_pre_covid$`Black Friday e saldi  Emilia-Romagna` = factor(ristorante1_pre_covid$`Black Friday e saldi  Emilia-Romagna`, levels = c(0, 1))

#install.packages('caTools')
library(caTools)
set.seed(123)
split = sample.split(ristorante1_pre_covid$vendite1, SplitRatio = 0.75)
training_set = subset(ristorante1_pre_covid, split == TRUE)
test_set = subset(ristorante1_pre_covid, split == FALSE)

training_set[-3] = scale(training_set[-3])
test_set[-3] = scale(test_set[-3])


head(training_set)
sapply(training_set, function(x)(sum(is.na(x)))) 
sapply(test_set, function(x)(sum(is.na(x)))) 
training_set <- na.omit(training_set)
test_set <- na.omit(test_set)


library(randomForest)
classifier = randomForest(x = training_set[-3],
                          y = training_set$vendite1,
                          ntree = 500, random_state = 0)

y_pred = predict(classifier, newdata = test_set[-3])

y_pred

#confusion matrix
cm = table(test_set[, 3], y_pred)
cm


dim(training_set)
dim(test_set)

View(training_set)

set.seed(100)
library(randomForest)
rf = randomForest(vendite1 ~ stagione + mese + weekday + holiday + tipo_giorno + sky + mediaset + dazn + top + milan + inter + juventus + emilia + pioggia + meteo_aperto, data = training_set)
print(rf)

predictions = predict(rf, newdata = training_set)
mape(training_set$vendite1, predictions)
predictions = predict(rf, newdata = test_set)
mape(training_set$vendite1, predictions) 

varImpPlot(rf)


#dal grafico risulta che solo weekday, tipo_giorno, holiday e mese sono significative
#bisognerebbe ripetere il modello con all'interno solo queste variabili 




