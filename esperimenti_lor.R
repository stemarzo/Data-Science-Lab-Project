# WORKING IN PROGRESS #

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





# random forest con regressori----

# si utilizza vendite1_day_pre

vendite1_day_pre_split_auto <- ts_split(vendite1_day_pre)
train_auto_pre <- vendite1_day_pre_split_auto$train
test_auto_pre <- vendite1_day_pre_split_auto$test

autoplot(vendite1_day_pre) +
  autolayer(train_auto_pre, series="Training") +
  autolayer(train_auto_pre, series="Test")

mape <- function(actual,pred){
  mape <- mean(abs((actual - pred)/actual))*100
  return (mape)
}


set.seed(100)

library(randomForest)

rf = randomForest(vendite1_day_pre ~ Inventory + year + yday + quarter + month + day + weekdays + weekend + week, data = train)

print(rf)

# https://www.pluralsight.com/guides/machine-learning-for-time-series-data-in-r





