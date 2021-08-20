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

# vendite settimanali
vendite1_sett_avg


# aggiungo nuove colonne
ristorante1_copy <- ristorante1

# colonna covid 
ristorante1_copy$covid <- 0
ristorante1_copy[ristorante1_copy$data > "2020-03-09",]$covid <- 1

# colonna covid settimanale
# week_rist1 <- as.Date(cut(ristorante1_copy$data, "week"))
week_covid <- aggregate(covid ~ week_rist1, ristorante1_copy, sum)  # per settimana
# la colonna covid "somma" non ha molto senso

# colonna chiuso: numero giorni chiusura per settimana
week_chiuso <- aggregate(chiuso ~ week_rist1, ristorante1_copy, sum)  # per settimana


# colonna zona rossa
ristorazione$rossa <- ifelse(ristorazione$colore_emilia_romagna == "rosso", 1, 0)
ristorante1_copy$rossa <- ristorazione$rossa
week_rossa <- aggregate(rossa ~ week_rist1, ristorante1_copy, sum)  # per settimana
week_rossa <- week_rossa$rossa


# dataframe dati giornalieri
View(ristorante1_copy)

# dataframe dati settimanali
df7 <- data.frame(week_covid$covid, week_chiuso$chiuso, week_rossa)
View(df7)
colnames(df7) <- c("covid_sum", "chiuso_sum", "rossa_sum")


# trasformazione colonne precedenti in valori binari
df7 <- df7 %>%
  mutate(covid_bin = ifelse(covid_sum>0, 1, 0))

df7 <- df7 %>%
  mutate(rossa_bin = ifelse(rossa_sum>4, 1, 0))

df7 <- df7 %>%
  mutate(chiuso_bin = ifelse(chiuso_sum>4, 1, 0))

# verifica collinearità variabili
library(corrplot)

numeric.var <- sapply(df7, is.numeric)
corr.matrix <- cor(df7[,numeric.var])
corrplot(corr.matrix, main="\n\nCorrelation Plot for Numerical Variables", method="number")


# modello auto.arima con regressori

## PROVA1- regressori: "covid_bin", "rossa_sum", "chiuso_sum" ----
# per ora modello migliore
MT7 <- auto.arima(vendite1_sett_avg, seasonal = TRUE, 
                  xreg = data.matrix(df7[, c("covid_bin", "rossa_sum", "chiuso_sum")]))
summary(MT7)  # AIC: 3486.2   
checkresiduals(MT7)
tsdisplay(residuals(MT7), lag.max=52, main='Seasonal Model Residuals')

# verifica p-value
valori <- MT7$coef["chiuso_sum"]/sqrt(diag(MT7$var.coef))
pvalue = 2*pt(valori["chiuso_sum"] ,221)
pvalue

valori <- MT7$coef["covid_bin"]/sqrt(diag(MT7$var.coef))
pvalue = 2*pt(valori["covid_bin"] ,221)
pvalue

valori <- MT7$coef["rossa_sum"]/sqrt(diag(MT7$var.coef))
pvalue = 2*pt(valori["rossa_sum"] ,221)
pvalue

# verifica adattamento modello
autoplot(MT7$fitted) + autolayer(vendite1_sett_avg)
# autoplot(M3$fitted) + autolayer(train_auto)





## PROVA2- regressori: "covid_sum", "chiuso_sum" ----
MT7 <- auto.arima(vendite1_sett_avg, seasonal = TRUE, xreg = 
                    data.matrix(df7[, c("covid_sum", "chiuso_sum")]))
summary(MT7)  # AIC: 3506.24   
checkresiduals(MT7)
tsdisplay(residuals(MT7), lag.max=52, main='Seasonal Model Residuals')

# verifica p-value
valori <- MT7$coef["chiuso_sum"]/sqrt(diag(MT7$var.coef))
pvalue = 2*pt(valori["chiuso_sum"] ,222)
pvalue

valori <- MT7$coef["covid_sum"]/sqrt(diag(MT7$var.coef))
pvalue = 2*pt(valori["covid_sum"] ,222)
pvalue

# verifica adattamento modello
autoplot(MT7$fitted) + autolayer(vendite1_sett_avg)
autoplot(M3$fitted) + autolayer(train_auto)



# creazione df per previsioni ----

# colori aggiornati fino al 12 agosto 2021
# il file è nella cartella "dati"
colori_zone_aggiornato <- read_csv("colori_zone_aggiornato.csv")  # https://github.com/imcatta/restrizioni_regionali_covid/blob/main/dataset.csv

colori_emilia_romagna_new <- colori_zone_aggiornato %>% 
  filter(denominazione_regione == "Emilia-Romagna")
names(colori_emilia_romagna_new)[3] <- "colore_emilia_romagna"

reference_date_colori <- as.Date("2021-04-19", format = "%Y-%m-%d")  # 19 aprile e non 12 perchè se no dopo con le settiman è un casino. L'ultima settimana che abbiamo è quella del 12 (anche se i dati non sono completi per tutta la settimana), per i dati aggionrati devo prendere la nuova settimana del 19 aprile

colori_emilia_romagna_new <- colori_emilia_romagna_new  %>% 
  filter(data > reference_date_colori)


# creazione df (dal 12 aprile 2021 al 12 agosto 2021) 
df_dati_aggiornati <- data.frame(colori_emilia_romagna_new)  # deve essere l'analogo di df_dati_aggiornati
df_dati_aggiornati <- df_dati_aggiornati[,-2]

# colonna zona rossa
df_dati_aggiornati$rossa <- ifelse(df_dati_aggiornati$colore_emilia_romagna == "rosso", 1, 0)  # no zone rosse


# covid aggiornato fino al 12 agosto
df_dati_aggiornati$covid <- 1  # il covid è presents


# chiuso aggiornato fino al 12 agosto
df_dati_aggiornati$chiuso <- 0  # non ci sono date in cui i ristoranti avrebbero potuto chiudere


# divisione in settimane
week_new_rist1 <- as.Date(cut(df_dati_aggiornati$data, "week"))

week_rossa_new <- aggregate(rossa ~ week_new_rist1, df_dati_aggiornati, sum)  # per settimana
week_chiuso_new <- aggregate(chiuso ~ week_new_rist1, df_dati_aggiornati, sum)  # per settimana
covid_chiuso_new <- aggregate(covid ~ week_new_rist1, df_dati_aggiornati, sum)  # per settimana

df7_new <- data.frame(covid_chiuso_new$covid, week_chiuso_new$chiuso, week_rossa_new$rossa)
View(df7_new)
colnames(df7_new) <- c("covid_sum", "chiuso_sum", "rossa_sum")


# trasformazione colonne precedenti in valori binari
df7_new <- df7_new %>%
  mutate(covid_bin = ifelse(covid_sum>0, 1, 0))

df7_new <- df7_new %>%
  mutate(rossa_bin = ifelse(rossa_sum>4, 1, 0))

df7_new <- df7_new %>%
  mutate(chiuso_bin = ifelse(chiuso_sum>4, 1, 0))

# creazione df regressori per previsioni
df_previsioni <- rbind(df7, df7_new)

# previsione vendite settimanali su dati nuovi - DA RIVEDERE
forecast <- MT7 %>%
  forecast(h=3,  xreg =data.matrix(df_previsioni[, c("chiuso_sum","rossa_sum", "covid_bin")])) 

autoplot(forecast)
