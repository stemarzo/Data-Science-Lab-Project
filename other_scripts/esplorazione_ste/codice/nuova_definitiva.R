# setting librerie
library(readxl)
library(readr)
library(forecast)
library(dplyr)
library(magrittr)
library(ggplot2)
library(forcats)
library(lubridate)
library(RQuantLib)
library(devtools)
library(dplyr)
library(patchwork)

ristorazione_original <- read_excel("C:/Users/Stefano/Documents/progetto_dslab/dati/Ristorazione.xls")

source("C:/Users/Stefano/Desktop/ds_lab/sistemazione_dataset.R")
source("C:/Users/Stefano/Desktop/ds_lab/creazione_ristoranti.R")



#### ESPLORAZIONE #### 

#### STUDIO PRIMO RISTORANTE #### 
#### DA AGGIUNGERE RISTORANTE 4 5 6 E IL 3 BISOGNA TOGLIERE GLI ANNI INZIALI ####

source("C:/Users/Stefano/Desktop/ds_lab/esplorazione1.R")
#source("C:/Users/Stefano/Desktop/ds_lab/esplorazione2.R")
#source("C:/Users/Stefano/Desktop/ds_lab/esplorazione3.R")

#### DECOMPOSIZIONE ####

source("C:/Users/Stefano/Desktop/ds_lab/decomposizione1.R")
#source("C:/Users/Stefano/Desktop/decomposizione2.R")
#source("C:/Users/Stefano/Desktop/decomposizione3.R")



#### PRE COVID ####
#### ESPLORAZIONE #### 

#### STUDIO PRIMO RISTORANTE #### 
#### DA AGGIUNGERE RISTORANTE 4 5 6 E IL 3 BISOGNA TOGLIERE GLI ANNI INZIALI ####

source("C:/Users/Stefano/Desktop/ds_lab/esplorazione1_pre.R")
#source("C:/Users/Stefano/Desktop/ds_lab/esplorazione2.R")
#source("C:/Users/Stefano/Desktop/ds_lab/esplorazione3.R")

#### DECOMPOSIZIONE ####

source("C:/Users/Stefano/Desktop/ds_lab/decomposizione1_pre.R")
#source("C:/Users/Stefano/Desktop/decomposizione2.R")
#source("C:/Users/Stefano/Desktop/decomposizione3.R")




