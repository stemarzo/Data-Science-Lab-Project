library(readxl)
library(readr)

Ristorazione <- read_excel("C:/Users/Stefano/Desktop/dslab/Ristorazione.xls")
Ristorazione<-Ristorazione[-c(1),-c(2,3,4,5,7,10)]
colnames(Ristorazione)[1]<- "data"
colnames(Ristorazione)[2]<- "data_anno_prec"
colnames(Ristorazione)[3]<- "vendite1"
colnames(Ristorazione)[4]<- "scontrini1"
colnames(Ristorazione)[5]<- "vendite2"
colnames(Ristorazione)[6]<- "scontrini2"
colnames(Ristorazione)[7]<- "vendite3"
colnames(Ristorazione)[8]<- "scontrini3"
colnames(Ristorazione)[9]<- "vendite4"
colnames(Ristorazione)[10]<- "scontrini4"
colnames(Ristorazione)[11]<- "vendite5"
colnames(Ristorazione)[12]<- "scontrini5"
colnames(Ristorazione)[13]<- "vendite6"
colnames(Ristorazione)[14]<- "scontrini6"

Ristorazione$vendite1 <- as.numeric(Ristorazione$vendite1)
Ristorazione$scontrini1 <- as.numeric(Ristorazione$scontrini1)
Ristorazione$vendite2 <- as.numeric(Ristorazione$vendite2)
Ristorazione$scontrini2 <- as.numeric(Ristorazione$scontrini2)
Ristorazione$vendite3 <- as.numeric(Ristorazione$vendite3)
Ristorazione$scontrini3 <- as.numeric(Ristorazione$scontrini3)
Ristorazione$vendite4 <- as.numeric(Ristorazione$vendite4)
Ristorazione$scontrini4 <- as.numeric(Ristorazione$scontrini4)
Ristorazione$vendite5 <- as.numeric(Ristorazione$vendite5)
Ristorazione$scontrini5 <- as.numeric(Ristorazione$scontrini5)
Ristorazione$vendite6 <- as.numeric(Ristorazione$vendite6)
Ristorazione$scontrini6 <- as.numeric(Ristorazione$scontrini6)

Ristorazione["giorno"] <- substr(Ristorazione$data, 1, 2)
Ristorazione$data <- substr(Ristorazione$data, 3, 100)
Ristorazione$data <- parse_date(Ristorazione$data, "%d %b %Y", locale = locale("it"))
Ristorazione$data_anno_prec <- parse_date(Ristorazione$data_anno_prec, "%d %b %Y", locale = locale("it"))
View(Ristorazione)

ristorante1 <- data.frame(Ristorazione$data, Ristorazione$data_anno_prec,Ristorazione$giorno, Ristorazione$vendite1, Ristorazione$scontrini1)
colnames(ristorante1)[1]<- "data"
colnames(ristorante1)[2]<- "data_anno_prec"
colnames(ristorante1)[3]<- "giorno"
colnames(ristorante1)[4]<- "vendite"
colnames(ristorante1)[5]<- "scontrini"

ristorante2 <- data.frame(Ristorazione$data, Ristorazione$data_anno_prec,Ristorazione$giorno, Ristorazione$vendite2, Ristorazione$scontrini2)
colnames(ristorante2)[1]<- "data"
colnames(ristorante2)[2]<- "data_anno_prec"
colnames(ristorante2)[3]<- "giorno"
colnames(ristorante2)[4]<- "vendite"
colnames(ristorante2)[5]<- "scontrini"

ristorante3 <- data.frame(Ristorazione$data, Ristorazione$data_anno_prec,Ristorazione$giorno, Ristorazione$vendite3, Ristorazione$scontrini3)
colnames(ristorante3)[1]<- "data"
colnames(ristorante3)[2]<- "data_anno_prec"
colnames(ristorante3)[3]<- "giorno"
colnames(ristorante3)[4]<- "vendite"
colnames(ristorante3)[5]<- "scontrini"

ristorante4 <- data.frame(Ristorazione$data, Ristorazione$data_anno_prec,Ristorazione$giorno, Ristorazione$vendite4, Ristorazione$scontrini4)
colnames(ristorante4)[1]<- "data"
colnames(ristorante4)[2]<- "data_anno_prec"
colnames(ristorante4)[3]<- "giorno"
colnames(ristorante4)[4]<- "vendite"
colnames(ristorante4)[5]<- "scontrini"

ristorante5 <- data.frame(Ristorazione$data, Ristorazione$data_anno_prec,Ristorazione$giorno, Ristorazione$vendite5, Ristorazione$scontrini5)
colnames(ristorante5)[1]<- "data"
colnames(ristorante5)[2]<- "data_anno_prec"
colnames(ristorante5)[3]<- "giorno"
colnames(ristorante5)[4]<- "vendite"
colnames(ristorante5)[5]<- "scontrini"


ristorante6 <- data.frame(Ristorazione$data, Ristorazione$data_anno_prec,Ristorazione$giorno, Ristorazione$vendite6, Ristorazione$scontrini6)
colnames(ristorante6)[1]<- "data"
colnames(ristorante6)[2]<- "data_anno_prec"
colnames(ristorante6)[3]<- "giorno"
colnames(ristorante6)[4]<- "vendite"
colnames(ristorante6)[5]<- "scontrini"

rist <- ristorante1[1:365, ]
plot(rist$data, rist$vendite, xlab = "data", ylab = "vendite", type="l")
abline(h=mean(as.integer(rist$vendite)))

summary(Ristorazione)



