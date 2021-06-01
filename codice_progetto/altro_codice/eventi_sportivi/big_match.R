setwd('C:/Users/stefano/Desktop/dslab')
library(lubridate)
library(chron)

stagione_16_17<-read.csv('stagione_17.csv', header=T, sep=',')
stagione_17_18<-read.csv('stagione_18.csv', header=T, sep=',')
stagione_18_19<-read.csv('stagione_19.csv', header=T, sep=',')
stagione_19_20<-read.csv('stagione_20.csv', header=T, sep=',')
stagione_20_21<-read.csv('stagione_21.csv', header=T, sep=',')

partite <- rbind(stagione_16_17,stagione_17_18,stagione_18_19,stagione_19_20,stagione_20_21)
partite$data<-dmy(partite$data)
partite$ora <- times(paste(partite$ora, "00", sep=":"))
partite <- partite[partite[["ora"]] >= "19:00:00", ]
partite <- partite[partite[["data"]] >= "2017-01-01", ]
partite <- partite[partite[["data"]] <= "2021-04-21", ]

partite$milan <- ifelse((partite$squadra1 == "Milan") | (partite$squadra2 == "Milan"), 1, 0)
partite$juventus <- ifelse((partite$squadra1 == "Juventus") | (partite$squadra2 == "Juventus"), 1, 0)
partite$inter <- ifelse((partite$squadra1 == "Inter") | (partite$squadra2 == "Inter"), 1, 0)
partite$emilia <- ifelse(((partite$squadra1 == "Sassuolo") | (partite$squadra2 == "Sassuolo")
                          |(partite$squadra1 == "Parma") | (partite$squadra2 == "Parma")
                          |(partite$squadra1 == "Bologna") | (partite$squadra2 == "Bologna")
                          |(partite$squadra1 == "Spal") | (partite$squadra2 == "Spal")), 1, 0)
partite$top <- ifelse(((partite$squadra1 == "Milan") & (partite$squadra2 == "Juventus")) |
                        ((partite$squadra1 == "Juventus") & (partite$squadra2 == "Milan")) |
                        ((partite$squadra1 == "Juventus") & (partite$squadra2 == "Inter")) |
                        ((partite$squadra1 == "Inter") & (partite$squadra2 == "Juventus")) |
                        ((partite$squadra1 == "Milan") & (partite$squadra2 == "Inter")) |
                        ((partite$squadra1 == "Inter") & (partite$squadra2 == "Milan")), 1, 0)

partite <- partite[, c("data", "sky", "mediaset", "dazn" ,"top", "milan", "inter", "juventus", "emilia")]
partite<-aggregate(partite[c("sky", "mediaset", "dazn", "top", "milan", "inter", "juventus", "emilia")], by=partite["data"], sum)
finale <- data.frame(seq(as.Date('2017-01-01'), as.Date('2021-04-21'), by = 'days'))
colnames(finale) <- c("data")
finale <-merge(x = finale, y = partite, by = "data", all.x = TRUE)
finale[is.na(finale)]<-0

write.csv2(finale,"calcio.csv")
