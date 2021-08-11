## Ristorante1 ----

# estati ristorante 1
ristorante1_estate_2019 <- subset(ristorante1, data >= inizio_estate_2019 & data <= fine_estate_2019)
ristorante1_estate_2019 <- ristorante1_estate_2019[,-2]

ristorante1_estate_2020 <- subset(ristorante1, data >= inizio_estate_2020 & data <= fine_estate_2020)
ristorante1_estate_2020 <- ristorante1_estate_2020[,-2]

ristorante1_unione_2019_2020 <- rbind(ristorante1_estate_2019, ristorante1_estate_2020)

ristorante1_unione_2019_2020$Year <- format(ristorante1_unione_2019_2020$data, "%Y")
ristorante1_unione_2019_2020$Month <- format(ristorante1_unione_2019_2020$data, "%b")
ristorante1_unione_2019_2020$Day <- format(ristorante1_unione_2019_2020$data, "%d")

ristorante1_unione_2019_2020$MonthDay <- format(ristorante1_unione_2019_2020$data, "%d-%b")
ristorante1_unione_2019_2020$MonthDay2 <- ristorante1_unione_2019_2020$MonthDay

ristorante1_unione_2019_2020$MonthDay2[as.numeric(row.names(ristorante1_unione_2019_2020))%%3!=0] <- ""
labels <- ristorante1_unione_2019_2020$MonthDay2

p <- ggplot(data=ristorante1_unione_2019_2020, mapping=aes(x=MonthDay, y=vendite, shape=Year, color=Year)) + geom_point() +geom_line(aes(group = 1))
p <- p + facet_grid(facets = Year ~ ., margins = FALSE) + theme_bw()
print(
  p + scale_y_continuous() + scale_x_discrete(labels=labels) + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, size = 8)) +
  ggtitle("Ristorante 1: confronto estati")
)


# decomposizione estate 2019 e estate 2020 
estate_2019_ristorante1 <- ts(ristorante1_estate_2019$vendite, start=c(2019,06,21), frequency = 7)
multi_vendite1_dec <- stl(estate_2019_ristorante1, s.window = "periodic")
print(autoplot(multi_vendite1_dec) + ggtitle("Ristorante 1: Decomposizione estate 2019"))

estate_2020_ristorante1 <- ts(ristorante1_estate_2020$vendite, start=c(2020,06,21), frequency = 7)
multi_vendite1_dec <- stl(estate_2020_ristorante1, s.window = "periodic")
print(autoplot(multi_vendite1_dec) + ggtitle("Ristorante 1: Decomposizione estate 2020"))