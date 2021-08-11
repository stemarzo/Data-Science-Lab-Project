## Ristorante2 ----

# estati ristorante 2
ristorante2_estate_2019 <- subset(ristorante2, data >= inizio_estate_2019 & data <= fine_estate_2019)
ristorante2_estate_2019 <- ristorante2_estate_2019[,-2]

ristorante2_estate_2020 <- subset(ristorante2, data >= inizio_estate_2020 & data <= fine_estate_2020)
ristorante2_estate_2020 <- ristorante2_estate_2020[,-2]

ristorante2_unione_2019_2020 <- rbind(ristorante2_estate_2019, ristorante2_estate_2020)

ristorante2_unione_2019_2020$Year <- format(ristorante2_unione_2019_2020$data, "%Y")
ristorante2_unione_2019_2020$Month <- format(ristorante2_unione_2019_2020$data, "%b")
ristorante2_unione_2019_2020$Day <- format(ristorante2_unione_2019_2020$data, "%d")

ristorante2_unione_2019_2020$MonthDay <- format(ristorante2_unione_2019_2020$data, "%d-%b")
ristorante2_unione_2019_2020$MonthDay2 <- ristorante2_unione_2019_2020$MonthDay

ristorante2_unione_2019_2020$MonthDay2[as.numeric(row.names(ristorante2_unione_2019_2020))%%3!=0] <- ""
labels <- ristorante2_unione_2019_2020$MonthDay2

p <- ggplot(data=ristorante2_unione_2019_2020, mapping=aes(x=MonthDay, y=vendite, shape=Year, color=Year)) + geom_point() +geom_line(aes(group = 1))
p <- p + facet_grid(facets = Year ~ ., margins = FALSE) + theme_bw()
print(
  p + scale_y_continuous() + scale_x_discrete(labels=labels) + 
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, size = 8)) +
    ggtitle("Ristorante 2: confronto estati")
)


# decomposizione estate 2019 e estate 2020 
estate_2019_ristorante2 <- ts(ristorante2_estate_2019$vendite, start=c(2019,06,21), frequency = 7)
multi_vendite2_dec <- stl(estate_2019_ristorante2, s.window = "periodic")
print(autoplot(multi_vendite2_dec) + ggtitle("Ristorante 2: Decomposizione estate 2019"))

estate_2020_ristorante2 <- ts(ristorante2_estate_2020$vendite, start=c(2020,06,21), frequency = 7)
multi_vendite2_dec <- stl(estate_2020_ristorante2, s.window = "periodic")
print(autoplot(multi_vendite2_dec) + ggtitle("Ristorante 2: Decomposizione estate 2020"))
