## Ristorante6 ----

# estati ristorante 6
ristorante6_estate_2019 <- subset(ristorante6, data >= inizio_estate_2019 & data <= fine_estate_2019)
ristorante6_estate_2019 <- ristorante6_estate_2019[,-2]

ristorante6_estate_2020 <- subset(ristorante6, data >= inizio_estate_2020 & data <= fine_estate_2020)
ristorante6_estate_2020 <- ristorante6_estate_2020[,-2]

ristorante6_unione_2019_2020 <- rbind(ristorante6_estate_2019, ristorante6_estate_2020)

ristorante6_unione_2019_2020$Year <- format(ristorante6_unione_2019_2020$data, "%Y")
ristorante6_unione_2019_2020$Month <- format(ristorante6_unione_2019_2020$data, "%b")
ristorante6_unione_2019_2020$Day <- format(ristorante6_unione_2019_2020$data, "%d")

ristorante6_unione_2019_2020$MonthDay <- format(ristorante6_unione_2019_2020$data, "%d-%b")
ristorante6_unione_2019_2020$MonthDay2 <- ristorante6_unione_2019_2020$MonthDay

ristorante6_unione_2019_2020$MonthDay2[as.numeric(row.names(ristorante6_unione_2019_2020))%%3!=0] <- ""
labels <- ristorante6_unione_2019_2020$MonthDay2

p <- ggplot(data=ristorante6_unione_2019_2020, mapping=aes(x=MonthDay, y=vendite, shape=Year, color=Year)) + geom_point() +geom_line(aes(group = 1))
p <- p + facet_grid(facets = Year ~ ., margins = FALSE) + theme_bw()
print(
  p + scale_y_continuous() + scale_x_discrete(labels=labels) + 
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, size = 8)) +
    ggtitle("Ristorante 6: confronto estati")
)


# decomposizione estate 2019 e estate 2020 
estate_2019_ristorante6 <- ts(ristorante6_estate_2019$vendite, start=c(2019,06,21), frequency = 7)
multi_vendite6_dec <- stl(estate_2019_ristorante6, s.window = "periodic")
print(autoplot(multi_vendite6_dec) + ggtitle("Ristorante 6: Decomposizione estate 2019"))

estate_2020_ristorante6 <- ts(ristorante6_estate_2020$vendite, start=c(2020,06,21), frequency = 7)
multi_vendite6_dec <- stl(estate_2020_ristorante6, s.window = "periodic")
print(autoplot(multi_vendite6_dec) + ggtitle("Ristorante 6: Decomposizione estate 2020"))
