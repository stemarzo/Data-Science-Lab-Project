## Ristorante5 ----

# estati ristorante 5
ristorante5_estate_2019 <- subset(ristorante5, data >= inizio_estate_2019 & data <= fine_estate_2019)
ristorante5_estate_2019 <- ristorante5_estate_2019[,-2]

ristorante5_estate_2020 <- subset(ristorante5, data >= inizio_estate_2020 & data <= fine_estate_2020)
ristorante5_estate_2020 <- ristorante5_estate_2020[,-2]

ristorante5_unione_2019_2020 <- rbind(ristorante5_estate_2019, ristorante5_estate_2020)

ristorante5_unione_2019_2020$Year <- format(ristorante5_unione_2019_2020$data, "%Y")
ristorante5_unione_2019_2020$Month <- format(ristorante5_unione_2019_2020$data, "%b")
ristorante5_unione_2019_2020$Day <- format(ristorante5_unione_2019_2020$data, "%d")

ristorante5_unione_2019_2020$MonthDay <- format(ristorante5_unione_2019_2020$data, "%d-%b")
ristorante5_unione_2019_2020$MonthDay2 <- ristorante5_unione_2019_2020$MonthDay

ristorante5_unione_2019_2020$MonthDay2[as.numeric(row.names(ristorante5_unione_2019_2020))%%3!=0] <- ""
labels <- ristorante5_unione_2019_2020$MonthDay2

p <- ggplot(data=ristorante5_unione_2019_2020, mapping=aes(x=MonthDay, y=vendite, shape=Year, color=Year)) + geom_point() +geom_line(aes(group = 1))
p <- p + facet_grid(facets = Year ~ ., margins = FALSE) + theme_bw()
print(
  p + scale_y_continuous() + scale_x_discrete(labels=labels) + 
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, size = 8)) +
    ggtitle("Ristorante 5: confronto estati")
)


# decomposizione estate 2019 e estate 2020 
estate_2019_ristorante5 <- ts(ristorante5_estate_2019$vendite, start=c(2019,06,21), frequency = 7)
multi_vendite5_dec <- stl(estate_2019_ristorante5, s.window = "periodic")
print(autoplot(multi_vendite5_dec) + ggtitle("Ristorante 5: Decomposizione estate 2019"))

estate_2020_ristorante5 <- ts(ristorante5_estate_2020$vendite, start=c(2020,06,21), frequency = 7)
multi_vendite5_dec <- stl(estate_2020_ristorante5, s.window = "periodic")
print(autoplot(multi_vendite5_dec) + ggtitle("Ristorante 5: Decomposizione estate 2020"))
