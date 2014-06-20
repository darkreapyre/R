## Summarize the Top 10 with the most impact to Population Health
PopHealthTop10 <- PopHealthData[order(PopHealthData$Total, decreasing = TRUE)[1:10], ]
EconomicTop10 <- EconomicData[order(EconomicData$Total, decreasing = TRUE)[1:10], ]

## Plot the Population Health Top5
with(PopHealthTop10, barplot(rbind(Fatalities, Injuries), names.arg=Type,
                        cex.names = 0.7, beside = TRUE,
                        col = c("red", "blue"),
                        legend = c("Fatalities", "Injuries"),
                        xlab = "Event Type", ylab = "Total"
                        main = "Impact of Severe Storms to the Population Health"))

## Plot the Economic Top5
with(EconomicTop10, barplot(rbind(Property, Crops)/1000000,
                           names.arg = Type, cex.names = 0.7,
                           beside = TRUE, col = c("green", "orange"),
                           legend = c("Property Damage", "Crop Damage"),
                           xlab = "Event Type", ylab = "Damage Costs (Million $)",
                           main = "Impact of Severe Storms to the Economy"))

##Summarise (Tornado)
TornadoDeath <- as.numeric(PopHealthTop10$Total[[1]])/as.numeric(sum(PopHealthData$Total))*100
TornadoDamage <- as.numeric(EconomicTop10$Total[[1]])/as.numeric(sum(EconomicData$Total))*100