## Summary of the Top 10 events impacting Population Health
HealthTop10 <- PopHealthData[order(PopHealthData$Total, decreasing = TRUE)[1:10], ]

## Summary of the Top 1- events impacting the Economy
EconomicTop10 <- EconomicData[order(EconomicData$Total, decreasing = TRUE)[1:10], ]

## Show this via a Plot
require(plyr)
require(reshape2)
require(ggplot2)
## Prepare the data for plotting (reformatting required for qplot)
tmp1Data <- ddply(HealthTop10, .(Injuries, Fatalities), summarize,
           Total = Injuries + Fatalities)
tmp1Data <- tmp1Data[order(tmp1Data$Total, decreasing = TRUE), ]
tmp1Data$Type <- HealthTop10$Type
Plot1Data <- melt(tmp1Data, id.vars = "Type")
p1 <- qplot(x = Type, y = value, fill = variable,
           data = Plot1Data, geom = "bar", stat = "identity",
           position = "dodge") + xlab("Event Type") + ylab("Total") + 
        ggtitle("Impact of Severe Storms to the Population Health") +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))

## Apply same methadology for Ecomonic Data
tmp2Data <- ddply(EconomicTop10, .(Property, Crops), summarize,
                  Total = Property + Crops)
tmp2Data <- tmp2Data[order(tmp2Data$Total, decreasing = TRUE), ]
tmp2Data$Type <- EconomicTop10$Type
Plot2Data <- melt(tmp2Data, id.vars = "Type")
p2 <- qplot(x = Type, y = value/1000000, fill = variable,
           data = Plot2Data, geom = "bar", stat = "identity",
           position = "dodge") + xlab("Event Type") + 
        ylab("Damage Costs (Million $)") + 
        ggtitle("Impact of Severe Storms to the Economy") + 
        theme(axis.text.x = element_text(angle = 45, hjust = 1))




##              OLD

## Summarize the Top 10 with the most impact to Population Health
##PopHealthTop10 <- PopHealthData[order(PopHealthData$Total, decreasing = TRUE)[1:10], ]
##EconomicTop10 <- EconomicData[order(EconomicData$Total, decreasing = TRUE)[1:10], ]

## Plot the Population Health Top5
##with(PopHealthTop10, barplot(rbind(Fatalities, Injuries), names.arg = Type,
##                        cex.names = 0.7, beside = TRUE,
##                        col = c("red", "blue"),
##                        legend = c("Fatalities", "Injuries"),
##                        xlab = "Event Type", ylab = "Total",
##                        main = "Impact of Severe Storms to the Population Health"))

## Alternative 
##require(ggplot2)
##ggplot(PopHealthTop10, aes(x = Type, y = Total)) +
##        geom_bar(stat = "identity", fill = "red")



## Plot the Economic Top5
##with(EconomicTop10, barplot(rbind(Property, Crops)/1000000,
##                           names.arg = Type, cex.names = 0.5,
##                           beside = TRUE, col = c("green", "orange"),
##                           legend = c("Property Damage", "Crop Damage"),
##                           xlab = "Event Type", ylab = "Damage Costs (Million $)",
##                           main = "Impact of Severe Storms to the Economy"))

##Summarise (Tornado)
TornadoDeath <- as.numeric(HealthTop10$Total[[1]])/as.numeric(sum(PopHealthData$Total))*100
TornadoDamage <- as.numeric(EconomicTop10$Total[[1]])/as.numeric(sum(EconomicData$Total))*100