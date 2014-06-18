library(ggplot2)
library(plyr)
## Read in the Data
NEI <- readRDS("./data/summarySCC_PM25.rds")
SCC <- readRDS("./data/Source_Classification_Code.rds")

## Get Vehicle Data (ON-ROAD) for Baltimore and Summarize
BaltimoreRoad <- subset(NEI, fips == "24510" & type == "ON-ROAD")
BaltimoreSum <- ddply(BaltimoreRoad, .(year), summarize,
                      Total = sum(Emissions))
BaltimoreSum$City <- rep("Baltimore", 4)

## Get Vehicle Data (ON-ROAD) for Baltimore and Summarize
LARoad <- subset(NEI, fips == "06037" & type == "ON-ROAD")
LASum <- ddply(LARoad, .(year), summarize, Total = sum(Emissions))
LASum$City <- rep("LA County", 4)

## Combine the two datasets
combined <- rbind(BaltimoreSum, LASum)

## Plot to PNG file
png(file = "Plot6.png")
p <- qplot(factor(year), data = combined, geom="bar", fill = City,
      weight = Total, position = "dodge")
p + ggtitle("Total Motor Vehicle Emissions") + 
        ylab(expression(paste('PM'[2.5], ' Emissions'))) +
        xlab("Year")
dev.off()
