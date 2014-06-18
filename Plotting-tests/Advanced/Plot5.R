library(ggplot2)
library(plyr)
## Read in the Data
NEI <- readRDS("./data/summarySCC_PM25.rds")
SCC <- readRDS("./data/Source_Classification_Code.rds")

## Get Vehicle Data (ON-ROAD) for Baltimore
BaltimoreRoad <- subset(NEI, fips == 24510 & type == "ON-ROAD")

##Summarize by Year
Emissions <- ddply(BaltimoreRoad, .(year), summarize,
                   Total = sum(Emissions))

##Plot the data to PNG file
png(file = "Plot5.png")
p <- ggplot(Emissions, aes(year, Total))
p + geom_line(aes(col = Total)) + 
        geom_point(aes(col = Total)) +
        ggtitle("Total Motor Vehicle Emissions in Baltimore") + 
        ylab(expression(paste('PM'[2.5], ' Emissions'))) +
        xlab("Year")
dev.off()