library(ggplot2)
library(plyr)
## Read in the Data
NEI <- readRDS("./data/summarySCC_PM25.rds")
SCC <- readRDS("./data/Source_Classification_Code.rds")

## Summarize the NEI Emmissions data by year for Balimore
BaltimoreCity <- subset(NEI, fips == 24510)

Emissions <- ddply(BaltimoreCity, .(year, type), summarize,
                         sum = sum(Emissions))

## Plot to PNG file
png(file = "Plot3.png", width = 640, height = 480)
p <- ggplot(Emissions, aes(year,sum))
p + geom_point() + 
        facet_grid(.~type) + 
        ggtitle("Emissions per type in Baltimore City") + 
        ylab(expression(paste('Total', ' PM'[2.5], ' Emissions'))) + 
        xlab('Year')
dev.off()