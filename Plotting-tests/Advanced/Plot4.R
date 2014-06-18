library(ggplot2)
library(plyr)
## Read in the Data
NEI <- readRDS("./data/summarySCC_PM25.rds")
SCC <- readRDS("./data/Source_Classification_Code.rds")

## Get coal combustion-related data
coalData <- SCC[grepl("coal", SCC$Short.Name, ignore.case=TRUE), ]

## Merge coalData with NEI using the SCC ID
mergedData <- merge(NEI, coalData, by = "SCC")

## Summarize the data by Year
coalDataByYear <- ddply(mergedData, .(year), summarize,
                        Total= sum(Emissions))

##Plot the data to PNG file
png(file = "Plot4.png")
p <- ggplot(coalDataByYear, aes(year, Total/1000))
p + geom_line(aes(col = Total)) + 
        geom_point(aes(col = Total)) +
        ggtitle(expression('Total Coal Combustion-related Emissions of PM'[2.5])) + 
        ylab(expression(paste('PM'[2.5], ' Emissions in Kilotons'))) +
        xlab("Year")
dev.off()