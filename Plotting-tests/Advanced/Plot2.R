## Read in the Data
NEI <- readRDS("./data/summarySCC_PM25.rds")
SCC <- readRDS("./data/Source_Classification_Code.rds")

## Summarize the NEI Emmissions data by year for Balimore
BaltimoreCity <- subset(NEI, fips == 24510)
EmissionsByYear <- tapply(BaltimoreCity$Emissions,
                          BaltimoreCity$year, sum)
## Make the result readable (kilotons)
EmissionsByYear <- EmissionsByYear/(10^3)

## Plot to PNG file
png(file = "Plot2.png")
plot(EmissionsByYear, type = "b", ann = "FALSE", xaxt = "n", pch = 20)
lines(EmissionsByYear)
title(main = expression('Total PM'[2.5]*' Emissions for Baltimore City (1999 - 2008)'),
      xlab = 'Year', ylab = expression('Total Emissions in Kilotons'))
axis(1, at = c(1:4), labels = names(EmissionsByYear))
dev.off()