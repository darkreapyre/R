## Read in the Data
NEI <- readRDS("./data/summarySCC_PM25.rds")
SCC <- readRDS("./data/Source_Classification_Code.rds")

## Summarize the NEI Emmissions data by year
EmissionsByYear <- tapply(NEI$Emissions, NEI$year, sum)
## Make the result readable (kilotons)
EmissionsByYear <- EmissionsByYear/(10^6)

## Plot to PNG file
png(file = "Plot1.png")
plot(EmissionsByYear, type = "b", ann = "FALSE", axes = "FALSE", pch = 20)
title(main = expression('Total PM'[2.5]*' Emissions (1999 to 2008)'),
      xlab = 'Year', ylab = expression('Total Emissions in Kilotons'))
axis(1, at = c(1:4), labels = names(EmissionsByYear))
axis(2, at = c(1:8))
dev.off()