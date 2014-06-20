## Summarize the Top 10 with the most impact to Population Health
PopHealthData[order(PopHealthData$Total, decreasing = TRUE)[1:5], ]
EconomicData[order(EconomicData$Total, decreasing = TRUE)[1:5], ]


## Prepare the Population Data for plotting
Plot1Data <- melt(PopHealthData[, 1:3], id = "Type")
