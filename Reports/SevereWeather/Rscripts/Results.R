## Prepare the Population Data for plotting
library(ggplot2)
library(reshape2)
Plot1Data <- melt(PopHealthData[, 1:3], id.vars = 1)
