## Initialize the environment
library(caret) ## Hide output
if (!file.exists("data")) {
        dir.create("data")
}

## Download the Data
trainURL <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv"
testURL <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv"
download.file(trainURL, "./data/train.csv", method = "curl")
download.file(testURL, "./data/test.csv", method = "curl")
downloadDate <- date()

## Read the file into R
traindf <- read.csv("./data/train.csv")
testdf <- read.csv("./data/test.csv")

## Subset the data for model testing/validation (60/40)
set.seed(6969)
subSet <- createDataPartition(y = traindf$classe, p = 0.6, list = FALSE)
trainSet <- traindf[subSet, ]
valSet <- traindf[-subSet, ]



## Remove colums that have NA values
naVals <- apply(is.na(trainSet), 2, any)
trainSet <- trainSet[, !naVals]

## Potentially add the Near Zero Variance routine

## Potentiall Exploratory data Plot With NA
totals <- c("total_accel_forearm", "total_accel_arm", "total_accel_belt",
            "total_accel_dumbbell")
featurePlot(x = trainSet[, totals],
            y = trainSet$classe,
            plot = "density",
            scales = list(x = list(relation = "free"),
                          y = list(relation = "free")),
            adjust = 1.5,
            pch = "|",
            layout = c(2, 2),
            auto.key = list(columns = 4))

featurePlot(x = trainSet,
            y = trainSet$classe,
            plot = "ellipse")

