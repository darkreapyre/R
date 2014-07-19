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
